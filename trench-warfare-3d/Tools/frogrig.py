# Blender (background): rig the owner's three-LOD Tripo frog (2026-09-26: Downloads/frog+warrior+3d+model (1).zip =
# LOD0, frog+knight = LOD1, frog+warrior = LOD2; the same T-posed design at 4.6k / 2.7k / 1.1k tris), make a fourth,
# much simpler LOD3 for far away, and export ONE skinned FBX: one armature, four meshes Frog_LOD0..3.
#
# The skeleton uses Mixamo's bone names and hierarchy (mixamorig:Hips ...), so Unity's Humanoid auto-mapper takes it
# and the game's Mixamo clips retarget onto it; the rolls are Blender's, which Humanoid does not care about.
# Weights: LOD0 is skinned by Blender's bone heat (automatic weights); any vertex the heat solve leaves empty gets a
# geometric weight (nearest bone segments, same side only). LOD1..3 do NOT get their own solve: their weights are
# TRANSFERRED from LOD0's surface (nearest face, interpolated), so a point on the frog bends the same way at every
# LOD and the silhouette does not change when the LOD switches mid-animation. Then the simpler LODs get simpler rigs:
#   LOD0, LOD1  23 bones, 4 per vertex
#   LOD2        18 bones (spine1 -> spine/chest, neck -> head, toes -> feet), 2 per vertex
#   LOD3        13 bones (spine -> chest, hands -> forearms, feet -> shins), 2 per vertex, on a mesh decimated from
#               LOD2 to ~TW_LOD3_TRIS triangles. (TW_LOD3_RIGID=1 makes it 1 per vertex, split into rigid segments:
#               tried, it opens gaps and shards at every bent joint.)
# Joints are measured off LOD0 in the model frame (front -Y, left +X, feet on z = 0, height ~0.707) and scaled with it.
#
# usage: blender -b --factory-startup -P frogrig.py -- <lod0.fbx> <lod1.fbx> <lod2.fbx> <out.fbx> <renderdir>
#   textures beside each fbx as <stem>_tex0_0.jpg; the out dir also gets <NAME>_LOD<k>_Base.jpg and frogrig.json
import bpy, bmesh, sys, os, math, json, shutil
from mathutils import Vector, Matrix
argv = sys.argv[sys.argv.index("--") + 1:]
SRC, OUT, RENDERDIR = argv[0:3], argv[3], argv[4]
NAME = os.path.splitext(os.path.basename(OUT))[0]   # the playground folder and file name: Art/Units/<NAME>/<NAME>.fbx
os.makedirs(os.path.dirname(OUT), exist_ok=True); os.makedirs(RENDERDIR, exist_ok=True)
HEIGHT = float(os.environ.get("TW_HEIGHT", "1.78"))       # metres: VATBaker normalises its men to 1.78 m
LOD3_TRIS = int(os.environ.get("TW_LOD3_TRIS", "300"))   # measured, see docs/22: 219 -> pop IoU 0.82, 300 -> 0.85, 380 -> 0.87

# ------------------------------------------------------------------------------------------------ the skeleton
# (name, parent, head, tail) in the model frame. L = +X.
J = {
    "Hips":      ((0, 0.005, 0.235), (0, 0.005, 0.29)),
    "Spine":     ((0, 0.005, 0.29), (0, 0.005, 0.35)),
    "Spine1":    ((0, 0.005, 0.35), (0, 0.005, 0.41)),
    "Spine2":    ((0, 0.005, 0.41), (0, 0.005, 0.49)),
    "Neck":      ((0, 0.005, 0.49), (0, 0.0, 0.525)),
    "Head":      ((0, 0.0, 0.525), (0, 0.0, 0.66)),
    "HeadTop_End": ((0, 0.0, 0.66), (0, 0.0, 0.71)),
}
SIDE = {   # left side; the right mirrors x
    "Shoulder":  ((0.045, 0.005, 0.455), (0.145, 0.01, 0.44), "Spine2"),
    "Arm":       ((0.145, 0.01, 0.44), (0.285, 0.018, 0.435), "Shoulder"),
    "ForeArm":   ((0.285, 0.018, 0.435), (0.405, 0.005, 0.432), "Arm"),
    "Hand":      ((0.405, 0.005, 0.432), (0.49, 0.005, 0.436), "ForeArm"),
    "UpLeg":     ((0.078, 0.01, 0.215), (0.086, 0.0, 0.12), "Hips"),
    "Leg":       ((0.086, 0.0, 0.12), (0.088, 0.025, 0.048), "UpLeg"),
    "Foot":      ((0.088, 0.025, 0.048), (0.095, -0.06, 0.015), "Leg"),
    "ToeBase":   ((0.095, -0.06, 0.015), (0.097, -0.095, 0.012), "Foot"),
}
BONES = []   # (name, parent, head, tail)
par = {"Hips": None, "Spine": "Hips", "Spine1": "Spine", "Spine2": "Spine1", "Neck": "Spine2", "Head": "Neck", "HeadTop_End": "Head"}
for n, (h, t) in J.items(): BONES.append(("mixamorig:" + n, ("mixamorig:" + par[n]) if par[n] else None, h, t))
for side, sx in (("Left", 1), ("Right", -1)):
    for n, (h, t, p) in SIDE.items():
        pn = ("mixamorig:" + side + p) if p in SIDE else ("mixamorig:" + p)
        BONES.append(("mixamorig:" + side + n, pn, (h[0] * sx, h[1], h[2]), (t[0] * sx, t[1], t[2])))
DEFORM = [b[0] for b in BONES if not b[0].endswith("_End")]

# the simpler rigs: bone -> the bone it folds into
def fold2(n):
    s = n.replace("mixamorig:", "")
    # the shoulders stay: they carry the whole arm, and folded into the upper arm they moved its outline at every switch
    # below LOD1. Measured 2026-09-27 (lodpop, worst side): 1->2 IoU 0.945 -> 0.958, block colour 8.8 -> 6.2-6.9; 2->3
    # 0.849 -> 0.855. Keeping every bone LOD2 folds gave 2->3 0.874 but LOD2 22 bones and LOD3 17: no simpler rig left.
    m = {"Spine1": "Spine2", "Neck": "Head", "LeftToeBase": "LeftFoot", "RightToeBase": "RightFoot"}
    return "mixamorig:" + m.get(s, s)
def fold3(n):
    s = fold2(n).replace("mixamorig:", "")
    m = {"Spine": "Spine2", "LeftHand": "LeftForeArm", "RightHand": "RightForeArm", "LeftFoot": "LeftLeg", "RightFoot": "RightLeg"}
    return "mixamorig:" + m.get(s, s)
RIGS = {0: (lambda n: n, 4), 1: (lambda n: n, 4), 2: (fold2, 2), 3: (fold3, 2)}

# ------------------------------------------------------------------------------------------------------ helpers
def load(fbx, name):
    before = set(bpy.data.objects)
    bpy.ops.import_scene.fbx(filepath=fbx)
    o = [x for x in bpy.data.objects if x not in before and x.type == 'MESH'][0]
    for x in bpy.context.selected_objects: x.select_set(False)
    o.select_set(True); bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    o.name = name; o.data.name = name
    stem = os.path.splitext(fbx)[0]
    img = bpy.data.images.load(stem + "_tex0_0.jpg"); img.name = name + "_Base"
    mat = bpy.data.materials.new(name + "_Mat"); mat.use_nodes = True
    t = mat.node_tree.nodes.new("ShaderNodeTexImage"); t.image = img
    mat.node_tree.links.new(t.outputs["Color"], mat.node_tree.nodes["Principled BSDF"].inputs["Base Color"])
    o.data.materials.clear(); o.data.materials.append(mat)
    return o, stem

def select_only(objs, active):
    for x in bpy.context.selected_objects: x.select_set(False)
    for x in objs: x.select_set(True)
    bpy.context.view_layer.objects.active = active

def seg_dist(p, a, b):
    ab = b - a; t = max(0.0, min(1.0, (p - a).dot(ab) / max(ab.length_squared, 1e-12)))
    return (a + ab * t - p).length

def weights_of(o):
    """vertex -> {group name: weight}"""
    names = {g.index: g.name for g in o.vertex_groups}
    return [{names[g.group]: g.weight for g in v.groups if g.weight > 1e-4} for v in o.data.vertices]

def write_weights(o, W):
    for g in list(o.vertex_groups): o.vertex_groups.remove(g)
    groups = {}
    for i, w in enumerate(W):
        for n, x in w.items():
            if n not in groups: groups[n] = o.vertex_groups.new(name=n)
            groups[n].add([i], x, 'REPLACE')

def side_ok(bone, x):
    """A left-side bone never moves a vertex on the right of the centre line, and the other way round."""
    if ":Left" in bone: return x > -0.004 * SC
    if ":Right" in bone: return x < 0.004 * SC
    return True

def clean(W, verts, fold, k):
    out = []
    for i, w in enumerate(W):
        m = {}
        for n, x in w.items():
            if n not in DEFORM or not side_ok(n, verts[i].co.x): continue
            f = fold(n); m[f] = m.get(f, 0.0) + x
        top = sorted(m.items(), key=lambda kv: -kv[1])[:k]
        s = sum(x for _, x in top)
        out.append({n: x / s for n, x in top} if s > 1e-6 else {})
    return out

def geometric(p, bones_ws):
    """Fallback weight for a vertex the heat solve missed: nearest segments on its own side, inverse distance^4."""
    ds = sorted(((seg_dist(p, a, b), n) for n, a, b in bones_ws if side_ok(n, p.x)))[:3]
    ws = {n: 1.0 / (d + 0.01 * SC) ** 4 for d, n in ds}
    s = sum(ws.values())
    return {n: w / s for n, w in ws.items()}


def vert_islands(o):
    me = o.data; par = list(range(len(me.vertices)))
    def find(a):
        while par[a] != a: par[a] = par[par[a]]; a = par[a]
        return a
    for e in me.edges:
        a, b = find(e.vertices[0]), find(e.vertices[1])
        if a != b: par[a] = b
    g = {}
    for v in me.vertices: g.setdefault(find(v.index), []).append(v.index)
    return sorted(g.values(), key=len, reverse=True)

def mean_weights(ws):
    m = {}
    for w in ws:
        for n, x in w.items(): m[n] = m.get(n, 0.0) + x
    s = sum(m.values())
    return {n: x / s for n, x in m.items()} if s > 1e-9 else {}

def rigid_accessories(o, W, body_min):
    """A strap, a pauldron, a rivet, the hat, the belt: every loose island smaller than body_min verts rides the body
    rigidly. It takes the mean weight of the body surface nearest its vertices (or, if it is itself part of the body
    list, keeps its own). Returns the number of islands made rigid."""
    from mathutils.kdtree import KDTree
    isl = vert_islands(o)
    body = [i for g in isl if len(g) >= body_min for i in g]
    kd = KDTree(len(body))
    for i in body: kd.insert(o.data.vertices[i].co, i)
    kd.balance()
    n = 0
    for g in isl:
        if len(g) >= body_min: continue
        near = [W[kd.find(o.data.vertices[i].co)[1]] for i in g]
        m = mean_weights(near)
        for i in g: W[i] = dict(m)
        n += 1
    return n

ARM = ("Shoulder", "Arm", "ForeArm", "Hand")
def gate(o, W):
    """What may move a vertex, read off where it stands in the T-pose (model units, x = out along the arms): far out
    on an arm only that side's arm bones; on the torso never a forearm or a hand. A transfer onto a coarse LOD otherwise
    hands an armpit vertex the chest's weights, or a flank vertex the forearm's (critic round 1: LOD3's arm flapped from
    shoulder to hip, and folded to the head in the rifle pose)."""
    n = 0
    for i, v in enumerate(o.data.vertices):
        x, z = v.co.x / SC, v.co.z / SC
        side = "Left" if x > 0 else "Right"
        ax = abs(x)
        if ax > 0.20 and z > 0.33:
            ok = lambda b: any(b == "mixamorig:" + side + a for a in ARM)
        elif ax < 0.13:
            ok = lambda b: not any(b in ("mixamorig:" + sd + "ForeArm", "mixamorig:" + sd + "Hand") for sd in ("Left", "Right"))
        else:
            continue
        w = {b: x_ for b, x_ in W[i].items() if ok(b)}
        if len(w) != len(W[i]):
            n += 1
            sm = sum(w.values())
            if sm > 1e-6: W[i] = {b: x_ / sm for b, x_ in w.items()}
            else:
                g = {b: x_ for b, x_ in geometric(v.co, [bw for bw in bones_ws if ok(bw[0])]).items()}
                W[i] = g if g else W[i]
    return n

def tunic(o, W, legs_island):
    """The tunic over the thighs leans on the hips, so a swinging leg does not tear it (big LOD2 triangles show it)."""
    lo, hi = 0.155 * SC, 0.30 * SC; n = 0
    for i in legs_island:
        z = o.data.vertices[i].co.z
        if lo < z < hi:
            k = min(1.0, (z - lo) / (0.03 * SC)) * 0.6
            w = {b: x * (1 - k) for b, x in W[i].items()}
            w["mixamorig:Hips"] = w.get("mixamorig:Hips", 0.0) + k
            W[i] = w; n += 1
    return n

def rigid_split(o, W):
    """LOD3: every face belongs to ONE bone (the heaviest over its corners) and the mesh is split where the bone
    changes, so each segment moves as a rigid piece: no stretched triangles across a joint."""
    bm = bmesh.new(); bm.from_mesh(o.data); bm.faces.ensure_lookup_table(); bm.verts.ensure_lookup_table()
    fb = {}
    for f in bm.faces:
        acc = {}
        for v in f.verts:
            for n, x in W[v.index].items(): acc[n] = acc.get(n, 0.0) + x
        fb[f.index] = max(acc, key=acc.get)
    seam = [e for e in bm.edges if len(e.link_faces) == 2 and fb[e.link_faces[0].index] != fb[e.link_faces[1].index]]
    bmesh.ops.split_edges(bm, edges=seam)
    bm.faces.ensure_lookup_table()
    bone_of_vert = {}
    for f in bm.faces:
        for v in f.verts: bone_of_vert[v] = fb[f.index]
    bm.verts.index_update()
    names = [bone_of_vert.get(v, "mixamorig:Hips") for v in bm.verts]
    bm.to_mesh(o.data); bm.free()
    return [{n: 1.0} for n in names], len(seam)

# --------------------------------------------------------------------------------------------------------- main
bpy.ops.wm.read_factory_settings(use_empty=True)
lods = []
stems = []
for k, f in enumerate(SRC):
    o, stem = load(f, "Frog_LOD%d" % k); lods.append(o); stems.append(stem)
# TW_DERIVE (default 12, the owner's choice 2026-09-27; 1 derives LOD1 only, 0 keeps Tripo's own): LOD1 and LOD2 are LOD0 decimated to Tripo's
# triangle counts, on LOD0's atlas, instead of Tripo's own sculpts. Each Tripo LOD bakes its own texture, so the same
# spot is a different colour at every level: measured 2026-09-26 the colour change inside the silhouette at each switch
# is ~20/255, four times what a one-degree turn of the camera does, and a per-LOD tint removes only its mean (~5/255).
# TW_DERIVE=1 derives LOD1 only, =12 both. Measured 2026-09-26 (docs/22): LOD1 from LOD0 takes the 0->1 pop from IoU
# 0.919 to 0.968 and the block colour shift from 11.1 to 5.0; LOD2 from LOD0 (24% of its triangles) breaks the cap and
# was worse at 1->2 (0.837 against 0.855) - but that was the rig, not the mesh: its islands were re-made rigid on the
# coarse surface and the head rode the arms. Stepped down from LOD1 and keeping LOD0's weights, measured 2026-09-27:
# 0->1 IoU 0.986 (Tripo's LOD1 0.918), 1->2 0.945 (0.857), block colour 3.1 and 8.5 (13.4 and 15.4).
DERIVE = os.environ.get("TW_DERIVE", "12")
DERIVED = set()
if DERIVE in ("1", "12"):
    for k in ((1, 2) if DERIVE == "12" else (1,)):
        # each level from the one above it (decimating LOD0 straight to a quarter of its triangles broke the cap)
        src = lods[k - 1]; tris0 = sum(len(p.vertices) - 2 for p in src.data.polygons)
        tris_own = sum(len(p.vertices) - 2 for p in lods[k].data.polygons)
        if k == 2 and os.environ.get("TW_LOD2_TRIS"): tris_own = int(os.environ["TW_LOD2_TRIS"])
        dk = src.copy(); dk.data = src.data.copy(); bpy.context.scene.collection.objects.link(dk)
        bpy.data.objects.remove(lods[k]); dk.name = "Frog_LOD%d" % k; dk.data.name = "Frog_LOD%d" % k
        select_only([dk], dk)
        md = dk.modifiers.new("dec", 'DECIMATE'); md.decimate_type = 'COLLAPSE'; md.ratio = min(1.0, tris_own / tris0)
        md.use_symmetry = os.environ.get("TW_DERIVE_SYM", "1") == "1"; md.symmetry_axis = 'X'; md.use_collapse_triangulate = True
        bpy.ops.object.modifier_apply(modifier=md.name)
        lods[k] = dk; stems[k] = stems[0]; DERIVED.add(k)
        print("LOD%d: made from LOD0, %d tris (Tripo's LOD%d had %d)" % (k, sum(len(p.vertices) - 2 for p in dk.data.polygons), k, tris_own))
# TW_REBAKE=1 (an option for the owner, off by default): keep Tripo's LOD1 and LOD2 shapes but paint them from LOD0 -
# LOD0's colour baked (Cycles, selected-to-active) onto each lower LOD's own UVs - so the same spot is the same colour
# at every level. TW_DERIVE decimating LOD0 instead broke the head at LOD2's count (24% of LOD0's triangles).
def rebake(src, dst, path):
    h = max(src.dimensions)
    old = [n for n in dst.active_material.node_tree.nodes if n.type == 'TEX_IMAGE'][0]
    w, hh = old.image.size
    # texels the bake misses take the LOD's own texture afterwards: where the two shapes part further than the ray
    # reaches (inside the cap, under a plate) a miss would otherwise stay black
    img = bpy.data.images.new(dst.name + "_rebake", w, hh, alpha=False); img.generated_color = (1.0, 0.0, 1.0, 1.0)
    nodes = dst.active_material.node_tree.nodes
    tn = nodes.new("ShaderNodeTexImage"); tn.image = img
    for n in nodes: n.select = False
    tn.select = True; nodes.active = tn
    scn = bpy.context.scene; scn.render.engine = 'CYCLES'; scn.cycles.samples = 1; scn.cycles.device = 'CPU'
    select_only([src, dst], dst)
    bpy.ops.object.bake(type='DIFFUSE', pass_filter={'COLOR'}, use_selected_to_active=True, cage_extrusion=0.02 * h,
                        max_ray_distance=0.12 * h, margin=8)
    import numpy as np
    px = np.array(img.pixels[:], dtype=np.float32).reshape(-1, 4); own = np.array(old.image.pixels[:], dtype=np.float32).reshape(-1, 4)
    # a ray that finds nothing writes black (measured: the magenta never survives), so a texel is a miss where the bake
    # is black and the LOD's own texture is not
    miss = ((px[:, 0] > 0.98) & (px[:, 1] < 0.02) & (px[:, 2] > 0.98)) | ((px[:, :3].sum(1) < 0.02) & (own[:, :3].sum(1) > 0.08))
    px[miss] = own[miss]; img.pixels[:] = px.ravel()
    print("%s: %.1f%% of texels missed by the bake, kept from its own texture" % (dst.name, 100.0 * miss.mean()))
    img.filepath_raw = path; img.file_format = 'JPEG'; img.save()
    old.image = img; nodes.remove(tn)

if os.environ.get("TW_REBAKE") == "1":
    for k in (1, 2):
        stem = os.path.join(os.path.dirname(OUT), "rebake_LOD%d" % k)
        rebake(lods[0], lods[k], stem + "_tex0_0.jpg"); stems[k] = stem
        print("LOD%d: repainted from LOD0 (%s)" % (k, stem + "_tex0_0.jpg"))
# TW_LOD2_FROM_LOD1=1 (an option for the owner, off by default): LOD2 is LOD1 decimated to LOD2's triangle count, on
# LOD1's atlas, instead of Tripo's own LOD2 sculpt - which is a different model (a plated shoulder, a broader back) and
# pops at the LOD1->LOD2 switch (worst side 0.857, critic r5). The owner's art stays the default.
if os.environ.get("TW_LOD2_FROM_LOD1") == "1":
    tris_own = sum(len(p.vertices) - 2 for p in lods[2].data.polygons)
    d2 = lods[1].copy(); d2.data = lods[1].data.copy(); bpy.context.scene.collection.objects.link(d2)
    bpy.data.objects.remove(lods[2]); d2.name = "Frog_LOD2"; d2.data.name = "Frog_LOD2"
    tris1 = sum(len(p.vertices) - 2 for p in d2.data.polygons)
    select_only([d2], d2)
    md = d2.modifiers.new("dec2", 'DECIMATE'); md.decimate_type = 'COLLAPSE'; md.ratio = min(1.0, tris_own / tris1)
    md.use_symmetry = True; md.symmetry_axis = 'X'; md.use_collapse_triangulate = True
    bpy.ops.object.modifier_apply(modifier=md.name)
    lods[2] = d2; stems[2] = stems[1]
    print("LOD2: made from LOD1, %d tris (Tripo's LOD2 had %d)" % (sum(len(p.vertices) - 2 for p in d2.data.polygons), tris_own))
# LOD3: LOD2 decimated
src2 = lods[2]
lod3 = src2.copy(); lod3.data = src2.data.copy(); lod3.name = "Frog_LOD3"; lod3.data.name = "Frog_LOD3"
bpy.context.scene.collection.objects.link(lod3)
tris2 = sum(len(p.vertices) - 2 for p in src2.data.polygons)
select_only([lod3], lod3)
# TW_LOD3_KEEP > 0 weights the decimation to keep the cap's flat top and the gap between the legs (critic r4's idea).
# Measured 2026-09-26 it makes the LOD2->LOD3 pop WORSE (worst side 0.77 against 0.85 at 300 tris): the triangles it
# keeps there are taken from the shoulders and arms. Off by default; kept so the next figure can be tried with it.
keep = lod3.vertex_groups.new(name="keep")   # inert while the factor is 0
for v in lod3.data.vertices:
    x, z = abs(v.co.x), v.co.z
    w = 1.0 if z > 0.60 else (1.0 if (z < 0.17 and x < 0.075) else (0.6 if z < 0.06 else 0.0))
    if w > 0: keep.add([v.index], w, 'REPLACE')
m = lod3.modifiers.new("dec", 'DECIMATE'); m.decimate_type = 'COLLAPSE'; m.ratio = min(1.0, LOD3_TRIS / tris2)
m.use_symmetry = True; m.symmetry_axis = 'X'; m.use_collapse_triangulate = True
m.vertex_group = "keep"; m.invert_vertex_group = True; m.vertex_group_factor = float(os.environ.get("TW_LOD3_KEEP", "0"))
bpy.ops.object.modifier_apply(modifier=m.name)
lods.append(lod3); stems.append(stems[2])
# scale to HEIGHT, feet on the ground, centred over x = 0 (y left as modelled: the joints are measured in it)
h = max(v.co.z for v in lods[0].data.vertices)
SC = HEIGHT / h
for o in lods:
    for v in o.data.vertices: v.co = v.co * SC
# armature
arm_data = bpy.data.armatures.new("Armature"); arm = bpy.data.objects.new("Armature", arm_data)
bpy.context.scene.collection.objects.link(arm)
select_only([arm], arm)
bpy.ops.object.mode_set(mode='EDIT')
eb = {}
for n, p, hd, tl in BONES:
    b = arm_data.edit_bones.new(n); b.head = Vector(hd) * SC; b.tail = Vector(tl) * SC
    b.use_deform = not n.endswith("_End"); eb[n] = b
for n, p, hd, tl in BONES:
    if p: eb[n].parent = eb[p]; eb[n].use_connect = (eb[p].tail - eb[n].head).length < 1e-5
# rolls: limbs roll so the bend axis is sensible (Humanoid ignores it; this is for the Blender checks)
bpy.ops.armature.select_all(action='SELECT'); bpy.ops.armature.calculate_roll(type='GLOBAL_POS_Z')
bpy.ops.object.mode_set(mode='OBJECT')
bones_ws = [(n, Vector(hd) * SC, Vector(tl) * SC) for n, p, hd, tl in BONES if not n.endswith("_End")]
# LOD0: bone heat, geometric fallback
select_only([lods[0], arm], arm)
bpy.ops.object.parent_set(type='ARMATURE_AUTO')
W0 = weights_of(lods[0])
missing = 0
for i, v in enumerate(lods[0].data.vertices):
    if not any(n in DEFORM for n in W0[i]): W0[i] = geometric(v.co, bones_ws); missing += 1
isl0 = vert_islands(lods[0])
legs = min(isl0[:3], key=lambda g: min(lods[0].data.vertices[i].co.z for i in g))
nacc = rigid_accessories(lods[0], W0, 300)
ntun = tunic(lods[0], W0, legs)
ngate = gate(lods[0], W0)
W0 = clean(W0, lods[0].data.vertices, lambda n: n, 4)
write_weights(lods[0], W0)
print("LOD0: %d verts, heat missed %d (geometric fallback), %d accessory islands made rigid, %d tunic verts eased, %d gated" % (len(W0), missing, nacc, ntun, ngate))
# LOD1..3: transfer from LOD0's surface
for k in (1, 2, 3):
    o = lods[k]
    for g in list(o.vertex_groups): o.vertex_groups.remove(g)
    for n in DEFORM: o.vertex_groups.new(name=n)
    select_only([o], o)
    # LOD3 is LOD2 decimated: its nearest surface is LOD2's, not LOD0's (LOD0's nearest face to a coarse armpit vertex
    # can be the flank). LOD2 is done by now.
    dt = o.modifiers.new("dt", 'DATA_TRANSFER'); dt.object = lods[2] if k == 3 else lods[0]
    dt.use_vert_data = True; dt.data_types_verts = {'VGROUP_WEIGHTS'}
    dt.vert_mapping = 'POLYINTERP_NEAREST'; dt.layers_vgroup_select_src = 'ALL'; dt.layers_vgroup_select_dst = 'NAME'
    bpy.ops.object.modifier_apply(modifier=dt.name)
    # smooth shading carried from LOD0: a coarse LOD with the importer's 55 degree split reads as crumpled foil and
    # triples its vertex count (critic round 1)
    for poly in o.data.polygons: poly.use_smooth = True
    dn = o.modifiers.new("dn", 'DATA_TRANSFER'); dn.object = lods[0]
    dn.use_loop_data = True; dn.data_types_loops = {'CUSTOM_NORMAL'}; dn.loop_mapping = 'POLYINTERP_NEAREST'
    try: bpy.ops.object.modifier_apply(modifier=dn.name)
    except Exception as ex: print("LOD%d: normal transfer failed: %s" % (k, ex)); o.modifiers.remove(dn)
    fold, kmax = RIGS[k]
    W = weights_of(o)
    # the body/accessory line scales with the mesh from LOD0's 300 vertices: a LOD decimated from LOD0 keeps LOD0's
    # islands at a quarter of their vertices, and a fixed share of its own count made the torso, legs and head
    # 'accessories' riding the arms (derived LOD2 skinned to 8 bones, its head sank with the arms)
    # a LOD decimated from LOD0 has LOD0's islands, which LOD0's weights already made rigid, so it keeps what it was given:
    # re-made rigid on its own coarse surface, the head took the shoulders' weights and sank with the arms
    derived = k in DERIVED or (k == 3 and 2 in DERIVED)
    nacc = 0 if derived else rigid_accessories(o, W, max(12, int(300 * len(o.data.vertices) / len(lods[0].data.vertices))))
    ngate = gate(o, W)
    if k == 3 and os.environ.get("TW_LOD3_RIGID") == "1":   # tried 2026-09-26: gaps and shards when bent; kept as an option
        W, nseam = rigid_split(o, clean(W, o.data.vertices, fold, 4))
        print("LOD3: split %d seam edges into rigid segments" % nseam)
    W = clean(W, o.data.vertices, fold, kmax)
    empty = 0
    for i, v in enumerate(o.data.vertices):
        if not W[i]:
            g = geometric(v.co, bones_ws); W[i] = clean([g], [v], fold, kmax)[0]; empty += 1
    write_weights(o, W)
    used = sorted({n for w in W for n in w})
    print("LOD%d: %d verts, %d tris, %d bones used, %d per vertex, %d refilled, %d gated, %d rigid islands" % (k, len(W), sum(len(p.vertices) - 2 for p in o.data.polygons), len(used), kmax, empty, ngate, nacc))
# LOD3 is drawn from ~170 m, where a figure is some twenty pixels tall: its colour goes into the vertices and its UVs
# go, because Tripo's atlas is cut into so many islands that every UV seam splits a vertex (Unity imported the 121
# positions of LOD3 as 298 vertices, every split a UV seam, none a normal).
if os.environ.get("TW_LOD3_TEXTURED") != "1":
    o3 = lods[3]; me3 = o3.data
    img = None
    for n in o3.material_slots[0].material.node_tree.nodes:
        if n.type == 'TEX_IMAGE' and n.image: img = n.image
    w_, h_ = img.size
    import array
    px = array.array('f', [0.0]) * (w_ * h_ * 4); img.pixels.foreach_get(px)
    def sample(u, v):
        x = min(w_ - 1, max(0, int((u % 1.0) * w_))); y = min(h_ - 1, max(0, int((v % 1.0) * h_)))
        i = (y * w_ + x) * 4
        return (px[i], px[i + 1], px[i + 2])
    uvl = me3.uv_layers.active.data
    acc = [[0.0, 0.0, 0.0, 0.0] for _ in me3.vertices]
    for poly in me3.polygons:
        uvs = [uvl[li].uv for li in poly.loop_indices]
        cu = sum(u.x for u in uvs) / len(uvs); cv = sum(u.y for u in uvs) / len(uvs)
        # nine points over the face (the centre, and toward each corner at two depths) and the MEDIAN of them: the face's
        # own paint, not a stray texel of its island's rim (a mean gave LOD3's shoulder a pale lilac block, critic r5)
        pts = [(cu, cv)] + [(cu + (u.x - cu) * f, cv + (u.y - cv) * f) for u in uvs[:4] for f in (0.35, 0.7)]
        cs = [sample(u, v) for u, v in pts[:9]]
        c = [sorted(x[k] for x in cs)[len(cs) // 2] for k in range(3)]
        a_ = poly.area
        for vi in poly.vertices:
            a = acc[vi]; a[0] += c[0] * a_; a[1] += c[1] * a_; a[2] += c[2] * a_; a[3] += a_
    col = me3.color_attributes.new("Col", 'FLOAT_COLOR', 'POINT')
    for vi, a in enumerate(acc):
        # stored as sampled: Unity hands FBX vertex colours to the shader untouched, and converting them to linear here
        # drew LOD3 visibly darker and more saturated than LOD2 beside it (r3)
        n = max(1e-9, a[3]); col.data[vi].color = (a[0] / n, a[1] / n, a[2] / n, 1.0)
    while me3.uv_layers: me3.uv_layers.remove(me3.uv_layers[0])
    print("LOD3: colour baked into %d vertices, UVs removed" % len(me3.vertices))

# parent all to the armature with a modifier
for o in lods:
    o.parent = arm
    if not any(md.type == 'ARMATURE' for md in o.modifiers):
        md = o.modifiers.new("Armature", 'ARMATURE'); md.object = arm
# ------------------------------------------------------------------------------------------------- pose checks
scn = bpy.context.scene
scn.render.engine = 'BLENDER_WORKBENCH'; scn.display.shading.light = 'STUDIO'; scn.display.shading.color_type = 'TEXTURE'
scn.render.resolution_x = 1400; scn.render.resolution_y = 520
cam = bpy.data.objects.new("cam", bpy.data.cameras.new("cam")); scn.collection.objects.link(cam); scn.camera = cam
cam.data.type = 'ORTHO'; cam.data.ortho_scale = 4 * 1.4 * HEIGHT / 1.78 * 1.0
def pose(name):
    pb = arm.pose.bones
    for b in pb: b.rotation_mode = 'XYZ'; b.rotation_euler = (0, 0, 0); b.location = (0, 0, 0)
    def rot(bn, axis, deg):
        b = pb["mixamorig:" + bn]; e = list(b.rotation_euler); e["XYZ".index(axis)] += math.radians(deg); b.rotation_euler = e
    if name == "rest": return
    if name == "armsdown":
        rot("LeftArm", "Z", 0); rot("LeftArm", "X", 0)
    # swing each limb in armature space by rotating about the world axis through its head (pose via matrices)
    def world_rot(bn, axis, deg):
        b = pb["mixamorig:" + bn]
        M = b.matrix.copy(); hd = M.translation.copy()
        R = Matrix.Rotation(math.radians(deg), 4, axis)
        b.matrix = Matrix.Translation(hd) @ R @ Matrix.Translation(-hd) @ M
        bpy.context.view_layer.update()
    if name in ("armsdown", "walk", "crouch", "aim"):
        world_rot("LeftArm", 'Y', 70); world_rot("RightArm", 'Y', -70)
    if name == "walk":
        world_rot("LeftUpLeg", 'X', 30); world_rot("RightUpLeg", 'X', -30); world_rot("RightLeg", 'X', -35)
        world_rot("LeftForeArm", 'X', 30); world_rot("RightForeArm", 'X', 30)
    if name == "crouch":
        world_rot("Hips", 'X', 0)
        for s in ("Left", "Right"):
            world_rot(s + "UpLeg", 'X', 70); world_rot(s + "Leg", 'X', -100); world_rot(s + "Foot", 'X', 30)
        world_rot("Spine1", 'X', 25); world_rot("Head", 'X', -20)
    if name == "rifle":
        world_rot("RightArm", 'Z', 60); world_rot("RightForeArm", 'Z', 110)
        world_rot("LeftArm", 'Z', -70); world_rot("LeftForeArm", 'Z', -60)
        world_rot("RightArm", 'Y', -25); world_rot("LeftArm", 'Y', 25)
    if name == "aim":
        world_rot("LeftArm", 'Z', 50); world_rot("RightArm", 'Z', -40); world_rot("RightForeArm", 'Z', -60)
        world_rot("LeftForeArm", 'Z', 30); world_rot("Head", 'Z', 25); world_rot("Spine1", 'Z', 20)
for tag in ("rest", "armsdown", "walk", "crouch", "aim", "rifle"):
    pose(tag)
    for k, o in enumerate(lods):
        for j, x in enumerate(lods): x.hide_render = (j != k)
        scn.render.resolution_x = 520
        cam.data.ortho_scale = 1.25 * HEIGHT
        for view in ("front", "side"):
            if view == "front": cam.location = (0, -10, 0.5 * HEIGHT); cam.rotation_euler = (math.pi / 2, 0, 0)
            else: cam.location = (10, 0, 0.5 * HEIGHT); cam.rotation_euler = (math.pi / 2, 0, math.pi / 2)
            scn.render.filepath = os.path.join(RENDERDIR, "frog_%s_LOD%d_%s.png" % (tag, k, view))
            bpy.ops.render.render(write_still=True)
    for x in lods: x.hide_render = False
pose("rest")
# ----------------------------------------------------------------------------------------------------- export
select_only([arm] + lods, arm)
bpy.ops.export_scene.fbx(filepath=OUT, use_selection=True, object_types={'ARMATURE', 'MESH'}, apply_unit_scale=True,
                         apply_scale_options='FBX_SCALE_ALL', bake_space_transform=False, axis_forward='-Z', axis_up='Y',
                         mesh_smooth_type='OFF', use_mesh_modifiers=False, add_leaf_bones=False, primary_bone_axis='Y',
                         secondary_bone_axis='X', use_armature_deform_only=False, bake_anim=False, path_mode='STRIP',
                         embed_textures=False, use_tspace=False, colors_type='LINEAR')
d = os.path.dirname(OUT)
for k, stem in enumerate(stems):
    shutil.copyfile(stem + "_tex0_0.jpg", os.path.join(d, "%s_LOD%d_Base.jpg" % (NAME, k))) if k < 3 else None
info = {"source": "Tools/frogrig.py", "height_m": HEIGHT, "scale": SC, "lods": []}
for k, o in enumerate(lods):
    W = weights_of(o)
    info["lods"].append({"lod": k, "verts": len(o.data.vertices), "tris": sum(len(p.vertices) - 2 for p in o.data.polygons),
                         "bones": sorted({n for w in W for n in w}), "max_per_vertex": max(len(w) for w in W),
                         "texture": "%s_LOD%d_Base.jpg" % (NAME, min(k, 2))})
info["skeleton"] = [{"name": n, "parent": p, "head": [round(x * SC, 4) for x in hd]} for n, p, hd, tl in BONES]
json.dump(info, open(os.path.join(d, "frogrig.json"), "w"), indent=1)
print("DONE", [(l["lod"], l["verts"], l["tris"], len(l["bones"]), l["max_per_vertex"]) for l in info["lods"]])
