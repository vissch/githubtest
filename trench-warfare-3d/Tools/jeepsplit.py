# Blender (background): split a Tripo wheeled vehicle (2026-09-27: Downloads/ambulance jeep 3d model.zip, 6,236 tris,
# and green toy jeep 3d model.zip, the same ambulance at 880 tris) into named rigid parts at three LODs, for the asset
# playground (Playground/Art/Tanks/<Name>/: the vehicle bay reads any folder with a tank3.json). Same conventions and
# manifest as tank3split.py: one object per part, origin on its pivot, front +Z, metres.
#
# LOD1 and LOD2 are DERIVED from LOD0 (the owner's call 2026-09-27, decisions.md): each part of LOD0 decimated, on
# LOD0's atlas, so a part is the same shape and colour at every level and falls apart the same way. Tripo's lower
# model only sets LOD2's triangle budget (880); LOD1 sits between them (the geometric mean).
#
# Parts, in the model's own frame (front -Y, left +X, ground z = 0, the longest side 1 unit), measured off the
# ambulance's islands and the side profile of its welded body:
#   * loose islands go whole to the region holding most of their area: the four wheels, the head lamps, the exhaust
#     stack, the rear doors and the small rear fittings; everything else rides the body;
#   * the welded body is CUT with planes: the chassis and wings below Z_DECK; above it the bonnet lid (in front of
#     Y_CAB), the cab (to Y_BOX) and the box's two sides (split at x = 0). Cut openings are capped with the atlas's
#     darkest texel: inside a burnt-out body is soot.
#
# usage: blender -b --factory-startup -P jeepsplit.py -- <name> <lod0.fbx> [<tripo lower lod.fbx>] <outdir> <renderdir>
#   each fbx beside its Tripo .fbm folder (base colour *_basecolor.jpg); short paths (MAX_PATH).
import bpy, bmesh, sys, os, math, json, random, glob, shutil
import numpy as np
from mathutils import Vector, Matrix
from mathutils.bvhtree import BVHTree

argv = sys.argv[sys.argv.index("--") + 1:]
NAME, OUTDIR, RENDERDIR = argv[0], argv[-2], argv[-1]
FBX = argv[1:-2]
os.makedirs(OUTDIR, exist_ok=True); os.makedirs(RENDERDIR, exist_ok=True)
SCALE = float(os.environ.get("TW_SCALE", "4.6"))   # metres per model unit: a WW1 field ambulance is ~4.5 m long

INF = 9.0
# box = (xmin, xmax, ymin, ymax, zmin, zmax) on an island's area-weighted face centres. First match wins.
REGIONS = [
    ("Wheel_FL", (0.15, INF, -INF, -0.10, -INF, 0.36), dict(wheel=True)),
    ("Wheel_FR", (-INF, -0.15, -INF, -0.10, -INF, 0.36), dict(wheel=True)),
    ("Wheel_RL", (0.15, INF, 0.10, INF, -INF, 0.36), dict(wheel=True)),
    ("Wheel_RR", (-INF, -0.15, 0.10, INF, -INF, 0.36), dict(wheel=True)),
    ("Lamp_L",   (0.05, INF, -INF, -0.37, 0.28, 0.47), {}),
    ("Lamp_R",   (-INF, -0.05, -INF, -0.37, 0.28, 0.47), {}),
    ("Stack",    (-INF, -0.12, -0.22, -0.10, 0.33, INF), {}),
    ("Door_BL",  (0.0, 0.16, 0.38, INF, 0.43, 0.70), {}),
    ("Door_BR",  (-0.16, 0.0, 0.38, INF, 0.43, 0.70), {}),
    ("Fitting",  (0.15, INF, 0.40, INF, 0.28, 0.56), {}),
    ("Spare",    (-INF, -0.15, 0.40, INF, 0.26, 0.40), {}),
]
Z_DECK = 0.40      # above the wings and running boards (0.30-0.39); through the bonnet's sides, under its lid (0.51)
Y_CAB = -0.22      # bonnet | cab: the windscreen stands at -0.15
Y_BOX = 0.04       # cab | box: the box's sides begin at +0.05
CUT_PARTS = ["Hood", "Cab", "Box_L", "Box_R"]
LOOSE_PARTS = [r[0] for r in REGIONS]
ALL_PARTS = ["Hull"] + CUT_PARTS + LOOSE_PARTS
PARENT = {n: "Hull" for n in CUT_PARTS + LOOSE_PARTS}
# destruction metadata the playground reads (mass share, what breaks it off first: 1 fittings, 2 running gear and
# doors, 3 the bonnet and cab, 4 the box's sides, 9 the chassis)
BREAK = {
    "Lamp_L": dict(tier=1, mass=0.2), "Lamp_R": dict(tier=1, mass=0.2), "Stack": dict(tier=1, mass=0.3),
    "Fitting": dict(tier=1, mass=0.2), "Spare": dict(tier=1, mass=0.4),
    "Wheel_FL": dict(tier=2, mass=1.2), "Wheel_FR": dict(tier=2, mass=1.2), "Wheel_RL": dict(tier=2, mass=1.2),
    "Wheel_RR": dict(tier=2, mass=1.2), "Door_BL": dict(tier=2, mass=0.6), "Door_BR": dict(tier=2, mass=0.6),
    "Hood": dict(tier=3, mass=1.0), "Cab": dict(tier=3, mass=2.0),
    "Box_L": dict(tier=4, mass=2.0), "Box_R": dict(tier=4, mass=2.0), "Hull": dict(tier=9, mass=8.0),
}

def in_box(c, b): return b[0] <= c[0] <= b[1] and b[2] <= c[1] <= b[3] and b[4] <= c[2] <= b[5]

def load(fbx):
    before = set(bpy.data.objects)
    bpy.ops.import_scene.fbx(filepath=fbx)
    obj = [o for o in bpy.data.objects if o not in before and o.type == 'MESH'][0]
    for o in bpy.context.selected_objects: o.select_set(False)
    obj.select_set(True); bpy.context.view_layer.objects.active = obj
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    stem = os.path.splitext(fbx)[0]
    base = (glob.glob(stem + ".fbm/*basecolor*") + glob.glob(stem + ".fbm/tripo_rgb*") + glob.glob(stem + "_tex0_0.jpg"))[0]
    img = bpy.data.images.load(base)
    mat = bpy.data.materials.new("atlas"); mat.use_nodes = True
    t = mat.node_tree.nodes.new("ShaderNodeTexImage"); t.image = img
    mat.node_tree.links.new(t.outputs["Color"], mat.node_tree.nodes["Principled BSDF"].inputs["Base Color"])
    obj.data.materials.clear(); obj.data.materials.append(mat)
    return obj, img, mat, base

def dark_uv(img):
    w, h = img.size
    a = np.empty(w * h * 4, dtype=np.float32); img.pixels.foreach_get(a)
    px = a.reshape(h, w, 4)[:, :, :3]; k = 32
    blocks = px[: h // k * k, : w // k * k].reshape(h // k, k, w // k, k, 3).mean(axis=(1, 3))
    by, bx = np.unravel_index(np.argmin(blocks @ np.array([0.3, 0.59, 0.11])), blocks.shape[:2])
    return ((bx + 0.5) * k / w, (by + 0.5) * k / h)

def islands(bm):
    bm.verts.ensure_lookup_table(); bm.faces.ensure_lookup_table()
    par = list(range(len(bm.verts)))
    def find(a):
        while par[a] != a: par[a] = par[par[a]]; a = par[a]
        return a
    for e in bm.edges:
        a, b = find(e.verts[0].index), find(e.verts[1].index)
        if a != b: par[a] = b
    g = {}
    for f in bm.faces: g.setdefault(find(f.verts[0].index), []).append(f.index)
    return sorted(g.values(), key=lambda fs: -len(fs))

def bounds(bm):
    co = [v.co for v in bm.verts]
    return (Vector((min(c.x for c in co), min(c.y for c in co), min(c.z for c in co))),
            Vector((max(c.x for c in co), max(c.y for c in co), max(c.z for c in co))))

def classify(bm, fs):
    votes = {}; total = 0.0
    vs = {v for i in fs for v in bm.faces[i].verts}
    lo = Vector([min(v.co[k] for v in vs) for k in range(3)]); hi = Vector([max(v.co[k] for v in vs) for k in range(3)])
    for i in fs:
        f = bm.faces[i]; a = f.calc_area(); c = f.calc_center_median(); total += a
        for name, box, opt in REGIONS:
            if not in_box(c, box): continue
            # a wheel is round: as tall as it is long, and reaches the ground
            if opt.get("wheel") and not (lo.z < 0.03 and 0.6 < (hi.z - lo.z) / max(1e-6, hi.y - lo.y) < 1.6): continue
            votes[name] = votes.get(name, 0.0) + a; break
    if not votes: return "Hull", 0.0
    win = max(votes, key=votes.get); share = votes[win] / max(total, 1e-9)
    return (win if share >= 0.5 else "Hull"), share

def take(bm, keep):
    out = bm.copy(); out.faces.ensure_lookup_table()
    bmesh.ops.delete(out, geom=[f for f in out.faces if f.index not in keep], context='FACES')
    bmesh.ops.delete(out, geom=[v for v in out.verts if not v.link_faces], context='VERTS')
    return out

def bisect(bm, co, no):
    bmesh.ops.bisect_plane(bm, geom=bm.verts[:] + bm.edges[:] + bm.faces[:], plane_co=co, plane_no=no)
    bm.faces.ensure_lookup_table(); bm.verts.ensure_lookup_table()

PLANES = [((0, 0, Z_DECK), (0, 0, 1)), ((0, Y_CAB, 0), (0, 1, 0)), ((0, Y_BOX, 0), (0, 1, 0)), ((0, 0, 0), (1, 0, 0))]
def on_planes(v, planes, eps=2e-3):
    return any(abs((v.co - Vector(co)).dot(Vector(no))) < eps for co, no in planes)

def cap(bm, planes, uv, eps=1e-4):
    """Close every open loop a cut left, with a fan round its centre, painted the soot texel."""
    bnd = [e for e in bm.edges if e.is_boundary]
    cut = {e for e in bnd if all(on_planes(v, planes, eps) for v in e.verts)}
    # which single plane each cut edge lies on: an outline is a cut only if it runs along ONE plane. One that turns the
    # corner onto another (the x = 0 split running into the rear doors' opening) filled the doorway with a black and
    # white triangle (critic loop 2 r30)
    on_one = [{e for e in cut if all(on_planes(v, [pl], eps) for v in e.verts)} for pl in planes]
    if not cut: return 0
    byv = {}
    for e in bnd:
        for v in e.verts: byv.setdefault(v, []).append(e)
    left = set(bnd); uvl = bm.loops.layers.uv.active; new = []
    while left:
        e0 = left.pop(); loop = [e0.verts[0], e0.verts[1]]; used = {e0}; has_cut = e0 in cut
        while True:
            nxt = [e for e in byv.get(loop[-1], []) if e in left and e not in used]
            if not nxt: break
            e = nxt[0]; used.add(e); left.discard(e); has_cut |= e in cut
            v = e.other_vert(loop[-1])
            if v is loop[0]: break
            loop.append(v)
        # only an outline the cut made: one that runs mostly along a window or a door the model already had stays open
        if len(loop) < 3 or max(sum(1 for e in used if e in one) for one in on_one) < 0.9 * len(used): continue
        # a proper fill of the outline: a fan round its centre overhangs a concave outline, and the overhang showed as a
        # black wedge on the outside of the cab and box
        try: made = bmesh.ops.triangle_fill(bm, use_beauty=True, use_dissolve=False, edges=list(used))["geom"]
        except Exception: made = []
        made = [f for f in made if isinstance(f, bmesh.types.BMFace)]
        if made: new.extend(made); continue
        vc = bm.verts.new(sum((v.co for v in loop), Vector()) / len(loop))
        for i in range(len(loop)):
            try: new.append(bm.faces.new((loop[i], loop[(i + 1) % len(loop)], vc)))
            except ValueError: pass
    for f in new:
        for l in f.loops: l[uvl].uv = uv
    mid = sum((v.co for v in bm.verts), Vector()) / max(1, len(bm.verts))
    flip = [f for f in new if f.normal.dot(f.calc_center_median() - mid) < 0]
    if flip: bmesh.ops.reverse_faces(bm, faces=flip)
    return len(new)

def cut_body(body):
    for co, no in PLANES: bisect(body, co, no)
    groups = {n: set() for n in CUT_PARTS + ["Hull"]}
    for f in body.faces:
        c = f.calc_center_median()
        if c.z < Z_DECK: groups["Hull"].add(f.index)
        elif c.y < Y_CAB: groups["Hood"].add(f.index)
        elif c.y < Y_BOX: groups["Cab"].add(f.index)
        else: groups["Box_L" if c.x > 0 else "Box_R"].add(f.index)
    return {n: take(body, idx) for n, idx in groups.items() if idx}

def join(bms):
    out = bmesh.new(); me = bpy.data.meshes.new("tmp")
    for b in bms: b.to_mesh(me); out.from_mesh(me)
    bpy.data.meshes.remove(me)
    return out

def carve(parts, n, lo, hi):
    """Where a lower model welded a loose part into its body, the part is the body's faces inside the box the part
    fills at LOD0 (a little padded): Tripo's 880-triangle jeep has its rear doors and back fittings in the body."""
    lo = lo - Vector((0.01, 0.03, 0.01)); hi = hi + Vector((0.01, 0.03, 0.01))
    got = []
    for m, pieces in parts.items():
        if m == n or m in LOOSE_PARTS: continue
        for i, pc in enumerate(pieces):
            # the lower model's faces are large: slice the piece on the box's sides first
            # (only the faces that reach into the box: slicing whole pieces tripled the lower model's triangles)
            for k in range(3):
                for v_ in (lo, hi):
                    near = [f for f in pc.faces if all(min(x.co[j] for x in f.verts) <= hi[j] and max(x.co[j] for x in f.verts) >= lo[j] for j in range(3))]
                    if not near: continue
                    co = [0, 0, 0]; co[k] = v_[k]; no = [0, 0, 0]; no[k] = 1
                    es = list({e for f in near for e in f.edges}); vs = list({x for f in near for x in f.verts})
                    bmesh.ops.bisect_plane(pc, geom=vs + es + near, plane_co=co, plane_no=no)
                    pc.faces.ensure_lookup_table()
            pc.faces.ensure_lookup_table()
            inside = {f.index for f in pc.faces if all(lo[k] <= f.calc_center_median()[k] <= hi[k] for k in range(3))}
            if not inside: continue
            got.append(take(pc, inside)); rest = {f.index for f in pc.faces} - inside
            pieces[i] = take(pc, rest)
    return got

def split(fbx, ref=None):
    obj, img, mat, base = load(fbx)
    uv = dark_uv(img)
    bm = bmesh.new(); bm.from_mesh(obj.data); bm.faces.ensure_lookup_table()
    isl = islands(bm)
    def vol(fs):
        co = [v.co for i in fs for v in bm.faces[i].verts]
        return np.prod([max(c[k] for c in co) - min(c[k] for c in co) for k in range(3)])
    body = max(range(len(isl)), key=lambda k: vol(isl[k]))
    parts = {n: [] for n in ALL_PARTS}; log = []
    for k, fs in enumerate(isl):
        if k == body: continue
        name, share = classify(bm, fs)
        piece = take(bm, set(fs))
        if name == "Hull":
            # a small loose bit above the deck rides the cut part it sits on (a mirror on the cab, a latch on the box)
            lo, hi = bounds(piece); c = (lo + hi) / 2
            if c.z >= Z_DECK and (hi - lo).length < 0.25:
                name = "Hood" if c.y < Y_CAB else "Cab" if c.y < Y_BOX else ("Box_L" if c.x > 0 else "Box_R")
        parts[name].append(piece); log.append((name, len(fs), round(share, 2)))
    cuts = cut_body(take(bm, set(isl[body])))
    for n, piece in cuts.items(): parts[n].insert(0, piece)
    if ref is not None:
        for n in LOOSE_PARTS:
            if not parts[n]:
                parts[n] = carve(parts, n, *bounds(ref[n]))
                print("  %s carved from the body: %d faces" % (n, sum(len(p.faces) for p in parts[n])))
    for n in parts: parts[n] = [p for p in parts[n] if len(p.faces)]
    out = {}
    for n in ALL_PARTS:
        assert parts[n], "part %s is EMPTY (the rules miss it)" % n
        out[n] = join(parts[n])
    print("%s: %d islands" % (os.path.basename(fbx), len(isl)))
    print("  islands by part: %s" % {n: sum(1 for x in log if x[0] == n) for n in ALL_PARTS})
    return out, img, mat, base, uv

def tris_of(bm): return sum(len(f.verts) - 2 for f in bm.faces)
def decimated(bm, ratio):
    me = bpy.data.meshes.new("dec"); bm.to_mesh(me)
    o = bpy.data.objects.new("dec", me); bpy.context.scene.collection.objects.link(o)
    for x in bpy.context.selected_objects: x.select_set(False)
    o.select_set(True); bpy.context.view_layer.objects.active = o
    md = o.modifiers.new("dec", 'DECIMATE'); md.decimate_type = 'COLLAPSE'; md.ratio = max(0.02, min(1.0, ratio))
    md.use_collapse_triangulate = True
    md.use_symmetry = os.environ.get("TW_SYM", "1") == "1"; md.symmetry_axis = 'X'
    bpy.ops.object.modifier_apply(modifier=md.name)
    out = bmesh.new(); out.from_mesh(o.data)
    bpy.data.objects.remove(o); bpy.data.meshes.remove(me)
    return out

def pivots_and_sockets(P):
    piv = {"Hull": Vector((0, 0, 0))}
    for n, b in P.items():
        if n == "Hull": continue
        lo, hi = bounds(b); piv[n] = (lo + hi) / 2
    lo, hi = bounds(P["Stack"]); piv["Stack"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    # a rear door hangs on its outer edge
    for n, s in (("Door_BL", 1), ("Door_BR", -1)):
        lo, hi = bounds(P[n]); piv[n] = Vector((hi.x if s > 0 else lo.x, (lo.y + hi.y) / 2, (lo.z + hi.z) / 2))
    sock = {}
    slo, shi = bounds(P["Stack"]); sock["Socket_Exhaust0"] = ("Stack", Vector(((slo.x + shi.x) / 2, (slo.y + shi.y) / 2, shi.z + 0.004)))
    # fire under the bonnet, in the cab and in the box, ray-cast onto the chassis top (what burns once the top is gone)
    tree = BVHTree.FromBMesh(P["Hull"])
    for i, y in enumerate((-0.36, -0.05, 0.25)):
        o = Vector((0.0, y, 2.0)); hit = tree.ray_cast(o, Vector((0, 0, -1)))
        sock["Socket_Fire%d" % i] = ("Hull", Vector((0.0, y, hit[0].z if hit[0] else Z_DECK)))
    for side, n in (("L", "Wheel_RL"), ("R", "Wheel_RR")):
        lo, hi = bounds(P[n]); sock["Socket_Dust_" + side] = ("Hull", Vector(((lo.x + hi.x) / 2, hi.y, lo.z + 0.02)))
    sock["Socket_Deck"] = ("Hull", Vector((0, 0.0, Z_DECK)))
    return piv, sock

TURN = Matrix.Rotation(math.pi, 4, 'Z')
def unity(v): return [round(-v.x, 5), round(v.z, 5), round(-v.y, 5)]

def make(lod, P, piv, mat):
    root = bpy.data.objects.new("%s_LOD%d" % (NAME, lod), None); bpy.context.scene.collection.objects.link(root)
    S = Matrix.Scale(SCALE, 4); objs = {}
    for n in ALL_PARTS:
        b = P[n].copy()
        bmesh.ops.transform(b, matrix=S @ TURN @ Matrix.Translation(-piv[n]), verts=b.verts)
        me = bpy.data.meshes.new("%s_LOD%d_%s" % (NAME, lod, n)); b.to_mesh(me); b.free(); me.materials.append(mat)
        o = bpy.data.objects.new(n, me); bpy.context.scene.collection.objects.link(o); objs[n] = o
    for n, o in objs.items(): o.parent = root; o.location = (TURN @ piv[n]) * SCALE
    return root, objs

def export(root, path):
    for o in bpy.context.selected_objects: o.select_set(False)
    root.select_set(True)
    for c in root.children: c.select_set(True)
    bpy.context.view_layer.objects.active = root
    bpy.ops.export_scene.fbx(filepath=path, use_selection=True, object_types={'MESH', 'EMPTY'}, apply_unit_scale=True,
                             apply_scale_options='FBX_SCALE_ALL', bake_space_transform=True, axis_forward='-Z', axis_up='Y',
                             mesh_smooth_type='OFF', use_mesh_modifiers=False, add_leaf_bones=False, path_mode='STRIP',
                             embed_textures=False, use_custom_props=False, use_tspace=False)

def render(tag, objs, colour, exploded=0.0):
    scn = bpy.context.scene
    scn.render.engine = 'BLENDER_WORKBENCH'; scn.display.shading.light = 'STUDIO'; scn.display.shading.show_cavity = True
    scn.render.resolution_x = 560; scn.render.resolution_y = 480
    if not scn.camera:
        cd = bpy.data.cameras.new("cam"); cd.type = 'ORTHO'
        cam = bpy.data.objects.new("cam", cd); scn.collection.objects.link(cam); scn.camera = cam
    cam = scn.camera
    for o in scn.objects:
        if o.type == 'MESH': o.hide_render = o not in objs.values()
    saved = {o: o.location.copy() for o in objs.values()}
    if exploded:
        for n, o in objs.items():
            if n == "Hull": continue
            d = o.location.copy(); d.z = max(d.z, 0) * 1.5 + 0.3
            o.location = o.location + d.normalized() * exploded
    bpy.context.view_layer.update()
    rnd = random.Random(5)
    for n in ALL_PARTS: objs[n].color = (rnd.random() * .8 + .2, rnd.random() * .8 + .2, rnd.random() * .8 + .2, 1)
    scn.display.shading.color_type = colour
    mid = Vector((0, 0, SCALE * 0.4)); d = Vector((-1, 1.1, 0.8)).normalized()
    cam.location = mid + d * 30; cam.rotation_euler = (-d).to_track_quat('-Z', 'Y').to_euler()
    cam.data.ortho_scale = SCALE * 1.5 + exploded * 1.6; cam.data.clip_end = 100
    scn.render.filepath = os.path.join(RENDERDIR, tag + ".png")
    bpy.ops.render.render(write_still=True)
    for o, l in saved.items(): o.location = l

def rebake(src_parts, src_mat, dst_parts, dst_img, path):
    def obj_of(parts, mat, name):
        me = bpy.data.meshes.new(name); join(list(parts.values())).to_mesh(me); me.materials.append(mat)
        o = bpy.data.objects.new(name, me); bpy.context.scene.collection.objects.link(o); return o
    src = obj_of(src_parts, src_mat, "bake_src")
    w, h = dst_img.size
    img = bpy.data.images.new("rebake", w, h, alpha=False); img.generated_color = (1.0, 0.0, 1.0, 1.0)
    mat = bpy.data.materials.new("rebake_mat"); mat.use_nodes = True
    tn = mat.node_tree.nodes.new("ShaderNodeTexImage"); tn.image = img
    mat.node_tree.nodes.active = tn
    dst = obj_of(dst_parts, mat, "bake_dst")
    scn = bpy.context.scene; scn.render.engine = 'CYCLES'; scn.cycles.samples = 1; scn.cycles.device = 'CPU'
    for x in bpy.context.selected_objects: x.select_set(False)
    src.select_set(True); dst.select_set(True); bpy.context.view_layer.objects.active = dst
    bpy.ops.object.bake(type='DIFFUSE', pass_filter={'COLOR'}, use_selected_to_active=True, cage_extrusion=0.02,
                        max_ray_distance=0.08, margin=8)
    px = np.array(img.pixels[:], dtype=np.float32).reshape(-1, 4); own = np.array(dst_img.pixels[:], dtype=np.float32).reshape(-1, 4)
    miss = ((px[:, 0] > 0.98) & (px[:, 1] < 0.02) & (px[:, 2] > 0.98)) | ((px[:, :3].sum(1) < 0.02) & (own[:, :3].sum(1) > 0.08))
    px[miss] = own[miss]; img.pixels[:] = px.ravel()
    print("LOD2 rebaked from LOD0: %.1f%% of texels missed, kept from its own paint" % (100.0 * miss.mean()))
    img.filepath_raw = path; img.file_format = 'JPEG'; img.save()
    out = bpy.data.materials.new("LOD2_rebaked"); out.use_nodes = True
    t = out.node_tree.nodes.new("ShaderNodeTexImage"); t.image = img
    out.node_tree.links.new(t.outputs["Color"], out.node_tree.nodes["Principled BSDF"].inputs["Base Color"])
    for o in (src, dst): bpy.data.objects.remove(o)
    scn.render.engine = 'BLENDER_WORKBENCH'
    return path, out

# ------------------------------------------------------------------------------------------------------------ main
bpy.ops.wm.read_factory_settings(use_empty=True)
P0, img0, mat0, base0, uv0 = split(FBX[0])
t0 = sum(tris_of(b) for b in P0.values())
low = None
if len(FBX) > 1:
    o, _, _, _ = load(FBX[1]); low = sum(len(p.vertices) - 2 for p in o.data.polygons); bpy.data.objects.remove(o)
budget = {2: int(os.environ.get("TW_LOD2_TRIS", "0")) or low or int(t0 * 0.15)}; budget[1] = int(math.sqrt(t0 * budget[2]))
lods = [P0]; mats = [mat0]; bases = [base0]; uvs = [uv0]; epss = [1e-4]
# LOD2 from Tripo's own lower model (TW_LOD2=tripo, the default when one is given) or decimated from LOD0 (derive).
# Measured 2026-09-27 on the ambulance: LOD0 decimated to 13-20 % of its triangles tore into spikes and holes at any
# budget up to 1,742 (the roofs and the box's thin panels collapse first); Tripo's 880-triangle retopology is clean.
# LOD1 (38 %) derives cleanly.
LOD2_FROM = os.environ.get("TW_LOD2", "tripo" if len(FBX) > 1 else "derive")
for k in ((1, 2) if LOD2_FROM == "derive" else (1,)):
    ratio = budget[k] / t0; floor = 48 if k == 1 else 24
    P = {}
    for n in ALL_PARTS:
        have = tris_of(P0[n]); want = max(int(have * ratio), min(have, floor))
        P[n] = decimated(P0[n], want / max(1, have))
    lods.append(P); mats.append(mat0); bases.append(base0); uvs.append(uv0); epss.append(2e-3)
    print("LOD%d: derived from LOD0, %d tris (budget %d)" % (k, sum(tris_of(b) for b in P.values()), budget[k]))
snapped = []
if LOD2_FROM == "tripo":
    P2, img2, mat2, base2, uv2 = split(FBX[1], ref=P0)
    # a fitting Tripo placed elsewhere at the lower level is moved onto LOD0's centre, so the switch does not move it
    for n in LOOSE_PARTS:
        if BREAK[n]["tier"] != 1: continue
        lo0, hi0 = bounds(P0[n]); lo, hi = bounds(P2[n]); d = (lo0 + hi0) / 2 - (lo + hi) / 2
        if d.length > 0.02:
            bmesh.ops.transform(P2[n], matrix=Matrix.Translation(d), verts=P2[n].verts)
            snapped.append({"lod": 2, "part": n, "moved": round(d.length * SCALE, 3)})
    # TW_REBAKE (default 1): Tripo's low mesh painted with LOD0's colours (a Cycles bake onto its own UVs), so the switch
    # keeps the clean shape and loses the other texture bake: with Tripo's own paint 1->2 block colour was 11.0, the
    # worst on the board (loop 2 r32). Texels the bake misses keep the low model's own paint.
    if os.environ.get("TW_REBAKE", "1") == "1":
        base2, mat2 = rebake(P0, mat0, P2, img2, os.path.join(OUTDIR, "rebake_LOD2.jpg"))
    lods.append(P2); mats.append(mat2); bases.append(base2); uvs.append(uv2); epss.append(1e-4)
    print("LOD2: Tripo's own, %d tris; snapped %s" % (sum(tris_of(b) for b in P2.values()), snapped))
# a wheel thrown off shows its back: Tripo's lower jeep modelled only the outside of each tyre, and the hole round the
# rim (3.7 m at LOD2) would show as the ink pass's black. Each big open loop is filled and painted like the tyre round it.
def close_wheel(bm):
    before = set(bm.faces); orig = bm.copy()
    lo, hi = bounds(bm); edges = [e for e in bm.edges if e.is_boundary]
    if not edges: orig.free(); return 0
    bmesh.ops.holes_fill(bm, edges=edges, sides=0)
    new = [f for f in bm.faces if f not in before]
    if new: bmesh.ops.triangulate(bm, faces=new)
    new = [f for f in bm.faces if f not in before]
    tree = BVHTree.FromBMesh(orig); orig.faces.ensure_lookup_table()
    ouv = orig.loops.layers.uv.active; uvl = bm.loops.layers.uv.active; mid = (lo + hi) / 2
    for f in new:
        f.normal_update()
        if f.normal.dot(f.calc_center_median() - mid) < 0: f.normal_flip()
        for l in f.loops:
            hit = tree.find_nearest(l.vert.co)
            if hit[2] is None: continue
            of = orig.faces[hit[2]]; c = min(of.loops, key=lambda c: (c.vert.co - hit[0]).length)
            l[uvl].uv = c[ouv].uv
    orig.free()
    return len(new)
for k, P in enumerate(lods):
    print("LOD%d wheel backs closed: %s" % (k, {n: close_wheel(P[n]) for n in LOOSE_PARTS if n.startswith("Wheel_")}))
# caps last, on every LOD: decimated with its fans already in, a cut part collapsed toward each fan's centre and the
# box and cab roofs grew spikes
for k, P in enumerate(lods):
    caps = {n: cap(P[n], PLANES, uvs[k], epss[k]) for n in ["Hull"] + CUT_PARTS}
    print("LOD%d caps %s" % (k, caps))
piv, sock = pivots_and_sockets(P0)
manifest = {"source": "Tools/jeepsplit.py", "name": NAME, "scale": SCALE, "fling": 0.6, "parts": {}, "sockets": {}, "lods": [], "snapped": snapped, "derived": [1, 2] if LOD2_FROM == "derive" else [1]}
for n in ALL_PARTS: manifest["parts"][n] = {"parent": PARENT.get(n), "pivot": unity(piv[n] * SCALE), **BREAK[n]}
for s, (owner, p) in sock.items(): manifest["sockets"][s] = {"part": owner, "pos": unity((p - piv[owner]) * SCALE)}
# TW_BATTLE=1 (2026-09-28): the battle's form instead of the playground's (Tools/battleform.py): nested parts, two LODs,
# <outdir>/../<Name>Atlas.jpg; the manifest and a portrait render go to <renderdir>. Both LODs wear LOD0's atlas, so the
# far one has to be derived from LOD0: run it with TW_LOD2=derive.
#   TW_BATTLE=1 TW_LOD2=derive ... -- <Name> <lod0.fbx> [<lower.fbx>] Assets/_Project/Resources/Vehicles/<Name> <renderdir>
if os.environ.get("TW_BATTLE", "") == "1":
    if LOD2_FROM != "derive": sys.exit("TW_BATTLE needs TW_LOD2=derive: both battle LODs wear LOD0's atlas")
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    import battleform
    manifest["partList"] = [dict(name=n, **manifest["parts"][n]) for n in ALL_PARTS]
    for p in manifest["partList"]: p["parent"] = p["parent"] or ""
    manifest["socketList"] = [dict(name=s, **v) for s, v in manifest["sockets"].items()]
    battleform.write(NAME, OUTDIR, RENDERDIR, lods[0], lods[2], ALL_PARTS, PARENT, piv, sock, mats[0], bases[0], SCALE, manifest, tris_of)
    sys.exit(0)
for lod, P in enumerate(lods):
    root, objs = make(lod, P, piv, mats[lod])
    for n, o in objs.items(): o.name = "%d|%s" % (lod, n)
    render("%s_LOD%d_parts" % (NAME, lod), objs, 'OBJECT')
    render("%s_LOD%d_tex" % (NAME, lod), objs, 'TEXTURE')
    render("%s_LOD%d_exploded" % (NAME, lod), objs, 'OBJECT', exploded=1.6)
    for n, o in objs.items(): o.name = n
    export(root, os.path.join(OUTDIR, "%s_LOD%d.fbx" % (NAME, lod)))
    for n, o in objs.items(): o.name = "%d|%s" % (lod, n)
    entry = {"lod": lod, "parts": {}}
    for n in ALL_PARTS:
        lo, hi = bounds(P[n]); me = objs[n].data
        entry["parts"][n] = {"verts": len(me.vertices), "tris": sum(len(p.vertices) - 2 for p in me.polygons),
                             "min": unity(lo * SCALE), "max": unity(hi * SCALE)}
    entry["verts"] = sum(p["verts"] for p in entry["parts"].values()); entry["tris"] = sum(p["tris"] for p in entry["parts"].values())
    manifest["lods"].append(entry)
    shutil.copyfile(bases[lod], os.path.join(OUTDIR, "%s_LOD%d_Base.jpg" % (NAME, lod)))
    print("EXPORT LOD%d: %d verts %d tris" % (lod, entry["verts"], entry["tris"]))
manifest["partList"] = [dict(name=n, **manifest["parts"][n]) for n in ALL_PARTS]
for p in manifest["partList"]: p["parent"] = p["parent"] or ""
manifest["socketList"] = [dict(name=s, **v) for s, v in manifest["sockets"].items()]
manifest["lodList"] = [{"lod": e["lod"], "verts": e["verts"], "tris": e["tris"],
                        "parts": [dict(name=n, **e["parts"][n]) for n in ALL_PARTS]} for e in manifest["lods"]]
json.dump(manifest, open(os.path.join(OUTDIR, "tank3.json"), "w"), indent=1)
print("DONE")
