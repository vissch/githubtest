# Blender (background): add the throat sac to the committed frog, the one mesh change the "frog that deflates" gag
# needs (docs/design/idea-a-shot-frog-deflates-and-whizzes-off-lik.md, concept A: only the pale sac blows up).
#
# frogrig.py cannot be re-run - its Tripo sources are gone from Downloads - so the sac is added to the exported
# Frog.fbx and the file is written out again with frogrig.py's own export call. Per mesh LOD0/LOD1/LOD2 (not LOD3:
# the far form is capped at 300 tris, decisions.md 2026-10-06, and the battle's far tier is the box soldier,
# VATRenderer.cs:11) this script:
#   * adds ONE bone, mixamorig:Throat, child of mixamorig:Neck, its head on the mesh surface under the jaw, at the
#     height of Neck's head; the bone points down. Retarget.cs's Names has no Throat, so the clips leave it in its
#     bind pose: only the gag's own clip scales it.
#   * hangs a UV-sphere sac below that head, rest radius SAC_R, its centre pushed back until the sphere sits inside
#     the chest, so a frog standing in battle looks as it does today. Every sac vertex is weighted 1.0 to Throat
#     and to nothing else, so ONE uniform bone scale grows the ball DOWN over the chest (head clear above, limbs
#     at the sides) the way concept A draws it. At scale 7.6 it is ~1.07 m across: the design page's 1.2 m on a man
#     drawn 2.0 m, on a frog exported at 1.78 m.
#   * gives the sac ONE dedicated UV island: a small square of flat pale chin texels, found by searching the LOD's
#     own texture around the chin's UVs for the palest, most even patch (SAC_UV_HALF across). The nearest-body-vertex
#     copy this replaced (round 1) landed neighbouring vertices in different texture islands and the inflated ball
#     read as green/black/red chevrons. NO texture is repainted.
#   * hangs the ball in FRONT of the chest as it grows. The bone scales its sac about the bone head, so the offset
#     decides where the inflated ball sits: the head is sunk SAC_IN m inside the chest and the ball hangs forward of
#     it (-Y) and down, so at rest the ball's front is tangent to the chest (hidden, poke-out 0) and at the gag's
#     scale the ball has moved out in front of the body instead of swallowing the arms, the hands and the rifle
#     (round 1 buried them: the offset pointed backwards, so the inflated ball was centred inside the torso).
# Segments x rings: LOD0 16x8, LOD1 12x6, LOD2 10x5.
#
# usage: blender -b --factory-startup -P frogsac.py -- <in.fbx> <out.fbx> <report.json>
#   the out dir also gets frogrig.json (the old one with the new counts and the sac's numbers).
#   TW_SAC_R (0.07 m, rest radius), TW_SAC_TEST_SCALE (7.6, the scale the report measures the sac at).
import bpy, bmesh, sys, os, json, math
import numpy as np
from mathutils import Vector

argv = sys.argv[sys.argv.index("--") + 1:]
IN, OUT, REPORT = argv[0], argv[1], argv[2]
SAC_R = float(os.environ.get("TW_SAC_R", "0.07"))
# How far in FRONT of the chest skin the inflated ball's centre stands, at TW_SAC_TEST_SCALE. This is the number
# that keeps the arms, the hands and the rifle out of the ball: measured in the game (Playground, LOD0, Rifle Idle,
# Throat at 7.6) the deepest limb vertex then sits 0.012 m OUTSIDE the sac surface, the rifle 0.151 m outside.
SAC_OUT = float(os.environ.get("TW_SAC_OUT", "0.6032"))
# How far below the jaw the ball hangs at rest, and where its centre sits when it is inflated. Both are measured
# from the height of Neck's head. At rest the ball has to clear the collar as well as the chest - sinking it back
# alone left a disc on the coat - so it hangs in the chest; inflated it rides back up, its top at the jaw.
SAC_DOWN = float(os.environ.get("TW_SAC_DOWN", "0.21"))
SAC_DROP = float(os.environ.get("TW_SAC_DROP", "0.532"))
SAC_UV_HALF = float(os.environ.get("TW_SAC_UV_HALF", "0.006"))   # half-side of the sac's own UV square
TEST_SCALE = float(os.environ.get("TW_SAC_TEST_SCALE", "7.6"))
# segments x rings, per LOD. LOD2 is the one the VAT bake reads, and ProvingGroundModelTests holds that figure under
# 1100 vertices: 10x5 baked to 1167 and went red, 8x4 baked to 1099 (2026-10-10). Unity splits every sac vertex about
# four ways (flat shading, the UV seam), so a ring more on LOD2 costs about 40 baked vertices, not 10 - and 8x4 leaves
# ONE vertex of headroom, so the next LOD2 change has to shrink the sac or move the cap.
RINGS = {0: (16, 8), 1: (12, 6), 2: (8, 4)}
BONE = "mixamorig:Throat"
PARENT = "mixamorig:Neck"
os.makedirs(os.path.dirname(OUT) or ".", exist_ok=True)


def select_only(objs, active):
    for x in bpy.context.selected_objects: x.select_set(False)
    for x in objs: x.select_set(True)
    bpy.context.view_layer.objects.active = active


def uv_of_verts(o):
    """vertex index -> its UV (the first loop that uses it; the frog's seams are at the island edges)"""
    uv = o.data.uv_layers.active
    out = {}
    for p in o.data.polygons:
        for li in p.loop_indices:
            vi = o.data.loops[li].vertex_index
            if vi not in out: out[vi] = Vector(uv.data[li].uv)
    return out


def flat_patch(texpath, uvs, half):
    """The sac's own island: the palest, most even square of 2*half UV around one of these chin UVs.

    Scores every candidate centre by the largest per-channel standard deviation inside the square (even first) less a
    little of its luminance (pale second), and refuses any square that runs off the texture. Returns the centre, the
    square's mean colour 0..1, its spread and its luminance, so the report can state what colour the ball will be."""
    img = bpy.data.images.load(texpath)
    w, h = img.size
    buf = np.empty(w * h * 4, dtype=np.float32)
    img.pixels.foreach_get(buf)
    px = buf.reshape(h, w, 4)[:, :, :3]            # row 0 is v=0: the same way up as UV space
    best = None
    for uv in uvs:
        x0, x1 = int((uv.x - half) * w), int((uv.x + half) * w)
        y0, y1 = int((uv.y - half) * h), int((uv.y + half) * h)
        if x0 < 0 or y0 < 0 or x1 >= w or y1 >= h or x1 <= x0 or y1 <= y0: continue
        blk = px[y0:y1 + 1, x0:x1 + 1].reshape(-1, 3)
        mean = blk.mean(axis=0)
        sd = float(blk.std(axis=0).max())
        lum = float(0.2126 * mean[0] + 0.7152 * mean[1] + 0.0722 * mean[2])
        score = sd - 0.15 * lum
        if best is None or score < best[0]: best = (score, Vector(uv), mean, sd, lum)
    bpy.data.images.remove(img)
    if best is None: raise SystemExit("no UV square of %g fits inside %s near the chin" % (half, texpath))
    return best[1], [round(float(c), 4) for c in best[2]], round(best[3], 4), round(best[4], 4)


# --------------------------------------------------------------------------------------------------- the frog
bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.ops.import_scene.fbx(filepath=IN)
arm = [o for o in bpy.data.objects if o.type == 'ARMATURE'][0]
lods = []
for k in range(4):
    m = bpy.data.objects.get("Frog_LOD%d" % k)
    if m is None: raise SystemExit("no mesh Frog_LOD%d in %s" % (k, IN))
    lods.append(m)
neck = arm.data.bones[PARENT]
z_jaw = neck.head_local.z                       # the sac hangs from the height of Neck's head

# the throat point: the frontmost (front = -Y) body vertex under the jaw, near the middle
body = lods[0]
near = [v.co for v in body.data.vertices if z_jaw - 0.30 <= v.co.z <= z_jaw + 0.02 and abs(v.co.x) < 0.12]
if not near: raise SystemExit("no chest vertices under the jaw")
y_front = min(p.y for p in near)
# How deep the rest ball sits under the chest skin. The frog is built in layers (body, coat, webbing), so "inside
# the mesh" cannot be decided from the nearest surface - a point under the coat is outside the body shell. The sink
# is therefore settled by the picture: the in-session rest diff (the same held pose with the sac bone at 1 and at 0)
# must sit at its noise floor. Tangent showed a disc of the ball on the coat, 0.03 m a smaller one, 0.07 m with the
# ball hung 0.175 m a 38 px patch; 0.10 m hung 0.21 m is the first pair that comes back "the same picture".
SAC_SINK = float(os.environ.get("TW_SAC_SINK", "0.10"))
# The bone head's depth follows, so the inflated ball still stands SAC_OUT in front of the chest whatever the sink.
SAC_IN = (SAC_OUT + TEST_SCALE * (SAC_R + SAC_SINK)) / (TEST_SCALE - 1.0)
SAC_UP = (SAC_DROP - TEST_SCALE * SAC_DOWN) / (TEST_SCALE - 1.0)
head = Vector((0.0, y_front + SAC_IN, z_jaw + SAC_UP))     # the bone head sits inside the chest
centre = Vector((0.0, y_front + SAC_R + SAC_SINK, z_jaw - SAC_DOWN))
print("rest sink %.2f m, hang %.3f m; bone head %.4f m inside, %.4f m under the jaw" % (
    SAC_SINK, SAC_DOWN, SAC_IN, -SAC_UP))
# the ball hangs in front of that head and below it, so it sits inside the chest at rest and travels forward and
# down - out of the body, away from the limbs - as the bone scales it up.


# --------------------------------------------------------------------------------------------------- the bone
select_only([arm], arm)
bpy.ops.object.mode_set(mode='EDIT')
eb = arm.data.edit_bones.new(BONE)
eb.head = head
eb.tail = head + Vector((0.0, 0.0, -2.0 * SAC_R))
eb.parent = arm.data.edit_bones[PARENT]
eb.use_connect = False
eb.use_deform = True
bpy.ops.object.mode_set(mode='OBJECT')

# ---------------------------------------------------------------------------------------------------- the sacs
old = json.load(open(os.path.join(os.path.dirname(IN), "frogrig.json")))
sac_range = {}          # lod -> (first vertex index, count)
uv_patch = {}           # lod -> the island the sac was given
for k in sorted(RINGS):
    segs, rings = RINGS[k]
    o = lods[k]
    before_v = len(o.data.vertices)
    before_t = sum(len(p.vertices) - 2 for p in o.data.polygons)
    body_uv = uv_of_verts(o)
    cand = [(v.index, v.co.copy()) for v in o.data.vertices if z_jaw - 0.30 <= v.co.z <= z_jaw + 0.06]
    if not cand: raise SystemExit("LOD%d has no vertices under the jaw" % k)

    mesh = bpy.data.meshes.new("Sac%d" % k)
    bm = bmesh.new()
    bmesh.ops.create_uvsphere(bm, u_segments=segs, v_segments=rings, radius=SAC_R)
    bmesh.ops.translate(bm, verts=bm.verts, vec=centre)
    bm.to_mesh(mesh)
    bm.free()
    sac = bpy.data.objects.new("Sac%d" % k, mesh)
    bpy.context.scene.collection.objects.link(sac)
    # ONE dedicated island of flat pale chin texels: the sphere is unwrapped into a small square inside the chin's
    # own patch of the texture, so the whole ball reads as one chin colour from every side.
    chin_uvs = [body_uv[vi] for vi, co in cand if vi in body_uv]
    patch, patch_rgb, patch_sd, patch_lum = flat_patch(
        os.path.join(os.path.dirname(IN), old["lods"][k]["texture"]), chin_uvs, SAC_UV_HALF)
    uv_patch[k] = {"centre": [round(patch.x, 5), round(patch.y, 5)], "half": SAC_UV_HALF,
                   "mean_rgb": patch_rgb, "max_channel_sd": patch_sd, "luma": patch_lum}
    uvl = mesh.uv_layers.new(name=o.data.uv_layers.active.name)
    span = SAC_UV_HALF * 0.9                        # a texel of slack inside the square, for the mip chain
    for p in mesh.polygons:
        for li in p.loop_indices:
            d = mesh.vertices[mesh.loops[li].vertex_index].co - centre
            n = d.length or 1.0
            u = math.atan2(d.x, -d.y) / (2.0 * math.pi) + 0.5
            v = math.acos(max(-1.0, min(1.0, d.z / n))) / math.pi
            uvl.data[li].uv = (patch.x + (u - 0.5) * 2.0 * span, patch.y + (v - 0.5) * 2.0 * span)
    sac.vertex_groups.new(name=BONE).add(list(range(len(mesh.vertices))), 1.0, 'REPLACE')

    select_only([o, sac], o)
    bpy.ops.object.join()
    sac_range[k] = (before_v, len(o.data.vertices) - before_v)
    print("LOD%d sac %dx%d: verts %d -> %d, tris %d -> %d" % (
        k, segs, rings, before_v, len(o.data.vertices), before_t,
        sum(len(p.vertices) - 2 for p in o.data.polygons)))

# ---------------------------------------------------------------------- how big the sac blows up, and its clearance
pb = arm.pose.bones[BONE]
pb.scale = (TEST_SCALE, TEST_SCALE, TEST_SCALE)
bpy.context.view_layer.update()
dg = bpy.context.evaluated_depsgraph_get()
ev_obj = lods[0].evaluated_get(dg)
ev = ev_obj.to_mesh()
i0, n0 = sac_range[0]
pts = [ev.vertices[i].co.copy() for i in range(i0, i0 + n0)]
size = [max(p[i] for p in pts) - min(p[i] for p in pts) for i in range(3)]
# where the inflated ball stands, in the frog's own space: the Unity probe that measures what the limbs do uses this
infl_centre = [round((max(p[i] for p in pts) + min(p[i] for p in pts)) * 0.5, 4) for i in range(3)]
ev_obj.to_mesh_clear()
pb.scale = (1.0, 1.0, 1.0)
bpy.context.view_layer.update()
rest = [lods[0].data.vertices[i].co.copy() for i in range(i0, i0 + n0)]
# at rest: how far the ball's front sticks out of the chest (negative = inside, which is what it must be)
poke = y_front - min(p.y for p in rest)

# ------------------------------------------------------------------------------------------------------- export
select_only([arm] + lods, arm)
bpy.ops.export_scene.fbx(filepath=OUT, use_selection=True, object_types={'ARMATURE', 'MESH'}, apply_unit_scale=True,
                         apply_scale_options='FBX_SCALE_ALL', bake_space_transform=False, axis_forward='-Z', axis_up='Y',
                         mesh_smooth_type='OFF', use_mesh_modifiers=False, add_leaf_bones=False, primary_bone_axis='Y',
                         secondary_bone_axis='X', use_armature_deform_only=False, bake_anim=False, path_mode='STRIP',
                         embed_textures=False, use_tspace=False, colors_type='LINEAR')

rep = {"source": "Tools/frogsac.py", "in": IN, "out": OUT, "bone": BONE, "parent": PARENT,
       "bone_head": [round(x, 4) for x in head], "sac_centre": [round(x, 4) for x in centre],
       "rest_radius_m": SAC_R, "sunk_in_m": round(SAC_IN, 4), "rest_sink_m": SAC_SINK, "rest_hang_m": SAC_DOWN, "inflated_drop_m": SAC_DROP,
       "sac_out_m_at_test_scale": SAC_OUT, "uv_island_half": SAC_UV_HALF, "test_scale": TEST_SCALE,
       "sac_size_m_at_test_scale": [round(x, 4) for x in size],
       "sac_across_m_at_test_scale": round(max(size), 4),
       "sac_centre_m_at_test_scale": infl_centre, "sac_radius_m_at_test_scale": round(max(size) / 2.0, 4),
       "rest_poke_out_m": round(poke, 4), "lods": []}
for k, o in enumerate(lods):
    names = {g.index: g.name for g in o.vertex_groups}
    W = [{names[g.group]: g.weight for g in v.groups if g.weight > 1e-4} for v in o.data.vertices]
    row = {"lod": k, "verts": len(o.data.vertices), "tris": sum(len(p.vertices) - 2 for p in o.data.polygons),
           "bones": sorted({n for w in W for n in w}), "max_per_vertex": max(len(w) for w in W),
           "texture": old["lods"][k]["texture"]}
    if k in sac_range:
        row["sac"] = {"segments": RINGS[k][0], "rings": RINGS[k][1],
                      "first_vertex": sac_range[k][0], "verts": sac_range[k][1], "uv_island": uv_patch[k]}
    rep["lods"].append(row)
old["sac"] = {"source": "Tools/frogsac.py", "bone": BONE, "parent": PARENT,
              "bone_head": rep["bone_head"], "centre": rep["sac_centre"], "rest_radius_m": SAC_R,
              "across_m_at_scale_%g" % TEST_SCALE: rep["sac_across_m_at_test_scale"],
              "rest_poke_out_m": rep["rest_poke_out_m"], "sunk_in_m": round(SAC_IN, 4), "rest_sink_m": SAC_SINK,
             
              "centre_m_at_scale_%g" % TEST_SCALE: infl_centre,
              "uv_island_half": SAC_UV_HALF,
              "rings": {str(k): list(RINGS[k]) for k in sorted(RINGS)}}
old["lods"] = rep["lods"]
# newline="\n": the repo keeps frogrig.json with LF, and a CRLF copy makes git print a warning that Tools/selftest.py
# reads back as a file path (the gate went red on it, 2026-10-10).
json.dump(old, open(os.path.join(os.path.dirname(OUT), "frogrig.json"), "w", newline="\n"), indent=1)
json.dump(rep, open(REPORT, "w", newline="\n"), indent=1)
print("DONE", [(l["lod"], l["verts"], l["tris"]) for l in rep["lods"]],
      "sac across %.3f m at scale %g, rest poke-out %.4f m" % (rep["sac_across_m_at_test_scale"], TEST_SCALE, poke))
