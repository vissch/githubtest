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
#   * gives each sac vertex the UV of the nearest body vertex under the jaw, so the sac takes the frog's own pale
#     chin texels and NO texture is repainted.
# Segments x rings: LOD0 16x8, LOD1 12x6, LOD2 10x5.
#
# usage: blender -b --factory-startup -P frogsac.py -- <in.fbx> <out.fbx> <report.json>
#   the out dir also gets frogrig.json (the old one with the new counts and the sac's numbers).
#   TW_SAC_R (0.07 m, rest radius), TW_SAC_TEST_SCALE (7.6, the scale the report measures the sac at).
import bpy, bmesh, sys, os, json
from mathutils import Vector

argv = sys.argv[sys.argv.index("--") + 1:]
IN, OUT, REPORT = argv[0], argv[1], argv[2]
SAC_R = float(os.environ.get("TW_SAC_R", "0.07"))
TEST_SCALE = float(os.environ.get("TW_SAC_TEST_SCALE", "7.6"))
RINGS = {0: (16, 8), 1: (12, 6), 2: (10, 5)}     # segments x rings, per LOD
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
head = Vector((0.0, y_front, z_jaw))
# the ball: hung below the head, and pushed back so its front is tangent to the chest - inside the frog at rest
centre = Vector((0.0, y_front + SAC_R, z_jaw - SAC_R))

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
sac_range = {}          # lod -> (first vertex index, count)
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
    # the chin's own texels: each sac vertex takes the UV of the nearest body vertex under the jaw
    uvl = mesh.uv_layers.new(name=o.data.uv_layers.active.name)
    for p in mesh.polygons:
        for li in p.loop_indices:
            co = mesh.vertices[mesh.loops[li].vertex_index].co
            vi = min(cand, key=lambda c: (c[1] - co).length_squared)[0]
            uvl.data[li].uv = body_uv[vi]
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
ev_obj.to_mesh_clear()
pb.scale = (1.0, 1.0, 1.0)
bpy.context.view_layer.update()
rest = [lods[0].data.vertices[i].co.copy() for i in range(i0, i0 + n0)]
# at rest: how far the ball's front sticks out of the chest (negative = inside)
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
       "rest_radius_m": SAC_R, "test_scale": TEST_SCALE,
       "sac_size_m_at_test_scale": [round(x, 4) for x in size],
       "sac_across_m_at_test_scale": round(max(size), 4),
       "rest_poke_out_m": round(poke, 4), "lods": []}
old = json.load(open(os.path.join(os.path.dirname(IN), "frogrig.json")))
for k, o in enumerate(lods):
    names = {g.index: g.name for g in o.vertex_groups}
    W = [{names[g.group]: g.weight for g in v.groups if g.weight > 1e-4} for v in o.data.vertices]
    row = {"lod": k, "verts": len(o.data.vertices), "tris": sum(len(p.vertices) - 2 for p in o.data.polygons),
           "bones": sorted({n for w in W for n in w}), "max_per_vertex": max(len(w) for w in W),
           "texture": old["lods"][k]["texture"]}
    if k in sac_range:
        row["sac"] = {"segments": RINGS[k][0], "rings": RINGS[k][1],
                      "first_vertex": sac_range[k][0], "verts": sac_range[k][1]}
    rep["lods"].append(row)
old["sac"] = {"source": "Tools/frogsac.py", "bone": BONE, "parent": PARENT,
              "bone_head": rep["bone_head"], "centre": rep["sac_centre"], "rest_radius_m": SAC_R,
              "across_m_at_scale_%g" % TEST_SCALE: rep["sac_across_m_at_test_scale"],
              "rest_poke_out_m": rep["rest_poke_out_m"],
              "rings": {str(k): list(RINGS[k]) for k in sorted(RINGS)}}
old["lods"] = rep["lods"]
json.dump(old, open(os.path.join(os.path.dirname(OUT), "frogrig.json"), "w"), indent=1)
json.dump(rep, open(REPORT, "w"), indent=1)
print("DONE", [(l["lod"], l["verts"], l["tris"]) for l in rep["lods"]],
      "sac across %.3f m at scale %g, rest poke-out %.4f m" % (rep["sac_across_m_at_test_scale"], TEST_SCALE, poke))
