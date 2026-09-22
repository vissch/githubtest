# Blender (background): split one of the owner's Tripo environment sheets (Downloads/env sets, 2026-09-22) into its
# props and export each as its own FBX for Assets/_Project/Resources/Env/<Set> (EnvKitImport prepares them there):
# squared to the axes (the sheets lay them out at an angle), front turned to Unity +Z (Blender -Y in the check renders,
# turned half round for the export), scaled to metres,
# pivot at the middle of its base, Y up baked into the mesh. Also renders front/top/3-4 checks of every export.
# Loose parts are grouped into props where their footprints overlap ("3d": where their boxes overlap, for the plant
# sheet, which stands its props in two rows). The set textures go in beside them as <Set>.jpg (base colour, 2048).
# usage: blender -b --factory-startup -P envsplit.py -- <set key> <fbx> <outdir> <renderdir>
#   set keys and folders: siege Siege, weapon Weapons, stones Stones, wooden Wood, fence Fence, plant Plants
import bpy, bmesh, sys, os, math, json
from mathutils import Vector, Matrix
from mathutils.geometry import convex_hull_2d

argv = sys.argv[sys.argv.index("--") + 1:]
key, fbx, outdir, renderdir = argv[0], argv[1], argv[2], argv[3]
os.makedirs(outdir, exist_ok=True); os.makedirs(renderdir, exist_ok=True)

# per set: grouping, then per prop (cluster index, name, size rule, metres, quarter turns after squaring, flags)
# size rule: h = height, l = longest ground side, w = x extent after turning. flags: flat = lay its thinnest axis
# vertical first, longx = long side along X, noalign = keep the sheet's yaw, leaves/stems = split a cluster by part
SETS = {
    "siege": dict(mode="xy", margin=0.004, props=[
        (0, "SodShelterRuin", "h", 2.4, 0, ""),
        (1, "MGNest", "h", 1.9, 0, ""),
        (2, "ArmouredStand", "h", 2.8, 0, ""),
        (3, "Pillbox", "h", 2.1, 0, ""),
        (4, "Well", "h", 2.5, 0, "longx"),
    ]),
    "weapon": dict(mode="xy", margin=0.004, props=[
        (0, "FieldGun", "h", 1.8, 0, ""),
        (1, "TankTurret", "h", 1.05, 0, ""),
        (2, "Biplane", "w", 5.6, 0, "flat"),
        (3, "ShellStack", "h", 1.0, 0, "longx"),
        (4, "WreckedLimber", "h", 1.3, 0, ""),
        (5, "DudShell", "h", 0.95, 0, "noalign"),
    ]),
    "stones": dict(mode="xy", margin=0.004, props=[
        (0, "WallStub", "h", 2.3, 0, "longx"),
        (1, "RebarSlab", "l", 1.6, 0, "longx"),
        (2, "Boulder", "h", 1.0, 0, ""),
        (3, "Sandbag", "l", 0.8, 0, "longx"),
        (4, "Gabion", "h", 0.95, 0, "noalign"),
    ]),
    "wooden": dict(mode="xy", margin=0.004, props=[
        (0, "BracedPlank", "l", 1.5, 0, "flat longx"),
        (1, "CrossedBoards", "h", 1.5, 0, "longx"),
        (2, "HatchLid", "l", 0.85, 0, "flat"),
        (3, "PlankDoor", "h", 1.8, 0, "longx"),
        (4, "CorrugatedSheet", "l", 2.0, 0, ""),
    ]),
    "fence": dict(mode="xy", margin=0.004, props=[
        (0, "Stakes", "h", 1.35, 0, ""),
        (1, "TimberHedgehog", "h", 1.3, 0, ""),
        (2, "WireFence", "h", 1.3, 0, "longx"),
        (3, "WirePost", "h", 1.3, 0, "noalign"),
        (5, "StoneBarricade", "h", 0.95, 0, "longx"),
    ]),
    "plant": dict(mode="3d", margin=0.002, props=[
        (0, "FallenLog", "l", 2.4, 0, "longx"),
        (1, "SplitStumpTall", "h", 1.5, 0, "noalign"),
        (2, "SplitStump", "h", 1.2, 0, "noalign"),
        (3, "MossStump", "h", 0.85, 0, ""),
        (4, "Poppies", "h", 0.6, 0, "noalign"),
        (5, "Cattails", "h", 1.3, 0, "noalign"),
        (5, "GrassClump", "h", 0.55, 0, "noalign leaves"),
    ]),
}
# turns (degrees, after squaring) read off the check renders: the barrels and the nose to the front
TURNS = {"MGNest": 90, "FieldGun": 70, "TankTurret": -90, "Biplane": -130}
TURNS.update(json.loads(os.environ.get("TW_TURNS", "{}")))
cfg = SETS[key]

bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.ops.import_scene.fbx(filepath=fbx)
src = [o for o in bpy.context.scene.objects if o.type == 'MESH'][0]
bpy.context.view_layer.objects.active = src; src.select_set(True)
bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
me = src.data
bm = bmesh.new(); bm.from_mesh(me); bm.verts.ensure_lookup_table()

parent = list(range(len(bm.verts)))
def find(a):
    while parent[a] != a: parent[a] = parent[parent[a]]; a = parent[a]
    return a
for e in bm.edges:
    a, b = find(e.verts[0].index), find(e.verts[1].index)
    if a != b: parent[a] = b
parts = {}
for v in bm.verts: parts.setdefault(find(v.index), []).append(v.index)
pids = list(parts.keys())
def box(ids):
    co = [bm.verts[i].co for i in ids]
    return (Vector((min(c.x for c in co), min(c.y for c in co), min(c.z for c in co))), Vector((max(c.x for c in co), max(c.y for c in co), max(c.z for c in co))))
boxes = [box(parts[p]) for p in pids]
grp = list(range(len(pids)))
def gfind(a):
    while grp[a] != a: grp[a] = grp[grp[a]]; a = grp[a]
    return a
m = cfg["margin"]; three = cfg["mode"] == "3d"
for i in range(len(boxes)):
    for j in range(i + 1, len(boxes)):
        (a0, a1), (b0, b1) = boxes[i], boxes[j]
        if a0.x - m <= b1.x and b0.x - m <= a1.x and a0.y - m <= b1.y and b0.y - m <= a1.y and (not three or (a0.z - m <= b1.z and b0.z - m <= a1.z)):
            ri, rj = gfind(i), gfind(j)
            if ri != rj: grp[ri] = rj
groups = {}
for i in range(len(pids)): groups.setdefault(gfind(i), []).append(i)
clusters = []
for members in groups.values():
    lo = Vector((min(boxes[k][0].x for k in members), min(boxes[k][0].y for k in members), min(boxes[k][0].z for k in members)))
    hi = Vector((max(boxes[k][1].x for k in members), max(boxes[k][1].y for k in members), max(boxes[k][1].z for k in members)))
    clusters.append({"lo": lo, "hi": hi, "members": members})
clusters.sort(key=lambda c: (-round((c["lo"].z if three else (c["lo"].y + c["hi"].y) / 2) * 4), (c["lo"].x + c["hi"].x) / 2))

scene = bpy.context.scene
scene.render.engine = 'BLENDER_WORKBENCH'
scene.display.shading.light = 'STUDIO'
scene.display.shading.color_type = 'TEXTURE'
scene.display.shading.show_cavity = True
scene.render.resolution_x = 420; scene.render.resolution_y = 420
cam_data = bpy.data.cameras.new("cam"); cam_data.type = 'ORTHO'
cam = bpy.data.objects.new("cam", cam_data); scene.collection.objects.link(cam); scene.camera = cam
src.hide_render = True

def min_area_angle(points2d):
    hull = convex_hull_2d(points2d)
    pts = [points2d[i] for i in hull]
    best, angle = 1e18, 0.0
    for i in range(len(pts)):
        a, b = pts[i], pts[(i + 1) % len(pts)]
        edge = (b[0] - a[0], b[1] - a[1])
        if abs(edge[0]) + abs(edge[1]) < 1e-9: continue
        th = math.atan2(edge[1], edge[0])
        c, s = math.cos(-th), math.sin(-th)
        xs = [p[0] * c - p[1] * s for p in pts]; ys = [p[0] * s + p[1] * c for p in pts]
        area = (max(xs) - min(xs)) * (max(ys) - min(ys))
        if area < best: best, angle = area, th
    return angle

manifest = []
for (ci, name, rule, metres, quarter, flags) in cfg["props"]:
    c = clusters[ci]
    members = c["members"]
    if "leaves" in flags:
        # the low, spreading parts only (the blades round the foot of a clump, not the tall stems and heads)
        height = c["hi"].z - c["lo"].z
        members = [k for k in members if boxes[k][1].z - c["lo"].z < 0.55 * height]
    keep = set()
    for k in members: keep.update(parts[pids[k]])
    nb = bm.copy(); nb.verts.ensure_lookup_table()
    bmesh.ops.delete(nb, geom=[v for v in nb.verts if v.index not in keep], context='VERTS')
    mesh = bpy.data.meshes.new(name); nb.to_mesh(mesh); nb.free()
    for mat in me.materials: mesh.materials.append(mat)
    ob = bpy.data.objects.new(name, mesh); scene.collection.objects.link(ob)
    verts = [v.co.copy() for v in mesh.vertices]
    centre = sum(verts, Vector()) / len(verts)
    rot = Matrix.Identity(3)
    if "flat" in flags:
        # lay it down: the axis of least spread becomes vertical
        cov = Matrix(((0,0,0),(0,0,0),(0,0,0)))
        for v in verts:
            d = v - centre
            for r in range(3):
                for q in range(3): cov[r][q] += d[r] * d[q]
        # power iteration for the largest two axes; the smallest is their cross product
        def dominant(M, guess):
            x = guess.normalized()
            for _ in range(60): x = (M @ x).normalized()
            return x
        e1 = dominant(cov, Vector((1, 0.3, 0.1)))
        lam1 = (cov @ e1).dot(e1)
        M2 = cov - lam1 * Matrix(((e1.x*e1.x, e1.x*e1.y, e1.x*e1.z), (e1.y*e1.x, e1.y*e1.y, e1.y*e1.z), (e1.z*e1.x, e1.z*e1.y, e1.z*e1.z)))
        e2 = dominant(M2, Vector((0.2, 1, 0.1)))
        e3 = e1.cross(e2).normalized()
        if e3.z < 0: e3 = -e3
        rot = e3.rotation_difference(Vector((0, 0, 1))).to_matrix()
    verts0 = [v - centre for v in verts]
    flat_turned = [rot @ v for v in verts0]
    yaw = 0.0
    if "noalign" not in flags:
        yaw = -min_area_angle([(v.x, v.y) for v in flat_turned])
    yaw += math.radians(quarter * 90 + TURNS.get(name, 0))
    rz = Matrix.Rotation(yaw, 3, 'Z')
    total = rz @ rot
    turned = [total @ v for v in verts0]
    if "longx" in flags:
        sx = max(v.x for v in turned) - min(v.x for v in turned); sy = max(v.y for v in turned) - min(v.y for v in turned)
        if sy > sx * 1.02 and name not in TURNS:
            total = Matrix.Rotation(math.pi / 2, 3, 'Z') @ total; turned = [total @ v for v in verts0]
    lo = Vector((min(v.x for v in turned), min(v.y for v in turned), min(v.z for v in turned)))
    hi = Vector((max(v.x for v in turned), max(v.y for v in turned), max(v.z for v in turned)))
    size = hi - lo
    current = size.z if rule == "h" else max(size.x, size.y) if rule == "l" else size.x
    k = metres / current
    base = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    # through the object transform, so transform_apply turns the custom split normals with the vertices
    ob.matrix_world = Matrix.Scale(k, 4) @ Matrix.Translation(-base) @ total.to_4x4() @ Matrix.Translation(-centre)
    for o in scene.objects: o.select_set(False)
    ob.select_set(True); bpy.context.view_layer.objects.active = ob
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    bpy.context.view_layer.update()
    size_m = size * k
    tris = sum(len(p.vertices) - 2 for p in mesh.polygons)
    # checks: front (from -Y; Unity +Z once exported), top (front at the bottom), 3/4 from front-right
    for o in scene.objects:
        if o.type == 'MESH': o.hide_render = o is not ob
    mid = Vector((0, 0, size_m.z / 2))
    span = max(size_m.x, size_m.y, size_m.z)
    for view, d in (("front", Vector((0, -1, 0.0001))), ("top", Vector((0.0001, -0.0001, 1))), ("q", Vector((0.9, -1.0, 0.7)).normalized())):
        dist = span * 4 + 2
        cam.location = mid + d * dist
        cam.rotation_euler = (mid - cam.location).to_track_quat('-Z', 'Y').to_euler()
        if view == "top": cam.rotation_euler = (0, 0, 0)
        cam_data.ortho_scale = span * 1.2; cam_data.clip_end = dist * 4
        scene.render.filepath = os.path.join(renderdir, "%s_%s.png" % (name, view))
        bpy.ops.render.render(write_still=True)
    # export alone, turned half round first: with the space transform baked, Blender's -Y lands on Unity -Z whatever
    # the forward axis says, so the front (at -Y in the checks above) goes to +Y to come out at Unity +Z
    ob.matrix_world = Matrix.Rotation(math.pi, 4, 'Z')
    for o in scene.objects: o.select_set(False)
    ob.select_set(True); bpy.context.view_layer.objects.active = ob
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    path = os.path.join(outdir, name + ".fbx")
    bpy.ops.export_scene.fbx(filepath=path, use_selection=True, object_types={'MESH'}, apply_unit_scale=True,
                             apply_scale_options='FBX_SCALE_ALL', bake_space_transform=True, axis_forward='-Z', axis_up='Y',
                             mesh_smooth_type='OFF', use_mesh_modifiers=True, add_leaf_bones=False, path_mode='STRIP',
                             embed_textures=False, use_custom_props=False, use_tspace=False)
    manifest.append({"set": key, "name": name, "tris": tris, "verts": len(mesh.vertices), "size_m": [round(size_m.x, 3), round(size_m.z, 3), round(size_m.y, 3)], "scale": round(k, 3)})
    ob.hide_render = True
    print("EXPORT %-16s tris %4d  %.2f x %.2f x %.2f m (x, height, z)  scale %.2f" % (name, tris, size_m.x, size_m.z, size_m.y, k))
with open(os.path.join(renderdir, key + ".json"), "w") as f: json.dump(manifest, f, indent=1)
print("DONE", key)
