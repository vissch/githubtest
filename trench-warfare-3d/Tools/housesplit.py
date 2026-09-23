# Blender (background): split the owner's Tripo "stylized village houses" sheet (already exported in Tripo parts) into
# six houses, square each to the axes, scale to metres, merge the tiny parts (window frames, stones) into the wall they
# touch, cut every part bigger than CUT into capped chunks (the cut face takes the UVs of the skin it closes), and
# export one FBX per chunk, pivot at the middle of the chunk's base (PropDestruction measures a hit to the pivot).
# houses.json lists each chunk's offset in its house, its bounds about its pivot, and stone or timber (read from the
# painted colour, TW_TEX), so BattlefieldKit places a house as one matrix and knows what rests on what.
# TW_LOOSE=1 for a sheet that comes as one welded object (the watchtower sheet): split into loose parts first.
# TW_ONE=1 TW_KEEP=1 for one of the kit's own props (Resources/Env/<set>/<name>.fbx, already metres, ground pivot, front
# +Z): all its parts are one building and it keeps its pivot and facing, so the sliced prop stands where the whole one did.
# usage (owner's Tripo sheet, 2026-09-23; run in Blender 5.0):
#   TW_TEX=<basecolor.jpg> TW_PIVOT=chunk TW_CUT=2.4 TW_SMALL=1.4 blender -b --factory-startup -P housesplit.py -- <fbx> <outdir> <renderdir>
import bpy, bmesh, sys, os, math, json, random
from mathutils import Vector, Matrix
from mathutils.geometry import convex_hull_2d

argv = sys.argv[sys.argv.index("--") + 1:]
fbx, outdir, renderdir = argv[0], argv[1], argv[2]
os.makedirs(outdir, exist_ok=True); os.makedirs(renderdir, exist_ok=True)
SCALE = float(os.environ.get("TW_SCALE", "22.7"))     # sheet units -> metres (two-storey house = 7 m)
CUT = float(os.environ.get("TW_CUT", "1.8"))          # chunk edge target, metres
SMALL = float(os.environ.get("TW_SMALL", "0.7"))      # parts under this get merged into a neighbour
NAMES = json.loads(os.environ.get("TW_NAMES", "{}"))  # cluster index -> house name
TURNS = json.loads(os.environ.get("TW_TURNS", "{}"))  # house name -> extra yaw degrees
PIVOT = os.environ.get("TW_PIVOT", "house")           # house | chunk

bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.ops.import_scene.fbx(filepath=fbx)
scene = bpy.context.scene
if os.environ.get("TW_LOOSE") == "1":
    # a sheet exported as one welded object: its loose parts first, then grouped into buildings like any other sheet
    one = [o for o in scene.objects if o.type == 'MESH'][0]
    bpy.context.view_layer.objects.active = one; one.select_set(True)
    bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.separate(type='LOOSE'); bpy.ops.object.mode_set(mode='OBJECT')
    for o in scene.objects: o.select_set(False)
objs = [o for o in scene.objects if o.type == 'MESH']
for o in objs:
    o.select_set(True); bpy.context.view_layer.objects.active = o
bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
for o in objs: o.select_set(False)
mats = list(objs[0].data.materials)

def bbox(o):
    co = [v.co for v in o.data.vertices]
    return (Vector((min(c.x for c in co), min(c.y for c in co), min(c.z for c in co))),
            Vector((max(c.x for c in co), max(c.y for c in co), max(c.z for c in co))))
def overlap(a, b, m):
    return a[0].x - m <= b[1].x and b[0].x - m <= a[1].x and a[0].y - m <= b[1].y and b[0].y - m <= a[1].y and a[0].z - m <= b[1].z and b[0].z - m <= a[1].z

# --- cluster the parts into houses (3D box overlap) ---
boxes = {o.name: bbox(o) for o in objs}
parent = {o.name: o.name for o in objs}
def find(a):
    while parent[a] != a: parent[a] = parent[parent[a]]; a = parent[a]
    return a
names = [o.name for o in objs]
for i, a in enumerate(names):
    for b in names[i + 1:]:
        if overlap(boxes[a], boxes[b], 0.01):
            parent[find(a)] = find(b)
groups = {}
for n in names: groups.setdefault(find(n), []).append(n)
if os.environ.get("TW_ONE") == "1": groups = {names[0]: names}   # one prop, however many loose parts
KEEP = os.environ.get("TW_KEEP") == "1"
clusters = []
for mem in groups.values():
    lo = Vector((min(boxes[n][0].x for n in mem), min(boxes[n][0].y for n in mem), min(boxes[n][0].z for n in mem)))
    hi = Vector((max(boxes[n][1].x for n in mem), max(boxes[n][1].y for n in mem), max(boxes[n][1].z for n in mem)))
    clusters.append({"lo": lo, "hi": hi, "members": mem})
# stable order: back row (high y) to front, left to right
clusters.sort(key=lambda c: (-round((c["lo"].y + c["hi"].y) / 2 * 3), (c["lo"].x + c["hi"].x) / 2))

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

def fix_fill_uvs(bm, fill_faces):
    uv = bm.loops.layers.uv.active
    if uv is None: return
    for f in fill_faces:
        for l in f.loops:
            src = None
            for l2 in l.vert.link_loops:
                if l2.face not in fill_faces: src = l2[uv].uv.copy(); break
            if src is not None: l[uv].uv = src

def cut(bm, co, no):
    """bisect bm by the plane; returns (inner, outer) bms, each capped. Either may be None."""
    halves = []
    for clear_inner, clear_outer in ((False, True), (True, False)):
        nb = bm.copy()
        res = bmesh.ops.bisect_plane(nb, geom=nb.verts[:] + nb.edges[:] + nb.faces[:], dist=1e-5,
                                     plane_co=co, plane_no=no, clear_inner=clear_inner, clear_outer=clear_outer)
        cut_edges = [e for e in res["geom_cut"] if isinstance(e, bmesh.types.BMEdge)]
        if cut_edges:
            try:
                r = bmesh.ops.edgenet_fill(nb, edges=cut_edges)
                fill = list(r["faces"])
                if fill:
                    r2 = bmesh.ops.triangulate(nb, faces=fill)
                    fill = list(r2["faces"])
                    want = no if clear_inner else -no
                    bad = [f for f in fill if f.normal.dot(want) < 0]
                    if bad: bmesh.ops.reverse_faces(nb, faces=bad)
                    fix_fill_uvs(nb, set(fill))
            except Exception as ex:
                print("FILL FAIL", ex)
        if len(nb.faces) > 0: halves.append(nb)
        else: nb.free(); halves.append(None)
    return halves

def split_big(bm):
    """cut bm along its axes until no chunk edge exceeds CUT; returns a list of bms"""
    out = [bm]; changed = True
    while changed:
        changed = False; nxt = []
        for b in out:
            co = [v.co for v in b.verts]
            lo = Vector((min(c.x for c in co), min(c.y for c in co), min(c.z for c in co)))
            hi = Vector((max(c.x for c in co), max(c.y for c in co), max(c.z for c in co)))
            size = hi - lo
            axis = max(range(3), key=lambda i: size[i])
            if size[axis] > CUT * 1.15:
                n = math.ceil(size[axis] / CUT)
                at = lo[axis] + size[axis] / n
                no = Vector((0, 0, 0)); no[axis] = 1
                pco = Vector((0, 0, 0)); pco[axis] = at
                a, c = cut(b, pco, no)
                b.free()
                for h in (a, c):
                    if h is not None: nxt.append(h)
                changed = True
            else:
                nxt.append(b)
        out = nxt
    return out

def to_object(bm, name):
    me = bpy.data.meshes.new(name); bm.to_mesh(me)
    for m in mats: me.materials.append(m)
    ob = bpy.data.objects.new(name, me); scene.collection.objects.link(ob)
    return ob

cam_data = bpy.data.cameras.new("cam"); cam_data.type = 'ORTHO'
cam = bpy.data.objects.new("cam", cam_data); scene.collection.objects.link(cam); scene.camera = cam
scene.render.engine = 'BLENDER_WORKBENCH'; scene.display.shading.light = 'STUDIO'
scene.display.shading.show_cavity = True; scene.display.shading.show_object_outline = True
scene.render.resolution_x = 900; scene.render.resolution_y = 700

TEX = os.environ.get("TW_TEX")
img = bpy.data.images.load(TEX) if TEX else None
px = list(img.pixels) if img else None
def material_of(bm):
    """stone or timber, from the chunk's painted colour: warm and saturated is wood and tile, grey is stone and plaster"""
    if px is None: return "stone"
    uv = bm.loops.layers.uv.active
    if uv is None: return "stone"
    W, H = img.size; warm = 0.0; total = 0.0
    for f in bm.faces:
        a = f.calc_area()
        for l in f.loops:
            u, v = l[uv].uv
            x = min(W - 1, max(0, int((u % 1.0) * W))); y = min(H - 1, max(0, int((v % 1.0) * H)))
            i = (y * W + x) * 4; r, g, b = px[i], px[i + 1], px[i + 2]
            mx, mn = max(r, g, b), min(r, g, b)
            sat = (mx - mn) / mx if mx > 1e-4 else 0.0
            if sat > 0.30 and r > g * 1.18 and r > b * 1.4: warm += a
            total += a
    return "timber" if total > 0 and warm / total > 0.5 else "stone"

manifest = []
for ci, c in enumerate(clusters):
    hname = NAMES.get(str(ci), "House%d" % ci)
    mem = [o for o in objs if o.name in c["members"]]
    allv = [v.co.copy() for o in mem for v in o.data.vertices]
    centre = sum(allv, Vector()) / len(allv)
    if KEEP:
        # the kit's FBX comes back in the axes it was exported in, after its half turn: undo that turn here, so the one
        # before export puts every vertex back where it was, and keep the prop's own origin as its pivot
        centre = Vector((0, 0, 0)); yaw = math.pi
    else:
        yaw = -min_area_angle([(v.x - centre.x, v.y - centre.y) for v in allv]) + math.radians(TURNS.get(hname, 0))
    rz = Matrix.Rotation(yaw, 3, 'Z')
    turned = [rz @ (v - centre) for v in allv]
    lo = Vector((min(v.x for v in turned), min(v.y for v in turned), min(v.z for v in turned)))
    hi = Vector((max(v.x for v in turned), max(v.y for v in turned), max(v.z for v in turned)))
    base = Vector((0, 0, 0)) if KEEP else Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    M = Matrix.Scale(SCALE, 4) @ Matrix.Translation(-base) @ rz.to_4x4() @ Matrix.Translation(-centre)
    size_m = (hi - lo) * SCALE
    parts = []
    for o in mem:
        bm = bmesh.new(); bm.from_mesh(o.data)
        bmesh.ops.transform(bm, matrix=M, verts=bm.verts)
        co = [v.co for v in bm.verts]
        plo = Vector((min(x.x for x in co), min(x.y for x in co), min(x.z for x in co)))
        phi = Vector((max(x.x for x in co), max(x.y for x in co), max(x.z for x in co)))
        parts.append({"bm": bm, "lo": plo, "hi": phi, "size": max((phi - plo)[i] for i in range(3)), "name": o.name})
    parts.sort(key=lambda p: -p["size"])
    big = [p for p in parts if p["size"] >= SMALL]
    small = [p for p in parts if p["size"] < SMALL]
    if not big: big, small = parts[:1], parts[1:]
    for s in small:
        host = None
        for b in big:
            if overlap((s["lo"], s["hi"]), (b["lo"], b["hi"]), 0.05): host = b; break
        if host is None:
            sc = (s["lo"] + s["hi"]) / 2
            host = min(big, key=lambda b: (sc - (b["lo"] + b["hi"]) / 2).length)
        tmp = bpy.data.meshes.new("tmp"); s["bm"].to_mesh(tmp); s["bm"].free()
        host["bm"].from_mesh(tmp); bpy.data.meshes.remove(tmp)
        host["lo"] = Vector((min(host["lo"][i], s["lo"][i]) for i in range(3)))
        host["hi"] = Vector((max(host["hi"][i], s["hi"][i]) for i in range(3)))
    chunks = []
    for pi, p in enumerate(big):
        for k, b in enumerate(split_big(p["bm"])):
            chunks.append((pi, k, b))
    # a sliver the cut left (a few triangles) joins the nearest chunk, so no chunk is a draw for nothing
    MINTRIS = int(os.environ.get("TW_MINTRIS", "24"))
    def centre_of(b):
        co = [v.co for v in b.verts]; return sum(co, Vector()) / len(co)
    merged = True
    while merged and len(chunks) > 1:
        merged = False
        for i, (pi, k, b) in enumerate(chunks):
            if len(b.faces) >= MINTRIS: continue
            c0 = centre_of(b)
            j = min((j for j in range(len(chunks)) if j != i), key=lambda j: (centre_of(chunks[j][2]) - c0).length)
            tmp = bpy.data.meshes.new("tmp"); b.to_mesh(tmp); b.free()
            chunks[j][2].from_mesh(tmp); bpy.data.meshes.remove(tmp)
            del chunks[i]; merged = True; break
    hobjs = []
    for (pi, k, b) in chunks:
        cname = "%s_%02d_%d" % (hname, pi, k)
        co = [v.co.copy() for v in b.verts]
        clo = Vector((min(x.x for x in co), min(x.y for x in co), min(x.z for x in co)))
        chi = Vector((max(x.x for x in co), max(x.y for x in co), max(x.z for x in co)))
        mat = material_of(b)
        cbase = Vector(((clo.x + chi.x) / 2, (clo.y + chi.y) / 2, clo.z)) if PIVOT == "chunk" else Vector((0, 0, 0))
        if PIVOT == "chunk": bmesh.ops.transform(b, matrix=Matrix.Translation(-cbase), verts=b.verts)
        ob = to_object(b, cname); b.free()
        ob.location = cbase
        tris = sum(len(f.vertices) - 2 for f in ob.data.polygons)
        hobjs.append(ob)
        # Unity axes of a point here, given the half turn before export and the exporter's baked transform (see
        # envsplit.py): Unity (x, y, z) = (-x, z, -y). The front at -Y in the checks comes out at Unity +Z.
        manifest.append({"house": hname, "chunk": cname, "tris": tris, "verts": len(ob.data.vertices), "mat": mat,
                         "offset": [round(-cbase.x, 3), round(cbase.z, 3), round(-cbase.y, 3)],
                         "min": [round(-(chi.x - cbase.x), 3), round(clo.z - cbase.z, 3), round(-(chi.y - cbase.y), 3)],
                         "max": [round(-(clo.x - cbase.x), 3), round(chi.z - cbase.z, 3), round(-(clo.y - cbase.y), 3)]})
    for o in scene.objects:
        if o.type == 'MESH': o.hide_render = o not in hobjs
    mid = Vector((0, 0, size_m.z / 2)); span = max(size_m)
    random.seed(ci)
    for o in hobjs: o.color = (random.random(), random.random(), random.random(), 1)
    for mode in ("TEXTURE", "OBJECT"):
        scene.display.shading.color_type = mode
        for view, d in (("q", Vector((0.9, -1.0, 0.7)).normalized()), ("top", Vector((0.0001, -0.0001, 1)))):
            dist = span * 4 + 2
            cam.location = mid + d * dist
            cam.rotation_euler = (mid - cam.location).to_track_quat('-Z', 'Y').to_euler()
            if view == "top": cam.rotation_euler = (0, 0, 0)
            cam_data.ortho_scale = span * 1.25; cam_data.clip_end = dist * 4
            scene.render.filepath = os.path.join(renderdir, "%s_%s_%s.png" % (hname, view, mode.lower()))
            bpy.ops.render.render(write_still=True)
    for ob in hobjs:
        ob.matrix_world = Matrix.Rotation(math.pi, 4, 'Z')
        for o in scene.objects: o.select_set(False)
        ob.select_set(True); bpy.context.view_layer.objects.active = ob
        bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
        bpy.ops.export_scene.fbx(filepath=os.path.join(outdir, ob.name + ".fbx"), use_selection=True, object_types={'MESH'},
                                 apply_unit_scale=True, apply_scale_options='FBX_SCALE_ALL', bake_space_transform=True,
                                 axis_forward='-Z', axis_up='Y', mesh_smooth_type='OFF', use_mesh_modifiers=True,
                                 add_leaf_bones=False, path_mode='STRIP', embed_textures=False, use_custom_props=False, use_tspace=False)
        ob.hide_render = True
    print("HOUSE %-10s parts %2d -> chunks %3d  %.1f x %.1f x %.1f m (x, height, z)  tris %d" % (
        hname, len(mem), len(hobjs), size_m.x, size_m.z, size_m.y, sum(m["tris"] for m in manifest if m["house"] == hname)))
with open(os.path.join(outdir, "houses.json"), "w") as f: json.dump({"chunks": manifest}, f, indent=1)
print("DONE", len(manifest), "chunks")
