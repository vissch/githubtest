# Blender (background): split the owner's three-LOD Tripo tank (2026-09-26: Downloads/tank+3d+model.zip = LOD0,
# "(1)" = LOD1, "(2)" = LOD2) into the SAME named rigid parts at every LOD, so the tank falls apart the same way
# whatever distance it is drawn at. Output feeds the playground (Resources/Playground/Tanks/<Name>/), and follows
# tanksplit.py's conventions so TankModel can take it later: one object per part, origin on its pivot, front +Z, metres.
#
# Why rules and not island tables: the three LODs are the same design but Tripo fused different things at each level.
# LOD0 has its fenders welded into the hull, LOD2 has its turret welded into the hull, and the LODs' loose parts do not
# correspond one to one. So each part is a REGION of space (in the model's own frame, front -Y, left +X, ground z=0,
# measured off all three LODs: they share one bounding box to 1 %), and:
#   * a loose island goes whole to the part whose region holds most of its area (bolts stay on their plate);
#   * the hull body (the largest island) is CUT with planes: the turret ring (only inside the turret's footprint) and
#     the casemate plates (z above the fenders, split left/right and front/back). The cuts are the same planes at
#     every LOD, so a plate blown off at LOD2 is the plate that was blown off at LOD0. Cut openings are capped with the
#     atlas' darkest texel: the inside of a hull reads as soot when a plate is gone.
# Every part must exist at every LOD; the script fails loudly otherwise, and writes each part's bounds per LOD into
# tank3.json so the playground's tests can check they agree.
#
# usage: blender -b --factory-startup -P tank3split.py -- <name> <lod0.fbx> <lod1.fbx> <lod2.fbx> <outdir> <renderdir>
#   each fbx beside its textures named <stem>_tex0_0.jpg (base colour) .. _3 (normal); short paths (MAX_PATH).
import bpy, bmesh, sys, os, math, json, random
import numpy as np
from mathutils import Vector, Matrix
from mathutils.bvhtree import BVHTree

argv = sys.argv[sys.argv.index("--") + 1:]
NAME, FBX, OUTDIR, RENDERDIR = argv[0], argv[1:4], argv[4], argv[5]
os.makedirs(OUTDIR, exist_ok=True); os.makedirs(RENDERDIR, exist_ok=True)
SCALE = float(os.environ.get("TW_SCALE", "6.6"))   # metres per model unit: tracks 0.77 units -> 5.1 m, Maw's length

# ------------------------------------------------------------------------------------------------ the part regions
# box = (xmin, xmax, ymin, ymax, zmin, zmax) on an island's area-weighted face centres. First match wins.
INF = 9.0
REGIONS = [
    ("Antenna",  (-0.20, -0.02, 0.15, 0.47, 0.44, INF), dict(thin=True)),
    ("Turret",   (-0.125, 0.125, -0.09, 0.27, 0.575, INF), {}),
    ("Lamp_L",   (0.09, 0.20, -0.22, -0.04, 0.50, 0.66), {}),
    ("Lamp_R",   (-0.20, -0.09, -0.22, -0.04, 0.50, 0.66), {}),
    ("Gun",      (-0.075, 0.075, -INF, -0.06, 0.365, 0.53), dict(long=True)),
    ("RearGun",  (-0.18, -0.02, 0.18, INF, 0.33, 0.52), {}),
    ("Stack",    (-INF, -0.18, -0.22, 0.02, 0.33, INF), {}),
    ("Track_L",  (0.15, INF, -INF, INF, -INF, 0.235), dict(ztop=0.30)),
    ("Track_R",  (-INF, -0.15, -INF, INF, -INF, 0.235), dict(ztop=0.30)),
]
# the hull body is cut into these (after the turret cut): chassis below Z_DECK, casemate plates above it
Z_DECK = 0.335          # just above the fenders (0.225-0.323 at every LOD)
Y_MID = 0.06            # front/back split of the casemate
TURRET_BOX = (-0.125, 0.125, -0.09, 0.27)
PLATES = {("L", "F"): "Plate_LF", ("R", "F"): "Plate_RF", ("L", "B"): "Plate_LB", ("R", "B"): "Plate_RB"}
PARENT = {n: "Hull" for n in ["Antenna", "Turret", "Lamp_L", "Lamp_R", "Gun", "RearGun", "Stack", "Track_L", "Track_R"] + list(PLATES.values())}
ALL_PARTS = ["Hull"] + list(PARENT)
# destruction metadata the playground reads (mass share, what breaks it off)
BREAK = {
    "Antenna": dict(tier=1, mass=0.2), "Lamp_L": dict(tier=1, mass=0.3), "Lamp_R": dict(tier=1, mass=0.3),
    "Stack": dict(tier=1, mass=0.6), "RearGun": dict(tier=2, mass=1.2), "Track_L": dict(tier=2, mass=4.0),
    "Track_R": dict(tier=2, mass=4.0), "Gun": dict(tier=3, mass=2.0), "Turret": dict(tier=3, mass=3.0),
    "Plate_LF": dict(tier=4, mass=2.5), "Plate_RF": dict(tier=4, mass=2.5), "Plate_LB": dict(tier=4, mass=2.5),
    "Plate_RB": dict(tier=4, mass=2.5), "Hull": dict(tier=9, mass=20.0),
}

def in_box(c, b): return b[0] <= c[0] <= b[1] and b[2] <= c[1] <= b[3] and b[4] <= c[2] <= b[5]

# -------------------------------------------------------------------------------------------------------- loading
def load(fbx, lod):
    before = set(bpy.data.objects)
    bpy.ops.import_scene.fbx(filepath=fbx)
    obj = [o for o in bpy.data.objects if o not in before and o.type == 'MESH'][0]
    for o in bpy.context.selected_objects: o.select_set(False)
    obj.select_set(True); bpy.context.view_layer.objects.active = obj
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    stem = os.path.splitext(fbx)[0]
    img = bpy.data.images.load(stem + "_tex0_0.jpg"); img.name = "LOD%d_atlas" % lod
    mat = bpy.data.materials.new("LOD%d_mat" % lod); mat.use_nodes = True
    t = mat.node_tree.nodes.new("ShaderNodeTexImage"); t.image = img
    mat.node_tree.links.new(t.outputs["Color"], mat.node_tree.nodes["Principled BSDF"].inputs["Base Color"])
    obj.data.materials.clear(); obj.data.materials.append(mat)
    return obj, img, mat, stem

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

def classify_island(bm, fs):
    """Area-weighted vote of the island's faces over the regions; returns the winner or 'Hull'."""
    votes = {}; total = 0.0
    zs = [v.co.z for i in fs for v in bm.faces[i].verts]
    lo = Vector((min(v.co.x for i in fs for v in bm.faces[i].verts), min(v.co.y for i in fs for v in bm.faces[i].verts), min(zs)))
    hi = Vector((max(v.co.x for i in fs for v in bm.faces[i].verts), max(v.co.y for i in fs for v in bm.faces[i].verts), max(zs)))
    size = hi - lo
    for i in fs:
        f = bm.faces[i]; a = f.calc_area(); c = f.calc_center_median(); total += a
        for name, box, opt in REGIONS:
            if not in_box(c, box): continue
            if opt.get("thin") and not (size.z > 0.1 and size.x < 0.1 and size.y < 0.13): continue
            if "ztop" in opt and hi.z > opt["ztop"]: continue
            if opt.get("long") and not (size.y > 1.4 * size.z): continue
            votes[name] = votes.get(name, 0.0) + a; break
    if not votes: return "Hull", 0.0
    win = max(votes, key=votes.get)
    share = votes[win] / max(total, 1e-9)
    return (win if share >= 0.5 else "Hull"), share

# ------------------------------------------------------------------------------------------------------- cutting
def take(bm, keep):
    out = bm.copy(); out.faces.ensure_lookup_table()
    bmesh.ops.delete(out, geom=[f for f in out.faces if f.index not in keep], context='FACES')
    bmesh.ops.delete(out, geom=[v for v in out.verts if not v.link_faces], context='VERTS')
    return out

def bisect(bm, co, no):
    geom = bm.verts[:] + bm.edges[:] + bm.faces[:]
    bmesh.ops.bisect_plane(bm, geom=geom, plane_co=co, plane_no=no)
    bm.faces.ensure_lookup_table(); bm.verts.ensure_lookup_table()

def on_planes(v, planes, eps=1e-4):
    return any(abs((v.co - Vector(co)).dot(Vector(no))) < eps for co, no in planes)

def cap(bm, planes, uv):
    """Fill every open loop that runs along a cut plane; the cap takes the soot texel."""
    edges = [e for e in bm.edges if e.is_boundary and all(on_planes(v, planes) for v in e.verts)]
    if not edges: return 0
    loops_edges = set(edges)
    # holes_fill needs whole boundary loops: take every boundary edge in a loop that has a cut edge
    bnd = [e for e in bm.edges if e.is_boundary]
    par = {e: e for e in bnd}
    def find(e):
        while par[e] is not e: par[e] = par[par[e]]; e = par[e]
        return e
    byv = {}
    for e in bnd:
        for v in e.verts: byv.setdefault(v, []).append(e)
    for es in byv.values():
        for e in es[1:]:
            a, b = find(es[0]), find(e)
            if a is not b: par[a] = b
    roots = {find(e) for e in loops_edges}
    fill = {e for e in bnd if find(e) in roots}
    # walk each loop and close it with a fan round its centre (holes_fill gives up on non-planar loops)
    uvl = bm.loops.layers.uv.active
    new = []
    while fill:
        e0 = fill.pop(); loop = [e0.verts[0], e0.verts[1]]; used = {e0}
        while True:
            nxt = [e for e in byv.get(loop[-1], []) if e in fill and e not in used]
            if not nxt: break
            e = nxt[0]; used.add(e); fill.discard(e)
            v = e.other_vert(loop[-1])
            if v is loop[0]: break
            loop.append(v)
        if len(loop) < 3: continue
        c = sum((v.co for v in loop), Vector()) / len(loop)
        vc = bm.verts.new(c)
        for i in range(len(loop)):
            a_, b_ = loop[i], loop[(i + 1) % len(loop)]
            try: f = bm.faces.new((a_, b_, vc))
            except ValueError: continue
            new.append(f)
    for f in new:
        for l in f.loops: l[uvl].uv = uv
    # a cap faces out of the part: flip any whose normal points at the part's own middle
    mid = sum((v.co for v in bm.verts), Vector()) / max(1, len(bm.verts))
    flip = [f for f in new if f.normal.dot(f.calc_center_median() - mid) < 0]
    if flip: bmesh.ops.reverse_faces(bm, faces=flip)
    return len(new)

def cut_hull(hull, uv, ring_top=None):
    """Turret ring (inside the turret footprint only) then the casemate plates. Returns {part: bmesh}, caps."""
    x0, x1, y0, y1 = TURRET_BOX
    out = {}
    # 1. turret ring: faces above the ring plane inside the footprint -> Turret. Ring height = the deck under the
    #    turret: the lowest z of the hull's upward faces inside the footprint above 0.5 (the casemate roof).
    z_ring = ring_top + 0.002 if ring_top is not None else None
    top = max(v.co.z for v in hull.verts)
    planes = []
    if z_ring is not None and top > z_ring + 0.03:
        bisect(hull, (0, 0, z_ring), (0, 0, 1)); planes.append(((0, 0, z_ring), (0, 0, 1)))
        tur = {f.index for f in hull.faces if f.calc_center_median().z > z_ring and x0 < f.calc_center_median().x < x1 and y0 < f.calc_center_median().y < y1}
        if tur:
            out["Turret"] = take(hull, tur)
            hull = take(hull, {f.index for f in hull.faces} - tur)
    # 2. casemate plates
    bisect(hull, (0, 0, Z_DECK), (0, 0, 1))
    bisect(hull, (0, 0, 0), (1, 0, 0))
    bisect(hull, (0, Y_MID, 0), (0, 1, 0))
    planes += [((0, 0, Z_DECK), (0, 0, 1)), ((0, 0, 0), (1, 0, 0)), ((0, Y_MID, 0), (0, 1, 0))]
    groups = {n: set() for n in list(PLATES.values()) + ["Hull"]}
    for f in hull.faces:
        c = f.calc_center_median()
        if c.z < Z_DECK: groups["Hull"].add(f.index)
        else: groups[PLATES[("L" if c.x > 0 else "R", "F" if c.y < Y_MID else "B")]].add(f.index)
    for n, idx in groups.items():
        if idx: out[n] = take(hull, idx)
    caps = {n: cap(bm, planes, uv) for n, bm in out.items()}
    return out, caps, z_ring

def join(bms):
    out = bmesh.new(); me = bpy.data.meshes.new("tmp")
    for bm in bms: bm.to_mesh(me); out.from_mesh(me)
    bpy.data.meshes.remove(me)
    return out

def bounds(bm):
    co = [v.co for v in bm.verts]
    return (Vector((min(c.x for c in co), min(c.y for c in co), min(c.z for c in co))),
            Vector((max(c.x for c in co), max(c.y for c in co), max(c.z for c in co))))

# -------------------------------------------------------------------------------------------------------- one LOD
def split(lod, fbx):
    obj, img, mat, stem = load(fbx, lod)
    uv = dark_uv(img)
    bm = bmesh.new(); bm.from_mesh(obj.data); bm.faces.ensure_lookup_table()
    isl = islands(bm)
    # the hull body: the island with the largest bounding volume
    def vol(fs):
        co = [v.co for i in fs for v in bm.faces[i].verts]
        return np.prod([max(c[k] for c in co) - min(c[k] for c in co) for k in range(3)])
    body = max(range(len(isl)), key=lambda k: vol(isl[k]))
    parts = {n: [] for n in ALL_PARTS}
    log = []
    boxes = []
    for fs in isl:
        co = [v.co for i in fs for v in bm.faces[i].verts]
        boxes.append((Vector([min(c[k] for c in co) for k in range(3)]), Vector([max(c[k] for c in co) for k in range(3)])))
    def touches(a, b, pad=0.012):
        return all(a[0][k] - pad <= b[1][k] and b[0][k] - pad <= a[1][k] for k in range(3))
    floaters = {k for k in range(len(isl)) if k != body and not any(touches(boxes[k], boxes[j]) for j in range(len(isl)) if j != k)}
    if floaters: print("LOD%d: dropped floaters %s" % (lod, [(k, len(isl[k]), [round(x, 3) for x in (boxes[k][0] + boxes[k][1]) / 2]) for k in floaters]))
    for k, fs in enumerate(isl):
        if k == body or k in floaters: continue
        name, share = classify_island(bm, fs)
        parts[name].append(take(bm, set(fs)))
        log.append((name, len(fs), round(share, 2)))
    hull_bm = take(bm, set(isl[body]))
    # the turret ring: the flattest, widest loose plate the Turret region took; the turret body is cut off the hull
    # just above it where Tripo fused the two (LOD2)
    rings = [p for p in parts["Turret"] if (bounds(p)[1].z - bounds(p)[0].z) < 0.05 and (bounds(p)[1].x - bounds(p)[0].x) > 0.15]
    ring_top = max(bounds(p)[1].z for p in rings) if rings else None
    cuts, caps, z_ring = cut_hull(hull_bm, uv, ring_top)
    # the hull's own share of loose bits (rivets, knobs, fenders) follow the plate they sit on when above the deck
    loose_hull = parts.pop("Hull"); parts["Hull"] = []
    for piece in loose_hull:
        lo, hi = bounds(piece); c = (lo + hi) / 2
        if c.z >= Z_DECK and (hi - lo).length < 0.2:
            n = PLATES[("L" if c.x > 0 else "R", "F" if c.y < Y_MID else "B")]
            parts[n].append(piece)
        else:
            parts["Hull"].append(piece)
    for n, piece in cuts.items(): parts[n].insert(0, piece)
    out = {}
    for n in ALL_PARTS:
        assert parts[n], "LOD%d: part %s is EMPTY (rules miss it at this LOD)" % (lod, n)
        out[n] = join(parts[n])
    print("LOD%d open edges by part: %s" % (lod, {n: sum(1 for e in bm_.edges if e.is_boundary) for n, bm_ in out.items()}))
    print("LOD%d: %d islands, body #%d, ring z %s, caps %s" % (lod, len(isl), body, z_ring, caps))
    return out, img, mat, stem, uv, log

# ------------------------------------------------------------------------------------------------ pivots/sockets
def pivots_and_sockets(P):
    """Measured on LOD0 and used for every LOD, so all three share one skeleton of pivots."""
    piv = {"Hull": Vector((0, 0, 0))}
    for n, bm in P.items():
        if n == "Hull": continue
        lo, hi = bounds(bm); c = (lo + hi) / 2
        piv[n] = c
    lo, hi = bounds(P["Turret"]); piv["Turret"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    lo, hi = bounds(P["Gun"]); gun_c = Vector(((lo.x + hi.x) / 2, 0, (lo.z + hi.z) / 2))
    piv["Gun"] = Vector((gun_c.x, hi.y - 0.02, gun_c.z))          # trunnion at the breech end (back of the barrel)
    lo, hi = bounds(P["RearGun"]); piv["RearGun"] = Vector(((lo.x + hi.x) / 2, lo.y + 0.02, (lo.z + hi.z) / 2))
    lo, hi = bounds(P["Antenna"]); piv["Antenna"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    lo, hi = bounds(P["Stack"]); piv["Stack"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    # sockets, in the model frame (front -Y)
    sock = {}
    glo, ghi = bounds(P["Gun"]); sock["Socket_Muzzle"] = ("Gun", Vector((gun_c.x, glo.y - 0.004, gun_c.z)))
    rlo, rhi = bounds(P["RearGun"]); sock["Socket_MuzzleRear"] = ("RearGun", Vector(((rlo.x + rhi.x) / 2, rhi.y + 0.004, (rlo.z + rhi.z) / 2)))
    slo, shi = bounds(P["Stack"]); sock["Socket_Exhaust0"] = ("Stack", Vector(((slo.x + shi.x) / 2, (slo.y + shi.y) / 2, shi.z + 0.004)))
    tlo, thi = bounds(P["Turret"]); sock["Socket_Crew"] = ("Turret", Vector(((tlo.x + thi.x) / 2, (tlo.y + thi.y) / 2, thi.z)))
    # fire on the rear deck and in each plate's opening, ray-cast onto the hull top
    tree = BVHTree.FromBMesh(join([P[n] for n in ["Hull"] + list(PLATES.values())]))
    hlo, hhi = bounds(P["Hull"])
    for i, (x, yf) in enumerate(((-0.08, 0.78), (0.09, 0.70), (0.0, 0.35))):
        o = Vector((x, hlo.y + (hhi.y - hlo.y) * yf, 2.0))
        hit = tree.ray_cast(o, Vector((0, 0, -1)))
        sock["Socket_Fire%d" % i] = ("Hull", Vector((x, o.y, hit[0].z if hit[0] else hhi.z)))
    for side, n in (("L", "Track_L"), ("R", "Track_R")):
        lo, hi = bounds(P[n])
        sock["Socket_Dust_" + side] = ("Hull", Vector(((lo.x + hi.x) / 2, hi.y, lo.z + 0.02)))
        sock["Socket_TrackFire_" + side] = (n, Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, hi.z)))
    sock["Socket_Deck"] = ("Hull", Vector((0, 0.05, Z_DECK)))
    return piv, sock

# ---------------------------------------------------------------------------------------------------- build + export
TURN = Matrix.Rotation(math.pi, 4, 'Z')   # front to Blender +Y, which the FBX export puts at Unity +Z
def unity(v): return [round(-v.x, 5), round(v.z, 5), round(-v.y, 5)]   # model frame (front -Y) -> Unity (front +Z)

def make(lod, P, piv, mat):
    root = bpy.data.objects.new("%s_LOD%d" % (NAME, lod), None); bpy.context.scene.collection.objects.link(root)
    S = Matrix.Scale(SCALE, 4); objs = {}
    for n in ALL_PARTS:
        bm = P[n].copy()
        bmesh.ops.transform(bm, matrix=S @ TURN @ Matrix.Translation(-piv[n]), verts=bm.verts)
        me = bpy.data.meshes.new("%s_LOD%d_%s" % (NAME, lod, n)); bm.to_mesh(me); bm.free()
        me.materials.append(mat)
        o = bpy.data.objects.new(n, me); bpy.context.scene.collection.objects.link(o)
        objs[n] = o
    # FLAT under the root empty: the playground places parts from tank3.json (nested FBX nodes import turned, see
    # TankImport), so the FBX only has to carry meshes whose origin is the pivot.
    for n, o in objs.items():
        o.parent = root; o.location = (TURN @ piv[n]) * SCALE
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
    mid = Vector((0, 0, 2.0)); d = Vector((-1, 1.1, 0.8)).normalized()
    cam.location = mid + d * 30; cam.rotation_euler = (-d).to_track_quat('-Z', 'Y').to_euler()
    cam.data.ortho_scale = 11 + exploded * 1.6; cam.data.clip_end = 100
    scn.render.filepath = os.path.join(RENDERDIR, tag + ".png")
    bpy.ops.render.render(write_still=True)
    for o, l in saved.items(): o.location = l

# ------------------------------------------------------------------------------------------------------------ main
bpy.ops.wm.read_factory_settings(use_empty=True)
lods = [split(k, f) for k, f in enumerate(FBX)]
# Tripo does not always put a small fitting in the same place at every LOD (this tank's antenna stands 0.1 units, 0.8 m
# drawn, further back at LOD1/2 than at LOD0), so the LOD switch would make it jump. A tier-1 part (a fitting, not a
# cut of the hull) whose centre strays from LOD0's by more than SNAP is moved onto LOD0's centre; each move is logged.
SNAP = 0.02
snapped = []
for k in (1, 2):
    for n in ALL_PARTS:
        if BREAK[n]["tier"] != 1: continue
        lo0, hi0 = bounds(lods[0][0][n]); lo, hi = bounds(lods[k][0][n])
        d = (lo0 + hi0) / 2 - (lo + hi) / 2
        if d.length > SNAP:
            bmesh.ops.transform(lods[k][0][n], matrix=Matrix.Translation(d), verts=lods[k][0][n].verts)
            snapped.append({"lod": k, "part": n, "moved": round(d.length * SCALE, 3)})
            print("LOD%d: snapped %s onto LOD0 (%.2f m)" % (k, n, d.length * SCALE))
# A track's face toward the hull was never meant to be seen, and Tripo painted it dark: thrown off, it showed a black
# slab (critic r3/r4). Each of those faces takes the texture of the outer side opposite it, mirrored across the track.
def mirror_inner(bm, side):
    bm.faces.ensure_lookup_table()
    lo, hi = bounds(bm); cx = (lo.x + hi.x) / 2
    inward = -1.0 if side == "L" else 1.0          # Track_L is at +X: its inner faces point -X
    uvl = bm.loops.layers.uv.active
    outer = [f for f in bm.faces if f.normal.x * inward < -0.6]
    inner = [f for f in bm.faces if f.normal.x * inward > 0.6]
    if not outer or not inner: return 0
    ob = bmesh.new(); vmap = {}; made = []
    for f in outer:
        vs = []
        for v in f.verts:
            if v not in vmap: vmap[v] = ob.verts.new(v.co)
            vs.append(vmap[v])
        try: ob.faces.new(vs); made.append(f)
        except ValueError: pass
    ob.faces.ensure_lookup_table()
    tree = BVHTree.FromBMesh(ob)
    src = {i: f for i, f in enumerate(made)}
    n = 0
    for f in inner:
        for l in f.loops:
            p = l.vert.co.copy(); p.x = 2 * cx - p.x
            hit = tree.find_nearest(p)
            if hit[0] is None or hit[2] is None or hit[2] not in src: continue
            of = src[hit[2]]
            # barycentric on the outer face's first triangle fan: nearest corner-weighted UV
            ds = [max(1e-6, (c.vert.co - hit[0]).length) for c in of.loops]
            ws = [1.0 / d for d in ds]; sw = sum(ws)
            u = sum(c[uvl].uv.x * w for c, w in zip(of.loops, ws)) / sw
            v = sum(c[uvl].uv.y * w for c, w in zip(of.loops, ws)) / sw
            l[uvl].uv = (u, v); n += 1
    ob.free()
    return n
def close_big_holes(bm, min_perimeter, uv):
    """Fan-fill every open loop longer than min_perimeter (model units). Tripo modelled only the outside of the tracks'
    skirts: thrown and tipped, the open back showed the ink outline's back faces as a black slab (critic r3-r5; the
    re-texture of the hull-facing faces could not help, there was no face there)."""
    uvl = bm.loops.layers.uv.active; made = 0
    blo, bhi = bounds(bm); mid = (blo + bhi) / 2
    # Blender's own filler first, on every open loop long enough to see into (grouped by shared vertices); the fan below
    # takes what it leaves (a loop through one vertex twice lost a fan triangle and left a sliver open)
    bnd = [e for e in bm.edges if e.is_boundary]
    par = {e: e for e in bnd}
    def find(e):
        while par[e] is not e: par[e] = par[par[e]]; e = par[e]
        return e
    byv = {}
    for e in bnd:
        for v in e.verts: byv.setdefault(v, []).append(e)
    for es in byv.values():
        for e in es[1:]:
            a, b = find(es[0]), find(e)
            if a is not b: par[a] = b
    groups = {}
    for e in bnd: groups.setdefault(find(e), []).append(e)
    before = set(bm.faces)
    for es in groups.values():
        if sum(e.calc_length() for e in es) >= min_perimeter:
            bmesh.ops.holes_fill(bm, edges=es, sides=0)
    filled = [f for f in bm.faces if f not in before]
    if filled: bmesh.ops.triangulate(bm, faces=filled)
    filled = [f for f in bm.faces if f not in before]
    for f in filled:
        f.normal_update()
        if f.normal.dot(f.calc_center_median() - mid) < 0: f.normal_flip()
        for l in f.loops: l[uvl].uv = uv
    made += len(filled)
    bnd = [e for e in bm.edges if e.is_boundary]
    byv = {}
    for e in bnd:
        for v in e.verts: byv.setdefault(v, []).append(e)
    left = set(bnd)
    while left:
        e0 = left.pop(); loop = [e0.verts[0], e0.verts[1]]; used = {e0}
        while True:
            nxt = [e for e in byv.get(loop[-1], []) if e in left and e not in used]
            if not nxt: break
            e = nxt[0]; used.add(e); left.discard(e)
            v = e.other_vert(loop[-1])
            if v is loop[0]: break
            loop.append(v)
        per = sum((loop[i].co - loop[(i + 1) % len(loop)].co).length for i in range(len(loop)))
        if len(loop) < 3 or per < min_perimeter: continue
        c = sum((v.co for v in loop), Vector()) / len(loop)
        vc = bm.verts.new(c); new = []
        for i in range(len(loop)):
            try: new.append(bm.faces.new((loop[i], loop[(i + 1) % len(loop)], vc)))
            except ValueError: pass
        for f in new:
            f.normal_update()
            if f.normal.dot(f.calc_center_median() - mid) < 0: f.normal_flip()
            for l in f.loops: l[uvl].uv = uv
        made += len(new)
    return made

def texture_caps(bm, caps, original):
    """Every cap corner takes the UV of the nearest point of the track's own surface as it was before capping: a cap is
    painted like the rim it closes, not with the soot texel (the caps that did not face the hull - the open underside of
    the tread loop - kept that texel and rendered pitch black, critic r6)."""
    tree = BVHTree.FromBMesh(original)
    original.faces.ensure_lookup_table()
    ouv = original.loops.layers.uv.active; uvl = bm.loops.layers.uv.active
    for f in caps:
        for l in f.loops:
            hit = tree.find_nearest(l.vert.co)
            if hit[0] is None or hit[2] is None: continue
            of = original.faces[hit[2]]
            ds = [max(1e-6, (c.vert.co - hit[0]).length) for c in of.loops]; ws = [1.0 / d for d in ds]; sw = sum(ws)
            l[uvl].uv = (sum(c[ouv].uv.x * w for c, w in zip(of.loops, ws)) / sw, sum(c[ouv].uv.y * w for c, w in zip(of.loops, ws)) / sw)

for k in range(3):
    for side in ("L", "R"):
        bm = lods[k][0]["Track_" + side]
        before_caps = bm.copy(); faces_before = set(bm.faces)
        lo, hi = bounds(bm)
        # repeated: where two open loops touch at a vertex, one pass walks and fills one of them and leaves the other
        made, step = 0, 1
        while step and made < 400:
            step = close_big_holes(bm, 0.12 * (hi.y - lo.y), lods[k][4]); made += step
        texture_caps(bm, [f for f in bm.faces if f not in faces_before], before_caps); before_caps.free()
        print("LOD%d Track_%s: %d faces closing its open back; %d inner corners re-textured from the outer side" % (k, side, made, mirror_inner(bm, side)))
piv, sock = pivots_and_sockets(lods[0][0])
manifest = {"source": "Tools/tank3split.py", "name": NAME, "scale": SCALE, "parts": {}, "sockets": {}, "lods": [], "snapped": snapped}
for n in ALL_PARTS:
    manifest["parts"][n] = {"parent": PARENT.get(n), "pivot": unity(piv[n] * SCALE), **BREAK[n]}
for s, (owner, p) in sock.items():
    manifest["sockets"][s] = {"part": owner, "pos": unity((p - piv[owner]) * SCALE)}
for lod, (P, img, mat, stem, uv, log) in enumerate(lods):
    root, objs = make(lod, P, piv, mat)
    for n, o in objs.items(): o.name = "%d|%s" % (lod, n)
    render("%s_LOD%d_parts" % (NAME, lod), objs, 'OBJECT')
    render("%s_LOD%d_tex" % (NAME, lod), objs, 'TEXTURE')
    render("%s_LOD%d_exploded" % (NAME, lod), objs, 'OBJECT', exploded=2.2)
    for n, o in objs.items(): o.name = n
    d = os.path.join(OUTDIR); os.makedirs(d, exist_ok=True)
    export(root, os.path.join(d, "%s_LOD%d.fbx" % (NAME, lod)))
    for n, o in objs.items(): o.name = "%d|%s" % (lod, n)
    entry = {"lod": lod, "parts": {}}
    for n in ALL_PARTS:
        lo, hi = bounds(P[n])
        me = objs[n].data
        entry["parts"][n] = {"verts": len(me.vertices), "tris": sum(len(p.vertices) - 2 for p in me.polygons),
                             "min": unity(lo * SCALE), "max": unity(hi * SCALE)}
    entry["verts"] = sum(p["verts"] for p in entry["parts"].values()); entry["tris"] = sum(p["tris"] for p in entry["parts"].values())
    manifest["lods"].append(entry)
    print("EXPORT LOD%d: %d verts %d tris; islands by part: %s" % (lod, entry["verts"], entry["tris"],
          {n: sum(1 for x in log if x[0] == n) for n in ALL_PARTS}))
    # textures: base, normal, and a packed metallic/smoothness for URP Lit if we ever want it
    import shutil
    for k, tagm in ((0, "Base"), (3, "Normal"), (1, "Rough"), (2, "Metal")):
        srcf = "%s_tex0_%d.jpg" % (stem, k)
        if os.path.exists(srcf): shutil.copyfile(srcf, os.path.join(d, "%s_LOD%d_%s.jpg" % (NAME, lod, tagm)))
# lists as well as maps: Unity's JsonUtility reads arrays of records, not dictionaries
manifest["partList"] = [dict(name=n, **manifest["parts"][n]) for n in ALL_PARTS]
for p in manifest["partList"]: p["parent"] = p["parent"] or ""
manifest["socketList"] = [dict(name=s, **v) for s, v in manifest["sockets"].items()]
manifest["lodList"] = [{"lod": e["lod"], "verts": e["verts"], "tris": e["tris"],
                        "parts": [dict(name=n, **e["parts"][n]) for n in ALL_PARTS]} for e in manifest["lods"]]
json.dump(manifest, open(os.path.join(OUTDIR, "tank3.json"), "w"), indent=1)
print("DONE")
