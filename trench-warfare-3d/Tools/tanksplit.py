# Blender (background): split the owner's two Tripo tank sheets (Downloads/tank+3d+model.zip = far model,
# Downloads/cartoon+tank+3d+model.zip = full model, 2026-09-22) into the two tanks each sheet holds, and each tank into
# the rigid parts TankRenderer animates. Exports one FBX per tank per LOD for Assets/_Project/Resources/Vehicles/<Tank>
# (TankImport prepares them there) and the two shared atlases beside them.
#
# Both sheets hold the same two designs stacked one above the other: Maw (lower: sponson guns, fanged hull front, no
# turret, the Mark IV of the roster) and Tusk (upper, tilted on the sheet: a turret with a long gun, the Renault FT).
# Each tank is squared to the axes by turning it until its area-weighted face normals line up with X, Y and Z best
# (the sheet leaves Tusk yawed 33 and rolled 20 degrees), stood on z = 0 and centred, front to Blender -Y. The export
# turns every part 180 degrees about Z first (Blender's FBX export with bake_space_transform puts Blender -Y at Unity -Z
# whatever axis_forward says; docs in envsplit.py), so the tanks face Unity +Z. Everything below is measured in the
# squared frame (front -Y); the manifest converts to Unity axes.
#
# Parts: Tripo leaves most details as loose parts. Named parts come from a table per sheet (index = rank by face count,
# checked against the expected counts so a changed sheet fails loudly); every loose part the table does not name goes
# to the named part nearest to it (rivets, bolts, straps). Tusk's full model has its turret and one track fused into
# the hull: the turret is cut off at the hull deck with a plane and both openings are capped; the track is split off
# by side. Caps take the atlas' darkest texel (the inside of a hull reads as soot when a turret is blown off).
#
# Pivots are the object origins: turret at its ring, guns at the trunnion (guns are levelled; the sheets model them
# elevated), sponsons at the housing, hatch at its hinge, horns at their root, wheels at the hub, tracks at their
# middle. Sockets are empties: Socket_Muzzle*, Socket_HullMG, Socket_Exhaust*, Socket_Fire*, Socket_Crew, Socket_Dust*.
# Left and right are the driver's: the tank faces -Y, so its left is Blender +X (Unity keeps it on the left).
#
# Masks for TW/Tank: UV1 = tread coordinate (u counts track links round the loop, seamless; v across the track),
# vertex colour R = moving tread surface, G = furnace region (TW/Tank lights only the atlas' fire colours inside it:
# Maw's mouth), B = exhaust outlet (glows when the engine runs hot or burns). TankImport moves the colour
# masks to UV2 and writes the painted form into the vertex colours and the smoothed outline normal into UV3.
# The far model is fitted to the full model's hull and uses the full model's pivots, so the two LODs line up.
#
# usage: blender -b --factory-startup -P tanksplit.py -- <full.fbx> <far.fbx> <outdir> <renderdir>
#   (each .fbx beside its Tripo .fbm folder; keep paths short, Blender cannot read textures past MAX_PATH)
import bpy, bmesh, sys, os, math, json, glob
import numpy as np
from mathutils import Vector, Matrix, Euler
from mathutils.bvhtree import BVHTree

argv = sys.argv[sys.argv.index("--") + 1:]
FULL_FBX, FAR_FBX, OUTDIR, RENDERDIR = argv[0], argv[1], argv[2], argv[3]
os.makedirs(OUTDIR, exist_ok=True); os.makedirs(RENDERDIR, exist_ok=True)

# metres per sheet unit, per tank: Maw's tracks 5.1 m long (it crosses the 3 m fire trenches), Tusk's 3.7 m (it cannot)
SCALE = {"Maw": 13.0, "Tusk": 11.0}
SPLIT_Z = 0.52   # the empty band between the two tanks on both sheets

# per sheet and tank: named parts -> loose part ranks, and the face counts those ranks must have
PARTS = {
    ("full", "A"): dict(tank="Maw", expect={0: 720, 1: 251, 2: 206, 3: 161, 4: 159, 5: 152, 6: 126, 7: 124, 8: 123}, groups={
        "Hull": [0, 14, 15, 47, 48], "Track_R": [4], "Track_L": [3],
        "Wheel_RF": [9], "Wheel_LF": [10], "Wheel_RR": [12], "Wheel_LR": [13],
        "Sponson_R": [1], "Sponson_L": [2, 11], "Cupola": [5], "Exhaust": [6, 37], "Horn_R": [8], "Horn_L": [7]}),
    ("full", "B"): dict(tank="Tusk", expect={0: 509, 1: 248, 2: 129, 3: 107, 4: 107, 7: 88}, groups={
        "Hull": [0], "HullBits": [12, 15, 16, 17, 18], "Track_L": [7], "Gun": [1], "Hatch": [2],
        "Turret": [8, 9], "Horn_R": [6], "Horn_L": [5], "Exhaust": [3, 4, 10]},
        cut=dict(src="Hull", turret_halfwidth=0.165, track_x=0.107)),
    ("far", "A"): dict(tank="Maw", expect={0: 105, 1: 42, 2: 40, 3: 35, 4: 34, 5: 26}, groups={
        "Hull": [0], "Horn_L": [1], "Horn_R": [2], "Sponson_R": [3], "Sponson_L": [4], "Cupola": [5], "Exhaust": [6, 7]}),
    ("far", "B"): dict(tank="Tusk", expect={0: 63, 1: 48, 2: 45, 3: 42, 9: 14}, groups={
        "Hull": [0], "Gun": [1], "Horn_L": [2], "Horn_R": [3], "Turret": [9, 4, 5, 11], "Hatch": [7], "Exhaust": [6, 10]}),
}
# hierarchy: part -> parent (root when missing)
PARENT = {"Track_L": "Hull", "Track_R": "Hull", "Turret": "Hull", "Gun": "Turret", "Hatch": "Turret",
          "Sponson_L": "Hull", "Sponson_R": "Hull", "Cupola": "Hull", "Exhaust": "Hull",
          "Wheel_LF": "Track_L", "Wheel_LR": "Track_L", "Wheel_RF": "Track_R", "Wheel_RR": "Track_R"}
def horn_parent(tank): return "Turret" if tank == "Tusk" else "Hull"

# ------------------------------------------------------------------------------------------------------------ load
def load(fbx, tag):
    before = set(bpy.data.objects)
    bpy.ops.import_scene.fbx(filepath=fbx)
    new = [o for o in bpy.data.objects if o not in before and o.type == 'MESH']
    src = new[0]
    for o in bpy.context.selected_objects: o.select_set(False)
    src.select_set(True); bpy.context.view_layer.objects.active = src
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    fbm = os.path.splitext(fbx)[0] + ".fbm"
    base = glob.glob(os.path.join(fbm, "tripo_rgb_*"))[0]
    img = bpy.data.images.load(base); img.name = tag + "_atlas"
    mat = bpy.data.materials.new(tag + "_mat"); mat.use_nodes = True
    nt = mat.node_tree
    tex = nt.nodes.new("ShaderNodeTexImage"); tex.image = img
    nt.links.new(tex.outputs["Color"], nt.nodes["Principled BSDF"].inputs["Base Color"])
    src.data.materials.clear(); src.data.materials.append(mat)
    return src, img, mat

def loose_parts(obj):
    """Loose parts as lists of face indices (edge-connected), ranked by face count."""
    me = obj.data
    parent = list(range(len(me.vertices)))
    def find(a):
        while parent[a] != a:
            parent[a] = parent[parent[a]]; a = parent[a]
        return a
    for e in me.edges:
        a, b = find(e.vertices[0]), find(e.vertices[1])
        if a != b: parent[a] = b
    groups = {}
    for f in me.polygons: groups.setdefault(find(f.vertices[0]), []).append(f.index)
    return sorted(groups.values(), key=lambda fs: -len(fs))

def sub_bmesh(obj, faces):
    bm = bmesh.new(); bm.from_mesh(obj.data); bm.faces.ensure_lookup_table()
    keep = set(faces)
    bmesh.ops.delete(bm, geom=[f for f in bm.faces if f.index not in keep], context='FACES')
    return bm

# -------------------------------------------------------------------------------------------------------- squaring
def squaring_matrix(bm):
    N = np.array([tuple(f.normal) for f in bm.faces]); A = np.array([f.calc_area() for f in bm.faces])
    def rot(yaw, pitch, roll):
        return np.array(Euler((math.radians(roll), math.radians(pitch), math.radians(yaw)), 'XYZ').to_matrix())
    def score(R): return float((A * ((N @ R.T) ** 4).sum(1)).sum())
    best = max(((score(rot(y, p, r)), y, p, r) for y in range(-45, 46, 3) for p in range(-45, 46, 3) for r in range(-45, 46, 3)))
    for step in (1.0, 0.25):
        s0, y0, p0, r0 = best
        cands = [(score(rot(y, p, r)), y, p, r)
                 for y in np.arange(y0 - 3 * step, y0 + 3 * step + 1e-6, step)
                 for p in np.arange(p0 - 3 * step, p0 + 3 * step + 1e-6, step)
                 for r in np.arange(r0 - 3 * step, r0 + 3 * step + 1e-6, step)]
        best = max(cands + [best])
    _, y, p, r = best
    print("  squared: yaw %.2f pitch %.2f roll %.2f" % (y, p, r))
    return Matrix(rot(y, p, r).tolist()).to_4x4()

def bm_bounds(bms):
    co = [v.co for bm in bms for v in bm.verts]
    return (Vector((min(c.x for c in co), min(c.y for c in co), min(c.z for c in co))),
            Vector((max(c.x for c in co), max(c.y for c in co), max(c.z for c in co))))

# ------------------------------------------------------------------------------------------------------ part build
_PX = {}
def pixels(img):
    if img.name not in _PX:
        w, h = img.size
        a = np.empty(w * h * 4, dtype=np.float32); img.pixels.foreach_get(a)
        _PX[img.name] = a.reshape(h, w, 4)
    return _PX[img.name]

def dark_uv(img):
    """UV of the darkest 64x64 block of the atlas: caps read as soot."""
    w, h = img.size
    px = pixels(img)[:, :, :3]
    k = 64
    blocks = px[: h // k * k, : w // k * k].reshape(h // k, k, w // k, k, 3).mean(axis=(1, 3))
    lum = blocks @ np.array([0.3, 0.59, 0.11])
    by, bx = np.unravel_index(np.argmin(lum), lum.shape)
    return ((bx + 0.5) * k / w, (by + 0.5) * k / h)

def cap_plane(bm, z, uv, up):
    """Close the openings a plane cut left at height z (only edges lying on the plane: other open edges stay open).
    The cap faces up on the part below the cut and down on the part above it."""
    edges = [e for e in bm.edges if e.is_boundary and all(abs(v.co.z - z) < 1e-4 for v in e.verts)]
    if not edges: return 0
    res = bmesh.ops.triangle_fill(bm, use_beauty=True, use_dissolve=False, edges=edges, normal=(0, 0, 1))
    faces = [g for g in res["geom"] if isinstance(g, bmesh.types.BMFace)]
    flip = [f for f in faces if (f.normal.z > 0) != up]
    if flip: bmesh.ops.reverse_faces(bm, faces=flip)
    uvl = bm.loops.layers.uv.active
    for f in faces:
        for l in f.loops: l[uvl].uv = uv
    return len(faces)

def cut_tusk(bm_hull, cfg, uv):
    """Tusk full model: hull, turret and one track in one loose part. Plane cut just above the hull deck; the part
    above it inside the turret's width is the turret. The track is everything beyond track_x."""
    deck = sorted(((f.calc_center_median().z, f.calc_area()) for f in bm_hull.faces
                   if f.normal.z > 0.9 and abs(f.calc_center_median().x) < 0.1 and 0.1 < f.calc_center_median().z < 0.2))
    half, acc, z_deck = sum(a for _, a in deck) / 2, 0.0, deck[-1][0]
    for z, a in deck:
        acc += a
        if acc >= half: z_deck = z; break
    z_cut = z_deck + 0.006
    geom = bm_hull.verts[:] + bm_hull.edges[:] + bm_hull.faces[:]
    bmesh.ops.bisect_plane(bm_hull, geom=geom, plane_co=(0, 0, z_cut), plane_no=(0, 0, 1))
    bm_hull.faces.ensure_lookup_table()
    turret, right, left = set(), set(), set()
    for f in bm_hull.faces:
        c = f.calc_center_median()
        if c.z > z_cut and abs(c.x) < cfg["turret_halfwidth"]: turret.add(f.index)
        elif abs(c.x) > cfg["track_x"] and c.z < z_cut: (right if c.x < 0 else left).add(f.index)
    def take(indices, invert=False):
        bm = bm_hull.copy(); bm.faces.ensure_lookup_table()
        bmesh.ops.delete(bm, geom=[f for f in bm.faces if (f.index in indices) == invert], context='FACES')
        bmesh.ops.delete(bm, geom=[v for v in bm.verts if not v.link_faces], context='VERTS')
        return bm
    bt, br, bl, bh = take(turret), take(right), take(left), take(turret | right | left, invert=True)
    caps = (cap_plane(bt, z_cut, uv, up=False), cap_plane(bh, z_cut, uv, up=True))   # the tracks' seams are hidden by the skirts
    print("  Tusk cut at z %.3f (deck %.3f): turret %d faces, tracks %d + %d, hull %d, caps %s" % (z_cut, z_deck, len(bt.faces), len(br.faces), len(bl.faces), len(bh.faces), caps))
    return bh, bt, br, bl, z_cut

def centre(bm): return sum((v.co for v in bm.verts), Vector()) / len(bm.verts)

def fix_sides(named):
    """Parts that tie on face count can swap ranks: the driver's left is +X, so a left part must sit at x > 0."""
    for n in list(named):
        head, _, tail = n.partition("_")
        if not tail or tail[0] != "L": continue
        other = head + "_R" + tail[1:]
        if other in named and centre(named[n]).x < centre(named[other]).x:
            named[n], named[other] = named[other], named[n]
            print("  swapped %s / %s (tied ranks)" % (n, other))

def recentre(named):
    """Origin on the ground under the middle of the footprint (the tracks, or the hull where they are fused)."""
    foot = [bm for n, bm in named.items() if n.startswith("Track_")] or [named["Hull"]]
    lo, hi = bm_bounds(foot)
    zlo = bm_bounds(list(named.values()))[0].z
    T = Matrix.Translation((-(lo.x + hi.x) / 2, -(lo.y + hi.y) / 2, -zlo))
    for bm in named.values(): bmesh.ops.transform(bm, matrix=T, verts=bm.verts)

def nearest_named(named, bm_small):
    """Name of the named part whose surface is nearest to a small part's centre."""
    c = sum((v.co for v in bm_small.verts), Vector()) / len(bm_small.verts)
    best, name = 1e9, None
    for n, tree in named.items():
        hit = tree.find_nearest(c)
        if hit[0] is not None and hit[3] < best: best, name = hit[3], n
    return name

def join(bms):
    out = bmesh.new()
    me = bpy.data.meshes.new("tmp")
    for bm in bms:
        bm.to_mesh(me); out.from_mesh(me)
    bpy.data.meshes.remove(me)
    return out

def pca(bm):
    V = np.array([tuple(v.co) for v in bm.verts]); c = V.mean(0)
    w, U = np.linalg.eigh(np.cov((V - c).T))
    ax = U[:, -1]; t = (V - c) @ ax
    return V, Vector(c), Vector(ax), t

def gun_frame(bm, level_yaw):
    """Trunnion, muzzle direction and length of a gun part. The muzzle end is the thinner one. The gun is levelled
    (and, for a turret gun, turned straight ahead) about its trunnion; returns the rotation applied."""
    V, c, ax, t = pca(bm)
    lo, hi = t.min(), t.max(); span = hi - lo
    def radius_at(frac):
        m = (t >= lo + span * frac) & (t <= lo + span * (frac + 0.2))
        d = V[m] - (np.array(c) + np.outer(t[m], np.array(ax)))
        return float(np.linalg.norm(d, axis=1).mean())
    if radius_at(0.8) > radius_at(0.0): ax = -ax; t = -t; lo, hi = t.min(), t.max()
    breech = c + ax * lo; muzzle = c + ax * hi
    trunnion = breech + ax * (span * (0.35 if not level_yaw else 0.02))
    want = Vector((0, -1, 0)) if level_yaw else Vector((ax.x, ax.y, 0)).normalized()
    R = ax.rotation_difference(want).to_matrix().to_4x4()
    M = Matrix.Translation(trunnion) @ R @ Matrix.Translation(-trunnion)
    bmesh.ops.transform(bm, matrix=M, verts=bm.verts)
    muzzle = M @ muzzle
    return trunnion, muzzle, want

def tread_coords(bm, s):
    """UV1 for a track: u counts links round the loop (seamless), v across. Loop runs forward on top."""
    lo, hi = bm_bounds([bm])
    y0, y1, z0, z1 = lo.y, hi.y, lo.z, hi.z
    r = (z1 - z0) / 2; zc = z0 + r; yF, yB = y0 + r, y1 - r
    L = max(0.0, yB - yF); P = 2 * L + 2 * math.pi * r
    links = max(8, round(P * s / 0.34))   # a link every ~34 cm
    uv1 = bm.loops.layers.uv.get("Tread") or bm.loops.layers.uv.new("Tread")
    col = bm.loops.layers.float_color.get("Mask") or bm.loops.layers.float_color.new("Mask")
    for f in bm.faces:
        tread = abs(f.normal.x) < 0.55
        for l in f.loops:
            y, z = l.vert.co.y, l.vert.co.z
            if yF <= y <= yB: d = (yB - y) if z >= zc else L + math.pi * r + (y - yF)
            elif y < yF: d = L + r * math.atan2(yF - y, z - zc)
            else: d = 2 * L + math.pi * r + r * math.atan2(y - yB, zc - z)
            l[uv1].uv = (d / P * links, l.vert.co.x * s)
            c = l[col]; l[col] = (1.0 if tread else 0.0, c[1], c[2], 1.0)
    return links, P * s / links

def ensure_layers(bm):
    if not bm.loops.layers.uv.get("Tread"): bm.loops.layers.uv.new("Tread")
    if not bm.loops.layers.float_color.get("Mask"):
        col = bm.loops.layers.float_color.new("Mask")
        for f in bm.faces:
            for l in f.loops: l[col] = (0.0, 0.0, 0.0, 1.0)

def mark_region(bm, where, channel, value):
    """Write a mask channel (0 R, 1 G, 2 B) on every corner of the faces whose centre passes where()."""
    col = bm.loops.layers.float_color["Mask"]; n = 0
    for f in bm.faces:
        if not where(f.calc_center_median()): continue
        for l in f.loops:
            c = list(l[col]); c[channel] = max(c[channel], value); c[3] = 1.0; l[col] = c; n += 1
    return n

# ------------------------------------------------------------------------------------------------------ one sheet
def build_sheet(fbx, sheet, full_frames=None):
    src, img, mat = load(fbx, sheet)
    uv_dark = dark_uv(img)
    parts = loose_parts(src)
    zc = [sum((src.data.vertices[i].co.z for f in fs for i in src.data.polygons[f].vertices), 0.0) /
          sum(len(src.data.polygons[f].vertices) for f in fs) for fs in parts]
    tanks = {}
    for grp in ("A", "B"):
        cfg = PARTS[(sheet, grp)]; tank = cfg["tank"]
        mine = [fs for fs, z in zip(parts, zc) if (z < SPLIT_Z) == (grp == "A")]
        for k, n in cfg["expect"].items():
            assert len(mine[k]) == n, "%s %s: part %d has %d faces, expected %d (sheet changed?)" % (sheet, tank, k, len(mine[k]), n)
        print("%s %s (%s): %d loose parts" % (sheet, tank, grp, len(mine)))
        bms = [sub_bmesh(src, fs) for fs in mine]
        whole = join(bms)
        M = squaring_matrix(whole); whole.free()
        for bm in bms: bmesh.ops.transform(bm, matrix=M, verts=bm.verts)
        lo, hi = bm_bounds(bms)
        T = Matrix.Translation((-(lo.x + hi.x) / 2, -(lo.y + hi.y) / 2, -lo.z))
        for bm in bms: bmesh.ops.transform(bm, matrix=T, verts=bm.verts)
        # named parts
        named = {}
        taken = set()
        for name, idx in cfg["groups"].items():
            named[name] = join([bms[i] for i in idx]); taken.update(idx)
        info = {}
        if "cut" in cfg:
            c = cfg["cut"]
            hull, turret, right, left, z_cut = cut_tusk(named[c["src"]], c, uv_dark)
            named[c["src"]].free(); named["Hull"] = hull
            named["Turret"] = join([turret, named["Turret"]]); named["Track_R"] = right
            named["Track_L"] = join([left, named["Track_L"]])
            info["turret_ring_z"] = z_cut
        trees = {n: BVHTree.FromBMesh(bm) for n, bm in named.items() if n != "HullBits"}
        extra = {n: [] for n in named}
        for i, bm in enumerate(bms):
            if i in taken: continue
            extra[nearest_named(trees, bm)].append(bm)
        for n, lst in extra.items():
            if lst: named[n] = join([named[n]] + lst)
        if "HullBits" in named:
            named["Hull"] = join([named["Hull"], named.pop("HullBits")])
        fix_sides(named)
        recentre(named)
        for bm in named.values(): ensure_layers(bm)
        tanks[tank] = dict(parts=named, info=info, img=img, mat=mat)
    return tanks

# --------------------------------------------------------------------------------------------------------- frames
def frames_for(tank, named, info):
    """Pivots and sockets (sheet units, squared frame) from the full model's geometry; levels the guns."""
    piv, sock, rest = {}, {}, {}
    def box(n): return bm_bounds([named[n]])
    piv["Hull"] = Vector((0, 0, 0))
    for n in named:
        if n.startswith("Track_") or n.startswith("Wheel_"):
            lo, hi = box(n); piv[n] = (lo + hi) / 2
    if tank == "Tusk":
        lo, hi = box("Turret")
        piv["Turret"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, info.get("turret_ring_z", lo.z)))
        tr, mz, d = gun_frame(named["Gun"], level_yaw=True)
        piv["Gun"] = tr; sock["Socket_Muzzle"] = ("Gun", mz + d * 0.004)
        lo, hi = box("Hatch"); piv["Hatch"] = Vector(((lo.x + hi.x) / 2, hi.y, lo.z))
        sock["Socket_Crew"] = ("Turret", Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, hi.z)))
    else:
        for side in ("L", "R"):
            n = "Sponson_" + side
            tr, mz, d = gun_frame(named[n], level_yaw=False)
            piv[n] = tr; sock["Socket_Muzzle_" + side] = (n, mz + d * 0.004)
            rest[n] = math.degrees(math.atan2(-d.x, -d.y))   # yaw from straight ahead, + to the tank's right (Unity)
        lo, hi = box("Cupola"); piv["Cupola"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
        sock["Socket_Crew"] = ("Cupola", Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, hi.z)))
        # the furnace mouth: front of the hull, centre line, mid height
        hull = named["Hull"]
        front = [v.co for v in hull.verts if abs(v.co.x) < 0.03 and 0.10 < v.co.z < 0.26]
        ymin = min(c.y for c in front)
        sock["Socket_HullMG"] = ("Hull", Vector((0, ymin - 0.004, sum(c.z for c in front) / len(front))))
    if tank == "Tusk":
        hull = named["Hull"]
        front = [v.co for v in hull.verts if abs(v.co.x) < 0.04 and 0.10 < v.co.z < 0.16]
        ymin = min(c.y for c in front)
        sock["Socket_HullMG"] = ("Hull", Vector((0, ymin - 0.004, sum(c.z for c in front) / len(front))))
    for n in named:
        if n.startswith("Horn_"):
            V = [v.co for v in named[n].verts]
            c = sum(V, Vector()) / len(V)
            V.sort(key=lambda p: p.z)
            base = V[: max(3, len(V) // 5)]
            piv[n] = sum(base, Vector()) / len(base)
    lo, hi = box("Exhaust"); piv["Exhaust"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    # exhaust outlets: the top of each loose lump of the exhaust part
    ex = named["Exhaust"]; ex.verts.ensure_lookup_table()
    root_of = list(range(len(ex.verts)))
    def find(a):
        while root_of[a] != a: root_of[a] = root_of[root_of[a]]; a = root_of[a]
        return a
    for e in ex.edges:
        a, b = find(e.verts[0].index), find(e.verts[1].index)
        if a != b: root_of[a] = b
    lumps = {}
    for v in ex.verts: lumps.setdefault(find(v.index), []).append(v.co)
    outs = []
    for pts in lumps.values():
        if len(pts) < 20 or max(p.z for p in pts) - min(p.z for p in pts) < 0.03: continue
        outs.append(Vector((sum(p.x for p in pts) / len(pts), sum(p.y for p in pts) / len(pts), max(p.z for p in pts))))
    outs.sort(key=lambda p: -p.z)
    for i, p in enumerate(outs[:2]): sock["Socket_Exhaust%d" % i] = ("Exhaust", p + Vector((0, 0, 0.004)))
    # fire on the engine deck (rear third of the hull top), dust behind each track
    hull = named["Hull"]; lo, hi = box("Hull")
    tree = BVHTree.FromBMesh(hull)
    for i, x in enumerate((-0.05, 0.05)):
        o = Vector((x, lo.y + (hi.y - lo.y) * 0.72, hi.z + 1))
        hit = tree.ray_cast(o, Vector((0, 0, -1)))
        z = hit[0].z if hit[0] is not None else hi.z
        sock["Socket_Fire%d" % i] = ("Hull", Vector((x, o.y, z)))
    for side in ("L", "R"):
        n = "Track_" + side
        if n in named:
            tlo, thi = box(n)
            sock["Socket_Dust_" + side] = ("Hull", Vector(((tlo.x + thi.x) / 2, thi.y, tlo.z + 0.02)))
    return piv, sock, rest

# ---------------------------------------------------------------------------------------------------------- build
def make_objects(tank, lod, named, piv, sock, s, mat, glow_mouth, img):
    """Scale to metres, one object per part with its origin on the pivot, parented; sockets as empties."""
    root = bpy.data.objects.new("%s_LOD%d" % (tank, lod), None)
    bpy.context.scene.collection.objects.link(root)
    objs = {}
    S = Matrix.Scale(s, 4)
    TURN = Matrix.Rotation(math.pi, 4, 'Z')   # front to Blender +Y, which the FBX export puts at Unity +Z
    tread = {}
    for n, bm in named.items():
        if n.startswith("Track_") and lod == 0:
            tread[n] = tread_coords(bm, s)
        if n == "Hull" and glow_mouth:
            front_y = min(v.co.y for v in bm.verts)
            k = mark_region(bm, lambda c: c.y < front_y + 0.07 and abs(c.x) < 0.13 and 0.06 < c.z < 0.30, 1, 1.0)
            print("  %s LOD%d furnace region corners: %d" % (tank, lod, k))
        if n == "Exhaust":
            top = max(v.co.z for v in bm.verts)
            mark_region(bm, lambda c: c.z > top - 0.015, 2, 1.0)
        p = piv[n]
        bmesh.ops.transform(bm, matrix=S @ TURN @ Matrix.Translation(-p), verts=bm.verts)
        me = bpy.data.meshes.new("%s_LOD%d_%s" % (tank, lod, n))
        bm.to_mesh(me)
        me.materials.append(mat)
        o = bpy.data.objects.new(n, me)   # renamed below: Blender needs unique names across both LODs
        bpy.context.scene.collection.objects.link(o)
        objs[n] = o
    def parent_of(n):
        if n.startswith("Horn_"): return horn_parent(tank)
        return PARENT.get(n)
    for n, o in objs.items():
        par = parent_of(n)
        pobj = objs.get(par, root) if par else root
        ppiv = piv.get(par, Vector()) if par in objs else Vector()
        o.parent = pobj
        o.location = (TURN @ (piv[n] - ppiv)) * s
    for sname, (owner, pos) in sock.items():
        e = bpy.data.objects.new(sname, None); bpy.context.scene.collection.objects.link(e)
        e.empty_display_size = 0.2
        e.parent = objs[owner]; e.location = (TURN @ (pos - piv[owner])) * s
        objs[sname] = e
    return root, objs, tread

def fit_far(far, full):
    """Uniform scale + offset that lays the far model's hull over the full model's hull and tracks."""
    flo, fhi = bm_bounds([full[n] for n in full if n == "Hull" or n.startswith("Track_") or n.startswith("Wheel_")])
    glo, ghi = bm_bounds([far["Hull"]])
    k = ((fhi.y - flo.y) / (ghi.y - glo.y) + (fhi.z - flo.z) / (ghi.z - glo.z)) / 2
    M = Matrix.Translation(((flo.x + fhi.x) / 2, (flo.y + fhi.y) / 2, 0)) @ Matrix.Scale(k, 4) @ Matrix.Translation((-(glo.x + ghi.x) / 2, -(glo.y + ghi.y) / 2, -glo.z))
    for bm in far.values(): bmesh.ops.transform(bm, matrix=M, verts=bm.verts)
    return k

def unity(v): return [round(-v.x, 4), round(v.z, 4), round(-v.y, 4)]

# --------------------------------------------------------------------------------------------------------- export
def export(root, path):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    for o in bpy.context.selected_objects: o.select_set(False)
    def walk(o):
        o.select_set(True)
        for c in o.children: walk(c)
    walk(root)
    bpy.context.view_layer.objects.active = root
    bpy.ops.export_scene.fbx(filepath=path, use_selection=True, object_types={'MESH', 'EMPTY'}, apply_unit_scale=True,
                             apply_scale_options='FBX_SCALE_ALL', bake_space_transform=True, axis_forward='-Z', axis_up='Y',
                             mesh_smooth_type='OFF', use_mesh_modifiers=False, add_leaf_bones=False, path_mode='STRIP',
                             embed_textures=False, use_custom_props=False, use_tspace=False, colors_type='LINEAR')

# --------------------------------------------------------------------------------------------------------- checks
def render_checks(tank, lod, root, objs):
    scene = bpy.context.scene
    scene.render.engine = 'BLENDER_WORKBENCH'
    scene.display.shading.light = 'STUDIO'; scene.display.shading.show_cavity = True
    scene.render.resolution_x = 640; scene.render.resolution_y = 520
    if not scene.camera:
        cd = bpy.data.cameras.new("cam"); cd.type = 'ORTHO'
        cam = bpy.data.objects.new("cam", cd); scene.collection.objects.link(cam); scene.camera = cam
    cam = scene.camera
    for o in scene.objects:
        if o.type == 'MESH':
            top = o
            while top.parent: top = top.parent
            o.hide_render = top is not root
    meshes = [o for o in objs.values() if o.type == 'MESH']
    pts = [o.matrix_world @ v.co for o in meshes for v in o.data.vertices]
    lo = Vector((min(p.x for p in pts), min(p.y for p in pts), min(p.z for p in pts)))
    hi = Vector((max(p.x for p in pts), max(p.y for p in pts), max(p.z for p in pts)))
    mid = (lo + hi) / 2; span = (hi - lo).length
    def shoot(name, d, colour):
        scene.display.shading.color_type = colour
        d = Vector(d).normalized()
        cam.location = mid + d * span * 3
        cam.rotation_euler = (-d).to_track_quat('-Z', 'Y').to_euler()
        cam.data.ortho_scale = span * 1.15; cam.data.clip_end = span * 10
        scene.render.filepath = os.path.join(RENDERDIR, "%s_LOD%d_%s.png" % (tank, lod, name))
        bpy.ops.render.render(write_still=True)
    import random
    rnd = random.Random(3)
    for o in meshes: o.color = (rnd.random() * 0.8 + 0.2, rnd.random() * 0.8 + 0.2, rnd.random() * 0.8 + 0.2, 1)
    # the exported frame faces +Y: the 3/4 view from the front right is from (-1, +1.1)
    shoot("q", (-1, 1.1, 0.8), 'TEXTURE')
    shoot("parts", (-1, 1.1, 0.8), 'OBJECT')
    shoot("back", (1, -1.1, 0.8), 'TEXTURE')
    # posed: turret/sponsons traversed, gun up, hatch open, wheels turned
    saved = {n: (o.rotation_euler.copy(), o.location.copy()) for n, o in objs.items()}
    for n, o in objs.items():
        if n == "Turret": o.rotation_euler.z = math.radians(-40)
        if n == "Gun": o.rotation_euler.x = math.radians(-12)
        if n == "Hatch": o.rotation_euler.x = math.radians(-110)
        if n.startswith("Sponson_"): o.rotation_euler.z = math.radians(25)
        if n == "Cupola": o.rotation_euler.z = math.radians(60)
    shoot("posed", (-1, 1.1, 0.8), 'TEXTURE')
    # exploded: every mesh part pushed out from the middle
    for n, o in objs.items():
        o.rotation_euler, o.location = saved[n]
    bpy.context.view_layer.update()
    world = {o: o.matrix_world.copy() for o in meshes}
    def depth(o): return 0 if o.parent is None else 1 + depth(o.parent)
    for o in sorted(meshes, key=depth):
        c = world[o] @ (sum((v.co for v in o.data.vertices), Vector()) / len(o.data.vertices))
        push = (c - mid); push.z = max(push.z, 0) * 1.6
        o.matrix_world = Matrix.Translation(push * 0.9) @ world[o]
        bpy.context.view_layer.update()
    shoot("exploded", (-1, 1.1, 0.8), 'OBJECT')
    for n, o in objs.items():
        o.rotation_euler, o.location = saved[n]
    bpy.context.view_layer.update()

# ------------------------------------------------------------------------------------------------------------ main
bpy.ops.wm.read_factory_settings(use_empty=True)
full = build_sheet(FULL_FBX, "full")
far = build_sheet(FAR_FBX, "far")
manifest = {"source": "Tools/tanksplit.py", "tanks": {}}
roots = []
for tank in ("Maw", "Tusk"):
    s = SCALE[tank]
    F, G = full[tank], far[tank]
    piv, sock, rest = frames_for(tank, F["parts"], F["info"])
    k = fit_far(G["parts"], F["parts"])
    # far model: its own guns levelled and trunnions moved onto the full model's
    gpiv = dict(piv)
    for n in ("Gun", "Sponson_L", "Sponson_R"):
        if n in G["parts"]:
            tr, mz, d = gun_frame(G["parts"][n], level_yaw=(n == "Gun"))
            bmesh.ops.transform(G["parts"][n], matrix=Matrix.Translation(piv[n] - tr), verts=G["parts"][n].verts)
    gsock = {n: v for n, v in sock.items() if v[0] in G["parts"]}
    for lod, (parts, psock, img, mat) in enumerate(((F["parts"], sock, F["img"], F["mat"]), (G["parts"], gsock, G["img"], G["mat"]))):
        # dimensions before the parts move to their pivots
        lo, hi = bm_bounds(list(parts.values()))
        tl = [bm_bounds([parts[n]]) for n in parts if n.startswith("Track_")]
        root, objs, tread = make_objects(tank, lod, parts, piv, psock, s, mat, tank == "Maw", img)
        for n, o in objs.items(): o.name = "%s|%d|%s" % (tank, lod, n)
        render_checks(tank, lod, root, {n: objs[n] for n in objs})
        for n, o in objs.items(): o.name = n + ".tmp"   # free the plain names for this export
        for n, o in objs.items(): o.name = n
        export(root, os.path.join(OUTDIR, tank, "%s_LOD%d.fbx" % (tank, lod)))
        for n, o in objs.items(): o.name = "%s|%d|%s" % (tank, lod, n)
        tris = sum(len(p.vertices) - 2 for o in objs.values() if o.type == 'MESH' for p in o.data.polygons)
        verts = sum(len(o.data.vertices) for o in objs.values() if o.type == 'MESH')
        entry = manifest["tanks"].setdefault(tank, {"scale": s, "lods": []})
        if lod == 0:
            entry["size_m"] = unity((hi - lo) * s); entry["size_m"] = [abs(x) for x in entry["size_m"]]
            entry["track_length_m"] = round(max(t[1].y - t[0].y for t in tl) * s, 3)
            entry["track_outer_halfwidth_m"] = round(max(max(abs(t[0].x), abs(t[1].x)) for t in tl) * s, 3)
            entry["pivots"] = {n: unity(p * s) for n, p in piv.items()}
            entry["sockets"] = {n: {"part": o, "pos": unity(p * s)} for n, (o, p) in sock.items()}
            entry["sponson_rest_yaw_deg"] = {n: round(y, 1) for n, y in rest.items()}
            entry["tread"] = {n: {"links": t[0], "link_m": round(t[1], 4)} for n, t in tread.items()}
        entry["lods"].append({"lod": lod, "parts": sorted(n for n in objs if not n.startswith("Socket")), "verts": verts, "tris": tris})
        print("EXPORT %s LOD%d: %d parts, %d verts, %d tris" % (tank, lod, len([o for o in objs.values() if o.type == 'MESH']), verts, tris))
    print("  far model fitted at x%.3f" % k)
# atlases: both tanks share one per LOD (the sheets are atlased together)
for img, name, size in ((full["Maw"]["img"], "TankAtlas_LOD0.jpg", 2048), (far["Maw"]["img"], "TankAtlas_LOD1.jpg", 512)):
    im = img.copy(); im.scale(size, size)
    path = os.path.join(OUTDIR, name)
    scene = bpy.context.scene
    scene.render.image_settings.file_format = 'JPEG'; scene.render.image_settings.quality = 90
    im.save_render(path, scene=scene)
with open(os.path.join(OUTDIR, "tanks.json"), "w") as f: json.dump(manifest, f, indent=1)
print("DONE")
