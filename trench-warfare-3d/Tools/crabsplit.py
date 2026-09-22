# Blender (background): cut the owner's two Tripo crab sheets (Downloads/robotic crab 3d model.zip and
# crab robot 3d model.zip, 2026-09-22) into the rigid parts a walker needs to be animated, and export one FBX per
# crab per LOD for Assets/_Project/Resources/Vehicles/<Crab> (CrabImport prepares them there) with its atlas.
#
# Unlike the tank sheets, these are not the same design twice: they are two different machines, so there is one per
# side. Pincer (the blue-and-white porcelain one, 2,128 faces): twin turret guns on the carapace, two heavy claws,
# six legs. Kettle (the red one, 564 faces): one big mortar over its back, two small claws, four legs. Both are
# named after what they do, in the way the tanks are (Maw, Tusk).
#
# The hard part is Pincer: Tripo welded its legs and claws to the carapace, so there is no loose part to take. They
# are cut off with one elliptical wall around the body, applied only BELOW the shell (the shell overhangs the wall,
# and cutting through it would take the rim off with the legs). Every limb then falls out as its own loose part, and
# the boundary loop the cut leaves is exactly the shoulder, so its centroid is the pivot to swing the limb about.
# Tripo also built Pincer asymmetrically — three legs one side, two the other — so the fuller side is mirrored onto
# the other and the crab walks straight.
#
# Kettle needs no cutting: its legs are already three loose parts each (thigh, shin, foot), which means its legs can
# bend at the knee where Pincer's swing in one piece.
#
# Pivots are the object origins: a limb at its shoulder, a shin at its knee, a foot at its ankle, a turret at its
# ring, a gun at its trunnion, the claw's jaw at its hinge. Sockets are empties: Socket_Muzzle*, Socket_Exhaust,
# Socket_Eye, Socket_Foot* (where a step throws dust), Socket_Fire*.
#
# The export turns every part 180 degrees about Z first, because Blender's FBX export with bake_space_transform puts
# Blender -Y at Unity -Z whatever axis_forward says (see envsplit.py); the crabs face Blender -Y, so this makes them
# face Unity +Z. Everything below is measured in the Blender frame; the manifest converts to Unity axes.
#
# usage: blender -b --factory-startup -P crabsplit.py -- <pincer> <kettle> <censer> <pavise> <outdir> <renderdir>
#   (each .fbx beside its Tripo .fbm folder; keep paths short, Blender cannot read textures past MAX_PATH)
import bpy, bmesh, sys, os, math, json, glob
import numpy as np
from mathutils import Vector, Matrix, Euler

argv = sys.argv[sys.argv.index("--") + 1:]
SHEETS, OUTDIR, RENDERDIR = argv[0:-2], argv[-2], argv[-1]
os.makedirs(OUTDIR, exist_ok=True); os.makedirs(RENDERDIR, exist_ok=True)

# One entry per machine. "cut" crabs have their limbs welded to the carapace and are taken apart with the elliptical
# wall; the others arrive as loose parts and only need sorting. "arm" is what the machine carries over its back, and
# how it is found: the tall piece above the shell (Kettle's mortar, Censer's drum) or everything above it (Pavise's
# gun, which Tripo left as a heap of small pieces).
CRABS = {
    "Pincer": dict(scale=3.8, cut=dict(wall=(0.30, 0.26), zcut=0.42), decimate=0.34),
    "Kettle": dict(scale=3.2, arm=("Mortar", "tall"), decimate=0.70),
    "Censer": dict(scale=3.3, arm=("Drum", "tall"), decimate=0.55),
    "Pavise": dict(scale=3.6, arm=("Gun", "above"), shield=True, decimate=0.36),
    # the two the owner added after the first four: a command walker carrying a standard, and a blockhouse on legs
    "Banner": dict(scale=3.4, arm=("Gun", "above"), banner=True, decimate=0.60),
    "Redoubt": dict(scale=3.6, arm=("Cupola", "tall"), decimate=0.45),
    # not a walker at all: the gunboat that stands off the beach. It has no limbs, so the limb pass finds nothing
    # and everything but the gun becomes its hull.
    "Cutter": dict(scale=16.0, arm=("Gun", "above"), still=True, decimate=0.45),
}

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
    base = (glob.glob(os.path.join(fbm, "*_basecolor.*")) or glob.glob(os.path.join(fbm, "*")))[0]
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
    bmesh.ops.delete(bm, geom=[v for v in bm.verts if not v.link_faces], context='VERTS')
    return bm

def bm_bounds(bms):
    co = [v.co for bm in bms for v in bm.verts]
    return (Vector((min(c.x for c in co), min(c.y for c in co), min(c.z for c in co))),
            Vector((max(c.x for c in co), max(c.y for c in co), max(c.z for c in co))))

def centre(bm):
    lo, hi = bm_bounds([bm]); return (lo + hi) / 2

def join(bms):
    out = bmesh.new()
    for bm in bms:
        me = bpy.data.meshes.new("tmp"); bm.to_mesh(me)
        out.from_mesh(me); bpy.data.meshes.remove(me)
    return out

# --------------------------------------------------------------------------------------------------- limb cutting
def open_loops(bm):
    """The boundary loops of a part: one per opening. A cut limb has exactly one, at its shoulder."""
    border = [e for e in bm.edges if e.is_boundary]
    loops, seen = [], set()
    for e in border:
        if e in seen: continue
        loop, stack = [], [e]
        while stack:
            cur = stack.pop()
            if cur in seen: continue
            seen.add(cur); loop.append(cur)
            for v in cur.verts:
                for ne in v.link_edges:
                    if ne.is_boundary and ne not in seen: stack.append(ne)
        loops.append(loop)
    return loops

def loop_centre(loop):
    vs = {v for e in loop for v in e.verts}
    return sum((v.co for v in vs), Vector()) / len(vs)

def cut_limbs(bm, wall, zcut):
    """One elliptical wall round the body, below the shell: everything outside it is a limb. Returns the body and a
    list of (bmesh, shoulder) for each limb, largest first."""
    lo, hi = bm_bounds([bm])
    cx, cy = (lo.x + hi.x) / 2, (lo.y + hi.y) / 2
    a, b = wall
    outside = set()
    for f in bm.faces:
        c = f.calc_center_median()
        if c.z < zcut and ((c.x - cx) / a) ** 2 + ((c.y - cy) / b) ** 2 > 1.0: outside.add(f.index)
    body = bm.copy(); body.faces.ensure_lookup_table()
    bmesh.ops.delete(body, geom=[f for f in body.faces if f.index in outside], context='FACES')
    bmesh.ops.delete(body, geom=[v for v in body.verts if not v.link_faces], context='VERTS')
    limbs_all = bm.copy(); limbs_all.faces.ensure_lookup_table()
    bmesh.ops.delete(limbs_all, geom=[f for f in limbs_all.faces if f.index not in outside], context='FACES')
    bmesh.ops.delete(limbs_all, geom=[v for v in limbs_all.verts if not v.link_faces], context='VERTS')
    # split what is outside into its own connected pieces
    me = bpy.data.meshes.new("limbs"); limbs_all.to_mesh(me)
    ob = bpy.data.objects.new("limbs", me); bpy.context.scene.collection.objects.link(ob)
    pieces = [sub_bmesh(ob, fs) for fs in loose_parts(ob)]
    bpy.data.objects.remove(ob, do_unlink=True); bpy.data.meshes.remove(me)
    out = []
    for p in pieces:
        loops = open_loops(p)
        if not loops: out.append((p, centre(p))); continue
        biggest = max(loops, key=len)
        out.append((p, loop_centre(biggest)))
    return body, out, Vector((cx, cy, 0))

def mirror(bm):
    """The same limb on the other side: mirrored in X, with its faces turned back the right way out."""
    m = bm.copy()
    bmesh.ops.transform(m, matrix=Matrix.Scale(-1, 4, (1, 0, 0)), verts=m.verts)
    bmesh.ops.reverse_faces(m, faces=m.faces)
    return m

# --------------------------------------------------------------------------------------------------- the two crabs
def build_pincer(src, cfg):
    """Blue porcelain, twin turrets, six legs: the body is cut apart, the guns and fittings are loose already."""
    groups = loose_parts(src)
    print("  %d loose parts, biggest %d faces" % (len(groups), len(groups[0])))
    parts = {}
    body_bm = sub_bmesh(src, groups[0])
    rest = [sub_bmesh(src, g) for g in groups[1:]]
    body, limbs, mid = cut_limbs(body_bm, cfg["cut"]["wall"], cfg["cut"]["zcut"])
    print("  cut: body %d faces, %d limbs" % (len(body.faces), len(limbs)))

    # a limb is small and sticks out; anything broad that the wall happened to cut (the skirt under the shell rim)
    # belongs to the body and goes straight back on
    def small(bm):
        lo, hi = bm_bounds([bm])
        return (hi.x - lo.x) < .45 and (hi.y - lo.y) < .45
    body = join([body] + [l[0] for l in limbs if not (len(l[0].faces) > 30 and small(l[0]))])
    limbs = [l for l in limbs if len(l[0].faces) > 30 and small(l[0])]
    limbs.sort(key=lambda l: -len(l[0].faces))
    claws, legs = limbs[:2], limbs[2:]
    claws.sort(key=lambda l: -centre(l[0]).x)         # the machine faces -Y, so its left is Blender +X
    parts["Claw_L"], parts["Claw_R"] = claws[0][0], claws[1][0]
    piv = {"Claw_L": claws[0][1], "Claw_R": claws[1][1]}

    # Tripo made one side of the crab with more legs than the other: mirror the fuller side across
    left = sorted([l for l in legs if centre(l[0]).x >= mid.x], key=lambda l: -centre(l[0]).y)
    right = sorted([l for l in legs if centre(l[0]).x < mid.x], key=lambda l: -centre(l[0]).y)
    keep = left if len(left) >= len(right) else right
    flip = -1 if keep is left else 1
    print("  legs: %d left, %d right -> %d a side, mirrored" % (len(left), len(right), len(keep)))
    for i, (bm, shoulder) in enumerate(keep):
        near, far = ("L", "R") if keep is left else ("R", "L")
        parts["Leg_%s%d" % (near, i + 1)] = bm
        piv["Leg_%s%d" % (near, i + 1)] = shoulder
        m = mirror(bm)
        bmesh.ops.transform(m, matrix=Matrix.Translation((2 * mid.x, 0, 0)), verts=m.verts)
        parts["Leg_%s%d" % (far, i + 1)] = m
        piv["Leg_%s%d" % (far, i + 1)] = Vector((2 * mid.x - shoulder.x, shoulder.y, shoulder.z))

    # the loose fittings: two gun barrels high up, the turret housings behind them, the reactor at the back,
    # the jaws of the claws low at the front, and small plates that belong to whatever they sit nearest
    barrels = sorted([b for b in rest if centre(b).z > .55 and (bm_bounds([b])[1].y - bm_bounds([b])[0].y) > .25],
                     key=lambda b: -centre(b).x)
    jaws = sorted([b for b in rest if centre(b).z < .30 and centre(b).y < mid.y], key=lambda b: -centre(b).x)
    reactor = max([b for b in rest if abs(centre(b).x - mid.x) < .08 and centre(b).y > mid.y + .2] or [None],
                  key=lambda b: len(b.faces) if b else 0, default=None)
    used = set(id(b) for b in barrels + jaws + ([reactor] if reactor else []))
    for side, barrel in zip(("L", "R"), barrels):
        parts["Gun_" + side] = barrel
        lo, hi = bm_bounds([barrel])
        piv["Gun_" + side] = Vector((centre(barrel).x, hi.y, centre(barrel).z))       # the trunnion: the barrel's back end
    for side, jaw in zip(("L", "R"), jaws):
        parts["Jaw_" + side] = jaw
        lo, hi = bm_bounds([jaw])
        piv["Jaw_" + side] = Vector((centre(jaw).x, hi.y, hi.z))                       # the hinge: its top back corner
    if reactor is not None:
        parts["Reactor"] = reactor; piv["Reactor"] = centre(reactor)
    # turret housings: what is left up top, by side
    housings = {"L": [], "R": []}
    spare = []
    for b in rest:
        if id(b) in used: continue
        c = centre(b)
        if c.z > .55 and abs(c.x - mid.x) > .08: housings["L" if c.x >= mid.x else "R"].append(b)
        else: spare.append(b)
    for side in ("L", "R"):
        if not housings[side]: continue
        bm = join(housings[side])
        parts["Turret_" + side] = bm
        lo, hi = bm_bounds([bm])
        piv["Turret_" + side] = Vector((centre(bm).x, centre(bm).y, lo.z))             # the ring it turns on
    if spare:
        body = join([body] + spare)                                                     # mouth, vents, small plates
    parts["Body"] = body; piv["Body"] = Vector((mid.x, mid.y, 0))
    return parts, piv

def build_loose(crab, src, cfg):
    """A sheet whose limbs Tripo left as separate pieces: sort them into shell, arm, claws and legs."""
    groups = loose_parts(src)
    print("  %d loose parts, biggest %d faces" % (len(groups), len(groups[0])))
    bms = [sub_bmesh(src, g) for g in groups]
    lo, hi = bm_bounds(bms)
    parts, piv = {}, {}
    # the carapace is the widest flat piece: broad across, and not tall. Scored as footprint over height, because
    # the mortar Kettle carries has the bigger footprint of the two and would otherwise be taken for the shell.
    def flatness(b):
        lo, hi = bm_bounds([b])
        return (hi.x - lo.x) * (hi.y - lo.y) / max(.05, hi.z - lo.z)
    shell = max(bms, key=flatness)
    rest = [b for b in bms if b is not shell]
    slo, shi = bm_bounds([shell])
    # everything is measured from the middle of the SHELL, not of the whole model: what the machine carries hangs off
    # the back (Censer's drum, Pavise's gun), and measuring from the model's middle throws the quadrants off so that
    # both left legs come out on the same side
    mid = Vector(((slo.x + shi.x) / 2, (slo.y + shi.y) / 2, 0))
    armName, armHow = cfg["arm"]

    # limbs: everything out past the shell's rim, grouped into four quadrants and then into thigh, shin and foot
    rx, ry = (shi.x - slo.x) / 2 * .78, (shi.y - slo.y) / 2 * .78
    # a limb is out past the shell's rim AND down at the ground: what the machine carries also overhangs the rim
    # (Censer's drum hangs off the back of its shell), and it is up at shell height
    low = slo.z + (shi.z - slo.z) * .25
    limbs = []
    for b in (() if cfg.get("still") else rest):   # a ship has no limbs: its fittings belong to the hull
        c = centre(b)
        if c.z < low and ((c.x - mid.x) / rx) ** 2 + ((c.y - mid.y) / ry) ** 2 > 1.0: limbs.append(b)
    # A crab is symmetrical: every limb has its mirror image on the other side. What has none is not a limb — it is
    # the shield Pavise carries on one flank, or a stray sliver Tripo left lying about. The biggest odd one out is the
    # shield if the machine has one; the rest go back on the body. (Slabness alone picked a one-face sliver, whose
    # width is zero and whose score is therefore infinite.)
    def paired(b):
        c = centre(b)
        for o in limbs:
            if o is b: continue
            d = centre(o)
            if abs(d.x - mid.x + c.x - mid.x) < .06 and abs(d.y - c.y) < .06: return True
        return False
    odd = [b for b in limbs if not paired(b)]
    limbs = [b for b in limbs if b not in odd]
    if cfg.get("shield") and odd:
        slab = max(odd, key=lambda b: len(b.faces))
        odd = [b for b in odd if b is not slab]
        parts["Shield"] = slab; piv["Shield"] = centre(slab)
    strays = odd

    # the claws are the pair furthest forward, one either side of the centre line
    front = sorted(limbs, key=lambda b: centre(b).y)
    claws = []
    for b in front:
        if len(claws) == 2: break
        if claws and (centre(b).x - mid.x) * (centre(claws[0]).x - mid.x) > 0: continue   # same side as the first: not its pair
        claws.append(b)
    claws.sort(key=lambda b: -centre(b).x)                                                # the machine's left is Blender +X
    for side, claw in zip(("L", "R"), claws):
        parts["Claw_" + side] = claw
        clo, chi = bm_bounds([claw])
        piv["Claw_" + side] = Vector((centre(claw).x, chi.y, chi.z))
    legs = [b for b in limbs if b not in claws]

    # what it carries: the biggest thing left that is neither shell nor limb (Kettle's mortar, Censer's drum), or
    # everything standing above the shell when the arm came in pieces (Pavise's gun and its mounting)
    inner = [b for b in rest if b not in limbs and b not in claws] + strays
    if armHow == "above":
        armBits = [b for b in inner if centre(b).z > shi.z - (shi.z - slo.z) * .25]
        if not armBits: armBits = [max(inner, key=lambda b: len(b.faces))]
    else:
        armBits = [max(inner, key=lambda b: len(b.faces))]
    # a standard is not part of the gun that swings under it: it is the highest thing on the machine and it stays
    # upright on the body (Banner)
    if cfg.get("banner") and len(armBits) > 1:
        flag = max(armBits, key=lambda b: centre(b).z)
        armBits = [b for b in armBits if b is not flag]
        parts["Banner"] = flag; piv["Banner"] = Vector((centre(flag).x, centre(flag).y, bm_bounds([flag])[0].z))
    arm = join(armBits) if len(armBits) > 1 else armBits[0]
    core = [b for b in inner if b not in armBits and b is not parts.get("Banner")]
    parts[armName] = arm
    mlo, mhi = bm_bounds([arm])
    piv[armName] = Vector((centre(arm).x, (mlo.y + mhi.y) / 2, mlo.z + (mhi.z - mlo.z) * .35))   # its trunnion

    # the legs, by the quarter of the shell they stand under, and within a quarter by height: thigh, shin, foot
    for b in legs:
        c = centre(b)
        print("    limb %4d faces at %6.3f %6.3f %6.3f -> %s%s" % (len(b.faces), c.x, c.y, c.z,
              "L" if c.x >= mid.x else "R", "F" if c.y < mid.y else "B"))
    quads = {}
    for b in legs:
        c = centre(b)
        quads.setdefault(("L" if c.x >= mid.x else "R") + ("F" if c.y < mid.y else "B"), []).append(b)
    for q, group in quads.items():
        group.sort(key=lambda b: -centre(b).z)
        names = ["Thigh", "Shin", "Foot"]
        for i, b in enumerate(group[:3]):
            name = "%s_%s" % (names[i], q)
            parts[name] = b
            blo, bhi = bm_bounds([b])
            piv[name] = Vector((centre(b).x, centre(b).y, bhi.z))       # the joint at the top of the piece
        for b in group[3:]: core.append(b)
    parts["Body"] = join([shell] + core); piv["Body"] = Vector((mid.x, mid.y, 0))
    print("  %s %d faces, %d legs in %d quadrants, %d claws" % (armName, len(arm.faces), len(legs), len(quads), len(claws)))
    return parts, piv

# ---------------------------------------------------------------------------------------------------- the skeleton
def hierarchy(name, parts):
    """part -> parent. Everything hangs off the Body, the way a tank's parts hang off its Hull: TankModel walks the
    tree from the root part, so anything parented to the FBX's own empty instead would never be drawn."""
    P = {}
    for n in parts:
        if n == "Body": continue
        if n.startswith("Gun_"): P[n] = "Turret_" + n[-1]
        elif n.startswith("Jaw_"): P[n] = "Claw_" + n[-1]
        elif n.startswith("Shin_"): P[n] = "Thigh_" + n[5:]
        elif n.startswith("Foot_"): P[n] = "Shin_" + n[5:]
        else: P[n] = "Body"
    return {n: p for n, p in P.items() if p in parts and p != n}

def sockets(name, parts, piv):
    """Empties the game hangs things on: muzzles, the exhaust, the eye, and a point under each foot."""
    s = {}
    for n, bm in parts.items():
        lo, hi = bm_bounds([bm])
        if n.startswith("Gun_"): s["Socket_Muzzle_" + n[-1]] = (n, Vector((centre(bm).x, lo.y, centre(bm).z)))
        if n == "Gun": s["Socket_Muzzle"] = (n, Vector((centre(bm).x, lo.y, centre(bm).z)))
        if n == "Drum": s["Socket_Vent"] = (n, Vector((centre(bm).x, hi.y, lo.z + (hi.z - lo.z) * .2)))
        if n == "Mortar": s["Socket_Muzzle"] = (n, Vector((centre(bm).x, lo.y + (hi.y - lo.y) * .04, hi.z - (hi.z - lo.z) * .06)))
        if n == "Reactor": s["Socket_Exhaust"] = (n, Vector((centre(bm).x, hi.y, centre(bm).z)))
        if n.startswith("Foot_") or n.startswith("Leg_"): s["Socket_" + n.replace("Leg_", "Toe_").replace("Foot_", "Toe_")] = (n, Vector((centre(bm).x, centre(bm).y, lo.z)))
    body = parts["Body"]
    blo, bhi = bm_bounds([body])
    s["Socket_Eye"] = ("Body", Vector((centre(body).x, blo.y, blo.z + (bhi.z - blo.z) * .62)))
    s["Socket_Fire"] = ("Body", Vector((centre(body).x, centre(body).y, bhi.z)))
    if "Socket_Exhaust" not in s: s["Socket_Exhaust"] = ("Body", Vector((centre(body).x, bhi.y, centre(body).z * 1.1)))
    return s

# ------------------------------------------------------------------------------------------------- objects, export
TURN = Matrix.Rotation(math.pi, 4, 'Z')   # see the header: the export puts Blender -Y at Unity -Z

def make_objects(crab, lod, parts, piv, sock, s, mat, decimate):
    objs = {}
    for n, bm in parts.items():
        me = bpy.data.meshes.new(n)
        moved = bm.copy()
        bmesh.ops.transform(moved, matrix=Matrix.Translation(-piv[n]), verts=moved.verts)   # origin to the joint
        bmesh.ops.transform(moved, matrix=TURN @ Matrix.Scale(s, 4), verts=moved.verts)
        moved.to_mesh(me); moved.free()
        me.materials.append(mat)
        o = bpy.data.objects.new(n, me); bpy.context.scene.collection.objects.link(o)
        objs[n] = o
        if decimate < 1.0 and len(me.polygons) > 24:
            mod = o.modifiers.new("dec", 'DECIMATE'); mod.ratio = decimate
            bpy.context.view_layer.objects.active = o
            bpy.ops.object.modifier_apply(modifier="dec")
    root = bpy.data.objects.new(crab, None); bpy.context.scene.collection.objects.link(root)
    root.empty_display_size = 0.3
    P = hierarchy(crab, parts)
    for n, o in objs.items():
        par = P.get(n)
        pobj = objs.get(par, root) if par else root
        ppiv = piv.get(par, Vector()) if par in objs else Vector()
        o.parent = pobj
        o.location = (TURN @ (piv[n] - ppiv)) * s
    for sname, (owner, pos) in sock.items():
        e = bpy.data.objects.new(sname, None); bpy.context.scene.collection.objects.link(e)
        e.empty_display_size = 0.15
        e.parent = objs[owner]; e.location = (TURN @ (pos - piv[owner])) * s
        objs[sname] = e
    return root, objs

def unity(v): return [round(-v.x, 4), round(v.z, 4), round(-v.y, 4)]

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

def render_checks(crab, lod, root, objs):
    scene = bpy.context.scene
    scene.render.engine = 'BLENDER_WORKBENCH'
    scene.display.shading.light = 'STUDIO'; scene.display.shading.show_cavity = True
    scene.render.resolution_x = 640; scene.render.resolution_y = 520
    scene.view_settings.view_transform = 'Standard'
    scene.render.image_settings.file_format = 'PNG'   # the atlas save leaves it on JPEG
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
        scene.render.filepath = os.path.join(RENDERDIR, "%s_LOD%d_%s.png" % (crab, lod, name))
        bpy.ops.render.render(write_still=True)
    import random
    rnd = random.Random(5)
    for o in meshes: o.color = (rnd.random() * .8 + .2, rnd.random() * .8 + .2, rnd.random() * .8 + .2, 1)
    shoot("q", (-1, 1.1, .8), 'TEXTURE')
    shoot("parts", (-1, 1.1, .8), 'OBJECT')
    shoot("front", (0, 1, .25), 'TEXTURE')
    # posed: mid-stride, turrets traversed, guns up, claws open
    saved = {n: (o.rotation_euler.copy(), o.location.copy()) for n, o in objs.items()}
    for n, o in objs.items():
        if n.startswith("Leg_"):
            i = int(n[-1]); side = 1 if n[4] == 'L' else -1
            o.rotation_euler.x = math.radians(18 * (1 if (i + (side > 0)) % 2 else -1))
            o.rotation_euler.z = math.radians(9 * side * (1 if i % 2 else -1))
        if n.startswith("Thigh_"): o.rotation_euler.x = math.radians(14 if n.endswith("F") else -12)
        if n.startswith("Shin_"): o.rotation_euler.x = math.radians(-20)
        if n.startswith("Turret_"): o.rotation_euler.z = math.radians(-32 if n.endswith("L") else 21)
        if n.startswith("Gun_"): o.rotation_euler.x = math.radians(-9)
        if n == "Mortar": o.rotation_euler.x = math.radians(-14)
        if n.startswith("Jaw_"): o.rotation_euler.x = math.radians(-26)
        if n.startswith("Claw_"): o.rotation_euler.z = math.radians(16 if n.endswith("L") else -16)
    shoot("posed", (-1, 1.1, .8), 'TEXTURE')
    for n, o in objs.items(): o.rotation_euler, o.location = saved[n]
    bpy.context.view_layer.update()
    # exploded, so every cut and every pivot can be checked
    world = {o: o.matrix_world.copy() for o in meshes}
    def depth(o): return 0 if o.parent is None else 1 + depth(o.parent)
    for o in sorted(meshes, key=depth):
        c = world[o] @ (sum((v.co for v in o.data.vertices), Vector()) / len(o.data.vertices))
        push = (c - mid); push.z = max(push.z, 0) * 1.5
        o.matrix_world = Matrix.Translation(push * .85) @ world[o]
        bpy.context.view_layer.update()
    shoot("exploded", (-1, 1.1, .8), 'OBJECT')
    for n, o in objs.items(): o.rotation_euler, o.location = saved[n]
    bpy.context.view_layer.update()

# ------------------------------------------------------------------------------------------------------------ main
bpy.ops.wm.read_factory_settings(use_empty=True)
manifest = {"source": "Tools/crabsplit.py", "crabs": {}}
for crab, fbx in zip(("Pincer", "Kettle", "Censer", "Pavise", "Banner", "Redoubt", "Cutter"), SHEETS):
    cfg = CRABS[crab]
    print("== %s" % crab)
    src, img, mat = load(fbx, crab)
    parts, piv = build_pincer(src, cfg) if "cut" in cfg else build_loose(crab, src, cfg)
    sock = sockets(crab, parts, piv)
    s = cfg["scale"]
    lo, hi = bm_bounds(list(parts.values()))
    piv = {n: p - Vector((0, 0, lo.z)) for n, p in piv.items()}      # the model's feet become z = 0
    sock = {n: (o, v - Vector((0, 0, lo.z))) for n, (o, v) in sock.items()}
    for bm in parts.values(): bmesh.ops.transform(bm, matrix=Matrix.Translation((0, 0, -lo.z)), verts=bm.verts)
    entry = manifest["crabs"].setdefault(crab, {"scale": s, "lods": []})
    entry["size_m"] = [abs(x) for x in unity((hi - lo) * s)]
    entry["pivots"] = {n: unity(p * s) for n, p in piv.items()}
    entry["sockets"] = {n: {"part": o, "pos": unity(p * s)} for n, (o, p) in sock.items()}
    entry["parents"] = hierarchy(crab, parts)
    entry["legs"] = sorted([n for n in parts if n.startswith("Leg_") or n.startswith("Thigh_")])
    for lod in (0, 1):
        for o in list(bpy.context.scene.objects):
            if o.type in ('MESH', 'EMPTY') and o.name != "cam": bpy.data.objects.remove(o, do_unlink=True)
        root, objs = make_objects(crab, lod, parts, piv, sock, s, mat, 1.0 if lod == 0 else cfg["decimate"])
        render_checks(crab, lod, root, objs)
        export(root, os.path.join(OUTDIR, crab, "%s_LOD%d.fbx" % (crab, lod)))
        tris = sum(len(p.vertices) - 2 for o in objs.values() if o.type == 'MESH' for p in o.data.polygons)
        verts = sum(len(o.data.vertices) for o in objs.values() if o.type == 'MESH')
        entry["lods"].append({"lod": lod, "parts": sorted(n for n in objs if not n.startswith("Socket")), "verts": verts, "tris": tris})
        print("EXPORT %s LOD%d: %d parts, %d verts, %d tris" % (crab, lod, len([o for o in objs.values() if o.type == 'MESH']), verts, tris))
    im = img.copy(); im.scale(1024, 1024)
    scene = bpy.context.scene
    scene.render.image_settings.file_format = 'JPEG'; scene.render.image_settings.quality = 90
    im.save_render(os.path.join(OUTDIR, "%sAtlas.jpg" % crab), scene=scene)
with open(os.path.join(OUTDIR, "crabs.json"), "w") as f: json.dump(manifest, f, indent=1)
print("DONE")
