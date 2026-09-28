# Blender (background): split a Tripo two-legged walker (2026-09-27: Downloads/steampunk+frog+robot = 9,738 tris,
# mecha frog = 3,702, mech+robot = 1,008: one frog mech at three levels of detail) into the rigid parts a walker is
# animated with. The output is tank3split.py's (Playground/Art/Tanks/<Name>/, flat parts on their pivots and a
# tank3.json with each part's parent), so the playground's VehicleRig breaks, burns and cooks it off like the tanks, the
# same at every LOD; the playground's walker (Runtime/WalkerDrive.cs) poses the parts with the game's WalkerGait:
#   Hull (the root, crabsplit's Body; pelvis, torso, face, backpack), Turret (the dome on its head) > Gun (the twin barrels),
#   Claw_L/R (shoulder, upper arm, forearm: swings at the shoulder) > Jaw_L/R (the hand: at the wrist),
#   Thigh_L/R (at the hip) > Shin_L/R (at the knee) > Foot_L/R (at the ankle).
# Pivots are the object origins; sockets are empties (Socket_Muzzle, Socket_Toe_L/R, Socket_Eye, Socket_Fire,
# Socket_Exhaust). Every part turns 180 degrees about Z before export (see crabsplit.py), so front -Y faces Unity +Z.
#
# Tripo made this one of loose pieces (150 at LOD0), left and right mirror images, so nothing is cut: each piece goes
# whole to a part by where its centre sits (model frame: front -Y, left +X, ground z = 0, the widest side 1 unit).
#
# LODs: LOD1 and LOD2 are LOD0 decimated part by part (the owner's call 2026-09-27), on LOD0's atlas, to Tripo's lower
# models' triangle counts (TW_LOD2=tripo splits Tripo's own lowest model by the same rules instead).
#
# TW_KIND=flyer: the same for a flying machine (2026-09-27: Downloads/green+tank+3d+model (1) = 5,903 tris, green+cartoon+
# tank = 3,297, green+tank = 993: a frog gunship on two ducted engines): Hull (body, eyes, belly), Wing_L/R (the stub wing)
# > Engine_L/R (the pod, its gun through its nose), Tail (stabiliser and fins), Skid_L/R, Turret (the block on top and its
# exhaust pipes). The
# manifest says "flyer" and the playground flies it (Runtime/FlyerDrive.cs).
#
# TW_KIND=hover: a hovercraft (2026-09-27: Downloads/military tank 3d model = 7,098 tris, turned 90 degrees against the
# other two, so TW_TURN=90 for it; (2) = 2,976; (1) = 1,607): Hull, Pod_FL/FR/RL/RR (the four hover pods), FanRing > Fan
# (the blades and hub, which spin), Engine, Turret > Gun. The manifest says "hover" (Runtime/FlyerDrive.cs, low).
# TW_TURN: degrees to turn the LOD0 model about Z first, so its front is -Y like the others.
#
# TW_KIND=halftrack: a half-track rocket truck (2026-09-28: Downloads/military+vehicle+3d+model = 7,863 tris, its front
# already -Y; (1) = 2,971 and rocket+launcher+vehicle+3d+model = 1,450, the same design turned 90 degrees, read for their
# triangle counts only): Hull (chassis, cab, the tracks and the two clawed legs braced at the tail), Wheel_L/R (the front
# tyres, which roll: pivot on the axle), Turret (the yoke the rack turns on) > Gun (the rocket box and its sixteen
# tubes, pitched at the yoke's top; Socket_Tube00..15 at the tube mouths). Tripo's texture is not called *basecolor* here: the image wired to
# the material's Base Color is used.
#
# TW_BATTLE=1: write the BATTLE's form instead of the playground's (2026-09-28): the parts nested (each under its parent,
# the Hull under the file's root, as Presentation/Camera/TankModel.cs walks them), the sockets as empties under their
# parts, and two LODs, as TankRenderer draws: <Name>_LOD0 (LOD0) and <Name>_LOD1 (the far LOD, derived to the LOD2
# budget). The base colour goes to <outdir>/../<Name>Atlas.jpg (Resources/Vehicles/<Name>Atlas), the manifest to
# <renderdir>/<Name>_battle.json, since Resources ships whatever sits in it, and a portrait render to
# <renderdir>/<Name>_portrait.png (the HUD's pictures are cut from it: Tools/portraitcut.py).
# TW_PIECE_TRIS: at a derived LOD, a part keeps at least this many triangles per loose piece (default 0: off).
# TW_KEEP="Part:share,...": at a derived LOD, a named part keeps at least that share of its LOD0 triangles.
#
# usage: blender -b --factory-startup -P mechsplit.py -- <name> <lod0.fbx> [<lod1.fbx> [<lod2.fbx>]] <outdir> <renderdir>
#   the battle's copy: TW_BATTLE=1 ... -- <Name> <lod0.fbx> <lod1.fbx> <lod2.fbx> Assets/_Project/Resources/Vehicles/<Name> <renderdir>
import bpy, bmesh, sys, os, math, json, random, glob, shutil
import numpy as np
from mathutils import Vector, Matrix

argv = sys.argv[sys.argv.index("--") + 1:]
NAME, OUTDIR, RENDERDIR = argv[0], argv[-2], argv[-1]
FBX = argv[1:-2]
os.makedirs(OUTDIR, exist_ok=True); os.makedirs(RENDERDIR, exist_ok=True)
KIND = os.environ.get("TW_KIND", "walker")
# metres per model unit: the mech stands 0.883 units, 5.8 m; the gunship is 1 unit long, 8 m
SCALE = float(os.environ.get("TW_SCALE", {"flyer": "8.0", "hover": "7.0", "halftrack": "8.0"}.get(KIND, "6.6")))
BATTLE = os.environ.get("TW_BATTLE", "") == "1"

PARTS = ["Hull", "Turret", "Gun", "Claw_L", "Claw_R", "Jaw_L", "Jaw_R", "Thigh_L", "Thigh_R", "Shin_L", "Shin_R", "Foot_L", "Foot_R"]
# destruction (VehicleRig): tier 1 fittings, 2 limbs, 3 the turret and gun, 9 the hull; mass shares
BREAK = {"Jaw_L": dict(tier=1, mass=0.4), "Jaw_R": dict(tier=1, mass=0.4), "Claw_L": dict(tier=2, mass=1.6), "Claw_R": dict(tier=2, mass=1.6),
         "Foot_L": dict(tier=2, mass=0.8), "Foot_R": dict(tier=2, mass=0.8), "Shin_L": dict(tier=2, mass=1.2), "Shin_R": dict(tier=2, mass=1.2),
         "Thigh_L": dict(tier=2, mass=1.6), "Thigh_R": dict(tier=2, mass=1.6), "Turret": dict(tier=3, mass=2.0), "Gun": dict(tier=3, mass=1.0),
         "Hull": dict(tier=9, mass=12.0)}
PARENT = {"Turret": "Hull", "Gun": "Turret", "Claw_L": "Hull", "Claw_R": "Hull", "Jaw_L": "Claw_L", "Jaw_R": "Claw_R",
          "Thigh_L": "Hull", "Thigh_R": "Hull", "Shin_L": "Thigh_L", "Shin_R": "Thigh_R", "Foot_L": "Shin_L", "Foot_R": "Shin_R"}

def part_of(c, lo, hi):
    """Which part a loose piece belongs to, from its area-weighted centre c and its bounds."""
    s = "L" if c.x > 0 else "R"; ax = abs(c.x)
    if ax < 0.16 and c.z > 0.745: return "Gun" if c.y < -0.15 else "Turret"
    # nothing of a leg stands further out than 0.31: out past 0.33 it is the arm, however low (the fingertips hang to
    # z 0.24 and went to the hull, then hung in the air under the swinging arm; loop 2 r33)
    if ax > 0.33 and (c.z < 0.44 and c.y < -0.08 or c.z <= 0.26): return "Jaw_" + s
    if ax >= 0.20 and c.z > 0.26:
        return ("Jaw_" + s) if (ax > 0.33 and c.z < 0.44 and c.y < -0.08) else ("Claw_" + s)
    if 0.07 <= ax <= 0.31 and c.z < 0.48 and not (ax < 0.14 and c.z > 0.34):
        if c.z < 0.075: return "Foot_" + s
        if c.z < 0.26: return "Shin_" + s
        return "Thigh_" + s
    return "Hull"

if KIND == "flyer":
    PARTS = ["Hull", "Turret", "Wing_L", "Wing_R", "Engine_L", "Engine_R", "Tail", "Skid_L", "Skid_R"]
    BREAK = {"Skid_L": dict(tier=1, mass=0.4), "Skid_R": dict(tier=1, mass=0.4), "Tail": dict(tier=2, mass=1.0),
             "Engine_L": dict(tier=2, mass=1.6), "Engine_R": dict(tier=2, mass=1.6), "Wing_L": dict(tier=2, mass=0.8),
             "Wing_R": dict(tier=2, mass=0.8), "Turret": dict(tier=3, mass=1.2), "Hull": dict(tier=9, mass=8.0)}
    PARENT = {"Turret": "Hull", "Wing_L": "Hull", "Wing_R": "Hull", "Engine_L": "Wing_L", "Engine_R": "Wing_R",
              "Tail": "Hull", "Skid_L": "Hull", "Skid_R": "Hull"}
    def part_of(c, lo, hi):
        s = "L" if c.x > 0 else "R"; ax = abs(c.x)
        if c.z < 0.22 and ax > 0.13: return "Skid_" + s
        if c.y > 0.26 and c.z > 0.40: return "Tail"
        if ax < 0.16 and c.z > 0.34 and -0.40 < c.y < -0.03: return "Turret"
        if ax >= 0.14 and 0.20 <= c.z < 0.36 and c.y > -0.25: return "Wing_" + s
        if ax >= 0.17 and c.z >= 0.36: return "Engine_" + s
        return "Hull"

if KIND == "hover":
    # the pods before the fan: a script's hit on "the first tier-2 part" takes a pod, not the fan the machine is known by
    PARTS = ["Hull", "Turret", "Gun", "Engine", "Pod_FL", "Pod_FR", "Pod_RL", "Pod_RR", "FanRing", "Fan"]
    BREAK = {"Gun": dict(tier=3, mass=0.4), "Turret": dict(tier=3, mass=1.0), "Engine": dict(tier=3, mass=1.4),
             # the fan: tier 1 made it the first thing any script hit took
             "FanRing": dict(tier=2, mass=1.0), "Fan": dict(tier=3, mass=0.5), "Hull": dict(tier=9, mass=8.0),
             **{"Pod_" + k: dict(tier=2, mass=0.7) for k in ("FL", "FR", "RL", "RR")}}
    PARENT = {"Turret": "Hull", "Gun": "Turret", "Engine": "Hull", "FanRing": "Hull", "Fan": "FanRing",
              **{"Pod_" + k: "Hull" for k in ("FL", "FR", "RL", "RR")}}
    def part_of(c, lo, hi):
        ax = abs(c.x)
        if c.y > 0.33 and c.z > 0.28 and ax < 0.3: return "FanRing" if (hi.x - lo.x) > 0.4 else "Fan"
        if 0.14 < c.y <= 0.33 and c.z > 0.3 and ax < 0.2: return "Engine"
        if ax < 0.06 and c.z > 0.58 and c.y < -0.1: return "Gun"
        if ax < 0.13 and c.z > 0.54: return "Turret"
        if ax > 0.22 and c.z < 0.26: return "Pod_" + ("F" if c.y < 0 else "R") + ("L" if c.x > 0 else "R")
        return "Hull"

if KIND == "halftrack":
    PARTS = ["Hull", "Turret", "Gun", "Wheel_L", "Wheel_R"]
    BREAK = {"Wheel_L": dict(tier=1, mass=0.5), "Wheel_R": dict(tier=1, mass=0.5), "Gun": dict(tier=3, mass=0.8),
             "Turret": dict(tier=3, mass=2.0), "Hull": dict(tier=9, mass=10.0)}
    PARENT = {"Turret": "Hull", "Gun": "Turret", "Wheel_L": "Hull", "Wheel_R": "Hull"}
    TYRES = {}   # side -> (lo, hi) of that front tyre, set in split() from the largest low piece forward on that side
    TUBES = []   # (lo, hi) of each rocket tube at LOD0, set in split(): a Socket_Tube## at each mouth
    # 2026-09-28 (the critic's round): the Turret is the YOKE the box turns on, and the Gun is the whole rocket box with
    # its tubes, pitched at the yoke's top, so the rack elevates as one piece (it was the tubes alone, inside the box)
    def part_of(c, lo, hi):
        ax = abs(c.x)
        for s, (tlo, thi) in TYRES.items():   # the tyre and everything inside its box (hub, rim, bolts); not the mudguard
            if all(lo[k] >= tlo[k] - 0.01 and hi[k] <= thi[k] + 0.01 for k in range(3)): return "Wheel_" + s
        if lo.z > 0.38 and c.z < 0.50 and ax < 0.12 and abs(c.y - 0.03) < 0.1: return "Turret"      # the yoke
        if c.z > 0.44 and lo.z > 0.38 and not (ax > 0.12 and c.y < -0.2): return "Gun"            # the box above it; not the stack
        return "Hull"

def base_colour(stem, obj):
    """The base colour image: Tripo's *_basecolor / tripo_rgb file, or else whatever the material wires to Base Color."""
    found = glob.glob(stem + ".fbm/*basecolor*") + glob.glob(stem + ".fbm/tripo_rgb*")
    if found: return found[0]
    for m in obj.data.materials:
        if m and m.node_tree:
            for l in m.node_tree.links:
                if l.to_socket.name == "Base Color" and l.from_node.type == 'TEX_IMAGE':
                    return bpy.path.abspath(l.from_node.image.filepath)
    raise RuntimeError("%s: no base colour texture" % stem)

def load(fbx):
    before = set(bpy.data.objects)
    bpy.ops.import_scene.fbx(filepath=fbx)
    obj = [o for o in bpy.data.objects if o not in before and o.type == 'MESH'][0]
    for o in bpy.context.selected_objects: o.select_set(False)
    obj.select_set(True); bpy.context.view_layer.objects.active = obj
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    base = base_colour(os.path.splitext(fbx)[0], obj)
    img = bpy.data.images.load(base)
    mat = bpy.data.materials.new("atlas"); mat.use_nodes = True
    t = mat.node_tree.nodes.new("ShaderNodeTexImage"); t.image = img
    mat.node_tree.links.new(t.outputs["Color"], mat.node_tree.nodes["Principled BSDF"].inputs["Base Color"])
    obj.data.materials.clear(); obj.data.materials.append(mat)
    return obj, mat, base

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
    return list(g.values())

def bounds(bm):
    co = [v.co for v in bm.verts]
    return (Vector((min(c.x for c in co), min(c.y for c in co), min(c.z for c in co))),
            Vector((max(c.x for c in co), max(c.y for c in co), max(c.z for c in co))))

def take(bm, keep):
    out = bm.copy(); out.faces.ensure_lookup_table()
    bmesh.ops.delete(out, geom=[f for f in out.faces if f.index not in keep], context='FACES')
    bmesh.ops.delete(out, geom=[v for v in out.verts if not v.link_faces], context='VERTS')
    return out

def join(bms):
    out = bmesh.new(); me = bpy.data.meshes.new("tmp")
    for b in bms: b.to_mesh(me); out.from_mesh(me)
    bpy.data.meshes.remove(me)
    return out

def split(fbx):
    obj, mat, base = load(fbx)
    bm = bmesh.new(); bm.from_mesh(obj.data)
    # the model frame: centred on x and y, feet on z = 0, the widest side 1 unit
    if fbx == FBX[0] and float(os.environ.get("TW_TURN", "0")):
        bmesh.ops.rotate(bm, verts=bm.verts, cent=(0, 0, 0), matrix=Matrix.Rotation(math.radians(float(os.environ["TW_TURN"])), 3, 'Z'))
    lo, hi = bounds(bm); size = max(hi - lo)
    bmesh.ops.transform(bm, matrix=Matrix.Scale(1.0 / size, 4) @ Matrix.Translation(-Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))), verts=bm.verts)
    bm.faces.ensure_lookup_table()
    groups = {n: [] for n in PARTS}
    pieces = []
    for fs in islands(bm):
        a = sum(bm.faces[i].calc_area() for i in fs)
        c = sum((bm.faces[i].calc_center_median() * bm.faces[i].calc_area() for i in fs), Vector()) / max(a, 1e-12)
        vs = {v for i in fs for v in bm.faces[i].verts}
        ilo = Vector([min(v.co[k] for v in vs) for k in range(3)]); ihi = Vector([max(v.co[k] for v in vs) for k in range(3)])
        pieces.append((fs, c, ilo, ihi))
    if KIND == "halftrack":
        TYRES.clear()
        for s, sign in (("L", 1), ("R", -1)):
            low = [p for p in pieces if p[1].x * sign > 0.1 and p[1].y < -0.2 and p[1].z < 0.2]
            best = max(low, key=lambda p: len(p[0]))
            TYRES[s] = (best[2], best[3])
    for fs, c, ilo, ihi in pieces:
        groups[part_of(c, ilo, ihi)].append(fs)
    if KIND == "halftrack" and fbx == FBX[0]:
        TUBES.clear()
        TUBES.extend((ilo, ihi) for fs, c, ilo, ihi in pieces
                     if part_of(c, ilo, ihi) == "Gun" and c.z > 0.55 and c.y < -0.1 and abs(c.x) < 0.14 and ihi.y - ilo.y > 0.08)
    out = {}
    for n in PARTS:
        assert groups[n], "%s: part %s is EMPTY" % (os.path.basename(fbx), n)
        out[n] = join([take(bm, set(fs)) for fs in groups[n]])
    print("%s: pieces by part %s" % (os.path.basename(fbx), {n: len(groups[n]) for n in PARTS}))
    return out, mat, base

def tris_of(bm): return sum(len(f.verts) - 2 for f in bm.faces)
def decimated(bm, ratio):
    me = bpy.data.meshes.new("dec"); bm.to_mesh(me)
    o = bpy.data.objects.new("dec", me); bpy.context.scene.collection.objects.link(o)
    for x in bpy.context.selected_objects: x.select_set(False)
    o.select_set(True); bpy.context.view_layer.objects.active = o
    md = o.modifiers.new("dec", 'DECIMATE'); md.decimate_type = 'COLLAPSE'; md.ratio = max(0.02, min(1.0, ratio))
    md.use_collapse_triangulate = True
    bpy.ops.object.modifier_apply(modifier=md.name)
    out = bmesh.new(); out.from_mesh(o.data)
    bpy.data.objects.remove(o); bpy.data.meshes.remove(me)
    return out

def top_centre(bm, share=0.12):
    """The middle of a part's top: where it hangs from the part above (a hip, a knee, an ankle, a wrist)."""
    lo, hi = bounds(bm); cut = hi.z - share * (hi.z - lo.z)
    vs = [v.co for v in bm.verts if v.co.z >= cut]
    return Vector((sum(v.x for v in vs) / len(vs), sum(v.y for v in vs) / len(vs), cut))

def flyer_pivots_and_sockets(P):
    piv = {}
    lo, hi = bounds(P["Hull"]); piv["Hull"] = Vector((0, 0, 0))
    lo, hi = bounds(P["Turret"]); piv["Turret"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    lo, hi = bounds(P["Tail"]); piv["Tail"] = Vector(((lo.x + hi.x) / 2, lo.y, (lo.z + hi.z) / 2))
    for s in "LR":
        lo, hi = bounds(P["Wing_" + s]); piv["Wing_" + s] = Vector((lo.x if s == "L" else hi.x, (lo.y + hi.y) / 2, (lo.z + hi.z) / 2))
        lo, hi = bounds(P["Engine_" + s]); piv["Engine_" + s] = (lo + hi) / 2
        piv["Skid_" + s] = top_centre(P["Skid_" + s])
    sock = {}
    # the guns run through the engine pods' noses
    for s in "LR":
        lo, hi = bounds(P["Engine_" + s]); sock["Socket_Muzzle_" + s] = ("Engine_" + s, Vector(((lo.x + hi.x) / 2, lo.y, (lo.z + hi.z) / 2)))
    sock["Socket_Muzzle"] = sock["Socket_Muzzle_L"]
    lo, hi = bounds(P["Hull"])
    for i, (x, f) in enumerate(((0.0, 0.9), (0.08, 0.6), (-0.08, 0.6))):
        sock["Socket_Fire%d" % i] = ("Hull", Vector((x, (lo.y + hi.y) / 2, lo.z + f * (hi.z - lo.z))))
    sock["Socket_Deck"] = ("Hull", Vector((0, (lo.y + hi.y) / 2, lo.z + 0.8 * (hi.z - lo.z))))
    for s in "LR":
        lo, hi = bounds(P["Engine_" + s]); sock["Socket_Exhaust" + ("0" if s == "L" else "1")] = ("Engine_" + s, Vector(((lo.x + hi.x) / 2, hi.y, (lo.z + hi.z) / 2)))
    return piv, sock

def hover_pivots_and_sockets(P):
    piv = {"Hull": Vector((0, 0, 0))}
    for n in PARTS:
        if n == "Hull": continue
        lo, hi = bounds(P[n]); piv[n] = (lo + hi) / 2
    lo, hi = bounds(P["Turret"]); piv["Turret"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    lo, hi = bounds(P["Gun"]); piv["Gun"] = Vector(((lo.x + hi.x) / 2, hi.y, (lo.z + hi.z) / 2))
    # the fan turns about its own hub: the ring's middle, on the fan's own axis
    lo, hi = bounds(P["FanRing"]); piv["Fan"] = Vector(((lo.x + hi.x) / 2, bounds(P["Fan"])[0].y, (lo.z + hi.z) / 2))
    for k in ("FL", "FR", "RL", "RR"): piv["Pod_" + k] = top_centre(P["Pod_" + k])
    sock = {}
    lo, hi = bounds(P["Gun"]); sock["Socket_Muzzle"] = ("Gun", Vector(((lo.x + hi.x) / 2, lo.y, (lo.z + hi.z) / 2)))
    lo, hi = bounds(P["Hull"])
    for i, (x, f) in enumerate(((0.0, 0.9), (0.1, 0.7), (-0.1, 0.7))):
        sock["Socket_Fire%d" % i] = ("Hull", Vector((x, (lo.y + hi.y) / 2, lo.z + f * (hi.z - lo.z))))
    sock["Socket_Deck"] = ("Hull", Vector((0, (lo.y + hi.y) / 2, hi.z)))
    lo, hi = bounds(P["Engine"]); sock["Socket_Exhaust0"] = ("Engine", Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, hi.z)))
    return piv, sock

def halftrack_pivots_and_sockets(P):
    piv = {"Hull": Vector((0, 0, 0))}
    # the turntable: the middle of the rocket box's lowest band (the yoke it turns on), at its foot
    lo, hi = bounds(P["Turret"]); band = [v.co for v in P["Turret"].verts if v.co.z <= lo.z + 0.04]
    piv["Turret"] = Vector((sum(v.x for v in band) / len(band), sum(v.y for v in band) / len(band), lo.z))
    piv["Gun"] = Vector((piv["Turret"].x, piv["Turret"].y, hi.z))   # the trunnion: the top of the yoke
    for s in "LR":
        lo, hi = bounds(P["Wheel_" + s]); piv["Wheel_" + s] = (lo + hi) / 2                     # the axle
    sock = {}
    # the tube mouths, top row first, left to right as the crew sees them (+x is left); the muzzle is their middle
    mouths = sorted((Vector(((a.x + b.x) / 2, a.y, (a.z + b.z) / 2)) for a, b in TUBES), key=lambda m: (-round(m.z, 2), -m.x))
    # under the HULL, not the Gun: a socket three nodes deep comes out of Unity's import out of place (TankImport puts
    # right only the nodes under the top part, and Socket_Muzzle by name); the renderer carries them onto the rack's pose
    for k, m in enumerate(mouths): sock["Socket_Tube%02d" % k] = ("Hull", m)
    sock["Socket_Muzzle"] = ("Gun", sum(mouths, Vector()) / len(mouths))
    lo, hi = bounds(P["Hull"])
    for i, (x, f) in enumerate(((0.0, 0.8), (0.1, 0.55), (-0.1, 0.55))):
        sock["Socket_Fire%d" % i] = ("Hull", Vector((x, (lo.y + hi.y) / 2, lo.z + f * (hi.z - lo.z))))
    sock["Socket_Deck"] = ("Hull", Vector((0, (lo.y + hi.y) / 2, hi.z)))
    # the exhaust: the top of the stack beside the cab, the highest point of the hull's front half
    stack = max((v.co for v in P["Hull"].verts if v.co.y < -0.1), key=lambda co: co.z)
    sock["Socket_Exhaust0"] = ("Hull", Vector(stack))
    for s, sign in (("L", 1), ("R", -1)):   # dust off the back of each track
        sock["Socket_Dust_" + s] = ("Hull", Vector((sign * 0.8 * hi.x, 0.3, 0.03)))
    return piv, sock

def pivots_and_sockets(P):
    if KIND == "flyer": return flyer_pivots_and_sockets(P)
    if KIND == "hover": return hover_pivots_and_sockets(P)
    if KIND == "halftrack": return halftrack_pivots_and_sockets(P)
    piv = {}
    lo, hi = bounds(P["Hull"]); piv["Hull"] = Vector((0, (lo.y + hi.y) / 2, lo.z))
    lo, hi = bounds(P["Turret"]); piv["Turret"] = Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z))
    lo, hi = bounds(P["Gun"]); piv["Gun"] = Vector(((lo.x + hi.x) / 2, hi.y, (lo.z + hi.z) / 2))
    for s in "LR":
        for n in ("Thigh_", "Shin_", "Foot_", "Jaw_"): piv[n + s] = top_centre(P[n + s])
        # the shoulder: the arm's inner top, where the pad meets the body
        lo, hi = bounds(P["Claw_" + s])
        piv["Claw_" + s] = Vector((lo.x if s == "L" else hi.x, (lo.y + hi.y) / 2, hi.z - 0.25 * (hi.z - lo.z)))
    sock = {}
    lo, hi = bounds(P["Gun"]); sock["Socket_Muzzle"] = ("Gun", Vector(((lo.x + hi.x) / 2, lo.y, (lo.z + hi.z) / 2)))
    for s in "LR":
        lo, hi = bounds(P["Foot_" + s]); sock["Socket_Toe_" + s] = ("Foot_" + s, Vector(((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, lo.z)))
    lo, hi = bounds(P["Hull"])
    for i, (x, f) in enumerate(((0.0, 0.85), (0.08, 0.55), (-0.08, 0.55))):
        sock["Socket_Fire%d" % i] = ("Hull", Vector((x, (lo.y + hi.y) / 2, lo.z + f * (hi.z - lo.z))))
    sock["Socket_Deck"] = ("Hull", Vector((0, (lo.y + hi.y) / 2, lo.z + 0.7 * (hi.z - lo.z))))
    sock["Socket_Exhaust0"] = ("Hull", Vector((0, hi.y, lo.z + 0.8 * (hi.z - lo.z))))
    sock["Socket_Eye"] = ("Hull", Vector((0, lo.y, lo.z + 0.6 * (hi.z - lo.z))))
    sock["Socket_Fire"] = ("Hull", Vector((0, (lo.y + hi.y) / 2, hi.z)))
    sock["Socket_Exhaust"] = ("Hull", Vector((0, hi.y, lo.z + 0.8 * (hi.z - lo.z))))
    return piv, sock

TURN = Matrix.Rotation(math.pi, 4, 'Z')
def unity(v): return [round(-v.x, 5), round(v.z, 5), round(-v.y, 5)]

def make(lod, P, piv, sock, mat):
    objs = {}
    for n in PARTS:
        b = P[n].copy()
        bmesh.ops.transform(b, matrix=TURN @ Matrix.Scale(SCALE, 4) @ Matrix.Translation(-piv[n]), verts=b.verts)
        me = bpy.data.meshes.new("%s_LOD%d_%s" % (NAME, lod, n)); b.to_mesh(me); b.free(); me.materials.append(mat)
        o = bpy.data.objects.new(n, me); bpy.context.scene.collection.objects.link(o); objs[n] = o
    root = bpy.data.objects.new("%s_LOD%d" % (NAME, lod), None); bpy.context.scene.collection.objects.link(root)
    for n, o in objs.items(): o.parent = root; o.location = (TURN @ piv[n]) * SCALE
    return root, objs

def make_battle(lod, P, piv, sock, mat):
    """The battle's form (crabsplit.py's): each part under its parent, the Hull under the root, sockets as empties."""
    objs = {}
    for n in PARTS:
        b = P[n].copy()
        bmesh.ops.transform(b, matrix=TURN @ Matrix.Scale(SCALE, 4) @ Matrix.Translation(-piv[n]), verts=b.verts)
        me = bpy.data.meshes.new("%s_LOD%d_%s" % (NAME, lod, n)); b.to_mesh(me); b.free(); me.materials.append(mat)
        o = bpy.data.objects.new(n, me); bpy.context.scene.collection.objects.link(o); objs[n] = o
    root = bpy.data.objects.new("%s_LOD%d" % (NAME, lod), None); bpy.context.scene.collection.objects.link(root)
    for n in PARTS:   # PARTS lists every parent before its children
        par = PARENT.get(n)
        objs[n].parent = objs[par] if par else root
        objs[n].location = (TURN @ (piv[n] - (piv[par] if par else Vector()))) * SCALE
    for sname, (owner, pos) in sock.items():
        e = bpy.data.objects.new(sname, None); bpy.context.scene.collection.objects.link(e)
        e.empty_display_size = 0.15; e.parent = objs[owner]; e.location = (TURN @ (pos - piv[owner])) * SCALE
        objs[sname] = e
    return root, objs

def export(root, path):
    for o in bpy.context.selected_objects: o.select_set(False)
    def walk(o):
        o.select_set(True)
        for c in o.children: walk(c)
    walk(root)
    bpy.context.view_layer.objects.active = root
    bpy.ops.export_scene.fbx(filepath=path, use_selection=True, object_types={'MESH', 'EMPTY'}, apply_unit_scale=True,
                             apply_scale_options='FBX_SCALE_ALL', bake_space_transform=True, axis_forward='-Z', axis_up='Y',
                             mesh_smooth_type='OFF', use_mesh_modifiers=False, add_leaf_bones=False, path_mode='STRIP',
                             embed_textures=False, use_custom_props=False, use_tspace=False)

def render(tag, objs, colour, exploded=0.0):
    scn = bpy.context.scene
    scn.render.engine = 'BLENDER_WORKBENCH'; scn.display.shading.light = 'STUDIO'; scn.display.shading.show_cavity = True
    scn.render.resolution_x = 520; scn.render.resolution_y = 560
    if not scn.camera:
        cd = bpy.data.cameras.new("cam"); cd.type = 'ORTHO'
        cam = bpy.data.objects.new("cam", cd); scn.collection.objects.link(cam); scn.camera = cam
    cam = scn.camera
    meshes = [o for o in objs.values() if o.type == 'MESH']
    for o in scn.objects:
        if o.type == 'MESH': o.hide_render = o not in meshes
    saved = {o: o.location.copy() for o in meshes}
    if exploded:
        for o in meshes:
            d = o.location - Vector((0, 0, SCALE * 0.4)); o.location = o.location + d * exploded
    bpy.context.view_layer.update()
    rnd = random.Random(5)
    for o in meshes: o.color = (rnd.random() * .8 + .2, rnd.random() * .8 + .2, rnd.random() * .8 + .2, 1)
    scn.display.shading.color_type = colour
    mid = Vector((0, 0, SCALE * 0.45)); d = Vector((-0.8, 1.1, 0.5)).normalized()   # front-left, Blender +Y is the front after the turn
    cam.location = mid + d * 40; cam.rotation_euler = (-d).to_track_quat('-Z', 'Y').to_euler()
    cam.data.ortho_scale = SCALE * 1.25 * (1 + exploded * 0.6); cam.data.clip_end = 100
    scn.render.filepath = os.path.join(RENDERDIR, tag + ".png")
    bpy.ops.render.render(write_still=True)
    for o, l in saved.items(): o.location = l

def portrait(objs, path):
    """The HUD's picture of the machine: LOD0 lit and textured, front-left three-quarters from above, on a transparent
    film (1024 px; cut to the portrait sizes outside Blender). Rendered here, where the parts stand where they belong."""
    scn = bpy.context.scene
    meshes = [o for o in objs.values() if o.type == 'MESH']
    for o in scn.objects:
        if o.type == 'MESH': o.hide_render = o not in meshes
    engines = [e.identifier for e in bpy.types.RenderSettings.bl_rna.properties['engine'].enum_items]
    was = scn.render.engine, scn.render.resolution_x, scn.render.resolution_y, scn.render.film_transparent
    scn.render.engine = 'BLENDER_EEVEE' if 'BLENDER_EEVEE' in engines else 'BLENDER_EEVEE_NEXT'
    scn.render.film_transparent = True; scn.render.resolution_x = scn.render.resolution_y = 1024
    scn.view_settings.view_transform = 'Standard'
    if not scn.world:
        scn.world = bpy.data.worlds.new("w"); scn.world.use_nodes = True
        bg = scn.world.node_tree.nodes["Background"]; bg.inputs["Color"].default_value = (0.55, 0.55, 0.6, 1); bg.inputs["Strength"].default_value = 0.9
    if "sun" not in bpy.data.objects:
        sun = bpy.data.objects.new("sun", bpy.data.lights.new("sun", 'SUN')); scn.collection.objects.link(sun)
        sun.data.energy = 3.5; sun.rotation_euler = (math.radians(50), 0, math.radians(215))
    bpy.context.view_layer.update()
    pts = [o.matrix_world @ Vector(c) for o in meshes for c in o.bound_box]
    lo = Vector([min(q[k] for q in pts) for k in range(3)]); hi = Vector([max(q[k] for q in pts) for k in range(3)])
    mid = (lo + hi) / 2; size = max(hi - lo)
    cam = scn.camera; d = Vector((-0.75, 1.0, 0.6)).normalized()   # Blender +Y is the front after the turn
    cam.location = mid + d * size * 4; cam.rotation_euler = (-d).to_track_quat('-Z', 'Y').to_euler()
    cam.data.ortho_scale = size * 1.25; cam.data.clip_end = size * 20
    scn.render.filepath = path
    bpy.ops.render.render(write_still=True)
    scn.render.engine, scn.render.resolution_x, scn.render.resolution_y, scn.render.film_transparent = was

# ------------------------------------------------------------------------------------------------------------ main
bpy.ops.wm.read_factory_settings(use_empty=True)
P0, mat0, base0 = split(FBX[0])
t0 = sum(tris_of(b) for b in P0.values())
lower = []
for f in FBX[1:]:
    o, _, _ = load(f); lower.append(sum(len(p.vertices) - 2 for p in o.data.polygons)); bpy.data.objects.remove(o)
budget = {1: lower[0] if len(lower) > 0 else int(t0 * 0.38), 2: lower[1] if len(lower) > 1 else int(t0 * 0.12)}
# TW_LOD2_TRIS overrides Tripo's count (the playground holds a far LOD under 1,500: the hovercraft's Tripo LOD2 is 1,607)
if os.environ.get("TW_LOD2_TRIS"): budget[2] = int(os.environ["TW_LOD2_TRIS"])
LOD2_FROM = os.environ.get("TW_LOD2", "derive")
lods = [P0]; mats = [mat0]; bases = [base0]
for k in (1, 2):
    if k == 2 and LOD2_FROM == "tripo" and len(FBX) > 2:
        P, m, b = split(FBX[2]); lods.append(P); mats.append(m); bases.append(b)
        print("LOD2: Tripo's own, %d tris" % sum(tris_of(x) for x in P.values())); continue
    ratio = budget[k] / t0; floor = 64 if k == 1 else 32
    P = {}
    for n in PARTS:
        have = tris_of(P0[n]); want = max(int(have * ratio), min(have, floor))
        # TW_PIECE_TRIS: a part of many small loose pieces keeps this many triangles a piece (the Salvo's sixteen rocket
        # tubes, 22 each, went to spikes at 4); 0 (the default) derives as before
        want = max(want, min(have, len(islands(P0[n])) * int(os.environ.get("TW_PIECE_TRIS", "0"))))
        # TW_KEEP="Part:share,...": a round part keeps at least that share of its LOD0 triangles (the Skimmer's fan ring
        # went octagonal at the far LOD, the Salvo's tyres too)
        for item in filter(None, os.environ.get("TW_KEEP", "").split(",")):
            part, share = item.split(":")
            if part == n: want = max(want, int(have * float(share)))
        P[n] = decimated(P0[n], want / max(1, have))
    lods.append(P); mats.append(mat0); bases.append(base0)
    print("LOD%d: derived from LOD0, %d tris (budget %d)" % (k, sum(tris_of(b) for b in P.values()), budget[k]))
piv, sock = pivots_and_sockets(P0)
# The battle's root part stands on the origin (2026-09-28): with the Hull's pivot at the walker's pelvis (2.24 m up), every
# part under it came out of Unity's import displaced by that height, turned (the Croaker's shins 2.2 m from its thighs).
# The crabs' Body and the hovercraft's Hull have always stood on it. The sockets and the parts keep their places: both
# are written relative to their owner's pivot.
if BATTLE: piv["Hull"] = Vector((0, 0, 0))
manifest = {"source": "Tools/mechsplit.py", "name": NAME, "scale": SCALE, "walker": KIND == "walker", "flyer": KIND in ("flyer", "hover"), "hover": KIND == "hover",
            # a cook-off throws its parts at 0.6 of the tank's speeds: it has no magazine, and at 1.0 an engine landed 29 m off
            "fling": 0.6, "lods": [], "snapped": [],
            "derived": [k for k in (1, 2) if not (k == 2 and LOD2_FROM == "tripo")],
            "partList": [dict(name=n, parent=PARENT.get(n, ""), pivot=unity(piv[n] * SCALE), **BREAK[n]) for n in PARTS],
            "socketList": [{"name": s, "part": o, "pos": unity((p - piv[o]) * SCALE)} for s, (o, p) in sock.items()]}
manifest["parts"] = {p["name"]: {k: v for k, v in p.items() if k != "name"} for p in manifest["partList"]}
manifest["sockets"] = {x["name"]: {"part": x["part"], "pos": x["pos"]} for x in manifest["socketList"]}
if BATTLE:
    # TankRenderer draws two levels: LOD0 near, and past 170 m the far one, which is the playground's LOD2 budget
    manifest["battle"] = True; manifest["lods"] = []
    for lod, P, mat in ((0, lods[0], mats[0]), (1, lods[2], mats[2])):
        root, objs = make_battle(lod, P, piv, sock, mat)
        tag = "%s_LOD%d" % (NAME, lod)
        render(tag + "_tex", objs, 'TEXTURE'); render(tag + "_parts", objs, 'OBJECT')
        if lod == 0: portrait(objs, os.path.join(RENDERDIR, NAME + "_portrait.png"))
        export(root, os.path.join(OUTDIR, tag + ".fbx"))
        # facing: from its breech the barrel runs to Blender +Y, which the export puts at Unity +Z (the axis trap)
        ys = [v.co.y for v in objs["Gun"].data.vertices] if "Gun" in objs else [0.0]   # a flyer has no Gun part: its guns are in its engine pods
        for n, o in objs.items(): o.name = "%d|%s" % (lod, n)
        t = sum(tris_of(b) for b in P.values())
        manifest["lods"].append({"lod": lod, "tris": t, "parts": {n: tris_of(P[n]) for n in PARTS}})
        print("EXPORT BATTLE LOD%d: %d tris; the barrel reaches %.2f m ahead of its breech, %.2f m behind" % (lod, t, max(ys), -min(ys)))
    shutil.copyfile(bases[0], os.path.join(os.path.dirname(os.path.normpath(OUTDIR)), NAME + "Atlas.jpg"))
    manifest["lodList"] = manifest["lods"]
    json.dump(manifest, open(os.path.join(RENDERDIR, NAME + "_battle.json"), "w"), indent=1)
    print("DONE")
    sys.exit(0)
for lod, P in enumerate(lods):
    root, objs = make(lod, P, piv, sock, mats[lod])
    tag = "%s_LOD%d" % (NAME, lod)
    render(tag + "_tex", objs, 'TEXTURE'); render(tag + "_parts", objs, 'OBJECT', exploded=0.35)
    for n in PARTS: objs[n].name = n
    export(root, os.path.join(OUTDIR, tag + ".fbx"))
    for n in PARTS: objs[n].name = "%d|%s" % (lod, n)
    shutil.copyfile(bases[lod], os.path.join(OUTDIR, tag + "_Base.jpg"))
    t = sum(tris_of(b) for b in P.values())
    entry = {"lod": lod, "verts": 0, "tris": t, "parts": []}
    for n in PARTS:
        lo, hi = bounds(P[n]); me = objs[n].data
        entry["parts"].append({"name": n, "verts": len(me.vertices), "tris": sum(len(p.vertices) - 2 for p in me.polygons),
                               "min": unity(lo * SCALE), "max": unity(hi * SCALE)})
        entry["verts"] += len(me.vertices)
    manifest["lods"].append(entry)
    print("EXPORT LOD%d: %d tris" % (lod, t))
manifest["lodList"] = manifest["lods"]
json.dump(manifest, open(os.path.join(OUTDIR, "tank3.json"), "w"), indent=1)
print("DONE")
