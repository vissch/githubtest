# Blender (background): leg bones and skin weights for a machine whose legs Tripo welded into its hull (2026-09-28: the
# Bullfrog, mechsplit.py TW_KIND=gatling). The Playground then skins the rig's own copy of each hull LOD mesh on the CPU
# (Runtime/HopDrive.cs), so the toad folds and kicks its legs through a hop although they are one piece with its body.
#
# Input: each hull LOD's vertices and triangles as Unity has them, in the hull part's frame (dumped from the Playground,
# see docs/22 "The Bullfrog's legs"): {"v": [x,y,z, ...], "t": [i,j,k, ...]}. Output: <Name>_legs.json, the bones
# (name, head, tail, parent; in the hull's frame) and per LOD the welded vertex positions with up to four bone weights
# each. Weights come from Blender's bone heat (Armature Deform with Automatic Weights) on the welded mesh: Unity splits
# vertices at UV seams, and heat on the split mesh treats each UV island as its own piece. Bones whose names start with
# "Body" hold the body still and are written as -1 (the body). HopDrive matches each Unity vertex to its welded position.
#
# The joints are laid out in the body's own frame (a to its right, y up, f forward; the sculpt stands turned YAW about
# its centre CEN) for its left legs, mirrored for the right. The Bullfrog's were placed on orthographic views of its
# hull (hip inside the thigh, knee at the rear bulge, ankle and toes along the ground; shoulder, elbow, wrist, fingers).
#   usage: blender -b --factory-startup -P legrig.py -- <Name> <out.json> <hull_lod0.json> [<hull_lod1.json> ...]
import bpy, bmesh, sys, os, json, math
from mathutils import Vector

argv = sys.argv[sys.argv.index("--") + 1:]
NAME, OUT, SRC = argv[0], argv[1], argv[2:]

RIGS = {
    "Bullfrog": dict(
        yaw=15.3, cen=(0.12, 0.19),
        left={"hip": (-0.85, 0.75, -0.55), "knee": (-0.92, 0.62, -1.55), "ankle": (-1.25, 0.25, -0.9), "toe": (-1.55, 0.08, -0.45),
              "shoulder": (-0.8, 1.05, 0.45), "elbow": (-1.05, 0.62, 0.5), "wrist": (-0.95, 0.2, 0.72), "finger": (-0.8, 0.05, 1.15)},
        chains=[("Thigh", "hip", "knee"), ("Shin", "knee", "ankle"), ("Foot", "ankle", "toe"),
                ("Arm", "shoulder", "elbow"), ("Fore", "elbow", "wrist"), ("Hand", "wrist", "finger")],
        parents={"Thigh": None, "Shin": "Thigh", "Foot": "Shin", "Arm": None, "Fore": "Arm", "Hand": "Fore"},
        # the body's bones: they take the torso, head and belly, so the leg bones' heat stops at the legs
        body=[("BodySpine", (0.0, 0.95, -1.1), (0.0, 1.0, 0.9)), ("BodyHead", (0.0, 1.45, 0.6), (0.0, 1.85, 1.3)),
              ("BodyBelly", (0.0, 0.35, -0.9), (0.0, 0.35, 0.9)), ("BodyBack", (0.0, 1.6, -1.2), (0.0, 1.7, 0.2))],
        # mirrored: the flanks over the thighs and the cheeks over the arms (with only the middle held, the thigh bone
        # took the whole rear flank and the arm bone the cheek)
        body_sides=[("BodyFlank", (-0.95, 1.15, -1.15), (-0.95, 1.2, 0.3)), ("BodyCheek", (-0.72, 1.4, 0.2), (-0.78, 1.5, 1.0)),
                    ("BodyRump", (-0.55, 0.55, -1.35), (-0.55, 0.9, -0.6))]),
}
RIG = RIGS[NAME]
MIDLINE = RIG.get("midline", (0.4, 0.7))
DIGITS = RIG.get("digits", 0.65)   # how far from the wrist or ankle the fingers and toes reach
DIGIT_LOW = RIG.get("digit_low", (0.17, 0.15))   # and how high they lie (hand, foot): the thigh folded over the foot is higher
# how far behind the wrist the palm's heel still rides the hand whole (below DIGIT_LOW): shared with the forearm it swung
# back as the elbow folded and rose a third as far as the wrist, so the body could not sink over its hands (critic g13)
PALM_BACK = RIG.get("palm_back", 0.45)
YAW = math.radians(RIG["yaw"]); CX, CZ = RIG["cen"]

def local(p):
    """Body frame (a, y, f) to the hull's frame (Unity x, y, z)."""
    a, y, f = p; c, s = math.cos(YAW), math.sin(YAW)
    return Vector((CX + a * c + f * s, y, CZ - a * s + f * c))

bones = []   # (name, head, tail, parent)
for side, sg in (("L", 1.0), ("R", -1.0)):
    J = {k: local((a * sg, y, f)) for k, (a, y, f) in RIG["left"].items()}
    for bn, h, t in RIG["chains"]:
        par = RIG["parents"][bn]
        bones.append((bn + "_" + side, J[h], J[t], par + "_" + side if par else None))
for bn, h, t in RIG["body"]:
    bones.append((bn, local(h), local(t), None))
for bn, h, t in RIG.get("body_sides", []):
    for side, sg in (("L", 1.0), ("R", -1.0)):
        bones.append((bn + side, local((h[0] * sg, h[1], h[2])), local((t[0] * sg, t[1], t[2])), None))

def weights_for(path):
    d = json.load(open(path))
    V = [tuple(d["v"][i:i + 3]) for i in range(0, len(d["v"]), 3)]
    T = [tuple(d["t"][i:i + 3]) for i in range(0, len(d["t"]), 3)]
    # weld by position (Unity split the vertices at UV seams and hard edges)
    key = {}; welded = []; remap = []
    for v in V:
        k = (round(v[0], 4), round(v[1], 4), round(v[2], 4))
        if k not in key: key[k] = len(welded); welded.append(v)
        remap.append(key[k])
    faces = []
    for a, b, c in T:
        f = (remap[a], remap[b], remap[c])
        if len(set(f)) == 3: faces.append(f)
    bpy.ops.wm.read_factory_settings(use_empty=True)
    me = bpy.data.meshes.new("hull"); me.from_pydata(welded, [], faces); me.update()
    ob = bpy.data.objects.new("hull", me); bpy.context.scene.collection.objects.link(ob)
    ad = bpy.data.armatures.new("legs"); arm = bpy.data.objects.new("legs", ad); bpy.context.scene.collection.objects.link(arm)
    bpy.context.view_layer.objects.active = arm; arm.select_set(True)
    bpy.ops.object.mode_set(mode='EDIT')
    eb = {}
    for n, h, t, p in bones:
        b = ad.edit_bones.new(n); b.head = h; b.tail = t; eb[n] = b
    for n, h, t, p in bones:
        if p: eb[n].parent = eb[p]; eb[n].use_connect = False
    bpy.ops.object.mode_set(mode='OBJECT')
    for o in bpy.context.selected_objects: o.select_set(False)
    ob.select_set(True); arm.select_set(True); bpy.context.view_layer.objects.active = arm
    bpy.ops.object.parent_set(type='ARMATURE_AUTO')
    # the hip and shoulder blend over a wider band: heat left a narrow seam where the thigh sheared off the body and
    # opened onto its dark underside when the leg unfolded (critic g10)
    names = [b[0] for b in bones]; leg = {n: i for i, n in enumerate(n for n in names if not n.startswith("Body"))}
    # fingers and toes ride their hand or foot whole: shared with the forearm, a finger stretched into a sliver at the
    # reach (critic g10)
    # every low vertex of that limb past the wrist or ankle (on the digits' side of a plane through it, square to the
    # hand or foot, and within DIGITS of it): a capsule along the bone missed the splayed and backward digits, and 43 of
    # 109 finger vertices kept 30-80 % forearm weight (critic g11)
    ends = []
    for n, h, t, p in bones:
        if n.startswith("Hand") or n.startswith("Foot"):
            side = n[-1]; chain = ("Arm", "Fore", "Hand") if n.startswith("Hand") else ("Thigh", "Shin", "Foot")
            ends.append((leg[n], Vector(h), Vector(t), {leg[c + "_" + side] for c in chain}, n.startswith("Hand")))
    def rigid(co, ws):
        for k, h, t, chain, is_hand in ends:
            if co.y > (DIGIT_LOW[0] if is_hand else DIGIT_LOW[1]): continue
            d = Vector((t.x - h.x, 0.0, t.z - h.z)).normalized(); r = Vector((co.x - h.x, 0.0, co.z - h.z))
            if r.dot(d) < (-PALM_BACK if is_hand else -0.05) or r.length > DIGITS: continue
            if sum(w for b, w in ws.items() if b in chain) < 0.3: continue
            return k
        return None
    out = []; unweighted = 0
    W = []
    for v in me.vertices:
        ws = {}
        for g in v.groups:
            n = ob.vertex_groups[g.group].name
            if g.weight <= 1e-4: continue
            k = leg.get(n, -1); ws[k] = ws.get(k, 0.0) + g.weight
        W.append(ws)
    # (smoothed by hand: Blender's vertex_group_smooth will not run in the background) the thigh and arm shares averaged
    # with their neighbours four times, half and half; what they give up goes to the body
    nb = [[] for _ in me.vertices]
    for e in me.edges: a, b = e.vertices; nb[a].append(b); nb[b].append(a)
    soft = [leg[n] for n in leg if n.startswith("Thigh") or n.startswith("Arm")]
    for _ in range(4):
        new = []
        for i, ws in enumerate(W):
            ws = dict(ws)
            for k in soft:
                if not nb[i]: continue
                m = sum(W[j].get(k, 0.0) for j in nb[i]) / len(nb[i])
                was = ws.get(k, 0.0); now = 0.5 * was + 0.5 * m
                if now > 1e-4 or was > 0: ws[k] = now; ws[-1] = max(0.0, ws.get(-1, 0.0) + (was - now))
            new.append(ws)
        W = new
    for v, ws in zip(me.vertices, W):
        ws = {k: w for k, w in ws.items() if w > 1e-4}
        if not ws: ws = {-1: 1.0}; unweighted += 1
        r = rigid(Vector(v.co), ws)
        if r is not None: ws = {r: 1.0}
        # nothing near the midline follows a leg (heat gave the thighs a share of the belly's underside): the legs'
        # weights fade out from MIDLINE[1] to MIDLINE[0] of the body's half width, and the body takes the rest
        a = abs((v.co.x - CX) * math.cos(YAW) - (v.co.z - CZ) * math.sin(YAW))
        keep = min(1.0, max(0.0, (a - MIDLINE[0]) / (MIDLINE[1] - MIDLINE[0])))
        keep = keep * keep * (3 - 2 * keep)
        for k in list(ws):
            if k >= 0: moved = ws[k] * (1 - keep); ws[k] -= moved; ws[-1] = ws.get(-1, 0.0) + moved
        top = sorted(ws.items(), key=lambda kv: -kv[1])[:4]; s = sum(w for _, w in top)
        out.append([[k, w / s] for k, w in top])
    legs = sum(1 for w in out if any(k >= 0 and x > 0.5 for k, x in w))
    print("LEGRIG %s: %d verts (%d welded), %d faces, %d mostly on a leg, %d with no weight (body)" % (os.path.basename(path), len(V), len(welded), len(faces), legs, unweighted))
    # four bone indices and weights a vertex, flat (-1: the body), as Unity's JsonUtility reads them
    wi, ww = [], []
    for w in out:
        w = (w + [[-1, 0.0]] * 4)[:4]
        wi += [k for k, _ in w]; ww += [round(x, 4) for _, x in w]
    return {"verts": [round(c, 4) for v in welded for c in v], "wi": wi, "ww": ww}

result = {"source": "Tools/legrig.py", "name": NAME, "yaw": RIG["yaw"], "cen": list(RIG["cen"]),
          "bones": [{"name": n, "head": [round(c, 4) for c in h], "tail": [round(c, 4) for c in t],
                     "parent": p or ""} for n, h, t, p in bones if not n.startswith("Body")],
          "lods": [weights_for(p) for p in SRC]}
json.dump(result, open(OUT, "w"))
print("LEGRIG DONE %s: %d bones, %d LODs" % (OUT, len(result["bones"]), len(result["lods"])))
