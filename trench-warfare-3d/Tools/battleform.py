"""The battle's form of a Playground machine, from the Playground's own files: no Tripo sources needed.
mechsplit.py TW_BATTLE=1 writes a battle model while it splits the sources; the Playground machines whose sources are not
on this station (the Brute, Croaker, Mercy, Hopper, 2026-09-28) are converted here from what the split already made:
Playground/Art/Tanks/<Name>/<Name>_LOD0.fbx and a far LOD, and tank3.json (parents, pivots, sockets). The Playground
hangs every part flat under the root and keeps sockets in the json; the battle (TankModel.Load) wants each part under
its parent, the Hull under the root, and sockets as empties under their part. The meshes, the facing and the atlas are
already the battle's (the same triangles and part bounds as the shipped Skimmer), so this only re-parents (keeping every
part where it is), adds the sockets and exports with mechsplit's own settings.

Usage (Blender 5.0): blender -b --factory-startup --python Tools/battleform.py -- <Name> [<out dir> [<check fbx>]]
  <out dir>   default Assets/_Project/Resources/Vehicles/<Name>: writes <Name>_LOD0.fbx, <Name>_LOD1.fbx (the far LOD) and
              ../<Name>Atlas.jpg (LOD0's base image; both LODs share its UVs).
  <check fbx> a battle LOD0 to compare against (the shipped Skimmer's): every part's parent, local position, and every
              socket's position must match within 1 mm; prints MATCH or the differences.
Env: TW_FAR=LOD2 (default) or LOD1, the far LOD's source (the Mercy's LOD2 is painted on its own UVs: use LOD1);
     TW_RENAME=Old:New,... renames parts (the Mercy's Wheel_FL -> Wheel_LF: TankModel reads the side after the first _);
     TW_PARENT=Part:Parent,... overrides a parent (the Brute's Gun under its Turret).
"""
import bpy, sys, os, json, shutil
from mathutils import Vector, Matrix

argv = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
if not argv:
    print(__doc__); sys.exit(2)
NAME = argv[0]
HERE = os.path.dirname(os.path.abspath(__file__))
PROJECT = os.environ.get("TW_PROJECT") or os.path.dirname(HERE)
SRC = os.path.join(PROJECT, "Assets", "_Project", "Playground", "Art", "Tanks", NAME)
OUT = argv[1] if len(argv) > 1 else os.path.join(PROJECT, "Assets", "_Project", "Resources", "Vehicles", NAME)
CHECK = argv[2] if len(argv) > 2 else None
FAR = os.environ.get("TW_FAR", "LOD2")

def pairs(env):
    return dict(p.split(":", 1) for p in os.environ.get(env, "").split(",") if ":" in p)

RENAME = pairs("TW_RENAME")
REPARENT = pairs("TW_PARENT")
man = json.load(open(os.path.join(SRC, "tank3.json"), encoding="utf-8"))
PARENT = {RENAME.get(p["name"], p["name"]): RENAME.get(p["parent"], p["parent"]) for p in man["partList"]}
PARENT.update(REPARENT)
PIVOT = {RENAME.get(p["name"], p["name"]): Vector(p["pivot"]) for p in man["partList"]}
SOCKETS = [(s["name"], RENAME.get(s["part"], s["part"]), Vector(s["pos"])) for s in man.get("socketList", [])]

def unity_to_local(v):
    """A Unity-space offset (x, y up, z ahead) as the imported root's local axes (the Playground FBX: x, z up, y ahead)."""
    return Vector((v.x, v.z, v.y))

def clear():
    bpy.ops.wm.read_factory_settings(use_empty=True)

def load(path):
    bpy.ops.import_scene.fbx(filepath=path)
    roots = [o for o in bpy.context.scene.objects if o.parent is None]
    root = next(o for o in roots if o.type == "EMPTY")
    parts = {}
    for o in list(root.children):
        n = o.name.split(".")[0]
        n = RENAME.get(n, n)
        o.name = n
        parts[n] = o
    # Every part to identity rotation, its turn baked into its mesh, and the root to identity: the structure crabsplit.py
    # and mechsplit.py write. Left as imported (the importer's quarter turn on the root, each part turned under it),
    # Blender's FBX export with bake_space_transform turns each level of nesting a further quarter turn: the Croaker's
    # thighs came out 90 degrees over, its feet 180 (2026-09-28; the Skimmer, one level deep, had matched by luck).
    bpy.context.view_layer.update()
    for o in parts.values():
        w = o.matrix_world.copy(); o.parent = None; o.matrix_world = w
    for o in parts.values():
        w = o.matrix_world
        if o.type == "MESH": o.data.transform(w.to_3x3().to_4x4())
        o.matrix_world = Matrix.Translation(w.translation)
    root.matrix_world = Matrix.Identity(4)
    bpy.context.view_layer.update()
    return root, parts

def rebuild(root, parts, lod, sockets):
    for n, o in parts.items():
        par = PARENT.get(n, "")
        if par and par not in parts:
            raise SystemExit("%s LOD%s: %s's parent %s is missing" % (NAME, lod, n, par))
        target = parts[par] if par else root
        m = o.matrix_world.copy(); o.parent = target; o.matrix_world = m
        bpy.context.view_layer.update()
    root.name = "%s_LOD%s" % (NAME, lod)
    if sockets:
        # a json position is Unity's frame; where it lands in this scene is fitted from the parts themselves: every part's
        # json pivot against where its origin stands (least squares, an affine map), so no axis convention is assumed
        import numpy as np
        names = [n for n in parts if n in PIVOT]
        A = np.array([[*PIVOT[n], 1.0] for n in names]); B = np.array([list(parts[n].matrix_world.translation) for n in names])
        M, res, *_ = np.linalg.lstsq(A, B, rcond=None)
        err = float(np.abs(A @ M - B).max())
        print("pivot fit over %d parts: worst %.2f mm" % (len(names), err * 1000))
        for sname, owner, pos in SOCKETS:
            if owner not in parts:
                print("socket %s: no part %s, skipped" % (sname, owner)); continue
            e = bpy.data.objects.new(sname, None); bpy.context.scene.collection.objects.link(e)
            e.empty_display_size = 0.15
            world = Vector(np.array([*(PIVOT[owner] + pos), 1.0]) @ M)
            e.parent = parts[owner]; e.matrix_world = Matrix.Translation(world)

def compensate(root):
    """Blender 5.0's FBX exporter (io_scene_fbx/fbx_utils.py, fbx_object_matrix) writes a node two or more levels under
    the root, under bake_space_transform, as G Lp^-1 G^-1 Lp Lo G^-1 instead of G Lo G^-1 (G the axis conversion, Lp its
    parent's local matrix, Lo its own): right only when the parent sits at its own parent's origin. The Skimmer's hull
    does; the Croaker's does not, and its claws, thighs, turret and feet landed 2-4 m off in Unity (2026-09-28). Each
    such node is given the local matrix that the exporter's formula turns into the right one: Lo' = Lp'^-1 G Lp Lo.
    Checked in Unity against the Playground's own import, part by part: all four machines, both LODs, 0.0 mm, no turn.
    crabsplit.py's walkers and mechsplit.py TW_BATTLE=1 export through the same path (docs/reference/pipelines.md)."""
    from bpy_extras.io_utils import axis_conversion
    G = axis_conversion(to_forward='-Z', to_up='Y').to_4x4()
    bpy.context.view_layer.update()
    true = {}
    def collect(o):
        for c in o.children:
            true[c] = o.matrix_world.inverted() @ c.matrix_world
            collect(c)
    collect(root)
    new = {}
    def assign(o):
        for c in o.children:
            new[c] = true[c] if o == root else new[o].inverted() @ G @ true[o] @ true[c]
            assign(c)
    assign(root)
    for c, m in new.items():
        c.matrix_parent_inverse = Matrix.Identity(4); c.matrix_basis = m

def export(root, path):
    for o in bpy.context.selected_objects: o.select_set(False)
    def walk(o):
        o.select_set(True)
        for c in o.children: walk(c)
    walk(root)
    bpy.context.view_layer.objects.active = root
    # mechsplit.py's export(), setting for setting: the battle's importer (Editor/TankImport.cs) expects exactly this
    bpy.ops.export_scene.fbx(filepath=path, use_selection=True, object_types={'MESH', 'EMPTY'}, apply_unit_scale=True,
                             apply_scale_options='FBX_SCALE_ALL', bake_space_transform=True, axis_forward='-Z', axis_up='Y',
                             mesh_smooth_type='OFF', use_mesh_modifiers=False, add_leaf_bones=False, path_mode='STRIP',
                             embed_textures=False, use_custom_props=False, use_tspace=False)

def describe(root):
    """Part -> (parent name, world position of its origin), socket likewise: two files compared as the importer sees them."""
    out = {}
    inv = Matrix.Identity(4)
    def walk(o):
        for c in o.children:
            n = c.name.split(".")[0]
            out[n] = (c.parent.name.split(".")[0] if c.parent != root else "", (inv @ c.matrix_world).translation.copy(), c.type)
            walk(c)
    walk(root)
    return out

os.makedirs(OUT, exist_ok=True)
built = {}
for lod, src in (("0", "LOD0"), ("1", FAR)):
    clear()
    root, parts = load(os.path.join(SRC, "%s_%s.fbx" % (NAME, src)))
    rebuild(root, parts, lod, sockets=(lod == "0"))
    missing = [n for n in PARENT if n not in parts]
    if missing: print("LOD%s from %s lacks %s" % (lod, src, missing))
    path = os.path.join(OUT, "%s_LOD%s.fbx" % (NAME, lod))
    compensate(root)
    export(root, path)
    print("wrote %s (%d parts%s)" % (path, len(parts), ", %d sockets" % len(SOCKETS) if lod == "0" else ""))

atlas = os.path.join(os.path.dirname(OUT.rstrip("/\\")), NAME + "Atlas.jpg")
shutil.copyfile(os.path.join(SRC, "%s_LOD0_Base.jpg" % NAME), atlas)
print("wrote " + atlas)

if CHECK:
    clear()
    bpy.ops.import_scene.fbx(filepath=os.path.join(OUT, "%s_LOD0.fbx" % NAME))   # what was written, read back
    built = describe(next(o for o in bpy.context.scene.objects if o.parent is None and o.type == "EMPTY"))
    clear()
    bpy.ops.import_scene.fbx(filepath=CHECK)
    ref_root = next(o for o in bpy.context.scene.objects if o.parent is None and o.type == "EMPTY")
    ref = describe(ref_root)
    bad = []
    for n, (par, at, kind) in ref.items():
        if n not in built: bad.append("%s missing" % n); continue
        bpar, bat, _ = built[n]
        if bpar != par: bad.append("%s parent %s, shipped %s" % (n, bpar or "root", par or "root"))
        if (bat - at).length > 0.001: bad.append("%s at %s, shipped %s (%.1f mm)" % (n, tuple(round(x, 3) for x in bat), tuple(round(x, 3) for x in at), (bat - at).length * 1000))
    for n in built:
        if n not in ref: bad.append("%s extra" % n)
    print("CHECK " + ("MATCH (%d nodes)" % len(ref) if not bad else "DIFFERS:\n  " + "\n  ".join(bad)))
