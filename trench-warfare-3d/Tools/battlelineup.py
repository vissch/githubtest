# Blender (background): the battle's machines as TankRenderer builds them, off the FBXs under Resources/Vehicles, for a
# look and a measurement without the editor (2026-09-28, the Skimmer and Salvo loop).
#   1. A lineup: the Maw (drawn 1.7x, VehicleSize.Tank), the Pincer (2.5x, VehicleSize.Walker), each machine named on
#      the command line at 1x (Tools/mechsplit.py TW_BATTLE=1 writes them in metres), and a 1.8 m man, on one ground,
#      textured with their atlases, noses to +X, from the front-right and from the right side: lineup_q.png, lineup_side.png.
#   2. Each named machine's LOD pop: LOD0 and LOD1 from four sides at the same ortho framing, 256 px, 15 degrees down;
#      the worst side's silhouette IoU and block colour (mean |RGB| difference of 8 px block means where both cover the
#      block, 0-255), docs/22's two measures: pop.json, pop_<Name>.png (the eight frames).
# Re-importing these FBXs puts nested parts out of place (the exporter's node turn that Editor/TankImport.cs undoes in
# Unity); this script undoes it the same way in Blender's frame: a part turned 90 degrees about X loses the turn and its
# offset (x, y, z) becomes (x, z, y); a part one below such a part has its offset's z negated. Checked against
# mechsplit's manifest pivots (Salvo Turret, Gun, Wheels; Skimmer FanRing, Fan). Deeper parts are sockets: not drawn.
#
# TW_ENGINE=workbench renders with Workbench instead of EEVEE (when memory is short).
# usage (from trench-warfare-3d/): blender -b --factory-startup -P Tools/battlelineup.py -- <outdir> <Name>...
import bpy, sys, os, math, json
import numpy as np
from mathutils import Vector

argv = sys.argv[sys.argv.index("--") + 1:]
OUT, NAMES = argv[0], argv[1:]
os.makedirs(OUT, exist_ok=True)
V = os.path.join("Assets", "_Project", "Resources", "Vehicles")

def fixed_import(name, lod, atlas):
    """Import one battle FBX, undo the exporter's node turn, give it its atlas; returns (root, meshes)."""
    before = set(bpy.data.objects)
    bpy.ops.import_scene.fbx(filepath=os.path.abspath(os.path.join(V, name, "%s_LOD%d.fbx" % (name, lod))))
    new = [o for o in bpy.data.objects if o not in before]
    root = [o for o in new if o.parent is None][0]
    turned = set()
    def fix(o, depth):
        r = o.rotation_euler
        if depth >= 2 and abs(math.degrees(r.x) - 90) < 1 and abs(r.y) < 1e-3 and abs(r.z) < 1e-3:
            x, y, z = o.location; o.location = (x, z, y); o.rotation_euler = (0, 0, 0); turned.add(o)
        elif depth >= 2 and o.parent in turned:
            x, y, z = o.location; o.location = (x, y, -z)
        for c in list(o.children): fix(c, depth + 1)
    fix(root, 0)
    img = bpy.data.images.load(os.path.abspath(atlas), check_existing=True)
    mat = bpy.data.materials.new(name + "_atlas"); mat.use_nodes = True
    t = mat.node_tree.nodes.new("ShaderNodeTexImage"); t.image = img
    b = mat.node_tree.nodes["Principled BSDF"]; b.inputs["Roughness"].default_value = 0.8
    mat.node_tree.links.new(t.outputs["Color"], b.inputs["Base Color"])
    meshes = [o for o in new if o.type == 'MESH']
    for o in meshes: o.data.materials.clear(); o.data.materials.append(mat)
    for o in new:
        if o.type == 'EMPTY' and o.name.split(".")[0].startswith("Socket_"): o.hide_render = True
    return root, meshes

def bounds(meshes):
    bpy.context.view_layer.update()
    pts = [o.matrix_world @ Vector(c) for o in meshes for c in o.bound_box]
    return Vector([min(p[k] for p in pts) for k in range(3)]), Vector([max(p[k] for p in pts) for k in range(3)])

def scene_setup(res_x, res_y, transparent):
    scn = bpy.context.scene
    engines = [e.identifier for e in bpy.types.RenderSettings.bl_rna.properties['engine'].enum_items]
    scn.render.engine = 'BLENDER_EEVEE' if 'BLENDER_EEVEE' in engines else 'BLENDER_EEVEE_NEXT'
    if os.environ.get("TW_ENGINE") == "workbench":   # light on memory: flat studio light, the atlas as colour
        scn.render.engine = 'BLENDER_WORKBENCH'; scn.display.shading.light = 'STUDIO'; scn.display.shading.color_type = 'TEXTURE'
    scn.render.resolution_x, scn.render.resolution_y = res_x, res_y
    scn.render.film_transparent = transparent
    scn.view_settings.view_transform = 'Standard'
    if not scn.world:
        scn.world = bpy.data.worlds.new("w"); scn.world.use_nodes = True
        bg = scn.world.node_tree.nodes["Background"]; bg.inputs["Color"].default_value = (0.6, 0.62, 0.66, 1); bg.inputs["Strength"].default_value = 0.8
    if "sun" not in bpy.data.objects:
        sun = bpy.data.objects.new("sun", bpy.data.lights.new("sun", 'SUN')); scn.collection.objects.link(sun)
        sun.data.energy = 3.0; sun.rotation_euler = (math.radians(45), 0, math.radians(200))
    if not scn.camera:
        cd = bpy.data.cameras.new("cam"); cd.type = 'ORTHO'
        cam = bpy.data.objects.new("cam", cd); scn.collection.objects.link(cam); scn.camera = cam
    return scn

def shoot(scn, target, d, ortho, path):
    cam = scn.camera; d = d.normalized()
    cam.location = target + d * 200; cam.rotation_euler = (-d).to_track_quat('-Z', 'Y').to_euler()
    cam.data.ortho_scale = ortho; cam.data.clip_end = 1000
    scn.render.filepath = path
    bpy.ops.render.render(write_still=True)

# ---------------------------------------------------------------------------------------------------- the lineup
bpy.ops.wm.read_factory_settings(use_empty=True)
scn = scene_setup(1600, 640, False)
line = [("Maw", 1.7, os.path.join(V, "TankAtlas_LOD0.jpg")), ("Pincer", 2.5, os.path.join(V, "PincerAtlas.jpg"))]
line += [(n, 1.0, os.path.join(V, n + "Atlas.jpg")) for n in NAMES]
x = 0.0; report = {"lineup": {}}
for name, scale, atlas in line:
    root, meshes = fixed_import(name, 0, atlas)
    root.scale = (scale, scale, scale)
    root.rotation_euler.z = math.radians(90)   # nose to +X, along the line: the side view shows every profile
    lo, hi = bounds(meshes)
    root.location.x += x - lo.x; root.location.z -= lo.z
    lo, hi = bounds(meshes)
    report["lineup"][name] = {"long_m": round(hi.x - lo.x, 2), "wide_m": round(hi.y - lo.y, 2), "tall_m": round(hi.z - lo.z, 2)}
    x = hi.x + 2.5
bpy.ops.mesh.primitive_cylinder_add(radius=0.25, depth=1.8, location=(x + 0.5, 0, 0.9))   # a man: 1.8 m
x += 1.5
bpy.ops.mesh.primitive_plane_add(size=1, location=(x / 2, 0, 0)); g = bpy.context.object; g.scale = (x + 20, 40, 1)
gm = bpy.data.materials.new("ground"); gm.use_nodes = True
gm.node_tree.nodes["Principled BSDF"].inputs["Base Color"].default_value = (0.32, 0.29, 0.24, 1); g.data.materials.append(gm)
mid = Vector((x / 2, 0, 3))
shoot(scn, mid, Vector((0.6, -1.0, 0.45)), x * 1.02, os.path.join(OUT, "lineup_q.png"))
shoot(scn, mid, Vector((0.0, -1.0, 0.08)), x * 1.02, os.path.join(OUT, "lineup_side.png"))

# ---------------------------------------------------------------------------------------------------- the LOD pop
def frame_rgba(path):
    img = bpy.data.images.load(path); a = np.array(img.pixels[:], dtype=np.float32).reshape(img.size[1], img.size[0], 4)
    bpy.data.images.remove(img); return a

report["pop"] = {}
for name in NAMES:
    bpy.ops.wm.read_factory_settings(use_empty=True)
    scn = scene_setup(256, 256, True)
    atlas = os.path.join(V, name + "Atlas.jpg")
    roots = []
    for lod in (0, 1):
        root, meshes = fixed_import(name, lod, atlas); roots.append((root, meshes))
    lo, hi = bounds(roots[0][1]); mid = (lo + hi) / 2; size = max(hi - lo) * 1.15
    sides = {}
    for side, yaw in (("front", 0), ("left", 90), ("back", 180), ("right", 270)):
        d = Vector((math.sin(math.radians(yaw)), -math.cos(math.radians(yaw)), math.tan(math.radians(15))))
        frames = []
        for lod in (0, 1):
            for k, (r, ms) in enumerate(roots):
                for o in ms: o.hide_render = k != lod
            p = os.path.join(OUT, "pop_%s_%s_LOD%d.png" % (name, side, lod))
            shoot(scn, mid, d, size, p); frames.append(frame_rgba(p))
        a, b = frames
        ma, mb = a[..., 3] > 0.5, b[..., 3] > 0.5
        iou = float((ma & mb).sum()) / max(1, float((ma | mb).sum()))
        diffs = []
        for by in range(0, 256, 8):
            for bx in range(0, 256, 8):
                ca, cb = ma[by:by + 8, bx:bx + 8], mb[by:by + 8, bx:bx + 8]
                if ca.mean() < 0.5 or cb.mean() < 0.5: continue
                ra = a[by:by + 8, bx:bx + 8, :3][ca].mean(0); rb = b[by:by + 8, bx:bx + 8, :3][cb].mean(0)
                diffs.append(float(np.abs(ra - rb).mean()) * 255)
        sides[side] = {"iou": round(iou, 3), "block": round(sum(diffs) / max(1, len(diffs)), 1)}
    worst = min(sides.values(), key=lambda s: s["iou"])
    report["pop"][name] = {"sides": sides, "worst_iou": worst["iou"], "worst_block": max(s["block"] for s in sides.values())}
    print("POP", name, json.dumps(report["pop"][name]))
json.dump(report, open(os.path.join(OUT, "pop.json"), "w"), indent=1)
print("LINEUP", json.dumps(report["lineup"]))
