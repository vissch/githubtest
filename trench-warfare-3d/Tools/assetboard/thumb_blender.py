# Blender (background): one three-quarter preview per job, for the asset board. Run by thumbs.py, not by hand:
#   blender -b --factory-startup -P Tools/assetboard/thumb_blender.py -- jobs.json results.json
# A job: {id, out, files: [{fbx, at: [x, y, z] in Unity axes or null}], texture, fix, hide: [substrings], expect: [x, y, z] or null,
#         view: [x, y, z] the direction the camera sits in, or null for the front-left three-quarter}
# - fix: the battle machines' FBXs come back with every node below the top part turned 90 degrees about X; this undoes
#   it exactly as Tools/battlelineup.py does (rotation cleared, offset (x, y, z) -> (x, z, -y)), which is what
#   Editor/TankImport.cs does in Unity.
# - at: a building is many chunk FBXs, each with its pivot at the middle of its base; houses.json gives where each
#   sits in the house, in Unity's axes. housesplit.py gives every chunk a half turn before it exports it (Unity
#   (x, y, z) = Blender (-x, z, -y) there), so a chunk read back here stands half-turned about its own pivot, and the
#   house only fits together with the offsets half-turned too: Unity (x, y, z) goes to Blender (x, z, y).
# - expect: the size the manifest says the whole should have (Unity x, y, z). A preview whose box is off by more than
#   a tenth is reported as "warn": the picture is kept and the page says it may be wrongly assembled.
# Workbench with the texture as colour: the same on every machine, light on a GPU the editors share. It is a
# preview of the model, not the game's look.
import bpy, json, math, os, sys, traceback
from mathutils import Vector

SIZE = 640
VIEW = Vector((-0.75, 1.0, 0.6)).normalized()     # front-left three-quarter from above (mechsplit.py portrait())


def reset():
    bpy.ops.wm.read_factory_settings(use_empty=True)
    scn = bpy.context.scene
    scn.render.engine = 'BLENDER_WORKBENCH'
    sh = scn.display.shading
    sh.light, sh.color_type = 'STUDIO', 'TEXTURE'
    sh.show_cavity, sh.show_object_outline = True, True
    scn.render.resolution_x = scn.render.resolution_y = SIZE
    scn.render.film_transparent = True
    scn.render.image_settings.file_format = 'PNG'
    scn.render.image_settings.color_mode = 'RGBA'
    scn.view_settings.view_transform = 'Standard'
    cam = bpy.data.objects.new("cam", bpy.data.cameras.new("cam"))
    cam.data.type = 'ORTHO'
    scn.collection.objects.link(cam)
    scn.camera = cam
    return scn, cam


def bring(path, fix):
    before = set(bpy.data.objects)
    bpy.ops.import_scene.fbx(filepath=os.path.abspath(path))
    new = [o for o in bpy.data.objects if o not in before]
    roots = [o for o in new if o.parent is None]
    if fix:
        def turn_back(o, depth):
            r = o.rotation_euler
            if depth >= 2 and abs(math.degrees(r.x) - 90) < 1 and abs(r.y) < 1e-3 and abs(r.z) < 1e-3:
                x, y, z = o.location
                o.location, o.rotation_euler = (x, z, -y), (0, 0, 0)
            for c in list(o.children):
                turn_back(c, depth + 1)
        for r in roots:
            turn_back(r, 0)
    return new, roots


def load(job):
    """The job's files in the scene, textured and shaded: (every object, the drawn meshes, the top node of each file)."""
    objects, tops = [], []
    for f in job["files"]:
        new, roots = bring(f["fbx"], job.get("fix", False))
        if f.get("at"):
            x, y, z = f["at"]
            for r in roots:
                r.location = Vector(r.location) + Vector((x, z, y))
        objects += new
        tops.append(roots)
    for o in objects:
        name = o.name.split(".")[0]
        if (o.type == 'EMPTY' and name.startswith("Socket_")) or any(h in o.name for h in job.get("hide", [])):
            o.hide_render = True
    meshes = [o for o in objects if o.type == 'MESH' and not o.hide_render]
    if not meshes:
        raise RuntimeError("no mesh in " + ", ".join(f["fbx"] for f in job["files"]))
    if job.get("texture"):
        img = bpy.data.images.load(os.path.abspath(job["texture"]), check_existing=True)
        mat = bpy.data.materials.new("board")
        mat.use_nodes = True
        tex = mat.node_tree.nodes.new("ShaderNodeTexImage")
        tex.image = img
        mat.node_tree.links.new(tex.outputs["Color"], mat.node_tree.nodes["Principled BSDF"].inputs["Base Color"])
        mat.node_tree.nodes.active = tex
        for o in meshes:
            o.data.materials.clear()
            o.data.materials.append(mat)
    for o in meshes:
        o.data.shade_smooth()
        try:
            o.data.set_sharp_from_angle(angle=math.radians(55))   # as Unity shades the machines (Editor/TankImport.cs)
        except Exception:
            pass
    bpy.context.view_layer.update()
    return objects, meshes, tops


def box(meshes):
    pts = [o.matrix_world @ Vector(c) for o in meshes for c in o.bound_box]
    lo = Vector([min(p[k] for p in pts) for k in range(3)])
    hi = Vector([max(p[k] for p in pts) for k in range(3)])
    return lo, hi


def render(job):
    scn, cam = reset()
    objects, meshes, _ = load(job)
    lo, hi = box(meshes)
    centre, size = (lo + hi) / 2, hi - lo
    view = Vector(job["view"]).normalized() if job.get("view") else VIEW
    cam.location = centre + view * (size.length * 3 + 10)
    cam.rotation_euler = (-view).to_track_quat('-Z', 'Y').to_euler()
    cam.data.ortho_scale = size.length * 1.02
    cam.data.clip_end = size.length * 8 + 50
    scn.render.filepath = os.path.abspath(job["out"])
    bpy.ops.render.render(write_still=True)
    out = {"ok": True, "size": [round(size.x, 2), round(size.z, 2), round(size.y, 2)], "meshes": len(meshes)}   # as Unity x, y, z
    if job.get("expect"):
        off = [abs(a - b) / max(b, 0.5) for a, b in zip(out["size"], job["expect"])]
        if max(off) > 0.1:
            out["warn"] = "assembled %s m, the manifest says %s m" % (out["size"], [round(v, 2) for v in job["expect"]])
    return out


if __name__ == "__main__":        # film_blender.py imports the scene and the loader from here
    argv = sys.argv[sys.argv.index("--") + 1:]
    jobs = json.load(open(argv[0], encoding="utf-8"))
    results = {}
    for job in jobs:
        try:
            results[job["id"]] = render(job)
        except Exception as e:
            results[job["id"]] = {"ok": False, "error": "%s: %s" % (type(e).__name__, e), "trace": traceback.format_exc()[-600:]}
        json.dump(results, open(argv[1], "w", encoding="utf-8"), indent=1)
