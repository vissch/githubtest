# Blender (imported by tank3split.py and jeepsplit.py when TW_BATTLE=1; not run by itself): write a split machine in
# the BATTLE's form, as mechsplit.py TW_BATTLE=1 does for the machines it splits (2026-09-28, the Brute and the Mercy):
# the parts nested (each under its parent, the Hull under the file's root, as Presentation/Camera/TankModel.cs walks
# them), each mesh about its own pivot, the sockets as empties under their parts, exported with the playground's
# exporter settings. Two files, as TankRenderer draws two levels: <Name>_LOD0.fbx and <Name>_LOD1.fbx (the far one,
# the playground's LOD2). The base colour goes beside the folder as <Name>Atlas.jpg, the manifest and a portrait
# render (Tools/portraitcut.py cuts the HUD's pictures from it) to the render folder, because Resources ships
# whatever sits in it.
#
# Both LODs wear LOD0's material, so the far LOD must be DERIVED from LOD0 (its UVs are LOD0's): the callers say so.
import bpy, bmesh, os, math, json, shutil
from mathutils import Vector, Matrix

TURN = Matrix.Rotation(math.pi, 4, 'Z')   # front to Blender +Y, which the FBX export puts at Unity +Z


def make(name, lod, P, parts, parent, piv, sock, mat, scale):
    """The nested objects of one LOD. `parts` lists every parent before its children."""
    objs = {}
    for n in parts:
        b = P[n].copy()
        bmesh.ops.transform(b, matrix=TURN @ Matrix.Scale(scale, 4) @ Matrix.Translation(-piv[n]), verts=b.verts)
        me = bpy.data.meshes.new("%s_LOD%d_%s" % (name, lod, n)); b.to_mesh(me); b.free(); me.materials.append(mat)
        o = bpy.data.objects.new(n, me); bpy.context.scene.collection.objects.link(o); objs[n] = o
    root = bpy.data.objects.new("%s_LOD%d" % (name, lod), None); bpy.context.scene.collection.objects.link(root)
    for n in parts:
        par = parent.get(n)
        objs[n].parent = objs[par] if par else root
        objs[n].location = (TURN @ (piv[n] - (piv[par] if par else Vector()))) * scale
    for sname, (owner, pos) in sock.items():
        e = bpy.data.objects.new(sname, None); bpy.context.scene.collection.objects.link(e)
        e.empty_display_size = 0.15; e.parent = objs[owner]; e.location = (TURN @ (pos - piv[owner])) * scale
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


def portrait(objs, path):
    """LOD0 lit and textured, front-left three-quarters from above, on a transparent film, 1024 px (mechsplit.py's)."""
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
    if scn.camera is None:
        cam = bpy.data.objects.new("portrait_cam", bpy.data.cameras.new("portrait_cam")); scn.collection.objects.link(cam)
        cam.data.type = 'ORTHO'; scn.camera = cam
    bpy.context.view_layer.update()
    pts = [o.matrix_world @ Vector(c) for o in meshes for c in o.bound_box]
    lo = Vector([min(q[k] for q in pts) for k in range(3)]); hi = Vector([max(q[k] for q in pts) for k in range(3)])
    mid = (lo + hi) / 2; size = max(hi - lo)
    cam = scn.camera; cam.data.type = 'ORTHO'
    d = Vector((-0.75, 1.0, 0.6)).normalized()   # Blender +Y is the front after the turn
    cam.location = mid + d * size * 4; cam.rotation_euler = (-d).to_track_quat('-Z', 'Y').to_euler()
    cam.data.ortho_scale = size * 1.25; cam.data.clip_end = size * 20
    scn.render.filepath = path
    bpy.ops.render.render(write_still=True)
    scn.render.engine, scn.render.resolution_x, scn.render.resolution_y, scn.render.film_transparent = was


def write(name, outdir, renderdir, near, far, parts, parent, piv, sock, mat, base, scale, manifest, tris_of):
    """Both LODs, the atlas, the manifest and the portrait. `near` and `far` are {part: bmesh}; `manifest` is the
    playground's, which gains "battle" and the two LODs' triangle counts."""
    os.makedirs(outdir, exist_ok=True); os.makedirs(renderdir, exist_ok=True)
    manifest = dict(manifest); manifest["battle"] = True; manifest["lods"] = []
    for lod, P in ((0, near), (1, far)):
        root, objs = make(name, lod, P, parts, parent, piv, sock, mat, scale)
        if lod == 0: portrait(objs, os.path.join(renderdir, name + "_portrait.png"))
        export(root, os.path.join(outdir, "%s_LOD%d.fbx" % (name, lod)))
        for n, o in objs.items(): o.name = "%d|%s" % (lod, n)   # free the names for the next LOD
        for o in objs.values():
            if o.type == 'MESH': o.hide_render = True
        t = sum(tris_of(P[n]) for n in parts)
        manifest["lods"].append({"lod": lod, "tris": t, "parts": {n: tris_of(P[n]) for n in parts}})
        print("EXPORT BATTLE LOD%d: %d tris" % (lod, t))
    shutil.copyfile(base, os.path.join(os.path.dirname(os.path.normpath(outdir)), name + "Atlas.jpg"))
    manifest["lodList"] = manifest["lods"]
    json.dump(manifest, open(os.path.join(renderdir, name + "_battle.json"), "w"), indent=1)
    print("DONE")
