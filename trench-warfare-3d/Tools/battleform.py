# Blender (imported by tank3split.py and jeepsplit.py when TW_BATTLE=1; not run by itself): write a split machine in
# the BATTLE's form, as mechsplit.py TW_BATTLE=1 does for the machines it splits (2026-09-28, the Brute and the Mercy):
# the parts nested (each under its parent, the Hull under the file's root, as Presentation/Camera/TankModel.cs walks
# them), each mesh about its own pivot, the sockets as empties under their parts, exported with the playground's
# exporter settings. Two files, as TankRenderer draws two levels: <Name>_LOD0.fbx and <Name>_LOD1.fbx (the far one,
# the playground's LOD2). The base colour goes beside the folder as <Name>Atlas.jpg, the manifest and a portrait
# render (Tools/portraitcut.py cuts the HUD's pictures from it) to the render folder, because Resources ships
# whatever sits in it.
#
# The far LOD wears LOD0's atlas when it was DERIVED from LOD0 (its UVs are LOD0's), and an atlas of its own
# (<Name>Atlas_LOD1.jpg, which TankRenderer loads for the far level when it is there) when it is another sculpt with
# its own UVs: seen in Play on 2026-09-28, the Brute's and the Mercy's derived far models were torn into shards
# (a quarter of their surface faced another way than the near model's beside it; the others 4-14 %), which
# jeepsplit.py had measured on the playground's jeep the day before. Tripo's own low sculpts are whole; rebake()
# paints one with LOD0's colours so that the switch changes the shape's detail and not its paint.
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


def joined(parts, mat, name):
    bm = bmesh.new()
    for b in parts.values():
        me = bpy.data.meshes.new("part"); b.to_mesh(me); bm.from_mesh(me); bpy.data.meshes.remove(me)
    me = bpy.data.meshes.new(name); bm.to_mesh(me); bm.free(); me.materials.append(mat)
    o = bpy.data.objects.new(name, me); bpy.context.scene.collection.objects.link(o)
    return o


def rebake(src_parts, src_mat, dst_parts, dst_img, path, keep=()):
    """Paint a far model that has UVs of its own with the near model's colours (a Cycles bake from the one onto the
    other, jeepsplit.py's); a texel the bake misses keeps the far model's own paint, and so does every part named in
    `keep` (a part whose faces lie across other parts' islands would be painted over them). Returns (path, material)."""
    import numpy as np
    dst_parts = {n: b for n, b in dst_parts.items() if n not in keep}
    os.makedirs(os.path.dirname(path), exist_ok=True)
    src = joined(src_parts, src_mat, "bake_src")
    w, h = dst_img.size
    img = bpy.data.images.new("rebake", w, h, alpha=False); img.generated_color = (1.0, 0.0, 1.0, 1.0)
    mat = bpy.data.materials.new("rebake_mat"); mat.use_nodes = True
    tn = mat.node_tree.nodes.new("ShaderNodeTexImage"); tn.image = img
    mat.node_tree.nodes.active = tn
    dst = joined(dst_parts, mat, "bake_dst")
    scn = bpy.context.scene; was = scn.render.engine
    scn.render.engine = 'CYCLES'; scn.cycles.samples = 1; scn.cycles.device = 'CPU'
    for x in bpy.context.selected_objects: x.select_set(False)
    src.select_set(True); dst.select_set(True); bpy.context.view_layer.objects.active = dst
    bpy.ops.object.bake(type='DIFFUSE', pass_filter={'COLOR'}, use_selected_to_active=True, cage_extrusion=0.02,
                        max_ray_distance=0.08, margin=8)
    px = np.array(img.pixels[:], dtype=np.float32).reshape(-1, 4); own = np.array(dst_img.pixels[:], dtype=np.float32).reshape(-1, 4)
    miss = ((px[:, 0] > 0.98) & (px[:, 1] < 0.02) & (px[:, 2] > 0.98)) | ((px[:, :3].sum(1) < 0.02) & (own[:, :3].sum(1) > 0.08))
    px[miss] = own[miss]; img.pixels[:] = px.ravel()
    print("FAR rebaked from LOD0: %.1f%% of texels missed, kept from its own paint" % (100.0 * miss.mean()))
    img.filepath_raw = path; img.file_format = 'JPEG'; img.save()
    out = bpy.data.materials.new("far_rebaked"); out.use_nodes = True
    t = out.node_tree.nodes.new("ShaderNodeTexImage"); t.image = img
    out.node_tree.links.new(t.outputs["Color"], out.node_tree.nodes["Principled BSDF"].inputs["Base Color"])
    for o in (src, dst): bpy.data.objects.remove(o)
    scn.render.engine = was
    return path, out


def write(name, outdir, renderdir, near, far, parts, parent, piv, sock, mat, base, scale, manifest, tris_of, far_mat=None, far_base=None):
    """Both LODs, the atlas, the manifest and the portrait. `near` and `far` are {part: bmesh}; `manifest` is the
    playground's, which gains "battle" and the two LODs' triangle counts. `far_mat` and `far_base`: the far model's
    own material and base colour, when it is not derived from the near one (written as <Name>Atlas_LOD1.jpg)."""
    os.makedirs(outdir, exist_ok=True); os.makedirs(renderdir, exist_ok=True)
    manifest = dict(manifest); manifest["battle"] = True; manifest["lods"] = []
    manifest["farAtlas"] = far_base is not None
    for lod, P in ((0, near), (1, far)):
        root, objs = make(name, lod, P, parts, parent, piv, sock, far_mat if lod == 1 and far_mat is not None else mat, scale)
        if lod == 0: portrait(objs, os.path.join(renderdir, name + "_portrait.png"))
        export(root, os.path.join(outdir, "%s_LOD%d.fbx" % (name, lod)))
        for n, o in objs.items(): o.name = "%d|%s" % (lod, n)   # free the names for the next LOD
        for o in objs.values():
            if o.type == 'MESH': o.hide_render = True
        t = sum(tris_of(P[n]) for n in parts)
        manifest["lods"].append({"lod": lod, "tris": t, "parts": {n: tris_of(P[n]) for n in parts}})
        print("EXPORT BATTLE LOD%d: %d tris" % (lod, t))
    shutil.copyfile(base, os.path.join(os.path.dirname(os.path.normpath(outdir)), name + "Atlas.jpg"))
    if far_base is not None: shutil.copyfile(far_base, os.path.join(os.path.dirname(os.path.normpath(outdir)), name + "Atlas_LOD1.jpg"))
    manifest["lodList"] = manifest["lods"]
    json.dump(manifest, open(os.path.join(renderdir, name + "_battle.json"), "w"), indent=1)
    print("DONE")
