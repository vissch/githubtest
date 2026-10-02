# Blender (background): the frames of the asset board's films. Run by films.py, not by hand:
#   blender -b --factory-startup -P Tools/assetboard/film_blender.py -- jobs.json results.json
# A job: {id, kind, out: a folder for the frames (00000.png ...), fps, size, and what its kind needs}
#   turn    the model once round on a turntable. The fields of a thumb job (thumb_blender.py), and seconds.
#   apart   a building drawn apart into the chunks it is cut into for destruction, and back. A thumb job's fields.
#   figure  a battle figure playing its baked clips: mesh (Resources/Units/Figure<F>Mesh.asset), atlas
#           (Figure<F>Atlas.bytes), rows: [{row, name}]. This is the data the game draws: the same vertices, frame
#           for frame, sampled the way the shader samples them (VatCodec.cs), so the rifle and the helmet are there.
# A result: {ok, frames, segments: [{caption, first, count}]}; films.py writes the captions and makes the film.
# Workbench, as the previews: the model, not the game's look.
import bpy, gzip, json, math, os, re, struct, sys, traceback
import numpy as np
from mathutils import Vector

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import thumb_blender as thumb   # noqa: E402

PLATE = (27, 29, 32)
FLOOR = ((30, 32, 36), (38, 40, 45))        # the two tiles of the floor
CLOTH = (163, 143, 87)                      # _TeamColorA in VAT_URP.shader: (0.64, 0.56, 0.34)


def lin(c):
    """sRGB 0..255 to the linear colour Blender wants."""
    out = []
    for v in c:
        v = v / 255.0
        out.append(v / 12.92 if v <= 0.04045 else ((v + 0.055) / 1.055) ** 2.4)
    return out


def stage(size, vertex_colour=False):
    scn, cam = thumb.reset()
    scn.render.film_transparent = False
    scn.render.resolution_x = scn.render.resolution_y = size
    scn.render.image_settings.color_mode = 'RGB'
    scn.render.image_settings.compression = 10
    world = bpy.data.worlds.new("plate")
    world.color = lin(PLATE)
    scn.world = world
    sh = scn.display.shading
    sh.color_type = 'VERTEX' if vertex_colour else 'TEXTURE'
    sh.show_shadows = True
    sh.shadow_intensity = 0.35
    scn.display.light_direction = (0.35, 0.45, 0.82)
    return scn, cam


def floor(z, centre, cell, span=60):
    """A chequered floor under the model: something for a shadow to fall on and for a walk to move over."""
    n = max(8, int(span / cell) // 2 * 2)
    half = n * cell / 2
    cx, cy = round(centre.x / (2 * cell)) * 2 * cell, round(centre.y / (2 * cell)) * 2 * cell
    verts = [(cx - half + i * cell, cy - half + j * cell, z) for j in range(n + 1) for i in range(n + 1)]
    faces = [(j * (n + 1) + i, j * (n + 1) + i + 1, (j + 1) * (n + 1) + i + 1, (j + 1) * (n + 1) + i) for j in range(n) for i in range(n)]
    me = bpy.data.meshes.new("floor")
    me.from_pydata(verts, [], faces)
    mats = []
    for k, c in enumerate(FLOOR):
        m = bpy.data.materials.new("floor%d" % k)
        m.diffuse_color = lin(c) + [1.0]
        me.materials.append(m)
        mats.append(lin(c) + [1.0])
    att = me.color_attributes.new("Col", 'FLOAT_COLOR', 'CORNER')
    cols = []
    for f, (i, j) in enumerate((i, j) for j in range(n) for i in range(n)):
        k = (i + j) & 1
        me.polygons[f].material_index = k
        cols += mats[k] * 4
    att.data.foreach_set("color", cols)
    ob = bpy.data.objects.new("floor", me)
    bpy.context.scene.collection.objects.link(ob)
    return ob


def aim(cam, centre, view, scale, far):
    cam.location = centre + view * (far * 3 + 10)
    cam.rotation_euler = (-view).to_track_quat('-Z', 'Y').to_euler()
    cam.data.ortho_scale = scale
    cam.data.clip_end = far * 8 + 120


def shoot(scn, job, index):
    scn.render.filepath = os.path.join(os.path.abspath(job["out"]), "%05d.png" % index)
    bpy.ops.render.render(write_still=True)


def cell_for(size):
    return 0.5 if size.length < 4 else (1.0 if size.length < 14 else 2.0)


def turn(job):
    scn, cam = stage(job["size"])
    objects, meshes, _ = thumb.load(job)
    lo, hi = thumb.box(meshes)
    centre, size = (lo + hi) / 2, hi - lo
    floor(lo.z, centre, cell_for(size))
    view0 = Vector(job["view"]).normalized() if job.get("view") else thumb.VIEW
    flat, up = math.hypot(view0.x, view0.y), view0.z
    start = math.atan2(view0.y, view0.x)
    frames = int(round(job["seconds"] * job["fps"]))
    for f in range(frames):
        a = start + 2 * math.pi * f / frames
        aim(cam, centre, Vector((math.cos(a) * flat, math.sin(a) * flat, up)), size.length * 1.04, size.length)
        shoot(scn, job, f)
    return {"ok": True, "frames": frames, "segments": [], "size": [round(size.x, 2), round(size.z, 2), round(size.y, 2)]}


def apart(job):
    scn, cam = stage(job["size"])
    objects, meshes, tops = thumb.load(job)
    lo, hi = thumb.box(meshes)
    centre, size = (lo + hi) / 2, hi - lo
    floor(lo.z, centre, cell_for(size))
    pieces = []                                   # (top nodes of one chunk, where they rest, the way the chunk goes)
    for roots in tops:
        own = [o for o in meshes if any(o == r or r in ancestors(o) for r in roots)]
        if not own:
            continue
        clo, chi = thumb.box(own)
        away = (clo + chi) / 2 - centre
        away.z = max(away.z, 0.0) * 0.6            # up and outward, never through the floor
        pieces.append((roots, [r.location.copy() for r in roots], away))
    spread = float(job.get("spread", 0.75))
    aim(cam, centre + Vector((0, 0, size.z * spread * 0.15)), thumb.VIEW, size.length * (1.0 + spread * 0.62), size.length * 2)
    frames = int(round(job["seconds"] * job["fps"]))
    for f in range(frames):
        t = f / frames
        k = ease(min(1.0, t / 0.38)) if t < 0.62 else 1.0 - ease(min(1.0, (t - 0.62) / 0.3))
        for roots, rest, away in pieces:
            for r, at in zip(roots, rest):
                r.location = at + away * (spread * k)
        shoot(scn, job, f)
    return {"ok": True, "frames": frames, "segments": [{"caption": "%d chunks" % len(pieces), "first": 0, "count": frames}]}


def ancestors(o):
    out = []
    while o.parent is not None:
        o = o.parent
        out.append(o)
    return out


def ease(t):
    return t * t * (3 - 2 * t)


# ---------------------------------------------------------------- the baked figures
def read_mesh(path):
    """A text-serialised Unity mesh: positions, vertex colours and triangles (16 or 32 bit indices)."""
    t = open(path, encoding="utf-8").read()
    count = int(re.search(r"m_VertexCount: (\d+)", t).group(1))
    chans = [tuple(int(v) for v in m) for m in re.findall(r"- stream: (\d+)\s+offset: (\d+)\s+format: (\d+)\s+dimension: (\d+)", t)]
    raw = bytes.fromhex(re.search(r"_typelessdata: ([0-9a-f]+)", t).group(1))
    stride = len(raw) // count
    if any(c[0] != 0 or c[2] != 0 for c in chans if c[3]):
        raise RuntimeError("the mesh is not one stream of floats: " + path)
    data = np.frombuffer(raw, dtype="<f4").reshape(count, stride // 4)
    pos = data[:, chans[0][1] // 4: chans[0][1] // 4 + 3]
    col = data[:, chans[3][1] // 4: chans[3][1] // 4 + 4] if chans[3][3] == 4 else np.full((count, 4), (0.5, 0.5, 0.5, 0.0), dtype="<f4")
    wide = int(re.search(r"m_IndexFormat: (\d+)", t).group(1)) == 1
    idx = np.frombuffer(bytes.fromhex(re.search(r"m_IndexBuffer: ([0-9a-f]+)", t).group(1)), dtype="<u4" if wide else "<u2")
    return pos, col, idx.reshape(-1, 3)


def read_atlas(path):
    """VatCodec.Decode: the row table and every frame's positions, in metres."""
    raw = gzip.decompress(open(path, "rb").read())
    n = raw[0]
    magic = raw[1:1 + n].decode()
    if magic not in ("TWVAT3", "TWVAT2"):
        raise RuntimeError("not a TW VAT atlas: " + path)
    o = 1 + n
    count, total, rows = struct.unpack_from("<iii", raw, o)
    o += 12
    table = [struct.unpack_from("<iif", raw, o + 12 * k) for k in range(rows)]
    o += 12 * rows
    lo = np.array(struct.unpack_from("<fff", raw, o), dtype="f4")
    size = np.array(struct.unpack_from("<fff", raw, o + 12), dtype="f4")
    o += 24
    pos = np.frombuffer(raw, dtype="<u2", count=count * total * 3, offset=o).reshape(total, count, 3)
    return dict(count=count, total=total, table=table, lo=lo, size=size, pos=pos)


def figure(job):
    scn, cam = stage(job["size"], vertex_colour=True)
    pos, col, tris = read_mesh(job["mesh"])
    atlas = read_atlas(job["atlas"])
    if atlas["count"] != len(pos):
        raise RuntimeError("the atlas has %d vertices, the mesh %d" % (atlas["count"], len(pos)))
    if any(r["row"] >= len(atlas["table"]) for r in job["rows"]):
        raise RuntimeError("the atlas has %d rows, the Clip enum asks for more: bake it again" % len(atlas["table"]))

    def frame(k):                                   # Unity (x, y, z), y up, to Blender (x, z, y): he faces +Y
        p = atlas["pos"][k].astype("f4") / 65535.0 * atlas["size"] + atlas["lo"]
        return p[:, [0, 2, 1]]

    me = bpy.data.meshes.new("figure")
    me.from_pydata(frame(atlas["table"][job["rows"][0]["row"]][0]).tolist(), [], tris[:, [0, 2, 1]].tolist())   # the turn of a face goes with the hand of the axes
    att = me.color_attributes.new("Col", 'FLOAT_COLOR', 'POINT')
    # VAT_URP.shader: albedo = lerp(rgb, rgb * team cloth, a). The vertex colours reach the shader as they are
    # (linear); the cloth colour is a material colour, which Unity takes from sRGB. Team 0's cloth, the shader's default.
    cloth = np.array(lin(job.get("cloth") or CLOTH), dtype="f8")
    rgb, mask = col[:, :3].astype("f8"), col[:, 3:4].astype("f8")
    c = np.clip(rgb * (1 - mask) + rgb * cloth * mask, 0, 1)
    att.data.foreach_set("color", np.concatenate([c, np.ones((len(c), 1))], axis=1).ravel())
    me.shade_smooth()
    ob = bpy.data.objects.new("figure", me)
    scn.collection.objects.link(ob)

    fps = job["fps"]
    plan, lo, hi = [], None, None                    # (caption, [(frame a, frame b, blend)])
    for r in job["rows"]:
        first, count, seconds = atlas["table"][r["row"]]
        loops, count = count > 0, max(1, abs(count))
        seconds = max(seconds, 1.0 / fps)
        if loops:
            play = seconds * max(1, int(math.ceil(job.get("least", 2.0) / seconds)))
        else:
            play = seconds + job.get("hold", 0.4)
        shots = []
        for f in range(max(2, int(round(play * fps)))):
            t = f / fps / seconds
            local = ((t - math.floor(t)) if loops else min(1.0, t)) * count
            f0 = min(math.floor(local), count - 1)
            f1 = (f0 + 1) % count if loops else min(f0 + 1, count - 1)
            shots.append((first + int(f0), first + int(f1), min(1.0, max(0.0, local - f0))))
        span = atlas["pos"][first:first + count]
        a = span.min(axis=(0, 1)).astype("f4") / 65535.0 * atlas["size"] + atlas["lo"]
        b = span.max(axis=(0, 1)).astype("f4") / 65535.0 * atlas["size"] + atlas["lo"]
        plan.append((r["name"], shots, Vector((a[0], a[2], min(a[1], 0.0))), Vector((b[0], b[2], b[1]))))
        lo = a if lo is None else np.minimum(lo, a)
        hi = b if hi is None else np.maximum(hi, b)
    lo, hi = Vector((lo[0], lo[2], lo[1])), Vector((hi[0], hi[2], hi[1]))
    lo.z = min(lo.z, 0.0)
    centre, size = (lo + hi) / 2, hi - lo
    floor(-0.005, centre, 0.5, span=30)
    view = Vector(job.get("view") or thumb.VIEW).normalized()
    aim(cam, centre, view, max(size.length * 0.92, 2.6), size.length)

    index, segments = 0, []
    if job.get("turn"):                              # standing as the first row's first frame, the camera goes round him
        view0 = Vector(job.get("view") or thumb.VIEW).normalized()
        flat, start = math.hypot(view0.x, view0.y), math.atan2(view0.y, view0.x)
        frames = int(round(job["turn"] * fps))
        for f in range(frames):
            ang = start + 2 * math.pi * f / frames
            aim(cam, centre, Vector((math.cos(ang) * flat, math.sin(ang) * flat, view0.z)), max(size.length * 1.04, 2.2), size.length)
            shoot(scn, job, f)
        return {"ok": True, "frames": frames, "segments": []}
    for caption, shots, clo, chi in plan:            # each clip framed on what it covers, the cut between clips hides the jump
        segments.append({"caption": caption, "first": index, "count": len(shots)})
        aim(cam, (clo + chi) / 2, view, max((chi - clo).length * 1.0, 2.7), size.length)
        for a, b, w in shots:
            p = frame(a) if w <= 0.0 or a == b else frame(a) * (1 - w) + frame(b) * w
            me.vertices.foreach_set("co", p.astype("f4").ravel())
            me.update()
            shoot(scn, job, index)
            index += 1
    return {"ok": True, "frames": index, "segments": segments}


KINDS = {"turn": turn, "apart": apart, "figure": figure}

if __name__ == "__main__":
    argv = sys.argv[sys.argv.index("--") + 1:]
    jobs = json.load(open(argv[0], encoding="utf-8"))
    results = {}
    for job in jobs:
        try:
            os.makedirs(job["out"], exist_ok=True)
            results[job["id"]] = KINDS[job["kind"]](job)
        except Exception as e:
            results[job["id"]] = {"ok": False, "error": "%s: %s" % (type(e).__name__, e), "trace": traceback.format_exc()[-800:]}
        json.dump(results, open(argv[1], "w", encoding="utf-8"), indent=1)
