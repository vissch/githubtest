# midfigure.py — the mid figures VATRenderer draws between MidDistance and LodDistance (owner, 2026-09-28: 250 triangles).
#
#   python Tools/midfigure.py [Soldier Sniper] [--tris 250]      (from trench-warfare-3d/; needs numpy and pymeshlab)
#
# Reads a baked figure (Resources/Units/Figure<Name>Mesh.asset and Figure<Name>Atlas.bytes), welds its UV seams (vertices
# that coincide in every sampled pose), and decimates the body with MeshLab's quadric edge collapse with optimal placement
# OFF: a vertex only ever merges into a neighbour, so every vertex left is one of the figure's own and plays its column of
# the existing atlas (VAT_URP reads the column from UV1.y). The rifle box (the baker's last 24 vertices) is kept whole.
# Each kept vertex takes the mean colour of the original vertices nearest to it (idle pose), the team mask by majority,
# and records where its column stood (the idle frame), so the writer can refuse a source the figure no longer matches.
# Writes Tools/midfigure/Figure<Name>Mid.json; TW/VAT/Write Mid Figures (VATBaker.WriteMidFigures, also run at the end of
# every infantry bake) turns it into Resources/Units/Figure<Name>MidMesh.asset.
# Why not decimate in Blender or build from hulls: measured on 64 held-out frames x 12 game-camera views, Blender-style
# optimal placement breaks the column link, convex hulls per bone cap at IoU 0.90 even with every corner (they fill the
# pack and the coat), and this reached 0.92 (Soldier) / 0.945 (Sniper) at 250 triangles; 400 would give 0.94 / 0.95.
import gzip, json, os, re, struct, sys
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
UNITS = os.path.join(HERE, "..", "Assets", "_Project", "Resources", "Units")
OUT = os.path.join(HERE, "midfigure")
RIFLE = 24   # VATBaker appends the helmet brim (none on the Tripo figures), then the rifle box: 24 vertices, 12 triangles
# MeshLab settings per figure, the best of a 96-setting grid on validation frames (the plateau is flat: +-0.005 IoU)
SETTINGS = {
    "Soldier": dict(qualitythr=0.3, boundaryweight=0.5, preservenormal=False, planarquadric=False, preservetopology=True),
    "Sniper": dict(qualitythr=0.1, boundaryweight=4.0, preservenormal=True, planarquadric=True, preservetopology=False),
}


def load_mesh(name):
    txt = open(os.path.join(UNITS, f"Figure{name}Mesh.asset"), encoding="utf-8").read()
    V = int(re.search(r"m_VertexCount: (\d+)", txt).group(1))
    fmt = int(re.search(r"m_IndexFormat: (\d)", txt).group(1))
    idx = np.frombuffer(bytes.fromhex(re.search(r"m_IndexBuffer: ([0-9a-f]+)", txt).group(1)), dtype=np.uint16 if fmt == 0 else np.uint32)
    data = bytes.fromhex(re.search(r"_typelessdata: ([0-9a-f]+)", txt).group(1))
    assert len(data) == V * 48, "expected position, normal, colour (4 floats) and UV1 (2 floats) per vertex"
    a = np.frombuffer(data, dtype=np.float32).reshape(V, 12)
    return dict(pos=a[:, 0:3].astype(np.float64), col=a[:, 6:10].copy(), tris=idx.astype(np.int64).reshape(-1, 3))


def load_frames(name, picks):
    b = gzip.decompress(open(os.path.join(UNITS, f"Figure{name}Atlas.bytes"), "rb").read())
    n = s = 0; o = 0
    while True:   # BinaryWriter string: 7-bit length, then the magic
        c = b[o]; o += 1; n |= (c & 127) << s; s += 7
        if c < 128: break
    o += n
    V, F, R = struct.unpack_from("<iii", b, o); o += 12 + R * 12
    mn = np.array(struct.unpack_from("<3f", b, o)); sz = np.array(struct.unpack_from("<3f", b, o + 12)); o += 24
    q = np.frombuffer(b, dtype=np.uint16, count=F * V * 3, offset=o).reshape(F, V, 3)
    picks = [p for p in picks(F)]
    return mn + q[picks].astype(np.float64) / 65535.0 * sz


def build(name, tris):
    import pymeshlab
    m = load_mesh(name)
    V = len(m["pos"]); body = V - RIFLE
    # seams: vertices that coincide in 8 poses spread over the whole atlas are one vertex of the surface
    P8 = load_frames(name, lambda F: np.linspace(0, F - 1, 8).astype(int))[:, :body]
    key = np.round(P8.transpose(1, 0, 2).reshape(body, -1) * 1e4).astype(np.int64)
    _, group = np.unique(key, axis=0, return_inverse=True); group = group.reshape(-1)
    W = group.max() + 1
    rep = np.zeros(W, dtype=np.int64)
    for v in range(body - 1, -1, -1): rep[group[v]] = v   # the lowest column stands for its group
    t = m["tris"]
    rifle_t = t[(t >= body).all(1)]
    faces = group[t[(t < body).all(1)]]
    faces = faces[(faces[:, 0] != faces[:, 1]) & (faces[:, 1] != faces[:, 2]) & (faces[:, 0] != faces[:, 2])]
    P = m["pos"][rep]   # the idle pose: the mesh asset's own vertices
    ms = pymeshlab.MeshSet()
    ms.add_mesh(pymeshlab.Mesh(vertex_matrix=P, face_matrix=faces.astype(np.int32)))
    ms.meshing_decimation_quadric_edge_collapse(targetfacenum=int(tris - len(rifle_t)), preserveboundary=False, optimalplacement=False,
                                                qualityweight=False, autoclean=True, **SETTINGS.get(name, SETTINGS["Soldier"]))
    out = ms.current_mesh(); ov = out.vertex_matrix(); of = out.face_matrix()
    where = {tuple(np.round(p, 7)): w for w, p in enumerate(P)}
    wid = np.array([where[tuple(np.round(p, 7))] for p in ov])   # no placement: every vertex is where it was
    used = np.unique(wid[of])
    kept = [[int(np.searchsorted(used, wid[a])) for a in f] for f in of]
    rest = m["pos"][:body]; near = np.argmin(np.linalg.norm(rest[:, None] - m["pos"][rep[used]][None], axis=-1), axis=1)
    colours = []
    for j in range(len(used)):
        c = m["col"][:body][near == j]
        if len(c) == 0: c = m["col"][[rep[used[j]]]]
        colours += [float(x) for x in c[:, :3].mean(0)] + [float(np.round(c[:, 3].mean()))]
    columns = [int(rep[w]) for w in used]
    nb = len(columns)
    columns += list(range(body, V))
    colours += [float(x) for x in m["col"][body:].reshape(-1)]
    kept += [[int(x - body + nb) for x in f] for f in rifle_t]
    positions = [round(float(x), 5) for c in columns for x in m["pos"][c]]   # where each column stood when it was cut
    return dict(columns=columns, tris=[x for f in kept for x in f], colours=[round(x, 5) for x in colours], positions=positions)


if __name__ == "__main__":
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    tris = int(sys.argv[sys.argv.index("--tris") + 1]) if "--tris" in sys.argv else 250
    names = [a for a in args if not a.isdigit()] or ["Soldier", "Sniper"]
    os.makedirs(OUT, exist_ok=True)
    for name in names:
        r = build(name, tris)
        json.dump(r, open(os.path.join(OUT, f"Figure{name}Mid.json"), "w"), separators=(",", ":"))
        print(f"{name}: {len(r['columns'])} vertices, {len(r['tris']) // 3} triangles -> Tools/midfigure/Figure{name}Mid.json")
