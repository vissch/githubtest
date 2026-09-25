// Every finished terrain chunk against the per-vertex formula the old Fill used: positions and normals must be equal
// bit for bit (the row-sliced, sample-sharing FillRow claims to compute exactly the same floats).
var view = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Terrain.GreyboxTerrainView>();
if (view == null) return "no terrain view (not in play?)";
var s = view.Surface;
var bf = System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance;
var list = (System.Collections.IList)typeof(TW.Presentation.Terrain.GreyboxTerrainView).GetField("chunks", bf).GetValue(view);
int checkedChunks = 0, dirtyChunks = 0, verts = 0, posBad = 0, nrmBad = 0;
const float step = .5f;
foreach (var c in list)
{
    var t = c.GetType();
    bool dirty = (bool)t.GetField("Dirty", bf).GetValue(c);
    if (dirty) { dirtyChunks++; continue; }
    var vs = (UnityEngine.Vector3[])t.GetField("Verts", bf).GetValue(c);
    var ns = (UnityEngine.Vector3[])t.GetField("Normals", bf).GetValue(c);
    int x0 = (int)t.GetField("X0", bf).GetValue(c), z0 = (int)t.GetField("Z0", bf).GetValue(c);
    int w = (int)t.GetField("W", bf).GetValue(c), l = (int)t.GetField("L", bf).GetValue(c);
    checkedChunks++;
    for (int z = 0; z < l; z++)
    for (int x = 0; x < w; x++)
    {
        float wx = x0 + x * step, wz = z0 + z * step;
        float top = s.VisualHeight(wx, wz);
        float dx = s.VisualHeight(wx + .25f, wz) - s.VisualHeight(wx - .25f, wz), dz = s.VisualHeight(wx, wz + .25f) - s.VisualHeight(wx, wz - .25f);
        var n = new UnityEngine.Vector3(-dx, .5f, -dz).normalized;
        var v = vs[z * w + x]; var m = ns[z * w + x];
        verts++;
        if (v.x != wx || v.y != top || v.z != wz) posBad++;
        if (m.x != n.x || m.y != n.y || m.z != n.z) nrmBad++;
    }
}
return "chunks checked " + checkedChunks + " (dirty, skipped: " + dirtyChunks + "), vertices " + verts + ", positions differing " + posBad + ", normals differing " + nrmBad;
