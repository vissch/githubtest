// Fingerprint every prop instance BattlefieldProps holds (module mesh + name, every matrix to the bit), after pausing the
// sim and forcing one whole Compose. EditorPrefs "tw.propsnap.mode": "sync" = call Compose() now; "incremental" =
// Recompose() and let Update spread it, fingerprint once it has applied (EditorApplication.update, result to
// EditorPrefs "tw.propsnap").
var bf = System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance;
var props = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>();
var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (props == null || host == null) return "not in play";
host.TimeScale = 0f; host.DropBacklog();
var pt = typeof(TW.Presentation.Terrain.BattlefieldProps);
System.Func<string> fingerprint = () =>
{
    var dict = (System.Collections.IDictionary)pt.GetField("batches", bf).GetValue(props);
    var lines = new System.Collections.Generic.List<string>();
    foreach (System.Collections.DictionaryEntry kv in dict)
    {
        var module = kv.Key; var batch = kv.Value; var bt = batch.GetType(); var mt = module.GetType();
        var meshObj = mt.GetField("Mesh", bf)?.GetValue(module) ?? mt.GetProperty("Mesh", bf)?.GetValue(module);
        string mname = (meshObj as UnityEngine.Mesh != null ? ((UnityEngine.Mesh)meshObj).name : "?") + "/" + (mt.GetField("Name", bf)?.GetValue(module) ?? "-");
        var pages = (System.Collections.IList)bt.GetField("Pages", bf).GetValue(batch);
        var counts = (System.Collections.Generic.List<int>)bt.GetField("Counts", bf).GetValue(batch);
        for (int p = 0; p < counts.Count; p++)
        {
            var arr = (UnityEngine.Matrix4x4[])pages[p];
            for (int s = 0; s < counts[p]; s++)
            {
                var m = arr[s]; var sbm = new System.Text.StringBuilder(mname);
                for (int k = 0; k < 16; k++) sbm.Append('|').Append(System.BitConverter.SingleToInt32Bits(m[k]).ToString("X8"));
                lines.Add(sbm.ToString());
            }
        }
    }
    lines.Sort(System.StringComparer.Ordinal);
    ulong h = 14695981039346656037UL;
    foreach (var l in lines) foreach (char c in l) { h ^= c; h *= 1099511628211UL; }
    return lines.Count + " instances, fingerprint " + h.ToString("X16");
};
string mode = UnityEditor.EditorPrefs.GetString("tw.propsnap.mode", "sync");
if (mode == "sync")
{
    pt.GetMethod("Compose", bf).Invoke(props, null);
    return "sync: " + fingerprint();
}
UnityEditor.EditorPrefs.SetString("tw.propsnap", "running");
props.Recompose();
int frames = 0;
UnityEditor.EditorApplication.CallbackFunction tick = null;
tick = () =>
{
    frames++;
    bool busy = pt.GetField("composing", bf)?.GetValue(props) != null || (bool)pt.GetField("dirty", bf).GetValue(props);
    if (busy && frames < 600) return;
    UnityEditor.EditorPrefs.SetString("tw.propsnap", "incremental after " + frames + " editor ticks: " + fingerprint());
    UnityEditor.EditorApplication.update -= tick;
};
UnityEditor.EditorApplication.update += tick;
return "incremental: started";
