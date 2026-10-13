// AOSA: read the whole-frame submission budget at the current view (docs/reference/aosa/README.md rule 7).
// Statement script for `unity command eval_file <abs path>`. FrameBudget reports the last COMPLETE frame, so this
// averages over 60 frames through an EditorApplication.update delegate that removes ITSELF (never set update = null:
// that kills every session's eval bridge), and leaves the result in EditorPrefs "aosa.<ns>.budget", namespaced per
// checkout (EditorPrefs is per Windows user, not per Unity project, so two editors on two checkouts would otherwise
// cross-write). An operator who does not already know <ns> can get it with one eval:
//   return "aosa." + System.Math.Abs(UnityEngine.Application.dataPath.GetHashCode()).ToString("x8") + ".";
// Run it, wait ~2 s, then: unity command eval "return UnityEditor.EditorPrefs.GetString(\"aosa.<ns>.budget\", \"\");"
// A LOST RUN IS VISIBLE: the value starts as "running <UTC ISO-8601 now> deadline <+120 s>". A domain reload (any
// recompile) drops the update delegate with it, so the value stays at that text. Read it: still "running" past its
// deadline means the sampler died and never wrote a result -- run this script again, do not trust the old value.
if (!UnityEngine.Application.isPlaying) return "not in Play";
string ns = "aosa." + System.Math.Abs(UnityEngine.Application.dataPath.GetHashCode()).ToString("x8") + ".";
var started = System.DateTime.UtcNow;
UnityEditor.EditorPrefs.SetString(ns + "budget", "running "
    + started.ToString("yyyy-MM-ddTHH:mm:ssZ", System.Globalization.CultureInfo.InvariantCulture)
    + " deadline "
    + started.AddSeconds(120).ToString("yyyy-MM-ddTHH:mm:ssZ", System.Globalization.CultureInfo.InvariantCulture));
int n = 0; long draws = 0, verts = 0, indirect = 0, vat = 0;
var units = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
UnityEditor.EditorApplication.CallbackFunction tick = null;
tick = () =>
{
    draws += TW.Presentation.FrameBudget.DrawCalls;
    verts += TW.Presentation.FrameBudget.Vertices;
    indirect += TW.Presentation.FrameBudget.IndirectDraws;
    if (units != null) vat += units.VerticesThisFrame;
    if (++n < 60) return;
    UnityEditor.EditorApplication.update -= tick;
    UnityEditor.EditorPrefs.SetString(ns + "budget",
        "{\"frames\":" + n + ",\"draws\":" + (draws / (double)n).ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)
        + ",\"vertices\":" + (verts / n) + ",\"indirect\":" + (indirect / (double)n).ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)
        + ",\"vat_vertices\":" + (vat / n) + "}");
};
UnityEditor.EditorApplication.update += tick;
return "sampling 60 frames into EditorPrefs " + ns + "budget";
