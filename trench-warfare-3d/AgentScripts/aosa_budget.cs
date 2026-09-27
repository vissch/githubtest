// AOSA: read the whole-frame submission budget at the current view (docs/reference/aosa/README.md rule 7).
// Statement script for `unity command eval_file <abs path>`. FrameBudget reports the last COMPLETE frame, so this
// averages over 60 frames through an EditorApplication.update delegate that removes ITSELF (never set update = null:
// that kills every session's eval bridge), and leaves the result in EditorPrefs "aosa.budget". Run it, wait ~2 s,
// then: unity command eval "return UnityEditor.EditorPrefs.GetString(\"aosa.budget\", \"\");"
if (!UnityEngine.Application.isPlaying) return "not in Play";
UnityEditor.EditorPrefs.SetString("aosa.budget", "running");
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
    UnityEditor.EditorPrefs.SetString("aosa.budget",
        "{\"frames\":" + n + ",\"draws\":" + (draws / (double)n).ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)
        + ",\"vertices\":" + (verts / n) + ",\"indirect\":" + (indirect / (double)n).ToString("0.0", System.Globalization.CultureInfo.InvariantCulture)
        + ",\"vat_vertices\":" + (vat / n) + "}");
};
UnityEditor.EditorApplication.update += tick;
return "sampling 60 frames into EditorPrefs aosa.budget";
