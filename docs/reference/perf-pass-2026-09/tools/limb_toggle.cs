// Sets _TW_LIMBCUT on the VAT materials in Play: EditorPrefs "tw.limb.living" / "tw.limb.fallen" = 1 on, 0 off.
// Living on = the old shader's behaviour (clip in every pass); the game's own state is living off, fallen on.
bool living = UnityEditor.EditorPrefs.GetInt("tw.limb.living", 0) == 1, fallenOn = UnityEditor.EditorPrefs.GetInt("tw.limb.fallen", 1) == 1;
var vr = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
if (vr == null) return "no VATRenderer";
var bf = System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance;
var t = typeof(TW.Presentation.Units.VATRenderer);
var figs = new System.Collections.Generic.List<object>();
foreach (var f in (System.Array)t.GetField("figures", bf).GetValue(vr)) figs.Add(f);
var far = t.GetField("far", bf).GetValue(vr); if (far != null) figs.Add(far);
int n = 0; var sb = new System.Text.StringBuilder();
foreach (var f in figs)
{
    var ft = f.GetType();
    var m = (UnityEngine.Material)ft.GetField("Material").GetValue(f);
    var fm = (UnityEngine.Material)ft.GetField("Fallen").GetValue(f);
    if (living) m.EnableKeyword("_TW_LIMBCUT"); else m.DisableKeyword("_TW_LIMBCUT");
    if (fallenOn) fm.EnableKeyword("_TW_LIMBCUT"); else fm.DisableKeyword("_TW_LIMBCUT");
    n++;
}
return n + " figures: living " + (living ? "on" : "off") + ", fallen " + (fallenOn ? "on" : "off");
