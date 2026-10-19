// water_death_night.cs - run with: Tools/tw run eval_file -- --file <abs path to this file>
//
// Harsh critic r1 fix 1 (2026-10-19, 38/100): at night, six staged 80 kg entries into the GreyboxCorridor ford
// read as a few faint blue streaks - 292 px against 118 px of noise (2.5x) - not a man dying knee-deep. This
// stages ONE man dying standing, knee-deep, in the ford, at night, and photographs the real splash at T1 (zoom
// 30) and O120 (zoom 120). The throw is the production line CombatFx.Deaths.cs runs for a standing death in
// water (fly.y <= 0.05f: WaterEntry.Throw(books, p, ManKg, scale: FigureScale(), night: SceneMood.Night)) and
// nothing else, so what is photographed is the shipped path, not a rig built to look good.
//
// --- the reference frame, same trick as water-entry-splash-r5's probe -----------------------------------------
// The water's foam and lapping are animated from _Time.y in TWWater.hlsl, so a still taken a moment later differs
// from an earlier one across the whole water sheet even with nothing landed in it. Each moment gets its OWN
// reference instead: the identical frozen frame re-rendered with WaterRings' rings parked and FlipbookFx's card
// list emptied, and nothing else touched. The difference between a moment and its own reference is the water
// entry and nothing else.
var dir = "C:/Users/PC/Documents/GitHub/githubtest-relay-work5/docs/evidence/water-splash-night-read/";
System.IO.Directory.CreateDirectory(dir);
var log = new System.Text.StringBuilder();
void Say(string s)
{
    log.AppendLine(s);
    System.IO.File.WriteAllText(dir + "stage.log", log.ToString());
    UnityEngine.Debug.Log("[night-splash] " + s);
}

var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var units = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
var fx = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
var atmo = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
var waterRings = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Terrain.WaterRings>();
if (host == null || units == null || fx == null || atmo == null || waterRings == null)
    return "not in Play in GreyboxCorridor";

var bookField = typeof(TW.Presentation.Tactical.CombatFx).GetField("books",
    System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
var books = bookField.GetValue(fx) as TW.Presentation.Tactical.FlipbookFx;
var figureScale = typeof(TW.Presentation.Tactical.CombatFx).GetMethod("FigureScale",
    System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
float scale = (float)figureScale.Invoke(fx, null);
var ringField = typeof(TW.Presentation.Terrain.WaterRings).GetField("rings",
    System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
var rings = (UnityEngine.Vector4[])ringField.GetValue(waterRings);
var ringsKeep = new UnityEngine.Vector4[rings.Length];
var cardField = typeof(TW.Presentation.Tactical.FlipbookFx).GetField("cards",
    System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
var cards = (System.Collections.IList)cardField.GetValue(books);
var cardsKeep = new System.Collections.Generic.List<object>();

void SplashOff()
{
    for (int i = 0; i < rings.Length; i++) { ringsKeep[i] = rings[i]; rings[i] = new UnityEngine.Vector4(0f, 0f, -1000f, 0f); }
    cardsKeep.Clear();
    foreach (var c in cards) cardsKeep.Add(c);
    cards.Clear();
}
void SplashOn()
{
    for (int i = 0; i < rings.Length; i++) rings[i] = ringsKeep[i];
    cards.Clear();
    foreach (var c in cardsKeep) cards.Add(c);
}

// The ford's knee-deep water, same point water-entry-splash-r5 proved: surface about 0.15, bed just under it.
float px = 15f, pz = 130f;
if (TW.Presentation.SceneHooks.IsWater == null || !TW.Presentation.SceneHooks.IsWater(px, pz))
    return "the death point is not water: " + px + ", " + pz;
float waterY = TW.Presentation.SceneHooks.WaterY(px, pz);
var deathPos = new UnityEngine.Vector3(px, waterY - 0.3f, pz);   // knee-deep: the bed under the surface

float keepScale = UnityEngine.Time.timeScale;
int keepCapture = UnityEngine.Time.captureFramerate;
float keepSim = host.TimeScale, keepRain = atmo.Rain, keepPinned = TW.Presentation.Terrain.Atmosphere.PinnedClock;
bool keepNight = TW.Presentation.SceneMood.Night;
UnityEngine.Time.captureFramerate = 60;
UnityEngine.Time.timeScale = 1f;
TW.Presentation.Terrain.Atmosphere.PinnedClock = 2f;   // 2 AM: the night the critic judged
atmo.Rain = 0f;
host.TimeScale = 0f;
TW.Presentation.SceneMood.Night = true;
TW.Editor.CaptureRig.Mark(px, pz);

void Freeze() { UnityEngine.Time.captureFramerate = 0; UnityEngine.Time.timeScale = 0f; }

void AtFrame(int target, System.Func<string> act)
{
    void Tick()
    {
        if (!UnityEngine.Application.isPlaying) { UnityEditor.EditorApplication.update -= Tick; return; }
        if (UnityEngine.Time.frameCount < target) return;
        UnityEditor.EditorApplication.update -= Tick;
        Say(act());
    }
    UnityEditor.EditorApplication.update += Tick;
}

string ShotAt(string name, float zoom) =>
    TW.Editor.CaptureRig.Shot(dir + name + ".png", px, pz, zoom, 21f, 25f, 1600, 900, waterY);

int f0 = UnityEngine.Time.frameCount;
Say("ford death point: " + deathPos + " waterY=" + waterY.ToString("0.000") + " figureScale=" + scale.ToString("0.000") + " night=true");

AtFrame(f0 + 2, () =>
{
    Freeze();
    var sp = TW.Presentation.Tactical.WaterEntry.Throw(books, deathPos, TW.Presentation.Tactical.WaterEntry.ManKg, scale: scale, night: true);
    return "thrown at frame " + UnityEngine.Time.frameCount + "; splash any=" + sp.Any
        + " ring=" + sp.Ring.ToString("0.000") + " card=" + sp.Card.ToString("0.000") + " cardY=" + sp.Y.ToString("0.000");
});

// T1 (zoom 30): the standing shot, a second render of the same frozen frame (the noise floor), and the nosplash reference.
AtFrame(f0 + 8, () => "t1 " + ShotAt("t1", 30f));
AtFrame(f0 + 14, () => "t1-b " + ShotAt("t1-b", 30f));
AtFrame(f0 + 20, () => { SplashOff(); return "t1-nosplash (rings and cards lifted out) " + ShotAt("t1-nosplash", 30f); });
AtFrame(f0 + 26, () => { SplashOn(); return "splash back"; });

// O120 (zoom 120): the far band, and its own nosplash reference.
AtFrame(f0 + 32, () => "o120 " + ShotAt("o120", 120f));
AtFrame(f0 + 38, () => { SplashOff(); return "o120-nosplash (rings and cards lifted out) " + ShotAt("o120-nosplash", 120f); });
AtFrame(f0 + 44, () =>
{
    SplashOn();
    UnityEngine.Time.timeScale = keepScale;
    UnityEngine.Time.captureFramerate = keepCapture;
    TW.Presentation.Terrain.Atmosphere.PinnedClock = keepPinned;
    atmo.Rain = keepRain;
    host.TimeScale = keepSim;
    TW.Presentation.SceneMood.Night = keepNight;
    TW.Editor.CaptureRig.Unmark();
    TW.Presentation.Tactical.WaterEntry.Clear();
    return "DONE, scene put back; pending=" + TW.Editor.CaptureRig.Pending();
});

return "staged; watch " + dir + "stage.log";
