// water_death_night_g2.cs - run with: Tools/tw run eval_file -- --file <abs path to this file>
//
// g2 fix: the g1 probe's bar cleared any pixel of anything (noise_px is 0 by construction between two renders of
// one frozen frame, so 6x0 is 0; o120's bar was > 0). This stages THREE independent entries - live time runs
// between them so each lands in a different phase of the water's own animation (TWWater.hlsl drives foam and
// lapping from _Time.y) - so the new probe can measure the water's own drift from three nosplash-vs-nosplash
// pairs and set a bar above it, not above zero.
var dir = "C:/Users/PC/Documents/GitHub/githubtest-relay-work2/docs/evidence/water-splash-night-read-g2/";
System.IO.Directory.CreateDirectory(dir);
var log = new System.Text.StringBuilder();
void Say(string s)
{
    log.AppendLine(s);
    System.IO.File.WriteAllText(dir + "stage.log", log.ToString());
    UnityEngine.Debug.Log("[night-splash-g2] " + s);
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
void Live() { UnityEngine.Time.captureFramerate = 60; UnityEngine.Time.timeScale = 1f; }

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

// --- entry 1: frozen immediately (phase 0), plus the one identical-repeat pair kept for the record only. ---
AtFrame(f0 + 2, () =>
{
    Freeze();
    var sp = TW.Presentation.Tactical.WaterEntry.Throw(books, deathPos, TW.Presentation.Tactical.WaterEntry.ManKg, scale: scale, night: true);
    return "entry 1 thrown at frame " + UnityEngine.Time.frameCount + "; splash any=" + sp.Any
        + " ring=" + sp.Ring.ToString("0.000") + " card=" + sp.Card.ToString("0.000") + " cardY=" + sp.Y.ToString("0.000");
});
AtFrame(f0 + 8, () => "t1-1 " + ShotAt("t1-1", 30f));
AtFrame(f0 + 14, () => "t1-1-b " + ShotAt("t1-1-b", 30f));
AtFrame(f0 + 20, () => { SplashOff(); return "t1-1-nosplash (rings and cards lifted out) " + ShotAt("t1-1-nosplash", 30f); });
AtFrame(f0 + 26, () => { SplashOn(); return "splash back"; });
AtFrame(f0 + 32, () => "o120-1 " + ShotAt("o120-1", 120f));
AtFrame(f0 + 38, () => { SplashOff(); return "o120-1-nosplash (rings and cards lifted out) " + ShotAt("o120-1-nosplash", 120f); });
AtFrame(f0 + 44, () => { SplashOn(); TW.Presentation.Tactical.WaterEntry.Clear(); return "entry 1 cleared"; });

// --- let live time run so the water's own animation moves on before entry 2 (a different phase). ---
AtFrame(f0 + 45, () => { Live(); return "live time running for entry 2"; });
AtFrame(f0 + 75, () =>
{
    Freeze();
    var sp = TW.Presentation.Tactical.WaterEntry.Throw(books, deathPos, TW.Presentation.Tactical.WaterEntry.ManKg, scale: scale, night: true);
    return "entry 2 thrown at frame " + UnityEngine.Time.frameCount + "; splash any=" + sp.Any
        + " ring=" + sp.Ring.ToString("0.000") + " card=" + sp.Card.ToString("0.000") + " cardY=" + sp.Y.ToString("0.000");
});
AtFrame(f0 + 81, () => "t1-2 " + ShotAt("t1-2", 30f));
AtFrame(f0 + 87, () => { SplashOff(); return "t1-2-nosplash (rings and cards lifted out) " + ShotAt("t1-2-nosplash", 30f); });
AtFrame(f0 + 93, () => { SplashOn(); return "splash back"; });
AtFrame(f0 + 99, () => "o120-2 " + ShotAt("o120-2", 120f));
AtFrame(f0 + 105, () => { SplashOff(); return "o120-2-nosplash (rings and cards lifted out) " + ShotAt("o120-2-nosplash", 120f); });
AtFrame(f0 + 111, () => { SplashOn(); TW.Presentation.Tactical.WaterEntry.Clear(); return "entry 2 cleared"; });

// --- entry 3, another live gap, another phase. ---
AtFrame(f0 + 112, () => { Live(); return "live time running for entry 3"; });
AtFrame(f0 + 142, () =>
{
    Freeze();
    var sp = TW.Presentation.Tactical.WaterEntry.Throw(books, deathPos, TW.Presentation.Tactical.WaterEntry.ManKg, scale: scale, night: true);
    return "entry 3 thrown at frame " + UnityEngine.Time.frameCount + "; splash any=" + sp.Any
        + " ring=" + sp.Ring.ToString("0.000") + " card=" + sp.Card.ToString("0.000") + " cardY=" + sp.Y.ToString("0.000");
});
AtFrame(f0 + 148, () => "t1-3 " + ShotAt("t1-3", 30f));
AtFrame(f0 + 154, () => { SplashOff(); return "t1-3-nosplash (rings and cards lifted out) " + ShotAt("t1-3-nosplash", 30f); });
AtFrame(f0 + 160, () => { SplashOn(); return "splash back"; });
AtFrame(f0 + 166, () => "o120-3 " + ShotAt("o120-3", 120f));
AtFrame(f0 + 172, () => { SplashOff(); return "o120-3-nosplash (rings and cards lifted out) " + ShotAt("o120-3-nosplash", 120f); });
AtFrame(f0 + 178, () =>
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
