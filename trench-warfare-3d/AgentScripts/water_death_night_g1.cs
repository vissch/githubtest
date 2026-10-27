// water_death_night_g1.cs - run with: Tools/tw run eval_file -- --file <abs path to this file>
//
// water-splash-night-read-g1. The night death staged by water_death_night.cs read as ONE pale blue crystal in the
// creek: t1.png had no crown, no wings, no foam skirt and no body, and t1-nosplash.png was empty water, so its
// 2990 px were that crystal. Three things were missing from the stage, and this script puts them back:
//   - the body. CombatFx.Deaths.cs runs TWO calls for a standing death in water, units.AddFallen(...) and then
//     WaterEntry.Throw(...). The old script ran only the throw, so nothing died in the shot.
//   - a man to read the splash against. Two live men stand in the frame (one wading, one on the bank), held by
//     host.TimeScale = 0 and Speed = 0. They are in a capture AND in its own nosplash reference, so they add no
//     diff pixels: they are the scale, and they make men_in_frame non-zero in every sidecar.
//   - three deaths, not one. Three points about 2.5 m apart along the ford, each with its own triple of renders.
//
// The reference frame, same trick as water-entry-splash-r5's probe: the water's foam is animated from _Time.y, so
// each moment gets its OWN reference - the identical frozen frame re-rendered with WaterRings' rings parked and
// FlipbookFx's card list emptied. The difference between a moment and its reference is the water entry and the
// body, and nothing else.
//
// Which names it writes: env TW_G1_PREFIX=before writes before-a/b/c (the unchanged build's record), anything
// else writes a1/a2/a3 (the run the probe's bar judges).
var dir = "C:/Users/PC/Documents/GitHub/githubtest-relay-work5/docs/evidence/water-splash-night-read-g1/";
System.IO.Directory.CreateDirectory(dir);
var log = new System.Text.StringBuilder();
void Say(string s)
{
    log.AppendLine(s);
    System.IO.File.WriteAllText(dir + "stage.log", log.ToString());
    UnityEngine.Debug.Log("[night-splash-g1] " + s);
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

// Three death points about 2.5 m apart along the ford water-entry-splash-r5 proved.
var pts = new[] { new UnityEngine.Vector2(15f, 130f), new UnityEngine.Vector2(17.5f, 131f), new UnityEngine.Vector2(13f, 129f) };
if (TW.Presentation.SceneHooks.IsWater == null) return "no SceneHooks.IsWater";
foreach (var p2 in pts)
    if (!TW.Presentation.SceneHooks.IsWater(p2.x, p2.y)) return "a death point is not water: " + p2.x + ", " + p2.y;

string prefix = System.Environment.GetEnvironmentVariable("TW_G1_PREFIX");
var names = prefix == "before"
    ? new[] { "before-a", "before-b", "before-c" }
    : new[] { "a1", "a2", "a3" };

float keepScale = UnityEngine.Time.timeScale;
int keepCapture = UnityEngine.Time.captureFramerate;
float keepSim = host.TimeScale, keepRain = atmo.Rain, keepPinned = TW.Presentation.Terrain.Atmosphere.PinnedClock;
bool keepNight = TW.Presentation.SceneMood.Night;
UnityEngine.Time.captureFramerate = 60;
UnityEngine.Time.timeScale = 1f;
TW.Presentation.Terrain.Atmosphere.PinnedClock = 2f;   // 2 AM: the night the critic judged
atmo.Rain = 0f;
TW.Presentation.SceneMood.Night = true;

void Freeze() { UnityEngine.Time.captureFramerate = 0; UnityEngine.Time.timeScale = 0f; }
void Thaw() { UnityEngine.Time.captureFramerate = 60; UnityEngine.Time.timeScale = 1f; }

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

int f0 = UnityEngine.Time.frameCount;
Say("figureScale=" + scale.ToString("0.000") + " night=true pinned=2 prefix=" + (prefix ?? "(unset -> a)")
    + "; splash card height per metre of width = " + books.CardHeight(TW.Presentation.Tactical.FlipbookFx.Book.Splash, 1f).ToString("0.000"));

// The men a player reads the splash against: one wading the ford, one on the bank. Spawned with the sim running
// for a few frames so they exist and are placed, then held still for the rest of the run.
AtFrame(f0 + 1, () =>
{
    var said = new System.Text.StringBuilder("men:");
    foreach (var m in new[] { new UnityEngine.Vector3(16.4f, 130.6f, 0f), new UnityEngine.Vector3(12.5f, 128f, 0f) })
    {
        string s = TW.Editor.TankCapture.Spawn(0, 0, m.x, m.y, m.z);
        said.Append(' ').Append(s);
        if (!s.StartsWith("slot ")) continue;
        int slot = int.Parse(s.Substring(5));
        host.WriteWorlds(w => { w.World.Speed[slot] = 0f; });
    }
    return said.ToString();
});

string ShotAt(string name, float x, float z, float waterY) =>
    TW.Editor.CaptureRig.Shot(dir + name + ".png", x, z, 30f, 21f, 25f, 1600, 900, waterY);

// Each death: freeze, kill him (body + throw, the two production calls), shoot, shoot the same frozen frame again
// (the noise floor), lift the splash out and shoot the reference, put it back, thaw.
// 60 frames of warm-up before the first death: the spawned men fade in on unscaled time, so a death staged a
// few frames after the spawn gets a first capture that differs from its OWN second render across the whole
// window (before-a of the first before run: 8924 noise px against 7094 changed). The men must be settled first.
int b = f0 + 60;
for (int i = 0; i < 3; i++)
{
    int k = i;
    float px = pts[k].x, pz = pts[k].y;
    float waterY = TW.Presentation.SceneHooks.WaterY(px, pz);
    var deathPos = new UnityEngine.Vector3(px, waterY - 0.3f, pz);   // knee-deep: the bed under the surface
    string name = names[k];

    AtFrame(b, () =>
    {
        host.TimeScale = 0f;                  // the men stand still from the first death on
        Freeze();
        cards.Clear();                        // only this death's cards are in this death's shot
        TW.Presentation.Tactical.WaterEntry.Clear();
        TW.Editor.CaptureRig.Mark(px, pz);
        units.AddFallen(new UnityEngine.Vector3(deathPos.x, deathPos.y - 0.02f, deathPos.z), 90f, 0, k);
        var sp = TW.Presentation.Tactical.WaterEntry.Throw(books, deathPos, TW.Presentation.Tactical.WaterEntry.ManKg, scale: scale, night: true);
        return name + " death at " + deathPos + " waterY=" + waterY.ToString("0.000")
            + "; splash any=" + sp.Any + " ring=" + sp.Ring.ToString("0.000") + " card=" + sp.Card.ToString("0.000") + " cardY=" + sp.Y.ToString("0.000");
    });
    AtFrame(b + 6, () => name + " " + ShotAt(name, px, pz, waterY));
    AtFrame(b + 12, () => name + "-b " + ShotAt(name + "-b", px, pz, waterY));
    AtFrame(b + 18, () => { SplashOff(); return name + "-nosplash " + ShotAt(name + "-nosplash", px, pz, waterY); });
    AtFrame(b + 24, () => { SplashOn(); Thaw(); return name + " done, splash back"; });
    b += 30;
}

AtFrame(b, () =>
{
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
