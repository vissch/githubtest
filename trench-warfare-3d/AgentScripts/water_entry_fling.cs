// water_entry_fling.cs - run with: Tools/tw run eval_file -- --file <abs path to this file>
//
// Stages the one thing no unit test can stand in for: a body flung into the GreyboxCorridor ford with the clock
// RUNNING, photographed before it lands and just after. The claim under test is r2's "the ring waits out the fall":
// WaterEntry.Throw queues the ring with the flight's own duration and WaterEntry.Pump opens it at the landing, so a
// frame taken mid-flight must show water with no ring on it and a frame taken just after the landing must show one.
// r2 proved that with Pump(0.4f)/Pump(0.45f) arithmetic only; this proves it in pixels.
//
// --- the reference frame, and why it is not a plate taken earlier ---------------------------------------------
// The first run of this script shot a no-throw plate and compared every later still against it. That does not work,
// and the failure is worth writing down: the water's foam and lapping are animated from `_Time.y` in TWWater.hlsl,
// so a still taken a second later differs from the plate over the WHOLE water sheet - about 1,600 changed pixels in
// the 3 m disc at the landing point, whether a body landed in it or not. Nothing about a splash is measurable that
// way (the first run: ring_px_pre 1577, ring_px_land 1703 - the instrument could not tell them apart).
//
// So each moment gets its own reference, taken at the SAME frozen instant with the water entry turned off. Both
// halves of it can be taken away without touching anything else: WaterRings keeps its sixteen rings in one array it
// pushes to the shader global `_TWRings` every frame, and FlipbookFx keeps its live cards in one list. Emptying both
// and re-rendering the frozen frame gives the identical picture - same foam, same body, same shadows, same sky -
// minus the splash alone. The difference between the two is therefore the water entry and nothing else, which is
// exactly what the claim is about. A plain no-throw plate (plate.png) is shot as well, for the record.
//
// Taking the rings away on their own was tried first and measured nothing: a man's ring is 1.8 m wide and opens from
// a point, so 0.03 s after the landing it is a 0.3 m circle two pixels thick on dark night water, under the render's
// own noise at the standard view (its peak luma change was 0.014 against a 0.011 noise floor). What a man's water
// entry actually shows at T1 is the Splash card standing on the surface; the ring is the fine detail under it. Both
// are turned off together, so what is measured is the splash a viewer would see.
//
// Why it is built the way it is:
//  - CaptureRig.Hold() is NOT used. It sets Time.timeScale = 0, which would stop the fall this capture is about.
//    The sky is pinned by hand instead (Atmosphere.PinnedClock = 120), rain is turned off (its pixels are noise),
//    and the SIM is frozen (SimHost.TimeScale = 0) so no man takes a step and no wading man rings the ford. The
//    PRESENTATION clock keeps running, which is what carries the body through the air.
//  - Time.captureFramerate = 60 makes every frame exactly 1/60 s of game time however slow the editor is, so the
//    moments below are frame-exact and two runs cannot drift.
//  - Everything is scheduled by Time.frameCount, not by seconds: each moment is shot with Time.timeScale = 0, and
//    seconds do not advance then, so a seconds-based timer (DeathLab.Later) would never fire again.
//  - CaptureRig.Shot poses on the frame it is queued and renders on the NEXT one.
//  - land.png and land-b.png are two renders of the identical frozen world: their difference is the instrument's own
//    noise floor, which is what probe.py measures ring_px_land against.
//  - The sim is frozen and no man is within 6 m of the ford, so the flipbook list holds this splash and nothing else
//    at the moments shot here.
//
// The throw itself is the two production lines CombatFx.Deaths.cs runs for a flung body - VATRenderer.AddFallen with
// the throw, and WaterEntry.Throw with FallenFlight.Solve's duration as the delay - and nothing else, so what is
// photographed is the shipped path.
var dir = "C:/Users/PC/Documents/GitHub/githubtest-relay-work2/docs/evidence/water-entry-splash-r5/";
System.IO.Directory.CreateDirectory(dir);
var log = new System.Text.StringBuilder();
void Say(string s)
{
    log.AppendLine(s);
    System.IO.File.WriteAllText(dir + "stage.log", log.ToString());
    UnityEngine.Debug.Log("[fling] " + s);
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

// WaterRings.Awake parks an unused slot at (0, 0, -1000, 0): born a thousand seconds ago, so the shader draws
// nothing for it. Parking every slot is "no rings at all" said in the renderer's own language. The flipbook cards
// are simply lifted out of the list and put back in the same order; time is frozen while they are out, so no card
// ages, expires or is added in between.
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

// The ford, read off the map (never hard-coded water): the bank at (12, 128) stands about 0.97 m above a knee-deep
// bed at (15, 130) with its surface at 0.15. A dry launch and a wet landing 3.6 m apart.
var from = new UnityEngine.Vector3(12f, TW.Presentation.RenderGround.Sample(host.Local.Map, 12f, 128f), 128f);
var fly = new UnityEngine.Vector3(3f, 0.5f, 2f);                 // fly.y 0.5 puts the flight at about 0.72 s: long
var land = from + new UnityEngine.Vector3(fly.x, 0f, fly.z);     // enough that a frame 0.2 s before the landing is
land.y = TW.Presentation.RenderGround.Sample(host.Local.Map, land.x, land.z);   // still well after the throw
if (TW.Presentation.SceneHooks.IsWater == null || !TW.Presentation.SceneHooks.IsWater(land.x, land.z))
    return "the landing point is not water: " + land;
if (TW.Presentation.SceneHooks.IsWater(from.x, from.z)) return "the launch point is not dry: " + from;
float waterY = TW.Presentation.SceneHooks.WaterY(land.x, land.z);
float delay = TW.Presentation.Units.FallenFlight.Solve(from, land, fly.y, 0f, units.ThrowGravity).Dur;

// ---- the staging -----------------------------------------------------------------------------------------------
float keepScale = UnityEngine.Time.timeScale;
int keepCapture = UnityEngine.Time.captureFramerate;
float keepSim = host.TimeScale, keepRain = atmo.Rain, keepPinned = TW.Presentation.Terrain.Atmosphere.PinnedClock;
UnityEngine.Time.captureFramerate = 60;
UnityEngine.Time.timeScale = 1f;
TW.Presentation.Terrain.Atmosphere.PinnedClock = 120f;
atmo.Rain = 0f;
host.TimeScale = 0f;                       // the men stand still, so no wading man rings the ford either
TW.Editor.CaptureRig.Mark(land.x, land.z); // every sidecar now carries this point and its projection

void Freeze() { UnityEngine.Time.captureFramerate = 0; UnityEngine.Time.timeScale = 0f; }
void Thaw() { UnityEngine.Time.timeScale = 1f; UnityEngine.Time.captureFramerate = 60; }

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

string Shoot(string name) =>
    TW.Editor.CaptureRig.Shot(dir + name + ".png", land.x, land.z, 30f, 21f, 25f, 1600, 900, waterY);

// One moment: freeze, shoot it, shoot it again with the rings off, put the rings back, let go. `extra` is a second
// render of the untouched frozen frame (the noise floor) and is only asked for at the landing.
void Moment(int at, string name, bool extra, System.Action<int> next)
{
    AtFrame(at, () => { Freeze(); return name + ": frozen at frame " + UnityEngine.Time.frameCount + ", t=" + UnityEngine.Time.time.ToString("0.0000"); });
    AtFrame(at + 2, () => name + " " + Shoot(name));
    if (extra) AtFrame(at + 8, () => name + "-b " + Shoot(name + "-b"));
    int off = extra ? 14 : 8;
    AtFrame(at + off, () => { SplashOff(); return name + "-nosplash (rings and cards lifted out) " + Shoot(name + "-nosplash"); });
    AtFrame(at + off + 6, () =>
    {
        SplashOn(); Thaw();
        next(UnityEngine.Time.frameCount);
        return name + " done, splash back, running again at frame " + UnityEngine.Time.frameCount
            + ", pending=" + TW.Editor.CaptureRig.Pending();
    });
}

int f0 = UnityEngine.Time.frameCount;
Say("ford: from=" + from + " land=" + land + " waterY=" + waterY.ToString("0.000") + " fly=" + fly
    + " flight=" + delay.ToString("0.000") + " s (" + UnityEngine.Mathf.RoundToInt(delay * 60f) + " frames at 1/60)"
    + " figureScale=" + scale.ToString("0.000"));
Say("plate (no throw at all) queued at frame " + f0 + ": " + Shoot("plate"));

AtFrame(f0 + 10, () =>
{
    int t = UnityEngine.Time.frameCount;
    int n = UnityEngine.Mathf.RoundToInt(delay * 60f);
    // --- the two production lines, exactly as CombatFx.Deaths.cs runs them for a flung body ---
    units.AddFallen(new UnityEngine.Vector3(from.x, from.y - 0.02f, from.z), 0f, 0, 0, fly: fly);
    var sp = TW.Presentation.Tactical.WaterEntry.Throw(books, land, TW.Presentation.Tactical.WaterEntry.ManKg, delay, scale);

    // land - 0.2 s. From here on the frame numbers are counted forward from each thaw, because freezing stops the
    // seconds but not the frames: 12 frames of game time is left to the landing, and 3 more to land + 0.05 s.
    Moment(t + n - 13, "pre", false, afterPre =>
        Moment(afterPre + 15, "land", true, afterLand =>
            Moment(afterLand + 15, "post", false, afterPost =>
                AtFrame(afterPost + 10, () =>
                {
                    UnityEngine.Time.timeScale = keepScale;
                    UnityEngine.Time.captureFramerate = keepCapture;
                    TW.Presentation.Terrain.Atmosphere.PinnedClock = keepPinned;
                    atmo.Rain = keepRain;
                    host.TimeScale = keepSim;
                    TW.Editor.CaptureRig.Unmark();
                    TW.Presentation.Tactical.WaterEntry.Clear();
                    return "DONE, scene put back; pending=" + TW.Editor.CaptureRig.Pending();
                }))));

    return "thrown at frame " + t + ", lands at frame " + (t + n) + "; splash any=" + sp.Any
        + " ring=" + sp.Ring.ToString("0.000") + " card=" + sp.Card.ToString("0.000") + " cardY=" + sp.Y.ToString("0.000");
});
return "staged; flight " + delay.ToString("0.000") + " s; watch " + dir + "stage.log";
