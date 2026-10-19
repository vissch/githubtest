// water_entry_fling.cs - run with: Tools/tw run eval_file -- --file <abs path to this file>
//
// Stages the one thing no unit test can stand in for: a body flung into the GreyboxCorridor ford with the clock
// RUNNING, photographed before it lands, just after, and a quarter second later. r6 answers harsh critic r1 on r5
// (60/100) on three counts, and nothing else here changed:
//
//  1. Exact timings. r5 threw with FallenFlight.Solve's own 0.721 s duration, which is 43.26 frames at 1/60, and
//     then counted frames forward from each thaw, so every shot missed its spec moment by the rounding
//     (+0.033 / +0.283 / -0.217 instead of +0.05 / +0.3 / -0.2). Here fly.y is BISECTED until the flight is 43
//     frames to within 1e-4 of a frame, so the body and the ring queued with that same duration both land exactly
//     on frame throw+43, and the three freezes are scheduled on GAME TIME from the landing instant
//     (tLand = Time.time + Dur, remembered at the throw) rather than on frame counts. Freezing stops the seconds
//     but not the frames, so the clock reads the same after a freeze as before it and every thawed frame is
//     exactly 1/60 s: the first frame at or past tLand + off is the spec moment. The achieved deltas are written
//     to timings.txt.
//  2. The noise floor comes from a SECOND LIVE THROW, not from re-rendering a frozen frame. r5's noise_px 0 was
//     true but worthless: two renders of one frozen world differ by nothing, so "ring_px_land >= 3x noise" passed
//     by construction. Here the scene is put back to before the throw (cards cleared, every ring slot parked, the
//     pending rings dropped, the fallen bodies emptied), UnityEngine.Random is re-seeded with the SAME seed used
//     before run 1, and the identical throw is made again; land-run2.png is its landing frame. The water itself is
//     pinned for both runs (WaterRings.PinnedClock = 120), so the difference between land.png and land-run2.png is
//     the splash's own repeatability and nothing else.
//  3. land-noring.png: the frozen landing frame with the ring slots parked but the CARDS kept, so the probe can
//     report ring_only_px. The critic's "the ring does not register" is answered with that number, not with prose.
//
// r7 changes two things and nothing else. Harsh critic r1 scored r6 56/100: ring_only_px was 0, so the ring still
// did not register, and the crown's value step over the water was never measured at all.
//  a. post-noring.png: the same rings-parked, cards-kept shot taken at land + 0.3 s as well. The crown card is
//     2.7 m wide and the landing ring only 1.9 m across, so at land + 0.05 s the card can cover the ring it is
//     meant to prove; at + 0.3 s the ring is the wider of the two. With both moments shot, ring_only_px is the MIN
//     of them, and a ring that only shows on one lucky frame cannot pass.
//  b. Every run writes its own docs/evidence/water-entry-splash-r7/run-N/ (N = the first free one), so the three
//     baseline runs and the three runs of the fixed build never overwrite each other and the probe can report the
//     spread over a set instead of one capture's word.
// Everything else stands: the bisected 43-frame flight, the shots on the spec moments, the pinned sky and water
// clocks, rain 0, the frozen sim and the noise floor from a second live throw.
//
// --- the reference frame, and why it is not a plate taken earlier ---------------------------------------------
// The first run of r5 shot a no-throw plate and compared every later still against it. That does not work, and the
// failure is worth keeping written down: the water's foam and lapping are animated from the water clock in
// TWWater.hlsl, so a still taken a second later differs from the plate over the WHOLE water sheet - about 1,600
// changed pixels in the 3 m disc at the landing point, whether a body landed in it or not.
//
// So each moment gets its own reference, taken at the SAME frozen instant with the water entry turned off. Both
// halves of it can be taken away without touching anything else: WaterRings keeps its sixteen rings in one array it
// pushes to the shader global every frame, and FlipbookFx keeps its live cards in one list. Emptying both and
// re-rendering the frozen frame gives the identical picture - same foam, same body, same shadows, same sky - minus
// the splash alone. A plain no-throw plate (plate.png) is shot as well, for the record.
//
// Why it is built the way it is:
//  - CaptureRig.Hold() is NOT used. It sets Time.timeScale = 0, which would stop the fall this capture is about.
//    The sky is pinned by hand (Atmosphere.PinnedClock = 120), the water clock too (WaterRings.PinnedClock = 120),
//    rain is turned off (its pixels are noise), and the SIM is frozen (SimHost.TimeScale = 0) so no man takes a
//    step and no wading man rings the ford. The PRESENTATION clock keeps running, which carries the body.
//  - Time.captureFramerate = 60 makes every frame exactly 1/60 s of game time however slow the editor is.
//  - Inside a moment the extra shots are ordered by frames, not seconds: seconds do not advance while frozen.
//  - CaptureRig.Shot poses on the frame it is queued and renders on the NEXT one.
//
// The throw itself is the two production lines CombatFx.Deaths.cs runs for a flung body - VATRenderer.AddFallen
// with the throw, and WaterEntry.Throw with FallenFlight.Solve's duration as the delay - and nothing else, so what
// is photographed is the shipped path.
// The first free run-N, so a capture never overwrites an earlier one: three runs prove the current build and three
// the fixed one, and the probe reads a set of run folders by name.
var root = "C:/Users/PC/Documents/GitHub/githubtest-relay-work3/docs/evidence/water-entry-splash-r7/";
int runNo = 1;
while (System.IO.Directory.Exists(root + "run-" + runNo)) runNo++;
var dir = root + "run-" + runNo + "/";
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
var fallenField = typeof(TW.Presentation.Units.VATRenderer).GetField("fallenMen",
    System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
var fallenMen = (System.Collections.IList)fallenField.GetValue(units);

// WaterRings.Awake parks an unused slot at (0, 0, -1000, 0): born a thousand seconds ago, so the shader draws
// nothing for it. Parking every slot is "no rings at all" said in the renderer's own language. The flipbook cards
// are lifted out of the list and put back in the same order; time is frozen while they are out, so no card ages,
// expires or is added in between.
void RingsOff()
{
    for (int i = 0; i < rings.Length; i++) { ringsKeep[i] = rings[i]; rings[i] = new UnityEngine.Vector4(0f, 0f, -1000f, 0f); }
}
void RingsOn() { for (int i = 0; i < rings.Length; i++) rings[i] = ringsKeep[i]; }
void CardsOff() { cardsKeep.Clear(); foreach (var c in cards) cardsKeep.Add(c); cards.Clear(); }
void CardsOn() { cards.Clear(); foreach (var c in cardsKeep) cards.Add(c); }

// The ford, read off the map (never hard-coded water): the bank at (12, 128) stands about 0.97 m above a knee-deep
// bed at (15, 130) with its surface at 0.15. A dry launch and a wet landing 3.6 m apart.
var from = new UnityEngine.Vector3(12f, TW.Presentation.RenderGround.Sample(host.Local.Map, 12f, 128f), 128f);
var land = from + new UnityEngine.Vector3(3f, 0f, 2f);
land.y = TW.Presentation.RenderGround.Sample(host.Local.Map, land.x, land.z);
if (TW.Presentation.SceneHooks.IsWater == null || !TW.Presentation.SceneHooks.IsWater(land.x, land.z))
    return "the landing point is not water: " + land;
if (TW.Presentation.SceneHooks.IsWater(from.x, from.z)) return "the launch point is not dry: " + from;
float waterY = TW.Presentation.SceneHooks.WaterY(land.x, land.z);

// --- fly.y, bisected so the flight is a whole number of frames ------------------------------------------------
// Solve's duration grows with the throw's height, so 0.3..0.8 brackets 43 frames (r5's fly.y 0.5 gave 43.26).
// Sixty halvings take the bracket far below the 1e-4 of a frame the spec asks for.
float Frames(float y) => TW.Presentation.Units.FallenFlight.Solve(from, land, y, 0f, units.ThrowGravity).Dur * 60f;
float lo = 0.3f, hi = 0.8f;
if (Frames(lo) > 43f || Frames(hi) < 43f) return "43 frames is not bracketed: " + Frames(lo) + ".." + Frames(hi);
for (int i = 0; i < 60; i++) { float mid = 0.5f * (lo + hi); if (Frames(mid) < 43f) lo = mid; else hi = mid; }
float flyY = 0.5f * (lo + hi);
if (UnityEngine.Mathf.Abs(Frames(flyY) - 43f) > 1e-4f)
    return "fly.y did not converge: " + flyY + " -> " + Frames(flyY) + " frames";
var fly = new UnityEngine.Vector3(3f, flyY, 2f);
float delay = TW.Presentation.Units.FallenFlight.Solve(from, land, fly.y, 0f, units.ThrowGravity).Dur;
const int Seed = 20261019;

// ---- the staging -----------------------------------------------------------------------------------------------
float keepScale = UnityEngine.Time.timeScale;
int keepCapture = UnityEngine.Time.captureFramerate;
float keepSim = host.TimeScale, keepRain = atmo.Rain, keepPinned = TW.Presentation.Terrain.Atmosphere.PinnedClock;
float keepWaterClock = TW.Presentation.Terrain.WaterRings.PinnedClock;
UnityEngine.Time.captureFramerate = 60;
UnityEngine.Time.timeScale = 1f;
TW.Presentation.Terrain.Atmosphere.PinnedClock = 120f;
TW.Presentation.Terrain.WaterRings.PinnedClock = 120f;   // set once, left for BOTH runs: the same water in both
atmo.Rain = 0f;
host.TimeScale = 0f;                       // the men stand still, so no wading man rings the ford either
// The ford starts with a clean sheet. A man waded it in the seconds of Play before this script ran, and a wading
// ring lives about 1.4 s, so two or three of them were still open on the water at the first moment shot. They are
// not the water entry, but lifting the rings out for a reference lifted them too, and the first r6 run measured
// 791 changed pixels at land - 0.2 s because of it. Parking every slot once here, with the sim already frozen so
// no new one can be added, leaves the entry's own ring as the only ring in any frame of this capture.
for (int i = 0; i < rings.Length; i++) rings[i] = new UnityEngine.Vector4(0f, 0f, -1000f, 0f);
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

// The first frame whose game clock has reached `when`. Every thawed frame is exactly 1/60 s and freezing does not
// advance the clock, so this fires on the spec moment itself.
void AtTime(float when, System.Func<string> act)
{
    void Tick()
    {
        if (!UnityEngine.Application.isPlaying) { UnityEditor.EditorApplication.update -= Tick; return; }
        if (UnityEngine.Time.time < when - 1e-5f) return;
        UnityEditor.EditorApplication.update -= Tick;
        Say(act());
    }
    UnityEditor.EditorApplication.update += Tick;
}

string Shoot(string name) =>
    TW.Editor.CaptureRig.Shot(dir + name + ".png", land.x, land.z, 30f, 21f, 25f, 1600, 900, waterY);

var timings = new System.Text.StringBuilder();
timings.AppendLine("water-entry-splash-r7 - the game-time delta from the landing instant of every frozen moment, as");
timings.AppendLine("the harness measured it (AgentScripts/water_entry_fling.cs), never typed. Spec: -0.200/+0.050/+0.300.");
timings.AppendLine("flight_s " + delay.ToString("0.00000"));
timings.AppendLine("flight_frames " + (delay * 60f).ToString("0.00000"));
void Timing(string name, float tLand)
{
    timings.AppendLine(name + " " + (UnityEngine.Time.time - tLand).ToString("+0.000;-0.000"));
    System.IO.File.WriteAllText(dir + "timings.txt", timings.ToString());
}

// One moment: freeze on its own game-time instant, shoot it, shoot it again with the water entry lifted out, put
// the splash back, let go. `noring` adds this moment's diagnostic shot, rings parked and cards kept, which is what
// lets the probe say in pixels how much of the frame the ring alone accounts for at that moment.
void Shots(string name, float when, float tLand, bool noring, System.Action<int> next)
{
    AtTime(when, () =>
    {
        Freeze(); Timing(name, tLand);
        int at = UnityEngine.Time.frameCount;
        AtFrame(at + 2, () => name + " " + Shoot(name));
        int step = at + 8;
        if (noring)
        {
            AtFrame(step, () => { RingsOff(); return name + "-noring (ring slots parked, cards kept) " + Shoot(name + "-noring"); });
            step += 6;
            AtFrame(step, () => { CardsOff(); return name + "-nosplash (rings and cards out) " + Shoot(name + "-nosplash"); });
        }
        else
        {
            AtFrame(step, () => { RingsOff(); CardsOff(); return name + "-nosplash (rings and cards out) " + Shoot(name + "-nosplash"); });
        }
        AtFrame(step + 6, () =>
        {
            RingsOn(); CardsOn(); Thaw();
            next(UnityEngine.Time.frameCount);
            return name + " done, splash back, running again at frame " + UnityEngine.Time.frameCount
                + ", pending=" + TW.Editor.CaptureRig.Pending();
        });
        return name + ": frozen at frame " + at + ", t=" + UnityEngine.Time.time.ToString("0.0000")
            + ", delta=" + (UnityEngine.Time.time - tLand).ToString("+0.0000;-0.0000");
    });
}

float ThrowOnce()
{
    float tLand = UnityEngine.Time.time + delay;
    units.AddFallen(new UnityEngine.Vector3(from.x, from.y - 0.02f, from.z), 0f, 0, 0, fly: fly);
    var sp = TW.Presentation.Tactical.WaterEntry.Throw(books, land, TW.Presentation.Tactical.WaterEntry.ManKg, delay, scale);
    Say("thrown at frame " + UnityEngine.Time.frameCount + ", t=" + UnityEngine.Time.time.ToString("0.0000")
        + ", lands at t=" + tLand.ToString("0.0000") + " (frame " + (UnityEngine.Time.frameCount + 43) + "); any=" + sp.Any
        + " ring=" + sp.Ring.ToString("0.000") + " card=" + sp.Card.ToString("0.000") + " cardY=" + sp.Y.ToString("0.000"));
    return tLand;
}

void PutBack()
{
    UnityEngine.Time.timeScale = keepScale;
    UnityEngine.Time.captureFramerate = keepCapture;
    TW.Presentation.Terrain.Atmosphere.PinnedClock = keepPinned;
    TW.Presentation.Terrain.WaterRings.PinnedClock = keepWaterClock;
    atmo.Rain = keepRain;
    host.TimeScale = keepSim;
    TW.Editor.CaptureRig.Unmark();
    TW.Presentation.Tactical.WaterEntry.Clear();
}

int f0 = UnityEngine.Time.frameCount;
Say("ford: from=" + from + " land=" + land + " waterY=" + waterY.ToString("0.000") + " fly=" + fly
    + " flight=" + delay.ToString("0.00000") + " s = " + (delay * 60f).ToString("0.00000") + " frames at 1/60"
    + " figureScale=" + scale.ToString("0.000"));
Say("plate (no throw at all) queued at frame " + f0 + ": " + Shoot("plate"));

// The throw waits a second and a half after the staging. Thrown at once, in a Play session that has only just
// started, the first frames still carry the startup's own long delta times - Time.captureFramerate binds only the
// frames after it is set - and one such frame put the land - 0.2 s shot two frames late (-0.167 instead of
// -0.200). Ninety frames of settled 1/60 s clock first, and every moment lands on its spec instant.
AtFrame(f0 + 90, () =>
{
    UnityEngine.Random.InitState(Seed);
    float tLand = ThrowOnce();
    Shots("pre", tLand - 0.2f, tLand, false, _ =>
        Shots("land", tLand + 0.05f, tLand, true, _ =>
            Shots("post", tLand + 0.3f, tLand, true, afterPost =>
                // --- run 2: the same throw again, from a scene put back to before run 1 ---
                AtFrame(afterPost + 10, () =>
                {
                    cards.Clear();
                    for (int i = 0; i < rings.Length; i++) rings[i] = new UnityEngine.Vector4(0f, 0f, -1000f, 0f);
                    TW.Presentation.Tactical.WaterEntry.Clear();
                    fallenMen.Clear();
                    if (units.FallenCount != 0) { PutBack(); return "run 2 aborted: FallenCount " + units.FallenCount; }
                    UnityEngine.Random.InitState(Seed);
                    float tLand2 = ThrowOnce();
                    AtTime(tLand2 + 0.05f, () =>
                    {
                        Freeze(); Timing("land-run2", tLand2);
                        int at = UnityEngine.Time.frameCount;
                        AtFrame(at + 2, () => "land-run2 " + Shoot("land-run2"));
                        AtFrame(at + 10, () => { PutBack(); return "DONE, scene put back; pending=" + TW.Editor.CaptureRig.Pending(); });
                        return "land-run2: frozen at frame " + at + ", delta="
                            + (UnityEngine.Time.time - tLand2).ToString("+0.0000;-0.0000");
                    });
                    return "run 2 thrown (scene reset, seed " + Seed + " again), lands at t=" + tLand2.ToString("0.0000");
                }))));
    return "run 1 staged";
});
return "staged; flight " + delay.ToString("0.00000") + " s (" + (delay * 60f).ToString("0.0000")
    + " frames); watch " + dir + "stage.log";
