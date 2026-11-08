// The committed capture driver for the T1 (zoom 30) measurement of the drop-zone marker: it stages the pose,
// shoots the control pair and the "before" still, calls the drop, and shoots a 9-frame series over the flight.
// Run via `Tools/tw run eval_file`, in Play in GreyboxCorridor, in THREE phases, because a queued Shot only
// renders on a later frame: phase 1 stages and queues the control pair + before, phase 2 (after Pending() == 0)
// fires the ability and queues the series, phase 3 (after the series drains) reports the clock so the run's own
// frame rate is measured rather than assumed (r4 measured 20.0 fps; it wanders between runs).
//   Tools/tw run eval_file --file dropshoot.cs   with `phase` set to 1, then 2, then 3, draining in between.
// Never CaptureRig.Hold(): it sets Time.timeScale = 0 and would freeze the streak cards themselves. The weather
// is held by Storm.Hold + Atmosphere.PinnedClock instead.
// The camera is moved to zoom 30 BEFORE the call, because FlipbookFx.FarGrow is read from IZoomSource.CurrentZoom
// when the event arrives: FarGrow(30) = 1, so each man's ShellFall card is a full 3 m (~78 px at 26.0 px/m).
int phase = /* 1, 2 or 3, set per call */ 1;
string dir = "<abs dir>";
var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var d = TW.Perf.GymDirector.Attach(host);
// The README's pose, and the pose t1px.json's 8 ring points were projected at: the drop zone is the fixed world
// point (45.00, 82.20), NOT d.Stage + (0, 30). GymDirector hands out a different Stage on a different gym run
// (this run's was (20, 52.2)), and the measured ring points only apply to one fixed point.
var target = new UnityEngine.Vector2(45f, 82.2f);
if (phase == 1)
{
    d.Quiet();
    d.Clear();
    TW.Presentation.Tactical.CombatFx.ShowOverlays = false;
    foreach (var ui in UnityEngine.Object.FindObjectsByType<UnityEngine.UIElements.UIDocument>(UnityEngine.FindObjectsSortMode.None))
        ui.rootVisualElement.style.display = UnityEngine.UIElements.DisplayStyle.None;
    TW.Presentation.Tactical.CameraShake.Strength = 0f;
    TW.Presentation.Tactical.CameraShake.Reset();
    TW.Presentation.Terrain.Storm.Hold = true;
    foreach (var atm in UnityEngine.Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(UnityEngine.FindObjectsSortMode.None)) atm.Lightning = false;
    TW.Presentation.Terrain.Atmosphere.PinnedClock = 120f;
    UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>().FrameFrom(target, 30f, 30f);
    float gy = TW.Presentation.RenderGround.Sample(host.Local.Map, target.x, target.y);
    TW.Editor.CaptureRig.Rig.Mark = new UnityEngine.Vector3(target.x, gy, target.y);
    // the control pair: the untouched scene twice, about the same frame gap as before -> the 2.5 s after still
    TW.Editor.CaptureRig.Series(dir, "t1-control", target.x, target.y, 30f, 30f, 25f, 2, 48, 1600, 900);
    TW.Editor.CaptureRig.Shot(dir + "/t1-before.png", target.x, target.y, 30f, 30f, 25f, 1600, 900);
    return "staged at " + target.x + "," + target.y + " zoom 30, queued control pair + before";
}
if (phase == 2)
{
    bool ok = d.Ability(TW.Sim.Match.OffMapAbilityId.ParaDrop, target);
    TW.Editor.CaptureRig.Series(dir, "t1-after", target.x, target.y, 30f, 30f, 25f, 9, 11, 1600, 900);
    return "ability=" + ok + " frame=" + UnityEngine.Time.frameCount + " time=" + UnityEngine.Time.time;
}
return "drained frame=" + UnityEngine.Time.frameCount + " time=" + UnityEngine.Time.time + " pending=" + TW.Editor.CaptureRig.Pending();
