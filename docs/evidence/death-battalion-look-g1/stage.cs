// One capture pass: a rifleman and a Death Battalion man side by side, at night, zoom 30, in ONE frame.
//
//   Tools/tw run eval_file -- --file <this file>
//
// The tag for the pass is read from %LOCALAPPDATA%\TrenchWarfare\deathbn-g1\tag.txt ("base1", "base2", "base3",
// "after"): it names the three PNGs this pass writes into that same folder.
//
//   <tag>_pair.png         both men, the clock held
//   <tag>_minus_death.png  the same held instant with the Death Battalion man written off the map
//   <tag>_minus_rifle.png  the same held instant with the rifleman written off the map
//
// Nothing ticks between the three: Hold() stops the clock, the man's Position is written away through
// SimHost.WriteWorlds and the presenter is re-captured by hand (twice, so prev == cur and he does not smear),
// so the ground, the light, the rain and the pose are the same in all three. A man's own pixels are then
// |pair - minus|, which is what probe.py measures.
//
// The pass returns at once and runs on EditorApplication.update. Its progress is in the same folder as
// stage-status.txt; the last line is "done" or "failed <why>".
var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
if (host == null) return "no SimHost: open GreyboxCorridor and enter Play";

string dir = System.IO.Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.LocalApplicationData), "TrenchWarfare", "deathbn-g1");
System.IO.Directory.CreateDirectory(dir);
string tagFile = System.IO.Path.Combine(dir, "tag.txt");
if (!System.IO.File.Exists(tagFile)) return "no tag.txt in " + dir;
string tag = System.IO.File.ReadAllText(tagFile).Trim();
string status = System.IO.Path.Combine(dir, "stage-status.txt");
foreach (string shot in new[] { tag + "_pair.png", tag + "_minus_death.png", tag + "_minus_rifle.png" })
{
    string stale = System.IO.Path.Combine(dir, shot);
    if (System.IO.File.Exists(stale)) System.IO.File.Delete(stale);   // a stale PNG must never pass for this pass's
}
System.IO.File.WriteAllText(status, "started " + tag + "\n");

const float MidX = 0f, MidZ = 0f, Half = 0.9f;   // 1.8 m apart, the same yaw, the same team
// Apart ACROSS the picture, not into it: the camera looks down its base yaw (-90) plus the shot's 21, so the
// screen's right in the world is (cos, -sin) of that. Spread along world X instead and the two men stand one
// behind the other and the back one is hidden - which is what the first pass came back with.
const float ShotYaw = 21f, CamYaw = (-90f + ShotYaw) * UnityEngine.Mathf.Deg2Rad;
float rx = UnityEngine.Mathf.Cos(CamYaw) * Half, rz = -UnityEngine.Mathf.Sin(CamYaw) * Half;
const int Rifle = 0, DeathBn = 28;               // InfantryArchetype
int step = 0, waited = 0, rifleSlot = -1, deathSlot = -1;
long startTick = 0;
var awayRifle = new Unity.Mathematics.float3(0f, 0f, 0f);
var awayDeath = new Unity.Mathematics.float3(0f, 0f, 0f);

int settle = 0;
System.Func<string, bool> shotDone = name =>
{
    if (TW.Editor.CaptureRig.Pending() != "0" || !System.IO.File.Exists(System.IO.Path.Combine(dir, name))) { settle = 0; return false; }
    return ++settle > 10;    // and a few frames after the PNG lands, so nothing is written mid-render
};

System.Action<string> say = m => System.IO.File.AppendAllText(status, m + "\n");

// The same pose on both men, every frame the set lasts: the clip each man idles in decides how wide his arms
// are and how much of his pale kit faces the camera, and between two passes that swamps the kit itself. The
// Death Battalion man is given the rifleman's row, phase and yaw, so what is left between them is the kit.
System.Action syncPose = () =>
{
    var pr = host.Presenter;
    if (rifleSlot < 0 || deathSlot < 0 || !pr.RowIn.IsCreated) return;
    pr.RowIn[deathSlot] = pr.RowIn[rifleSlot];
    pr.PhaseIn[deathSlot] = pr.PhaseIn[rifleSlot];
    pr.YawIn[deathSlot] = pr.YawIn[rifleSlot];
};

System.Action<int, Unity.Mathematics.float3> place = (slot, p) =>
{
    host.WriteWorlds(m => { if (m.World.IsAlive(slot)) m.World.Position[slot] = p; });
    host.Presenter.Capture(host.Local.World);    // twice: the first copies cur into prev, the second lands both on the new place
    host.Presenter.Capture(host.Local.World);
};

UnityEditor.EditorApplication.CallbackFunction tick = null;
tick = () =>
{
    try
    {
        if (++waited > 4000) { say("failed timed out at step " + step); UnityEditor.EditorApplication.update -= tick; return; }
        if (step >= 2) syncPose();
        switch (step)
        {
            case 0:
                TW.Editor.CaptureRig.Release();
                // No cast shadows for the set: a man's shadow changes with him, so it lands in his own-pixel mask
                // and a leaning shadow is wider than any greatcoat. Off for every pass, before and after alike.
                var vat = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
                if (vat != null) vat.CastShadows = false;
                host.TimeScale = 1f;   // the match's own clock: a session that was paused leaves it at 0 and nothing ticks
                string a = TW.Editor.TankCapture.Spawn(0, Rifle, MidX - rx, MidZ - rz, 0f);
                string b = TW.Editor.TankCapture.Spawn(0, DeathBn, MidX + rx, MidZ + rz, 0f);
                if (!a.StartsWith("slot ") || !b.StartsWith("slot ")) { say("failed spawn: " + a + " / " + b); UnityEditor.EditorApplication.update -= tick; return; }
                rifleSlot = int.Parse(a.Substring(5));
                deathSlot = int.Parse(b.Substring(5));
                startTick = host.Local.World.Tick;
                say("spawned rifle " + rifleSlot + " death " + deathSlot + " at tick " + startTick);
                step = 1; return;
            case 1:
                // Thirty ticks for a pose and a stride, then both men are put back on their marks: left to
                // themselves they walk off the camera's aim and end up on top of each other, which is the one
                // thing this pair of men must not do.
                if (host.Local.World.Tick < startTick + 30) return;
                awayRifle = new Unity.Mathematics.float3(MidX - rx, 0f, MidZ - rz);
                awayDeath = new Unity.Mathematics.float3(MidX + rx, 0f, MidZ + rz);
                host.WriteWorlds(m => { m.World.Position[rifleSlot] = awayRifle; m.World.Position[deathSlot] = awayDeath; m.World.Yaw[rifleSlot] = 0f; m.World.Yaw[deathSlot] = 0f; });
                host.Presenter.Capture(host.Local.World);
                host.Presenter.Capture(host.Local.World);
                TW.Editor.CaptureRig.Hold(120f);
                say("held at tick " + host.Local.World.Tick);
                step = 2; waited = 0; return;
            case 2:
                if (waited < 4) return;
                TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, tag + "_pair.png"), MidX, MidZ, 30f, ShotYaw, 25f, 3840, 2160, 0.8f);
                step = 3; return;
            case 3:
                // Pending() hits 0 one frame EARLY: the rig dequeues a shot, poses the camera, and renders it on the
                // NEXT frame. Touch the world the moment the queue empties and the change lands in the picture that
                // is still to be taken - which is how the first pass came back with the pair frame missing a man.
                if (!shotDone(tag + "_pair.png")) return;
                place(deathSlot, new Unity.Mathematics.float3(4000f, 0f, 4000f));
                step = 4; waited = 0; settle = 0; return;
            case 4:
                if (waited < 4) return;
                TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, tag + "_minus_death.png"), MidX, MidZ, 30f, ShotYaw, 25f, 3840, 2160, 0.8f);
                step = 5; return;
            case 5:
                if (!shotDone(tag + "_minus_death.png")) return;
                place(deathSlot, awayDeath);
                place(rifleSlot, new Unity.Mathematics.float3(4000f, 0f, 4000f));
                step = 6; waited = 0; settle = 0; return;
            case 6:
                if (waited < 4) return;
                TW.Editor.CaptureRig.Shot(System.IO.Path.Combine(dir, tag + "_minus_rifle.png"), MidX, MidZ, 30f, ShotYaw, 25f, 3840, 2160, 0.8f);
                step = 7; return;
            case 7:
                if (!shotDone(tag + "_minus_rifle.png")) return;
                place(rifleSlot, awayRifle);
                TW.Editor.CaptureRig.Release();
                var back = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
                if (back != null) back.CastShadows = true;
                host.WriteWorlds(m => { m.World.Despawn(rifleSlot); m.World.Despawn(deathSlot); });
                say("done");
                UnityEditor.EditorApplication.update -= tick;
                return;
        }
    }
    catch (System.Exception ex)
    {
        say("failed " + ex.Message);
        UnityEditor.EditorApplication.update -= tick;
    }
};
UnityEditor.EditorApplication.update += tick;
return "staging " + tag + ": watch " + status;
