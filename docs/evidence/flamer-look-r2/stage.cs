// Stage ONE man at a time on the same flat spot at night, freeze him, project his kit boxes and shoot the three
// view bands. Run in Play in GreyboxCorridor, one phase per call (the phase is a word in phase.txt beside this file,
// because an eval_file body takes no arguments):
//
//   echo flamer  > docs/evidence/flamer-look-r2/phase.txt   # place a Flamethrower, team 0, on the spot
//   echo assault > phase.txt                                # an Assault instead - the comparison
//   echo control > phase.txt                                # a second Assault - the control
//   Tools/tw run eval_file -- --file <abs path to this file>
//   echo freeze  > phase.txt ; eval_file                    # AT ONCE after the line above: TimeScale 0, Hold
//   echo shoot   > phase.txt ; eval_file                    # boxes, then queue the stills
//   echo merge   > phase.txt ; eval_file                    # the three boxes.<subject>.json -> boxes.json
//
// WHY ONE MAN AT A TIME, AND NOT TWO SIDE BY SIDE (measured 2026-10-19, the first way round)
// The plan photographed a Flamer and an Assault standing 2 m apart and compared the Flamer's kit box against the
// same box shifted onto the Assault. The control - two Assaults, which must come out near zero - came out at 7075
// differing pixels against a cap of 10, because the two boxes hold two different pieces of BACKGROUND: one man had
// a bunker behind him, the other the mud bank, and the box is 221x83 px at T3 with the man filling a fraction of
// it. Standing each man in turn on the SAME spot, under the same held weather and the same camera, leaves the man
// himself as the only difference between the two frames, and the control (one Assault against another Assault)
// falls to the few pixels the renderer itself is not repeatable in.
//
// There are no `using` directives in an eval, so everything is fully qualified.
// The camera pose below is a copy of CaptureRig.Rig.Pose: the same zoom/fov/pitch/distance arithmetic, so the boxes
// this writes are in the frame the rig photographs. The overlay probe.py draws is the guard that it really is.
var dir = System.IO.Path.GetFullPath(UnityEngine.Application.dataPath + "/../../docs/evidence/flamer-look-r2");
System.IO.Directory.CreateDirectory(dir);
var phase = System.IO.File.ReadAllText(System.IO.Path.Combine(dir, "phase.txt")).Trim();
var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
var tc = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
if (host == null || tc == null) return "no host or no tactical camera";

// the flat spot (sampled 2026-10-19: Map.Height is 2.5 m and level over x 30..48, z 10..16) and the shot yaw
float spotX = 42f, spotZ = 12f, shotYaw = 90f;
// The camera must stand behind him: the tanks are on his back. The shot yaw is 90 because TacticalCamera.BaseYaw is
// -90, so the camera looks along +z - and +z is the way these men are drawn. Their drawn yaw cannot be dictated:
// AnimationController turns a man with no target towards his own side's parapet (+z for team 0), and it held 0.000
// against a written yaw of -1.204 rad (2026-10-19). So the camera is moved to him instead.
float manYaw = (tc.BaseYaw + shotYaw) * UnityEngine.Mathf.Deg2Rad;
var subjectFile = System.IO.Path.Combine(dir, "subject.txt");
bool placing = phase == "flamer" || phase == "assault" || phase == "control";
if (placing) System.IO.File.WriteAllText(subjectFile, phase);
var subject = System.IO.File.ReadAllText(subjectFile).Trim();
byte archetype = subject == "flamer" ? (byte)36 : (byte)1;

System.Func<int> find = () =>
{
    var w0 = host.Local.World;
    for (int i = 0; i < w0.HighWater; i++)
        if ((w0.Flags[i] & 1u) != 0 && w0.Team[i] == 0 && (w0.Archetype[i] == 1 || w0.Archetype[i] == 36)) return i;
    return -1;
};

// ONE MAN, ONE SLOT, THE ARCHETYPE FLIPPED - NOT THREE MEN (measured 2026-10-19).
// Two men in two slots never render alike, whatever is pinned: VATRenderer gives each his own grime seed and
// character from his slot (`seed = slot * 37`, `chr = Char[slot]`, VATRenderer.cs ~413) and SimPresenter gives
// each his own place in the idle loop (`t = frac(AnimTime + i * 0.137f)`, SimPresenter.cs ~143). A second Assault
// came out 829 px different over the kit box with no kit in the frame at all, against the goal's cap of 10.
// So ONE man is placed, on one spot, in one slot, and photographed three times: as a Flamethrower, then with his
// archetype flipped to Assault (the only thing that changes is whether CombatFx draws him a kit - both archetypes
// are the same baked soldier, VATRenderer.FigureOfArchetype), then a second time as the Assault, the control.
// Every pixel that differs between the flamer stills and the assault stills is therefore the kit and nothing else.
if (phase == "assault" || phase == "control")
{
    int keep = find();
    if (keep < 0) return "no man on the spot (run the flamer phases first)";
    if (phase == "assault")
        host.WriteWorlds(m => { m.World.Archetype[keep] = 1; });
    host.TimeScale = 1f;   // a few frames, exactly as a freshly placed man gets, then freeze again
    return "the same man, slot " + keep + ", as the " + phase + " - freeze NOW";
}
else if (placing)
{
    int placed = 0;
    host.WriteWorlds(m =>
    {
        var w1 = m.World;
        for (int i = 0; i < w1.HighWater; i++)   // clear the previous subject's man
            if ((w1.Flags[i] & 1u) != 0 && w1.Team[i] == 0 && (w1.Archetype[i] == 1 || w1.Archetype[i] == 36)) w1.Flags[i] = 0u;
        placed = TW.Presentation.ProvingGround.Place(m, 0, archetype, 1);
    });
    int fresh = find();
    if (fresh < 0) return "placed " + placed + " but could not find him";
    host.WriteWorlds(m =>
    {
        var w2 = m.World;
        w2.Position[fresh] = new Unity.Mathematics.float3(spotX, 0f, spotZ);
        w2.Yaw[fresh] = manYaw;
    });
    host.TimeScale = 1f;   // a few frames, so the controller sees him and the presenter follows him
    return "placed " + subject + " (archetype " + archetype + ") as slot " + fresh + " - freeze NOW";
}

int man = find();
if (man < 0) return "no " + subject + " on the field";

if (phase == "freeze")
{
    // Snap him back onto the spot (an idle man drifts a little), stop the world and hold the weather clock. This is
    // its own phase and must follow the placing phase within a second or two: GreyboxCorridor runs a live scenario
    // that spawns wave after wave of team 1 and REUSES freed slots, and left running for six seconds it overwrote
    // the staged men with paratroopers (2026-10-19).
    host.WriteWorlds(m =>
    {
        var w3 = m.World;
        w3.Position[man] = new Unity.Mathematics.float3(spotX, 0f, spotZ);
        w3.Yaw[man] = manYaw;
        // Stand him up and give him nobody to shoot at. SimPresenter picks the drawn row from the SIM's stance
        // unless the animation controller is driving (SimPresenter.cs ~133), and the men on this field lie prone:
        // the first rounds photographed a man flat on his belly under a kit (2026-10-19).
        w3.StanceOf[man] = (byte)TW.Sim.Stance.Standing;
        w3.TargetSlot[man] = -1;
    });
    // The presenter only reads the world on a sim step, and we are about to stop stepping: a man placed and frozen
    // in the same frame was drawn at the world origin (2026-10-19: Drawn = 0,0,0, his sockets reporting a chest at
    // y 3.6 over nothing). Capture twice, so his previous and his next both hold the spot and the lerp is flat.
    // PIN THE DRAWN POSE. The yaw cannot be dictated through the sim (AnimationController turns a man with no
    // target towards his own parapet), and it had not finished turning when the frame was frozen: three subjects
    // came out at 0.493, 0.705 and -0.341 rad, so the two Assaults of the control faced three ways and 6829 px of
    // a 98x106 box differed on the men themselves. The controller's own arrays are what the renderer reads, so
    // every subject is pinned to one pose here: the same clip, the same place in it, no blend, no lift, no grime.
    var anim = host.Animation;
    if (anim.Yaw.IsCreated && man < anim.Yaw.Length)
    {
        anim.Yaw[man] = manYaw;
        // Clip.Idle, a standing man - Row 0 is Clip.None and the renderer drew him flat on his face (2026-10-19).
        ushort idle = (ushort)TW.Presentation.Clip.Idle;
        anim.Row[man] = idle; anim.Phase[man] = 0.25f;
        anim.PrevRow[man] = idle; anim.PrevPhase[man] = 0.25f; anim.Blend[man] = 0f;
        anim.Lift[man] = 0f; anim.Hop[man] = 0f; anim.Grime[man] = 0f;
    }
    // The pin has to come BEFORE the two Capture calls: VATRenderer draws a man from the PRESENTER's pose
    // (VATRenderer.cs ~409 reads p.AnimRow), which is a copy the presenter takes on a sim step. Pinned after the
    // captures, the pin never reached the screen and the men were drawn sprawled on the ground (2026-10-19).
    host.Presenter.Capture(host.Local.World);
    host.Presenter.Capture(host.Local.World);
    host.TimeScale = 0f;
    TW.Editor.CaptureRig.Hold(120f);
    return "frozen: " + subject + " slot " + man + " drawn at " + host.Presenter.Drawn(man)
        + " yaw " + host.Animation.Yaw[man].ToString("0.000");
}

if (phase == "shoot")
{
    var units = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
    var cam = UnityEngine.Camera.main;
    var inv = System.Globalization.CultureInfo.InvariantCulture;
    var sb = new System.Text.StringBuilder();
    sb.Append("{\n  \"subject\": \"").Append(subject).Append("\",\n  \"spot\": [")
      .Append(spotX.ToString("0.###", inv)).Append(", ").Append(spotZ.ToString("0.###", inv))
      .Append("],\n  \"stills\": {\n");

    var tiers = new float[][] { new float[] { 7.5f, 13f }, new float[] { 16f, 22f }, new float[] { 30f, 25f } };
    var names = new string[] { "t3", "t2", "t1" };
    int shots = 0;
    for (int ti = 0; ti < tiers.Length; ti++)
    {
        if (subject == "control" && names[ti] != "t3") continue;   // the control is one still, at T3
        float zoom = UnityEngine.Mathf.Clamp(tiers[ti][0], tc.ZoomMin, tc.ZoomMax), pitch = tiers[ti][1];
        int W = 1920, H = 1080;
        // --- pose the camera exactly as CaptureRig does, into a render texture of the still's size, so that
        //     WorldToScreenPoint lands in the still's pixels (batch mode's own screen is not 1920x1080).
        bool keepEnabled = tc.enabled; tc.enabled = false;
        float keepZoom = tc.Zoom; UnityEngine.Vector2 keepFocus = tc.Focus; float keepFov = cam.fieldOfView;
        var keepPos = cam.transform.position; var keepRot = cam.transform.rotation;
        var keepRt = cam.targetTexture;
        var rt = UnityEngine.RenderTexture.GetTemporary(W, H, 24);
        cam.targetTexture = rt;
        tc.Zoom = zoom; tc.Focus = new UnityEngine.Vector2(spotX, spotZ);
        float close = 1f - UnityEngine.Mathf.SmoothStep(0f, 1f, UnityEngine.Mathf.InverseLerp(tc.ZoomMin, tc.CloseZoom, zoom));
        float fov = UnityEngine.Mathf.Lerp(tc.Fov, tc.CloseFov, close);
        cam.fieldOfView = fov; cam.nearClipPlane = 0.2f;
        float pit = UnityEngine.Mathf.Clamp(pitch, tc.PitchMin, tc.PitchMax);
        var rot = UnityEngine.Quaternion.Euler(pit, tc.BaseYaw + shotYaw, 0f);
        float distance = zoom * UnityEngine.Mathf.Tan(30f * UnityEngine.Mathf.Deg2Rad) / UnityEngine.Mathf.Tan(fov * 0.5f * UnityEngine.Mathf.Deg2Rad);
        float ground = host.Local.Map.Height.Sample(spotX, spotZ);
        var aim = new UnityEngine.Vector3(spotX, ground * close + 1.1f * close, spotZ);
        cam.transform.SetPositionAndRotation(aim - rot * UnityEngine.Vector3.forward * distance, rot);

        // The man's own drawn size, which is what DrawKit sizes the kit with (VATRenderer.ScaleAt: one size at
        // every zoom, the owner's call of 2026-10-13).
        float scale = units != null ? units.ScaleAt(zoom) : 1f;
        UnityEngine.Vector3 muzzle = default, barrel = default, chest = default;
        bool sock = units != null && units.Sockets(man, out muzzle, out barrel, out chest);
        if (!sock)
        {
            // the near tier's sockets are gone (beyond VATRenderer.LodDistance, or no controller): estimate the way
            // every other CombatFx caller does, so a box still exists and the number stays honest.
            var wE = host.Local.World;
            var atE = host.Presenter != null ? (UnityEngine.Vector3)host.Presenter.Drawn(man) : (UnityEngine.Vector3)wE.Position[man];
            float gy = TW.Presentation.RenderGround.Sample(host.Local.Map, atE.x, atE.z);
            barrel = UnityEngine.Quaternion.Euler(0f, wE.Yaw[man] * UnityEngine.Mathf.Rad2Deg, 0f) * UnityEngine.Vector3.forward;
            muzzle = new UnityEngine.Vector3(atE.x, gy + 1.4f * scale, atE.z) + barrel * (0.75f * scale);
            chest = new UnityEngine.Vector3(atE.x, gy + 1.2f * scale, atE.z);
        }
        var drawn = host.Presenter != null ? (UnityEngine.Vector3)host.Presenter.Drawn(man) : (UnityEngine.Vector3)host.Local.World.Position[man];
        float gd = TW.Presentation.RenderGround.Sample(host.Local.Map, drawn.x, drawn.z);
        var feet = new UnityEngine.Vector3(drawn.x, gd, drawn.z);
        // The kit boxes are built for archetype 36 whoever is standing there: the Flamer's kit box must be the SAME
        // rectangle in the Assault's still, or the two frames are not being compared over the same pixels.
        var pieces = new UnityEngine.Matrix4x4[TW.Presentation.Tactical.CombatFx.KitPieceMax];
        int n = TW.Presentation.Tactical.CombatFx.KitPieces(36, chest, muzzle, barrel, scale, pieces);
        System.Func<UnityEngine.Matrix4x4[], int, int, float[]> box = (ms, from, to) =>
        {
            float x0 = 1e9f, y0 = 1e9f, x1 = -1e9f, y1 = -1e9f;
            for (int k = from; k < to; k++)
                for (int c = 0; c < 8; c++)
                {
                    var lc = new UnityEngine.Vector3((c & 1) == 0 ? -0.5f : 0.5f, (c & 2) == 0 ? -0.5f : 0.5f, (c & 4) == 0 ? -0.5f : 0.5f);
                    var sp = cam.WorldToScreenPoint(ms[k].MultiplyPoint3x4(lc));
                    float nx = sp.x / W, ny = 1f - sp.y / H;
                    x0 = UnityEngine.Mathf.Min(x0, nx); x1 = UnityEngine.Mathf.Max(x1, nx);
                    y0 = UnityEngine.Mathf.Min(y0, ny); y1 = UnityEngine.Mathf.Max(y1, ny);
                }
            return new float[] { x0, y0, x1, y1 };
        };
        System.Func<UnityEngine.Vector3, float, float[]> cube = (atC, half) =>
        {
            var one = new UnityEngine.Matrix4x4[] { UnityEngine.Matrix4x4.TRS(atC, UnityEngine.Quaternion.identity, UnityEngine.Vector3.one * (half * 2f)) };
            return box(one, 0, 1);
        };
        System.Func<float[], string> f4 = v => "[" + v[0].ToString("0.#####", inv) + ", " + v[1].ToString("0.#####", inv)
            + ", " + v[2].ToString("0.#####", inv) + ", " + v[3].ToString("0.#####", inv) + "]";
        var anchor = cam.WorldToScreenPoint(feet);
        var mj = new System.Text.StringBuilder();
        mj.Append("{\"slot\": ").Append(man).Append(", \"sockets\": ").Append(sock ? "true" : "false");
        mj.Append(", \"anchor\": [").Append((anchor.x / W).ToString("0.#####", inv)).Append(", ")
          .Append((1f - anchor.y / H).ToString("0.#####", inv)).Append("]");
        if (n >= 3)
        {
            mj.Append(", \"tanks\": ").Append(f4(box(pieces, 0, 2)));
            mj.Append(", \"wand\": ").Append(f4(box(pieces, 2, 3)));
            mj.Append(", \"kit\": ").Append(f4(box(pieces, 0, 3)));
        }
        mj.Append(", \"torso\": ").Append(f4(cube(chest, 0.18f * scale)));
        mj.Append(", \"ground\": ").Append(f4(cube(feet - UnityEngine.Vector3.up * (0.6f * scale), 0.2f * scale)));
        mj.Append("}");

        cam.targetTexture = keepRt; UnityEngine.RenderTexture.ReleaseTemporary(rt);
        tc.Zoom = keepZoom; tc.Focus = keepFocus; cam.fieldOfView = keepFov;
        cam.transform.SetPositionAndRotation(keepPos, keepRot); tc.enabled = keepEnabled;

        string png = System.IO.Path.Combine(dir, subject + "_" + names[ti] + ".png").Replace("\\", "/");
        TW.Editor.CaptureRig.Shot(png, spotX, spotZ, zoom, shotYaw, pitch, W, H);
        shots++;
        sb.Append("    \"").Append(subject).Append("_").Append(names[ti]).Append("\": {\"png\": \"")
          .Append(System.IO.Path.GetFileName(png)).Append("\", \"zoom\": ").Append(zoom.ToString("0.##", inv))
          .Append(", \"pitch\": ").Append(pitch.ToString("0.##", inv))
          .Append(", \"man\": ").Append(mj.ToString()).Append("},\n");
    }
    var text = sb.ToString().TrimEnd('\n', ',') + "\n  }\n}\n";
    System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "boxes." + subject + ".json"), text);
    return "queued " + shots + " shot(s) of the " + subject + ", boxes written";
}

if (phase == "merge")
{
    var parts = new string[] { "flamer", "assault", "control" };
    var out_ = new System.Text.StringBuilder("{\n");
    for (int i = 0; i < parts.Length; i++)
    {
        string p = System.IO.Path.Combine(dir, "boxes." + parts[i] + ".json");
        if (!System.IO.File.Exists(p)) return "missing " + System.IO.Path.GetFileName(p);
        out_.Append(" \"").Append(parts[i]).Append("\": ").Append(System.IO.File.ReadAllText(p).Trim());
        out_.Append(i == parts.Length - 1 ? "\n" : ",\n");
    }
    out_.Append("}\n");
    System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "boxes.json"), out_.ToString());
    return "merged";
}

return "unknown phase: " + phase;
