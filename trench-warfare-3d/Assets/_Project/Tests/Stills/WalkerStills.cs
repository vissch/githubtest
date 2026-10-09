// Phase: tooling (2026-09-27) — the one thing the walker board never had.
//
// docs/20-rig-scoreboard.md scores twenty-two lines, most of them worded as judgements about how the animation LOOKS
// ("pitch and roll read as a body carried on legs, not a box on a spring", "traverse reads as weight"). Its own rule
// is that "a line nobody has rendered evidence for scores 0, not 'unknown'". For twenty-seven consecutive cycles the
// board moved scores anyway, on arithmetic taken from EditMode harnesses, because nothing could photograph a walker
// without a human driving the editor. Captures/rigloop stops at a8. The board's second loop then shipped four changes
// to a SHOW-lane file without once meeting CLAUDE.md's SHOW gate, which is EditMode plus a look at the thing changed.
//
// This is the look, from batch mode, with nobody driving.
//
// Why it is shaped like this, since two obvious shapes do not work:
//  - CaptureRig has to pose the camera on one frame and photograph on the NEXT, after every LateUpdate that reads the
//    camera has run against the new pose (see its header — that two-frame dance is the whole reason it exists). So a
//    plain -executeMethod static call cannot take a still: there is no frame loop.
//  - It cannot live in TW.Tests.PlayMode either. CaptureRig, TankCapture and RiderLab are in TW.Editor, which is
//    Editor-only, and TW.Tests.PlayMode is built for all platforms; adding the reference would have forced that
//    assembly to Editor-only and changed the existing PlayMode gate.
// An assembly with includePlatforms ["Editor"] IS an EditMode test assembly as far as Unity's runner is concerned, so
// this is an EditMode test that drives the editor in and out of Play itself. That is what EnterPlayMode is for.
//
// It is [Explicit] so the ordinary gate skips it: a normal EditMode run must not enter Play. Run it by name, WITH a
// graphics device — not -nographics, or every PNG comes out empty:
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode \
//             -testFilter EveryWalkerIsPhotographedWalkingOnRealGround -testResults <out.xml> -logFile <out.log>
//
// What it asserts is deliberately thin: that a file was written, that it is a plausible size, and that the machine
// stayed alive to be photographed. It is an INSTRUMENT, not a judgement. Nothing here scores anything — the pictures
// go to a critique that has not seen the code, which is the half of the loop that was missing.
//
// OneWalkerIsFilmedThroughAStep (2026-10-07, the Banner's re-cut) is the before-and-after of ONE machine: the same
// locked-off side row, a close row on a rear and on a front foot from the frame it lands, and one frame at the zoom
// the game is played at. Environment: TW_STILLS_MACHINE the walker (default banner), TW_STILLS_DIR where the stills go
// (default %TEMP%/tw-walkerfilm: never inside the checkout), TW_STILLS_FIELD the look (Winter is the day field).
// The game clock steps 1/30 s a frame while it films, so two runs sample a step alike.
// TheBannersTopAndGunsAreFilmed (2026-10-09, banner-parts) photographs the Banner's new parts: its top laid on a
// target 0, 45 and 90 degrees off the nose, and a barrel back. TW_STILLS_DIR again.
using System.Collections;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class WalkerStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        // the six, in the order the board lists them
        static readonly (string name, byte archetype)[] Machines =
        {
            ("pincer",  VehicleArchetype.Pincer),
            ("censer",  VehicleArchetype.Censer),
            ("kettle",  VehicleArchetype.Kettle),
            ("pavise",  VehicleArchetype.Pavise),
            ("banner",  VehicleArchetype.Banner),
            ("redoubt", VehicleArchetype.Redoubt),
        };

        /// <summary>Where the stills land: &lt;project&gt;/Captures/rigloop/&lt;tag&gt;, beside the a2..a8 sets the
        /// first loop left behind.</summary>
        static string Dir(string tag)
        {
            string root = Directory.GetParent(Application.dataPath).FullName;
            string d = Path.Combine(root, "Captures", "rigloop", tag);
            Directory.CreateDirectory(d);
            return d;
        }

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>Wait for a shot to be ON DISK, and FAIL if it never arrives.
        ///
        /// Waiting on CaptureRig.Pending() alone is not enough and the reason is easy to miss: Pending() returns
        /// Queue.Count, and a shot leaves the queue when it is DEQUEUED — one frame of posing and one render before
        /// the PNG is written. So Pending() reads "0" while the last shot is still in flight, and a test that then
        /// reads the file gets FileNotFoundException, or worse, reads the PREVIOUS file and compares a still with
        /// itself. Wait for the queue, then for the file.</summary>
        static IEnumerator Drain(string path = null, int cap = 900)
        {
            for (int f = 0; f < cap && CaptureRig.Pending() != "0"; f++) yield return null;
            Assert.That(CaptureRig.Pending(), Is.EqualTo("0"),
                $"the capture queue stalled with {CaptureRig.Pending()} shots outstanding after {cap} frames");
            if (path == null) yield break;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
            Assert.That(File.Exists(path), Is.True, $"the queue drained but {Path.GetFileName(path)} was never written");
        }

        /// <summary>Is the machine in the picture at all? Three runs of stills came back with the terrain drawn
        /// correctly and no walker anywhere near the middle of the frame, which has two very different causes —
        /// badly framed, or not drawn — and guessing between them from a dark PNG wasted two capture runs. This
        /// photographs one pose with the machine alive, kills it, photographs the identical pose, and asks
        /// CaptureRig.Diff what changed. If the machine is being drawn, the difference is machine-shaped and sits
        /// where the machine was. If nothing changed, it is not being drawn and no amount of framing will help.</summary>
        [UnityTest, Explicit("Diagnostic: is a spawned walker drawn at all?")]
        public IEnumerator ASpawnedWalkerIsActuallyDrawn()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            for (int f = 0; f < 120; f++) yield return null;

            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;

            var size = Host.Local.Map.SizeMeters;
            float sx = size.x * 0.5f, sz = size.y * 0.35f;
            string spawned = TankCapture.Spawn(0, VehicleArchetype.Pincer, sx, sz, 0f);
            TestContext.Out.WriteLine("spawn said: " + spawned);
            Assert.That(spawned, Does.StartWith("slot "), "the spawn itself failed");
            int slot = int.Parse(spawned.Substring(5));
            for (int f = 0; f < 90; f++) yield return null;

            var w = Host.Local.World;
            var p = w.Position[slot];
            TestContext.Out.WriteLine($"alive={w.IsAlive(slot)} archetype={w.Archetype[slot]} team={w.Team[slot]} "
                + $"pos=({p.x:0.0},{p.y:0.0},{p.z:0.0}) speed={w.Speed[slot]:0.00}");
            var tanks = Object.FindFirstObjectByType<TankRenderer>();
            // TankRenderer.LateUpdate returns before Draw() unless Ready, and Ready is `maw != null && mats[0] != null`
            // — one missing model or material and EVERY vehicle, wreck and piece of debris silently disappears while
            // the terrain, which is ordinary MeshRenderers, carries on looking perfect.
            TestContext.Out.WriteLine($"TankRenderer in scene: {tanks != null}, Ready: {(tanks != null ? tanks.Ready.ToString() : "n/a")}");

            string dir = Dir("a41-diag");
            string a = Path.Combine(dir, "with.png"), b = Path.Combine(dir, "without.png");

            // The first version of this killed the machine between the two shots and let 60 frames pass. That
            // confounds the comparison twice over: the men walk on, and the mud, water and rain shaders advance
            // with Time — so the two stills differ whether or not a machine was ever drawn, and the 59 KB of
            // difference it reported proved nothing. Freeze the clock and toggle the RENDERER instead. Nothing in
            // the world changes between the two frames except whether TankRenderer is allowed to submit.
            RiderLab.Stop(slot);
            for (int f = 0; f < 30; f++) yield return null;
            CaptureRig.Hold(30f);                       // Time.timeScale = 0; frames and LateUpdates still run
            for (int f = 0; f < 5; f++) yield return null;

            CaptureRig.Shot(a, p.x, p.z, 15f, 20f, 22f, 1600, 900, p.y + 3f);
            yield return Drain(a);
            if (tanks != null) tanks.enabled = false;
            for (int f = 0; f < 5; f++) yield return null;
            CaptureRig.Shot(b, p.x, p.z, 15f, 20f, 22f, 1600, 900, p.y + 3f);
            yield return Drain(b);
            if (tanks != null) tanks.enabled = true;
            CaptureRig.Release();

            long la = new FileInfo(a).Length, lb = new FileInfo(b).Length;
            TestContext.Out.WriteLine($"with TankRenderer {la} bytes, without {lb} bytes, delta {la - lb}");

            string diff = CaptureRig.Diff(a, b, Path.Combine(dir, "diff.png"));
            TestContext.Out.WriteLine("DIFF: " + diff);
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();
        }

        /// <summary>The shot the following camera cannot give you.
        ///
        /// Three capture sets in, four of the board's lines still could not be scored, and the reason was the shot,
        /// not the rig. A camera that FOLLOWS a machine holds it at the centre of frame, which is precisely the
        /// condition under which a foot sliding along the ground is invisible: foot and background move together on
        /// screen. W1 is "a planted foot does not slide", so the instrument was blind to the thing it was built to
        /// judge.
        ///
        /// This locks the camera off. The focus is a fixed point on the ground and the machine walks THROUGH the
        /// frame past it, so the mud, the stakes and the duckboards are a fixed ruler. A foot that holds still
        /// against that ruler is planted; one that creeps is not.
        ///
        /// Low pitch on purpose, for two reasons: it puts the eye near foot height where ground contact reads, and
        /// it turns the unit disc that TankRenderer draws under every vehicle nearly edge-on. That disc sits exactly
        /// on the foot line and its bloom was covering the feet in every previous set. (It is drawn unconditionally
        /// in DrawRest, not as a selection highlight, so it cannot simply be switched off from here — and
        /// TankRenderer.cs belongs to another session's working tree this week.)</summary>
        [UnityTest, Explicit("Locked-off side-on strip; run by name, with a graphics device.")]
        public IEnumerator EveryWalkerIsPhotographedSideOnAgainstFixedGround()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            for (int f = 0; f < 120; f++) yield return null;

            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            var size = Host.Local.Map.SizeMeters;
            string tag = System.Environment.GetEnvironmentVariable("TW_STILLS_TAG");
            string dir = Dir((string.IsNullOrEmpty(tag) ? "adhoc" : tag) + "-side");

            string warm = Path.Combine(dir, "warmup.png");
            CaptureRig.Shot(warm, size.x * 0.5f, size.y * 0.35f, 26f, 0f, 16f);
            yield return Drain(warm);
            File.Delete(warm); File.Delete(Path.ChangeExtension(warm, ".json"));

            var written = new List<string>();
            int lane = 0;
            foreach (var mk in Machines)
            {
                // Lanes kept well inside the map: at 30 + lane*45 the last lane landed at z 255 on a 280 m map and
                // Redoubt was photographed against the edge ridge with no legs in frame at all.
                float x = size.x * 0.5f, z = 60f + lane * 35f;
                lane++;
                string spawned = TankCapture.Spawn(0, mk.archetype, x, z, 0f);
                Assert.That(spawned, Does.StartWith("slot "), mk.name + ": " + spawned);
                int slot = int.Parse(spawned.Substring(5));
                RiderLab.Drive(slot, 30f);
                for (int f = 0; f < 150; f++) yield return null;

                // THE CAMERA DOES NOT MOVE for this machine. Focus is a fixed patch of ground ahead of it; yaw 0 is
                // side-on to the line of march (the machines walk +z); the aim height is the ground, not the hull.
                // Zoom sets the STANDOFF, and the standoff sets how much track is in shot. At zoom 12 the camera
                // stood 26 m off with a 25-degree lens, so the visible strip of ground was only about 11.6 m wide —
                // narrower than the machine's own walk. It began 14 m left of focus, i.e. off-frame, and nearly
                // filled the picture when it did arrive. Zoom 26 gives roughly 25 m of track: the machine crosses
                // the frame instead of looming in it, and the fixed stakes stay in shot as a ruler the whole time.
                float fz = z + 10f;
                for (int k = 0; k < 12; k++)
                {
                    if (!Host.Local.World.IsAlive(slot)) break;
                    string path = Path.Combine(dir, $"{mk.name}_{k:00}.png");
                    // 2560x1440: the standoff that makes the background a usable ruler also makes the machine a
                    // small part of the frame, and at 1600 wide a critique could not tell which of eight
                    // near-identical legs it was following between frames.
                    // Pin the camera's base yaw before every shot. CaptureRig builds the pose from
                    // `tc.BaseYaw + shot.Yaw`, and the TacticalCamera eases BaseYaw on its own between sets — it
                    // is switched off only for the length of a queue. Measured drift across a strip was up to
                    // 0.0435 degrees, which at this standoff is 0.05-0.17 m of camera movement: the same order as
                    // the foot displacement this shot exists to measure. Pincer always read 0.00000 purely
                    // because it is photographed first, before BaseYaw has moved.
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, x, fz, 26f, 0f, 16f, 2560, 1440, 2.0f);
                    yield return Drain(path);
                    for (int f = 0; f < 6; f++) yield return null;   // ~0.1 s, twelve frames = two gait cycles
                    if (File.Exists(path) && new FileInfo(path).Length > 20000) written.Add(path);
                }

                for (int a = 0; a < 20 && Host.Local.World.IsAlive(slot); a++)
                {
                    RiderLab.Kill(slot);
                    for (int f = 0; f < 10; f++) yield return null;
                }
                for (int f = 0; f < 20; f++) yield return null;
            }

            foreach (var mk in Machines)
                CaptureRig.Sheet(dir, mk.name, Path.Combine(dir, mk.name + "_strip.png"), 4, 900);
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();

            TestContext.Out.WriteLine($"wrote {written.Count} side-on stills to {dir}");
            Assert.That(written.Count, Is.GreaterThanOrEqualTo(Machines.Length * 10),
                "too few side-on frames landed to judge a gait cycle");
        }

        /// <summary>The gait of a drawn machine (TankRenderer's private views, read by reflection as WreckStills does):
        /// its feet are the truth of where a foot stands, which a still alone cannot say.</summary>
        static WalkerGait GaitOf(int slot)
        {
            var r = Object.FindFirstObjectByType<TankRenderer>();
            if (r == null) return null;
            const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;
            var views = r.GetType().GetField("views", Any)?.GetValue(r) as System.Collections.IDictionary;
            if (views == null || !views.Contains(slot)) return null;
            var v = views[slot];
            return v.GetType().GetField("Legs", Any)?.GetValue(v) as WalkerGait;
        }

        // ------------------------------------------------------------------ the Banner's top and guns
        // The owner, 2026-10-08: the top "should also be able to rotate", the guns "need to be cut so they can
        // animate". Parts 1-3 of banner-parts cut them and proved them in EditMode. This is the look.
        //
        // Nothing is posed by hand: the lay comes from the sim, as it does in a battle. RiderLab.Enemies puts
        // riflemen at a bearing off the Banner's nose, TankGunnery lays Gun0 at them, and the top follows. A target
        // 90 degrees off the nose is photographed at the top's STOP, not at 90: TankSpec.Banner's arc is 38 degrees
        // either side and widening it would be a sim change. The recoil frames are shot when the view's own Recoil
        // reads high, so the shutter falls while a barrel is back; CaptureRig poses on one frame and renders on the
        // next, so the barrel has crept a little way home by the time the PNG is written - that is mid-recoil, which
        // is what is wanted.
        //
        // No shield frame: the Banner's sculpt holds no plate (leg 02 looked through all 42 loose pieces), so there
        // is no shield part to move and the standard forbids inventing one. The owner's third wish waits on him.
        // TW_STILLS_DIR says where the stills go (default %TEMP%/tw-bannerparts: never inside the checkout).
        const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;

        /// <summary>TankRenderer's own view of one slot, by reflection (View is a private nested type).</summary>
        static object ViewOf(int slot)
        {
            var r = Object.FindFirstObjectByType<TankRenderer>();
            var views = r?.GetType().GetField("views", Any)?.GetValue(r) as System.Collections.IDictionary;
            return views != null && views.Contains(slot) ? views[slot] : null;
        }

        static float[] ViewArr(object v, string field) => (float[])v.GetType().GetField(field, Any).GetValue(v);

        [UnityTest, Explicit("The Banner's top at 0, 45 and 90 of lay and a barrel mid-recoil; run by name, with a graphics device.")]
        public IEnumerator TheBannersTopAndGunsAreFilmed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            yield return FilmBannerParts();     // a fresh enumerator after the reload (WreckStills' lesson)
        }

        static IEnumerator FilmBannerParts()
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-bannerparts");
            Directory.CreateDirectory(dir);

            // quiet, and a still sky: nobody deploys, nobody shells the lane
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<TW.Sim.Match.AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            var size = Host.Local.Map.SizeMeters;
            float x = size.x * 0.5f, z = 40f;

            string warm = Path.Combine(dir, "warmup.png");
            CaptureRig.Shot(warm, x, z, 20f, 0f, 20f);
            yield return Drain(warm);
            File.Delete(warm); File.Delete(Path.ChangeExtension(warm, ".json"));

            string spawned = TankCapture.Spawn(0, VehicleArchetype.Banner, x, z, 0f);
            Assert.That(spawned, Does.StartWith("slot "), "banner: " + spawned);
            int slot = int.Parse(spawned.Substring(5));
            RiderLab.Stop(slot);                // it stands still: this is about the top, not the walk
            for (int f = 0; f < 120; f++) yield return null;
            var v = ViewOf(slot);
            Assert.That(v, Is.Not.Null, "the Banner is drawn without a view");
            var log = new System.Text.StringBuilder();
            int written = 0;

            // ---- the top, laid by the sim on a target 0, 45 and 90 degrees off the nose
            foreach (float bearing in new[] { 0f, 45f, 90f })
            {
                TestContext.Out.WriteLine(RiderLab.ClearEnemies(slot, 1000f));
                for (int f = 0; f < 60; f++) yield return null;
                if (bearing != 0f) TestContext.Out.WriteLine(RiderLab.Enemies(slot, 4, 45f, bearing));
                // let the traverse finish: the top moves at 34 deg/s, so 38 degrees takes about 1.1 s
                float t0 = Time.time;
                while (Time.time - t0 < 3f) yield return null;
                float lay = ViewArr(v, "GunYaw")[0] * Mathf.Rad2Deg;
                log.AppendLine($"target {bearing:0} degrees off the nose: the top is laid {lay:0.0} degrees");
                foreach (var shotKind in new[] { ("close", 9f, 18f), ("play", 30f, 40f) })
                {
                    var p = Host.Presenter.Drawn(slot);
                    string path = Path.Combine(dir, $"banner_top{bearing:00}_{shotKind.Item1}.png");
                    if (tc != null) tc.BaseYaw = 0f;    // from the Banner's front, so the lay reads as a turn
                    float aimY = shotKind.Item1 == "close" ? RenderGround.Sample(Host.Local.Map, p.x, p.z) + 2.6f : float.NaN;
                    CaptureRig.Shot(path, p.x, p.z, shotKind.Item2, 0f, shotKind.Item3, 1920, 1080, aimY);
                    yield return Drain(path);
                    if (File.Exists(path) && new FileInfo(path).Length > 50000) written++;
                }
            }

            // ---- a barrel mid-recoil: the enemies are still there, so the guns are firing
            var world = Host.Local.World;
            int shots = 0;
            float until = Time.time + 30f;
            while (shots < 3 && Time.time < until && world.IsAlive(slot))
            {
                var rec = ViewArr(v, "Recoil");
                if (Mathf.Max(rec[0], rec[1]) < 0.45f) { yield return null; continue; }
                var p = Host.Presenter.Drawn(slot);
                string path = Path.Combine(dir, $"banner_recoil_{shots:00}.png");
                log.AppendLine($"recoil {rec[0]:0.00} / {rec[1]:0.00} when the shutter was opened");
                if (tc != null) tc.BaseYaw = -90f;      // along the barrels, where the travel shows
                CaptureRig.Shot(path, p.x, p.z, 8f, 0f, 14f, 1920, 1080, RenderGround.Sample(Host.Local.Map, p.x, p.z) + 2.6f);
                yield return Drain(path);
                if (File.Exists(path) && new FileInfo(path).Length > 50000) written++;
                shots++;
                float w0 = Time.time;
                while (Time.time - w0 < 1.2f) yield return null;    // past this gun's return, into the next shot
            }
            log.AppendLine($"{shots} frames caught with a barrel back");
            log.AppendLine("no shield frame: the Banner's sculpt holds no plate, so there is no shield part to move");
            File.WriteAllText(Path.Combine(dir, "banner_parts.txt"), log.ToString());

            CaptureRig.Sheet(dir, "banner", Path.Combine(dir, "banner_parts_sheet.png"), 3, 640);
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();
            TestContext.Out.WriteLine(log.ToString());
            TestContext.Out.WriteLine($"wrote {written} stills of the Banner's parts to {dir}");
            Assert.That(shots, Is.GreaterThanOrEqualTo(1), "the guns never fired, so no barrel was photographed back");
            Assert.That(written, Is.GreaterThanOrEqualTo(7), "too few frames landed to judge the top and the guns");
        }

        [UnityTest, Explicit("One walker, side on, a foot close up and at play zoom; run by name, with a graphics device.")]
        public IEnumerator OneWalkerIsFilmedThroughAStep()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            if (System.Enum.TryParse(System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD"), true, out TW.Presentation.Terrain.Biome field))
            {
                foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
                foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            }
            yield return new EnterPlayMode();
            // everything after the reload is a fresh enumerator (WreckStills: a closure made before it comes back null)
            yield return FilmOne();
        }

        static IEnumerator FilmOne()
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-walkerfilm");
            Directory.CreateDirectory(dir);
            string wanted = System.Environment.GetEnvironmentVariable("TW_STILLS_MACHINE");
            if (string.IsNullOrEmpty(wanted)) wanted = "banner";
            int which = System.Array.FindIndex(Machines, m => m.name == wanted.ToLowerInvariant());
            Assert.That(which, Is.GreaterThanOrEqualTo(0), $"TW_STILLS_MACHINE={wanted} is not one of the six walkers");
            var mk = Machines[which];

            // quiet: nobody deploys, nobody shells the lane
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<TW.Sim.Match.AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            // a lane of open ground 44 m long, the most level one there is: no trench, wire or prop in it. It lies along
            // the map's +x edge, because the camera looks from that side and stands 15 to 40 m off: from outside the map
            // nothing stands between (a lane in the middle was filmed through a wreck and a row of ruins, which are
            // dressing the sim's map does not list)
            var map = Host.Local.Map;
            var size = map.SizeMeters;
            float x = -1f, z = -1f, best = float.MaxValue;
            for (float zz = 20f; zz < size.y - 60f; zz += 2f)
                for (float xx = size.x - 30f; xx <= size.x - 12f; xx += 2f)
                {
                    bool open = true;
                    for (float dz = -4f; dz <= 40f && open; dz += 2f)
                        for (float dx = -6f; dx <= 8f && open; dx += 2f)
                            if ((map.LayerAt(new Unity.Mathematics.float3(xx + dx, 0f, zz + dz)) & (TW.Sim.Terrain.NavLayer.Trench | TW.Sim.Terrain.NavLayer.Link | TW.Sim.Terrain.NavLayer.Blocked | TW.Sim.Terrain.NavLayer.Wire | TW.Sim.Terrain.NavLayer.Bunker)) != 0) open = false;
                    for (int i = 0; i < map.Props.Length && open; i++)
                    {
                        var pp = map.Props[i].Pos;
                        if (pp.x > xx - 7f && pp.z > zz - 6f && pp.z < zz + 42f) open = false;
                    }
                    if (!open) continue;
                    // how far the ground rises and falls along it, where the feet go: a walk is judged on the level
                    float lo = float.MaxValue, hi = float.MinValue;
                    for (float dz = -2f; dz <= 40f; dz += 1f)
                        for (float dx = -4f; dx <= 4f; dx += 2f)
                        {
                            float y = RenderGround.Sample(map, xx + dx, zz + dz);
                            lo = Mathf.Min(lo, y); hi = Mathf.Max(hi, y);
                        }
                    if (hi - lo < best) { x = xx; z = zz; best = hi - lo; }
                }
            // no such lane on this field: the spot the side-on strip uses, whatever stands on it
            if (x < 0f) { x = size.x * 0.5f; z = 60f; TestContext.Out.WriteLine("no open lane: filmed where the side-on strip films"); }
            TestContext.Out.WriteLine($"lane from {x:0}, {z:0} on a {size.x:0} x {size.y:0} m map; its ground rises and falls {best:0.00} m");

            string warm = Path.Combine(dir, "warmup.png");
            CaptureRig.Shot(warm, x, z, 26f, 0f, 16f);
            yield return Drain(warm);
            File.Delete(warm); File.Delete(Path.ChangeExtension(warm, ".json"));

            string spawned = TankCapture.Spawn(0, mk.archetype, x, z, 0f);
            Assert.That(spawned, Does.StartWith("slot "), mk.name + ": " + spawned);
            int slot = int.Parse(spawned.Substring(5));
            // nothing to shoot at: a machine that stops to fire is not walking
            TestContext.Out.WriteLine(RiderLab.ClearEnemies(slot, 1000f));
            RiderLab.Drive(slot, 36f);
            float since = Time.time;
            while (Time.time - since < 4f) yield return null;      // into its stride

            var world = Host.Local.World;
            var gait = GaitOf(slot);
            Assert.That(gait, Is.Not.Null, mk.name + " is drawn without a gait");
            int written = 0;

            // ---- the side row: the camera does not move; the machine walks through the frame past fixed ground
            {
                var p = Host.Presenter.Drawn(slot);
                float fx = p.x, fz = p.z + 2.4f, y0 = RenderGround.Sample(map, p.x, p.z), start = Time.time;
                for (int k = 0; k < 12 && world.IsAlive(slot); k++)
                {
                    while (Time.time < start + k * 0.15f) yield return null;
                    string path = Path.Combine(dir, $"{mk.name}_side_{k:00}.png");
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, fx, fz, 13f, 0f, 18f, 1920, 1080, y0 + 2.0f);
                    yield return Drain(path);
                    if (File.Exists(path) && new FileInfo(path).Length > 20000) written++;
                }
            }

            // ---- a foot close up, from the frame it lands: the rear leg and the front leg of the side the camera sees
            var cam = Camera.main;
            bool leftSeen = cam == null || cam.transform.position.x < Host.Presenter.Drawn(slot).x;
            int perSide = Mathf.Max(1, gait.Feet.Length / 2);
            int rear = leftSeen ? 0 : perSide, front = rear + perSide - 1;
            var log = new System.Text.StringBuilder();
            log.AppendLine($"camera sees the {(leftSeen ? "left" : "right")} side; rear leg {rear}, front leg {front}");
            foreach (var (label, leg) in new[] { ("rear", rear), ("front", front) })
            {
                // wait for this foot to come down (in the air, then planted), three seconds at most
                float t0 = Time.time; bool up = false;
                while (Time.time - t0 < 3f && world.IsAlive(slot))
                {
                    bool air = gait.Feet[leg].Swing >= 0f;
                    if (up && !air) break;
                    up |= air;
                    yield return null;
                }
                Vector3 anchor = gait.Feet[leg].Anchor;
                float start = Time.time;
                for (int k = 0; k < 10 && world.IsAlive(slot); k++)
                {
                    while (Time.time < start + k * 0.12f) yield return null;
                    var ft = gait.Feet[leg];
                    log.AppendLine($"{label} {k} t {Time.time - start:0.00} swing {ft.Swing:0.00} at {ft.At.x:0.000} {ft.At.y:0.000} {ft.At.z:0.000} planted {anchor.x:0.000} {anchor.y:0.000} {anchor.z:0.000} body {Host.Presenter.Drawn(slot).z:0.00} height {gait.Height:0.00}");
                    string path = Path.Combine(dir, $"{mk.name}_{label}_{k:00}.png");
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, anchor.x, anchor.z, 7f, 0f, 12f, 1600, 900, anchor.y + 1.0f);
                    yield return Drain(path);
                    if (File.Exists(path) && new FileInfo(path).Length > 20000) written++;
                }
            }
            File.WriteAllText(Path.Combine(dir, mk.name + "_feet.txt"), log.ToString());

            // ---- and as the game is played: the standard zoom and tilt
            for (int k = 0; k < 2 && world.IsAlive(slot); k++)
            {
                var p = Host.Presenter.Drawn(slot);
                string path = Path.Combine(dir, $"{mk.name}_play_{k:00}.png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(path, p.x, p.z, 30f, 40f, 25f, 1600, 900);
                yield return Drain(path);
                if (File.Exists(path) && new FileInfo(path).Length > 20000) written++;
                float w0 = Time.time;
                while (Time.time - w0 < 0.4f) yield return null;
            }

            foreach (string row in new[] { "side", "rear", "front" })
                CaptureRig.Sheet(dir, mk.name + "_" + row, Path.Combine(dir, $"{mk.name}_{row}_strip.png"), row == "side" ? 4 : 5, 640);
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();
            TestContext.Out.WriteLine($"wrote {written} stills of the {mk.name} to {dir}");
            Assert.That(written, Is.GreaterThanOrEqualTo(30), "too few frames landed to judge a step");
        }

        [UnityTest, Explicit("Enters Play and writes PNGs; run it by name, with a graphics device.")]
        public IEnumerator EveryWalkerIsPhotographedWalkingOnRealGround()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = true;      // every pose and render logged with its frame number

            // open the real scene in edit mode, then play it. Loading it from inside Play would need
            // SceneManager and the scene is not guaranteed to be the one the runner started in.
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();

            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host, Is.Not.Null, "GreyboxCorridor came up without a SimHost");
            Assert.That(Host.Local, Is.Not.Null, "the SimHost never built a match");
            // let the terrain view, the nav fields and the first frames of presentation settle
            for (int f = 0; f < 120; f++) yield return null;

            // GreyboxCorridor is a night field in a squall, and the first run of this came back as 36 photographs of
            // weather: luma_mean 0.12, the machine a smudge. CaptureRig.Hold() pins the sky but also sets
            // Time.timeScale = 0, which would stop the very thing being photographed, so the weather is pinned here
            // by hand and the clock left running. This does not "fix" the lighting — it removes the rain streaks and
            // the gusting so that six frames of one machine differ by the gait and nothing else.
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            for (int f = 0; f < 30; f++) yield return null;

            // one folder per cycle so sets stay comparable: set TW_STILLS_TAG before the run
            var size = Host.Local.Map.SizeMeters;
            string tag = System.Environment.GetEnvironmentVariable("TW_STILLS_TAG");
            string dir = Dir(string.IsNullOrEmpty(tag) ? "adhoc" : tag);

            // The first still of every run came back as an empty field — the rig's first Render of a session lands
            // before the renderers have built anything. Throw one away rather than lose a machine's first frame.
            string warm = Path.Combine(dir, "warmup.png");
            CaptureRig.Shot(warm, size.x * 0.5f, size.y * 0.35f, 15f, 20f, 22f);
            yield return Drain(warm);
            File.Delete(warm);
            File.Delete(Path.ChangeExtension(warm, ".json"));
            var written = new List<string>();
            var missing = new List<string>();

            int lane = 0;
            foreach (var mk in Machines)
            {
                // Each machine gets its OWN LANE, well outside the ~17 m the frame covers at zoom 15.
                // Killing the previous subject is not enough and the reason is in TankRenderer.Draw: it iterates
                // `wrecks` as well as live views, and a despawn without a VehicleDestroyed event goes through
                // Wreckify, so every kill leaves a burnt-out hull standing on the spot. Six subjects spawned at one
                // point meant every machine after the first was photographed inside a growing pile of its
                // predecessors' wrecks. The kills were succeeding the whole time — "slot 0 destroyed", six times.
                //
                // The lanes must run along Z. This is a CORRIDOR: about 90 m wide and 280 m long. A first attempt
                // spaced them 70 m apart in x and clamped to the map, so every machine after the first landed on
                // the same spot again and the sheet came back looking identical to the one before it.
                float x = size.x * 0.5f, z = 30f + lane * 45f;
                lane++;
                string spawned = TankCapture.Spawn(0, mk.archetype, x, z, 0f);
                if (!spawned.StartsWith("slot "))
                {
                    missing.Add(mk.name + ": " + spawned);
                    continue;
                }
                int slot = int.Parse(spawned.Substring(5));
                TankCapture.Follow(slot, 24f, 32f);
                RiderLab.Drive(slot, 15f);   // up the corridor, but not far enough to enter the next lane
                // Two seconds of gait before the first frame — and WAIT FOR THE MODEL. pincer_00 of set c2 came
                // back with no machine in it at all: the first subject of a run is photographed while the renderer
                // is still building its view, so the still is of an empty field.
                for (int f = 0; f < 180; f++) yield return null;

                // Six frames along the walk, each framed on the machine where it actually is. Series() takes one
                // fixed focus, which would let a walking machine stroll out of shot, so the shots are queued one at
                // a time against a re-read position.
                for (int k = 0; k < 6; k++)
                {
                    var w = Host.Local.World;
                    if (!w.IsAlive(slot)) { missing.Add($"{mk.name}: died before frame {k}"); break; }
                    var p = w.Position[slot];
                    // 20 degrees off the line of march: side-on enough to read pitch, angled enough to read roll
                    string path = Path.Combine(dir, $"{mk.name}_{k:00}.png");
                    // Zoom and pitch are the FIRST loop's proven numbers (Captures/rigloop/L1_*.json: zoom 34
                    // pitch 22, and a8/look.json: zoom 30 pitch 25), pulled in to 15 so one machine fills the frame
                    // instead of the county. Two earlier attempts here were wrong in opposite directions and both
                    // are worth not repeating: zoom 22 put the machine at ~40 px of a 1600 px frame, and zoom 8 put
                    // the camera 5 m off the ground — below the hull of a 300-tonne machine, photographing its
                    // unlit belly. Pitch must stay near 22: it is what gets light on the top surfaces.
                    // aim at the machine's own hull, a little above its feet. Without this the rig looks at world
                    // y = 0 at any zoom >= CloseZoom and a machine standing on raised ground rides off the top.
                    CaptureRig.Shot(path, p.x, p.z, 15f, 20f, 22f, 1600, 900, p.y + 3f);
                    yield return Drain(path);
                    // 7 frames, not 22. A step takes roughly a third of a second and 22 frames IS roughly a third
                    // of a second, so the set was sampled at very nearly the gait period and every frame caught the
                    // legs at the same phase. Six photographs of one pose, from a machine that had in fact walked
                    // 4.7-6.2 m across the sequence — a blind critique called five of six rows frozen and it was
                    // the sampling, not the rig. 7 frames is ~0.12 s, so six frames span about two gait cycles.
                    for (int f = 0; f < 7; f++) yield return null;

                    if (File.Exists(path) && new FileInfo(path).Length > 20000) written.Add(path);
                    else missing.Add($"{mk.name} frame {k}: "
                        + (File.Exists(path) ? "only " + new FileInfo(path).Length + " bytes" : "no file"));
                }

                // Kill can fail and SAY so — RiderLab.Kill returns "worlds a tick apart: try again" when the two
                // sim worlds are not aligned — and the first version of this ignored the return. The result was
                // that every machine after the first was photographed with its predecessor still standing in the
                // shot, close enough to interleave limbs with it. Pincer, captured first, was the only clean row,
                // and a blind critique correctly called two of six rows unreviewable. Retry until it is gone.
                for (int attempt = 0; attempt < 20 && Host.Local.World.IsAlive(slot); attempt++)
                {
                    TestContext.Out.WriteLine($"  kill {mk.name}: {RiderLab.Kill(slot)}");
                    for (int f = 0; f < 10; f++) yield return null;
                }
                Assert.That(Host.Local.World.IsAlive(slot), Is.False,
                    $"{mk.name} would not die and will pollute every later shot");
                for (int f = 0; f < 20; f++) yield return null;
            }

            // One contact sheet, so a critique can be handed a single picture of the whole field. Sheet() globs
            // `stem + "_*.png"` and composes synchronously, so the stem is a wildcard and there is nothing to wait for.
            string made = CaptureRig.Sheet(dir, "*", Path.Combine(dir, "sheet.png"), 6, 460);
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;

            yield return new ExitPlayMode();

            TestContext.Out.WriteLine($"wrote {written.Count} stills to {dir}");
            TestContext.Out.WriteLine("sheet: " + made);
            foreach (var m in missing) TestContext.Out.WriteLine("  MISSING " + m);
            Assert.That(missing, Is.Empty, "not every machine was photographed");
            Assert.That(written.Count, Is.EqualTo(Machines.Length * 6));
        }
    }
}
