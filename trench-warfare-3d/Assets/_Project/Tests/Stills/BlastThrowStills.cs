// Phase: blast knockback (2026-10-20, tooling) — the throw a player can actually see, filmed back to back so the
// flight is sampled, not jumped over. Copied in shape from DeathStills (read its header for why it is an [Explicit]
// EditMode test that enters Play itself and needs a graphics device).
//
// It does NOT use DeathLab.Scene("shell"): that stages the row and queues the shell in one call, so there is no
// standing frame before the burst. Here each of three captures stands the row, takes a stand still, then shells it
// and films twelve stills back to back (Gap 0) with Time.captureDeltaTime at 1/60, so the strip samples about every
// 0.15 s of game time and the flight gets several samples. A last still follows 1.5 s after the strip,
// c{c}_settled.png: the frame after the bodies have stopped moving.
//
// Every shot is the player's standard view: zoom 30 (T1, the zoom the game is played at), the base yaw pinned first.
// Night is the scene's own field (GreyboxCorridor is NightMud): TW_STILLS_FIELD is deliberately not read. Rain is off.
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter BlastThrowStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-blastthrow: never inside the checkout, whose
// untracked files land.py would see).
// It asserts only that the 45 stills landed: it is an instrument; the pictures go to a critique. strip.txt in the
// stills dir records each still's Time.time, so a reader can say which frame is which moment.
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using Unity.Mathematics;
using TW.UI;

namespace TW.Tests
{
    public class BlastThrowStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        const int Captures = 3;
        const int StripFrames = 12;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>A patch of open ground for a row: no trench, ladder, wire, bunker or blocked cell, and no prop on the
        /// row's own strip (DeathStills.Open, the same rule).</summary>
        static bool Open(MapData map, float x, float z, float w, float back, float ahead)
        {
            var size = map.SizeMeters;
            for (float zz = z - back - 4f; zz <= z + ahead + 4f; zz += 2f)
                for (float xx = x - 4f; xx <= x + w + 4f; xx += 2f)
                {
                    if (xx < 3f || zz < 3f || xx > size.x - 3f || zz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(xx, 0f, zz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = map.Props[i].Pos;
                if (q.x > x - 3f && q.x < x + w + 3f && q.z > z - 4f && q.z < z + 4f) return false;
            }
            return true;
        }

        [UnityTest, Explicit("The blast throw photographed at the player's zoom; run by name, with a graphics device.")]
        public IEnumerator TheBlastThrowIsPhotographedAtThePlayersZoom()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            // night is the scene's own field (NightMud): nothing is overridden here, on purpose
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            // read AFTER entering Play: the domain reloads there, and locals set before it come back as defaults
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-blastthrow");
            Directory.CreateDirectory(dir);
            // quiet: no enemy deploys or attacks, no stray shells, no rain (the owner named rain off)
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            // a still costs frames, not seconds: at 1/60 a back-to-back strip samples about every 0.15 s of game time,
            // so the flight gets several samples (at DeathStills' 1/30 the stills land 0.33 s apart and the arc falls
            // between them)
            Time.captureDeltaTime = 1f / 60f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            int written = 0;
            var used = new List<Vector2>();
            var strip = new StringBuilder();
            for (int c = 1; c <= Captures; c++)
            {
                // an open spot 20x24 m clear of the ones already used, the way DeathStills finds them
                float x = -1f, z = -1f;
                for (float zz = 40f; zz < size.y - 50f && x < 0f; zz += 6f)
                    for (float xx = 8f; xx < size.x - 24f && x < 0f; xx += 6f)
                    {
                        bool clear = true;
                        foreach (var u in used) if (Mathf.Abs(u.x - xx) < 20f && Mathf.Abs(u.y - zz) < 24f) clear = false;
                        if (clear && Open(map, xx, zz, 14f, 5f, 5f)) { x = xx; z = zz; }
                    }
                if (x < 0f) { TestContext.Out.WriteLine("capture " + c + ": no open ground left"); continue; }
                used.Add(new Vector2(x, z));

                // stand the row and let the men be drawn, THEN take the standing frame: a player's before
                TestContext.Out.WriteLine($"c{c} row at {x}, {z}: " + DeathLab.Row(6, x, z, 0, 3f, 100f));
                for (int f = 0; f < 30; f++) yield return null;
                string stand = Path.Combine(dir, $"c{c}_stand.png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(stand, x + 7.5f, z, 30f, 40f, 25f, 1280, 720);
                yield return Drain(stand);
                if (File.Exists(stand)) written++;
                strip.AppendLine($"c{c}_stand.png\t{Time.time.ToString("0.###", CultureInfo.InvariantCulture)}");
                TestContext.Out.WriteLine($"c{c}_stand.png at t={Time.time:0.###}");

                // the shell, then the strip: back to back, no gap, so the flight is sampled
                TestContext.Out.WriteLine($"c{c} shell: " + DeathLab.Shell(x + 7.5f, z - 2f, 8f, 600f));
                for (int k = 0; k < StripFrames; k++)
                {
                    string path = Path.Combine(dir, $"c{c}_{k:00}.png");
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, x + 7.5f, z, 30f, 40f, 25f, 1280, 720);
                    yield return Drain(path);
                    if (File.Exists(path)) written++;
                    strip.AppendLine($"c{c}_{k:00}.png\t{Time.time.ToString("0.###", CultureInfo.InvariantCulture)}");
                    TestContext.Out.WriteLine($"c{c}_{k:00}.png at t={Time.time:0.###}");
                }
                // and a frame after they have stopped: ThrowPath's bounce and skid can still be running at the end of
                // the strip's ~1.8 s window, so let 1.5 s of game time pass (90 frames at 1/60) and take one more
                for (int f = 0; f < 90; f++) yield return null;
                string settled = Path.Combine(dir, $"c{c}_settled.png");
                if (tc != null) tc.BaseYaw = -90f;
                CaptureRig.Shot(settled, x + 7.5f, z, 30f, 40f, 25f, 1280, 720);
                yield return Drain(settled);
                if (File.Exists(settled)) written++;
                strip.AppendLine($"c{c}_settled.png	{Time.time.ToString("0.###", CultureInfo.InvariantCulture)}");
                TestContext.Out.WriteLine($"c{c}_settled.png at t={Time.time:0.###}");
                for (int f = 0; f < 60; f++) yield return null;
            }
            File.WriteAllText(Path.Combine(dir, "strip.txt"), strip.ToString());
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            Time.captureDeltaTime = 0f;
            yield return new ExitPlayMode();
            TestContext.Out.WriteLine($"wrote {written} stills to {dir}");
            Assert.That(written, Is.EqualTo(Captures * (StripFrames + 2)), "a still of the strip is missing");
        }
    }
}
