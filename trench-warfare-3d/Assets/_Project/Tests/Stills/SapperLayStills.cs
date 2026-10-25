// Phase: sapper-lay-mine-anim-g1 (2026-10-25) — the sapper-lay-mine-anim still was a second standing body behind
// the first (the camera stood 22.4 m off, not the ~5 m the old call thought it asked for: CaptureRig.Pose clamps
// Zoom to tc.NearZoom, and NearZoom with the stock CloseFitHeight/ClosePitch/CloseFov works out to 14.9, not 6), and
// no mine was ever on the ground (MinePlaced, which draws it, fires SapperSystem.LaySeconds = 3 s after
// SapperLaying; the old shot was at 1.5 s). This version: shrinks NearZoom for the set so the lock-off is really
// close, picks a lit cell near a lamp with nobody else nearby so one figure cannot be mistaken for two, waits for
// the sim to say Laying before shooting, and holds for a MinePlaced event before the third shot.
//
// Run it by name WITH a graphics device (not -nographics), or `leg play SapperLayStills`:
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter SapperLayStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-sapperlay: never inside the checkout, whose
// untracked files land.py would see).
using System.Collections;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.Sim.Units;
using Unity.Mathematics;
using TW.UI;

namespace TW.Tests
{
    public class SapperLayStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        // Lit (within 4 m of a lamp) and alone (no living unit of either side within 25 m): what makes a night
        // still readable, and what keeps a second body out of frame (the fault this rewrite exists to fix).
        static bool Open(MapData map, SimWorld world, System.Collections.Generic.IReadOnlyList<Vector3> lamps, float x, float z)
        {
            var size = map.SizeMeters;
            // the lock-off camera stands ~5 m back on -Z (YawPin 0, pitch 13) from the focus cell: check the ground
            // it stands on and looks across, not just the cell under the sapper's feet, or a crate the camera parks
            // inside of (found 2026-10-25: a corner's bunker roof filled the whole frame) never shows up here.
            for (float zz = z - 9f; zz <= z + 6f; zz += 2f)
                for (float xx = x - 6f; xx <= x + 6f; xx += 2f)
                {
                    if (xx < 3f || zz < 3f || xx > size.x - 3f || zz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(xx, 0f, zz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = map.Props[i].Pos;
                if (q.x > x - 6f && q.x < x + 6f && q.z > z - 9f && q.z < z + 6f) return false;
            }
            // 3-7 m from a lamp: close enough to be lit (LanternRange 10 m) but standing clear of the composed site
            // (dugout, shelter, store) the lamp is posted at — a lamp's own doorway is lit AND cluttered, which
            // filled the whole frame at 4 m (found 2026-10-25, idle.png: a sandbag wall and a crate roof, nothing else).
            bool lit = false;
            for (int i = 0; i < lamps.Count; i++)
            {
                var l = lamps[i];
                float dx = l.x - x, dz = l.z - z;
                float d2 = dx * dx + dz * dz;
                if (d2 >= 6f * 6f && d2 <= 10f * 10f) { lit = true; break; }
            }
            if (!lit) return false;
            for (int i = 0; i < world.HighWater; i++)
            {
                if ((world.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                var p = world.Position[i];
                float dx = p.x - x, dz = p.z - z;
                if (dx * dx + dz * dz <= 25f * 25f) return false;
            }
            return true;
        }

        [UnityTest, Explicit("The sapper's kneel-and-place photographed; run by name, with a graphics device.")]
        public IEnumerator SapperLayingIsPhotographed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");

            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-sapperlay");
            Directory.CreateDirectory(dir);

            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            // stilled weather: rain and squalls move every frame, which would swamp the pose shift the probe looks for
            var sky = Object.FindFirstObjectByType<Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;

            var night = Object.FindFirstObjectByType<NightLights>();
            for (int f = 0; f < 300 && (night == null || !night.Built); f++) yield return null;
            Assert.That(night != null && night.Built, "the night lamps never built: no lit spot to pick");

            var map = Host.Local.Map;
            var world = Host.Local.World;
            var lamps = night.LampPoints;
            var size = map.SizeMeters;
            float x = -1f, z = -1f;
            // open ground well clear of the corners (the home trench's garrison and its own bunker roof sit right by
            // the map's low corner, which read as "nobody within 25 m, no prop in the box" and was neither)
            for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 2f)
                for (float xx = 8f; xx < size.x - 8f && x < 0f; xx += 2f)
                    if (Open(map, world, lamps, xx, zz)) { x = xx; z = zz; }
            Assert.That(x, Is.GreaterThanOrEqualTo(0f), "no lit, isolated ground for the sapper");

            // the camera this set shoots through: near enough that the man fills the frame instead of being one
            // of two indistinguishable smudges 22 m off (the old still's fault). Put back after the set.
            var tc = Object.FindFirstObjectByType<TacticalCamera>();
            Assert.That(tc, Is.Not.Null, "no TacticalCamera in the scene");
            float keepZoomMin = tc.ZoomMin, keepCloseFitHeight = tc.CloseFitHeight;
            tc.ZoomMin = 2f; tc.CloseFitHeight = 3f;
            CaptureRig.Rig.YawPin = 0f;

            try
            {
                string s = TankCapture.Spawn(0, (int)InfantryArchetype.Sapper, x, z, 0f);
                Assert.That(s, Does.StartWith("slot"), s);
                int slot = int.Parse(s.Substring(5));
                for (int f = 0; f < 30; f++) yield return null;

                // aim at where the sim actually put him, not the scanned cell: a nav snap moves him up to a cell
                // (NavCellSize 2 m), which at this close a view (frame ~4 m wide) is enough to push him off-frame.
                var spawnPos = Host.Local.World.Position[slot];
                x = spawnPos.x; z = spawnPos.z;

                const float zoom = 3.4f, pitch = 13f, aimY = 0.9f;
                string idle = Path.Combine(dir, "idle.png");
                CaptureRig.Shot(idle, x, z, zoom, 0f, pitch, 1280, 720, aimY);
                yield return Drain(idle);

                // 1.5 m ahead of him (he faces +Z, spawned at yaw 0), not his own cell: at this close a view his own
                // boots cover a 0.45 m disc entirely (found 2026-10-25, placed.png: nothing to see under a kneeling
                // man standing on top of his own mine). He steps to it and kneels, same as ProvingGround.OrderSappers.
                var w = Host.Local.World;
                var sapper = w.GetSystem<SapperSystem>();
                int laidBefore = sapper.Laid;
                var layAt = w.Position[slot] + new float3(0f, 0f, 1.5f);
                Host.LocalDriver.Issue(new SimCommand
                {
                    Tick = w.Tick, Player = 0, Type = CommandType.UnitAbility,
                    A = slot, B = (int)UnitAbilityId.LayMine, Pos = layAt
                });

                // don't shoot until the sim itself says he is laying: a command issued for the current tick can be
                // dropped, and a rejected order never gets a Laying phase at all.
                int waited = 0;
                for (; waited < 180 && sapper.Phase[slot] != SapperSystem.Laying; waited++) yield return null;
                Assert.That(sapper.Phase[slot], Is.EqualTo(SapperSystem.Laying),
                    "the order never reached Laying: Phase=" + sapper.Phase[slot] + " Ordered=" + sapper.Ordered
                    + " Laid=" + sapper.Laid + " charges=" + sapper.ChargesOf(w, slot));

                float start = Time.time;
                while (Time.time - start < 1.5f) yield return null;
                string laying = Path.Combine(dir, "laying.png");
                CaptureRig.Shot(laying, x, z, zoom, 0f, pitch, 1280, 720, aimY);
                yield return Drain(laying);

                // MinePlaced (SapperSystem.Laid incrementing is the sim's own record of it; the event itself is a
                // transient one-tick buffer that a poll a frame late never sees) draws the disc (CombatFx.Mines.cs).
                // Wait for it, then a few more frames for the FX pipeline to pick the mark up and draw it.
                for (int f = 0; f < 300 && sapper.Laid == laidBefore; f++) yield return null;
                Assert.That(sapper.Laid, Is.GreaterThan(laidBefore), "the mine was never laid: Ordered=" + sapper.Ordered);
                for (int f = 0; f < 10; f++) yield return null;
                string placedPath = Path.Combine(dir, "placed.png");
                CaptureRig.Shot(placedPath, x, z, zoom, 0f, pitch, 1280, 720, aimY);
                yield return Drain(placedPath);

                TestContext.Out.WriteLine("clip: " + Clip.KneelInspect + " / " + Clip.StandToKneel);
                Atmosphere.PinnedClock = -1f;
                Time.captureDeltaTime = 0f;
                tc.ZoomMin = keepZoomMin; tc.CloseFitHeight = keepCloseFitHeight;   // restore before the scene (and tc) go away
                CaptureRig.Rig.YawPin = float.NaN;
                yield return new ExitPlayMode();

                Assert.That(File.Exists(idle), "idle still missing");
                Assert.That(File.Exists(laying), "laying still missing");
                Assert.That(File.Exists(placedPath), "placed still missing");
            }
            finally
            {
                CaptureRig.Rig.YawPin = float.NaN;
            }
        }
    }
}
