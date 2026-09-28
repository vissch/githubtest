// Phase: wrecks (2026-09-28, tooling) — the look at a wreck breaking in stages (TankRenderer.WreckStages), from batch
// mode with nobody driving, the way WalkerStills photographs the walkers (its header says why this is an [Explicit]
// EditMode test that enters Play itself). A machine is killed by a shell inside a tick (a Despawn inside WriteWorlds
// would lose its VehicleDestroyed to the next Step, and with it the wreck prop), then its wreck is shelled through
// every stage: worn, broken, worn, scrap, cleared, and filmed after each hit.
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter WreckStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-wreckstills: never inside the checkout),
// TW_WRECK_MACHINE the archetype to wreck (default the Maw, 4).
using System.Collections;
using System.IO;
using NUnit.Framework;
using Unity.Mathematics;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.UI;

namespace TW.Tests
{
    public class WreckStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        static void Shell(float x, float z, float damage, float radius = 6f) => TestContext.Out.WriteLine(WreckLab.Shell(x, z, damage, radius));

        /// <summary>What TankRenderer holds for a slot (its private views, read by reflection: a still is no use when the
        /// machine in it is not drawn, and this says why).</summary>
        static string Drawn(int slot)
        {
            var r = Object.FindFirstObjectByType<TW.Presentation.Tactical.TankRenderer>();
            if (r == null) return "no TankRenderer";
            const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;
            var views = r.GetType().GetField("views", Any)?.GetValue(r) as System.Collections.IDictionary;
            var wrecks = r.GetType().GetField("wrecks", Any)?.GetValue(r) as System.Collections.IList;
            string s = $"TankRenderer ready {r.Ready}, enabled {r.enabled}, {views?.Count} views, {wrecks?.Count} wrecks";
            if (views != null && views.Contains(slot))
            {
                var v = views[slot];
                string F(string n) => v.GetType().GetField(n, Any)?.GetValue(v)?.ToString() ?? "?";
                var model = v.GetType().GetField("Model", Any)?.GetValue(v);
                s += $"; slot {slot}: pos {F("Pos")}, dead {F("Dead")}, seen {F("Seen")}, model {model?.GetType().GetField("Name", Any)?.GetValue(model) ?? "null"}";
            }
            else s += $"; no view for slot {slot}";
            return s;
        }

        [UnityTest, Explicit("A wreck shelled through its stages, photographed; run by name, with a graphics device.")]
        public IEnumerator AWreckIsPhotographedThroughEveryStage()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            // everything after the reload is a fresh enumerator: Film's captured locals live in a closure object, and one
            // made before the domain reload comes back null
            yield return InPlay();
        }

        static IEnumerator InPlay()
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            // read AFTER entering Play: the domain reloads there, and locals set before it come back as defaults
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-wreckstills");
            Directory.CreateDirectory(dir);
            int archetype = int.TryParse(System.Environment.GetEnvironmentVariable("TW_WRECK_MACHINE"), out int a) ? a : VehicleArchetype.Maw;
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            // open ground nearest the middle of the map: no trench, wire or blocked cell within 10 m and no prop within 12 m
            // (the first set stood it at the map's edge behind a hut, where none of it could be seen)
            float x = -1f, z = -1f, best = float.MaxValue;
            float2 middle = new float2(size.x * 0.5f, size.y * 0.5f);
            for (float zz = 30f; zz < size.y - 30f; zz += 4f)
                for (float xx = 30f; xx < size.x - 30f; xx += 4f)
                {
                    float d = math.distancesq(new float2(xx, zz), middle);
                    if (d >= best) continue;
                    bool open = true;
                    for (float dz = -10f; dz <= 10f && open; dz += 2f)
                        for (float dx = -10f; dx <= 10f && open; dx += 2f)
                            if ((map.LayerAt(new float3(xx + dx, 0f, zz + dz)) & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) open = false;
                    for (int i = 0; i < map.Props.Length && open; i++) if (math.distancesq(map.Props[i].Pos.xz, new float2(xx, zz)) < 144f) open = false;
                    if (open) { x = xx; z = zz; best = d; }
                }
            Assert.That(x, Is.GreaterThan(0f), "no open ground for the machine");
            TestContext.Out.WriteLine($"open ground at {x:0}, {z:0} on a {size.x:0} x {size.y:0} m map");
            string spawned = TankCapture.Spawn(0, archetype, x, z, 30f);
            Assert.That(spawned, Does.StartWith("slot "), spawned);
            int slot = int.Parse(spawned.Substring(5));
            RiderLab.Stop(slot);
            // batch-mode frames are far shorter than a tick: wait for ticks, until the presenter draws the machine where
            // the sim has it (the first set killed it 60 frames after the spawn, still drawn blending in from the map's
            // corner, and its wreck was left there)
            float spawnedAt = Time.time;
            for (int f = 0; f < 3000 && (Time.time - spawnedAt < 1.5f || math.distance(Host.Presenter.Drawn(slot).xz, Host.Local.World.Position[slot].xz) > 0.2f); f++) yield return null;
            TestContext.Out.WriteLine($"slot {slot} at {Host.Local.World.Position[slot].x:0.0}, {Host.Local.World.Position[slot].z:0.0}, drawn as a machine: {SceneHooks.IsTankSlot?.Invoke(slot)}");
            TestContext.Out.WriteLine(Drawn(slot));

            int shot = 0;
            IEnumerator Film(string label, float seconds, int frames)
            {
                float start = Time.time;
                for (int k = 0; k < frames; k++)
                {
                    float due = start + seconds * k / Mathf.Max(1, frames - 1);
                    while (Time.time < due) yield return null;
                    string path = Path.Combine(dir, $"wreck_{shot++:00}_{label}.png");
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Shot(path, x, z, 16f, 40f, 34f, 1280, 720, 1.5f);
                    yield return Drain(path);
                }
            }

            yield return Film("alive", 0.1f, 1);
            string close = Path.Combine(dir, "close_alive.png");
            if (tc != null) tc.BaseYaw = -90f;
            CaptureRig.Shot(close, x, z, 7f, 40f, 34f, 1280, 720, 1.5f);
            yield return Drain(close);
            TestContext.Out.WriteLine(Drawn(slot));
            // the kill: obliterated, it cooks off, and its burst comes the next tick (it spares its own wreck)
            Shell(x, z, 50000f, 4f);
            for (int f = 0; f < 30 && Host.Local.World.IsAlive(slot); f++) yield return null;
            Assert.IsFalse(Host.Local.World.IsAlive(slot), "the machine died");
            yield return Film("killed", 3f, 4);
            int prop = -1;
            for (int i = 0; i < map.Props.Length; i++) if (PropRules.IsWreckage(map.Props[i].Kind) && math.distance(map.Props[i].Pos.xz, new float2(x, z)) < 6f) prop = i;
            Assert.That(prop, Is.GreaterThanOrEqualTo(0), "its wreck prop");
            TestContext.Out.WriteLine($"wreck prop {prop}: {map.Props[prop].Kind}, hp {map.Props[prop].Hp:0}");
            // worn, broken, worn, scrap, cleared: each hit a little off the wreck's middle, from the same side
            float[] hits = { 420f, 700f, 330f, 500f, 500f };
            string[] labels = { "worn", "broken", "worn2", "scrap", "cleared" };
            for (int h = 0; h < hits.Length; h++)
            {
                Shell(x - 3f, z + 1f, hits[h]);
                for (int f = 0; f < 4; f++) yield return null;
                TestContext.Out.WriteLine($"after {labels[h]}: {map.Props[prop].Kind}, hp {map.Props[prop].Hp:0}");
                yield return Film(labels[h], 3.5f, 5);
            }
            yield return Film("after", 6f, 3);
            CaptureRig.Sheet(dir, "wreck", Path.Combine(dir, "wreck_sheet.png"), 5, 400);
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            Assert.AreEqual(PropKind.Cleared, map.Props[prop].Kind, "shelled through every stage to nothing");
            yield return new ExitPlayMode();
        }
    }
}
