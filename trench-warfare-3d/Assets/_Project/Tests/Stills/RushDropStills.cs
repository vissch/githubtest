// Phase: act-rush-and-drop (2026-10-23) - the evidence rig for the rush and drop under fire. A stress match really
// sends men across the open into machine-gun fire, so MovementSystem gives them the rush beat (StanceRules.RushTicks,
// RushUpTicks) that this unit draws: bent double on the dash, a dive flat into the hole. InfantryFiringStills cannot
// be used for it - it walks its squad by hand through SimHost.WriteWorlds, so the movement system never gives those
// men a rush at all.
//
// Two passes, one file stem each: night (Biome.NightMud, the scene's own look) and day (Biome.Winter), both at the
// zoom the game is played at. Each pass also COUNTS what its pictures claim, so the owner's two numbers are measured
// and not eyeballed, and asserts them: over alarmed men in the open, at least 70 % of the man-ticks in the rush up
// are drawn in a crouched row, and the drop reaches a prone row within 10 ticks (0.5 s at the sim's 20 ticks a
// second). The two lines go to measured.<variant>.json beside the stills.
//
// Run it by name WITH a graphics device (not -nographics):
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter RushDropStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-rushdropstills: never inside the checkout),
// TW_RUSHDROP_STRESS men a side (300), TW_RUSHDROP_SETTLE ticks before the counting starts (1200),
// TW_RUSHDROP_FILM ticks counted (1200 = 60 s).
using System.Collections;
using System.Globalization;
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
using TW.UI;

namespace TW.Tests
{
    public class RushDropStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        const int DropBudgetTicks = 10;      // the owner's half second, at 20 ticks a second
        const float CrouchedShare = 0.70f;   // the owner's 70 %
        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;
        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        static string Env(string k, string d) { string v = System.Environment.GetEnvironmentVariable(k); return string.IsNullOrEmpty(v) ? d : v; }

        static string Dir()
        {
            string dir = Env("TW_STILLS_DIR", Path.Combine(Path.GetTempPath(), "tw-rushdropstills"));
            Directory.CreateDirectory(dir);
            return dir;
        }

        // EnterPlayMode is yielded straight from the [UnityTest] method, not from a called helper: a level of
        // enumerator nesting round it breaks Unity's play-mode-state detection (TracerStills' note, it cost time).
        [UnityTest, Order(1), Explicit("The rush and drop at night; run by name, with a graphics device.")]
        public IEnumerator Night_RushAndDrop_IsFilmedAndCounted()
        {
            Setup(Biome.NightMud);
            yield return new EnterPlayMode();
            yield return InPlay("night");
        }

        [UnityTest, Order(2), Explicit("The rush and drop by day; run by name, with a graphics device.")]
        public IEnumerator Day_RushAndDrop_IsFilmedAndCounted()
        {
            Setup(Biome.Winter);
            yield return new EnterPlayMode();
            yield return InPlay("day");
        }

        static void Setup(Biome look)
        {
            System.Environment.SetEnvironmentVariable("TW_KNOBS", "stress.mix=1");
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            foreach (var g in Object.FindObjectsByType<GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = look;
            foreach (var at in Object.FindObjectsByType<Atmosphere>(FindObjectsSortMode.None)) at.Field = look;
            var host = Host;
            Assert.That(host, Is.Not.Null, "no SimHost in the scene");
            host.StressUnits = int.Parse(Env("TW_RUSHDROP_STRESS", "300"), Inv);
        }

        static IEnumerator InPlay(string variant)
        {
            for (int f = 0; f < 2000 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = Dir();
            var info = new System.Text.StringBuilder();
            void Say(string s) { info.AppendLine(s); File.WriteAllText(Path.Combine(dir, "info." + variant + ".txt"), info.ToString()); Debug.Log("[rushdropstills] " + s); }
            CombatFx.ShowOverlays = false;
            Say($"variant {variant}: knobs {Knobs.ToJson()}, seed {Host.Seed}, battlefield seed {Host.BattlefieldSeed}");

            var sky = Object.FindFirstObjectByType<Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            Atmosphere.PinnedClock = variant == "night" ? 2f : 11f;
            // three frames to a sim tick while counting: at 1/30 s a frame can carry two ticks and the tick the man
            // first shows flat is then missed, which reads a clean 10 as 11 (it did, on the first run here)
            Time.captureDeltaTime = 1f / 60f;

            // a stray shell's own dive (Rung.Reaction) would win over the rush dive and spoil both the count and a frame
            var stray = Host.Local.World.GetSystem<AmbientBombardmentSystem>();
            if (stray != null) stray.ShellsPerMinute = 0f;

            int settle = int.Parse(Env("TW_RUSHDROP_SETTLE", "1200"), Inv);
            Host.TimeScale = 8f;
            while (Host.Local.World.Tick < (uint)settle) yield return null;
            Host.TimeScale = 1f;
            Say($"settled at tick {Host.Local.World.Tick}, alive {Host.Local.World.AliveCount} (the stress side went over at tick {Host.StressAdvanceDelayTicks})");

            // ---- the counted stretch: one sample a sim tick, over alarmed men in the open ----
            int film = int.Parse(Env("TW_RUSHDROP_FILM", "1200"), Inv);   // 60 s: 400 gave only 3 drops to measure
            var world = Host.Local.World;
            int slots = Host.Animation.State.Length;
            var wasStance = new byte[slots];
            var dropAt = new uint[slots];              // the tick the sim dropped him, 0 for nobody falling
            for (int i = 0; i < slots; i++) { wasStance[i] = 255; dropAt[i] = 0u; }
            long upCrouched = 0, upTotal = 0, drops = 0, worst = -1;
            var trail = new System.Collections.Generic.Dictionary<int, System.Text.StringBuilder>();
            var slowest = new System.Text.StringBuilder();
            uint lastTick = world.Tick, until = world.Tick + (uint)film;
            while (world.Tick < until)
            {
                yield return null;
                if (world.Tick == lastTick) continue;
                lastTick = world.Tick;
                var anim = Host.Animation.State;
                for (int i = 0; i < world.HighWater && i < slots; i++)
                {
                    uint f = world.Flags[i];
                    bool open = (f & (uint)UnitFlags.Alive) != 0 && (f & (uint)UnitFlags.Vehicle) == 0
                                && (f & (uint)UnitFlags.InTrench) == 0;
                    byte now = world.StanceOf[i];
                    if (!open || world.Alarm[i] < StanceRules.CarefulAlarm) { wasStance[i] = now; dropAt[i] = 0u; continue; }

                    if (now == (byte)Stance.Crouch || now == (byte)Stance.Sprint)
                    {
                        upTotal++;
                        if (anim[i].Stance == (byte)Stance.Crouch) upCrouched++;
                    }
                    // the sim's switch into the hole: from the rush up straight onto his belly
                    if (now == (byte)Stance.Prone && (wasStance[i] == (byte)Stance.Crouch || wasStance[i] == (byte)Stance.Sprint))
                    {
                        dropAt[i] = world.Tick;
                        trail[i] = new System.Text.StringBuilder($"slot {i} dropped at tick {world.Tick}:");
                    }
                    if (dropAt[i] != 0u)
                    {
                        if (trail.TryGetValue(i, out var tb))
                            tb.Append($" [+{world.Tick - dropAt[i]} sim {(Stance)now} shown {(Stance)anim[i].Stance} {anim[i].Clip} {anim[i].Rung} v{anim[i].Speed:0.0} a{world.Alarm[i]:0}]");
                        if (now != (byte)Stance.Prone) { dropAt[i] = 0u; trail.Remove(i); }   // the sim put him up again: not a drop
                        else if (anim[i].Stance == (byte)Stance.Prone)
                        {
                            long took = world.Tick - dropAt[i];
                            drops++;
                            if (took > worst) { worst = took; slowest.Clear(); if (trail.TryGetValue(i, out var wb)) slowest.Append(wb.ToString()); }
                            dropAt[i] = 0u; trail.Remove(i);
                        }
                    }
                    wasStance[i] = now;
                }
            }
            float share = upTotal > 0 ? upCrouched / (float)upTotal : 0f;
            string lineUp = $"rush up crouched: {upCrouched}/{upTotal}";
            string lineDrop = $"drop to flat: worst {worst} ticks over {drops} drops";
            Say(lineUp);
            Say(lineDrop);
            if (slowest.Length > 0) Say("the slowest drop, tick by tick: " + slowest.ToString());
            File.WriteAllText(Path.Combine(dir, "measured." + variant + ".json"),
                "{\n" +
                $"  \"variant\": \"{variant}\",\n" +
                $"  \"rushUpCrouched\": {upCrouched},\n  \"rushUpManTicks\": {upTotal},\n" +
                $"  \"rushUpCrouchedShare\": {share.ToString("0.000", Inv)},\n" +
                $"  \"drops\": {drops},\n  \"dropToFlatWorstTicks\": {worst},\n" +
                $"  \"budgetTicks\": {DropBudgetTicks},\n  \"requiredShare\": {CrouchedShare.ToString("0.00", Inv)}\n" +
                "}\n");

            // ---- the pictures, at the busiest patch and the zoom the game is played at ----
            Time.captureDeltaTime = 1f / 30f;   // the films are a 1/30 s step, as the other stills rigs are
            Vector2 at = CaptureRig.Crowd();
            Say($"crowd at {at.x:0.0}, {at.y:0.0}");
            Say(CaptureRig.Hold(120f));
            for (int w = 0; w < 8; w++) yield return null;
            string stillPath = Path.Combine(dir, "after-" + variant + ".png");
            if (File.Exists(stillPath)) File.Delete(stillPath);
            CaptureRig.Shot(stillPath, at.x, at.y, 30f, 21f, 25f, 1600, 900);
            yield return Drain(stillPath);
            Assert.That(File.Exists(stillPath), Is.True, "the still was not written");
            Say(CaptureRig.Release());   // the world was held for the still; the series needs it running to show a drop

            CaptureRig.Series(dir, "after-" + variant + "-series", at.x, at.y, 30f, 21f, 25f, 4, 4, 1600, 900);
            yield return Drain(Path.Combine(dir, "after-" + variant + "-series_03.png"));

            Time.captureDeltaTime = 0f;
            Atmosphere.PinnedClock = -1f;
            yield return new ExitPlayMode();

            // asserted last, so the pictures and the numbers are on disk either way: a red run is the before picture
            Assert.Greater(upTotal, 0, "no alarmed man was in the rush up in the filmed stretch: nothing was measured");
            Assert.GreaterOrEqual(share, CrouchedShare, lineUp + " is under the owner's 70 %");
            Assert.Greater(drops, 0, "no man dropped into a hole in the filmed stretch: nothing was measured");
            Assert.LessOrEqual(worst, DropBudgetTicks, lineDrop + ": over the owner's half second");
        }
    }
}
