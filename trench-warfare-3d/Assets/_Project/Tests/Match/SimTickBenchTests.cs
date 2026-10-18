// Phase: tooling (2026-10-18) — the sim tick's own cost, read in code at DirectFire.Wrecks.cs (BuildShelters walks
// the whole prop list and scans spatial-hash cells every tick, for a list FireJob reads only when a slot has no
// target) and proved here, not only by the determinism tests staying bit for bit: a 48-man ShelledForest match
// (mostly trees on a shelled field, the script's own scene), each tick's wall-clock cost averaged over the run.
// shelter_skips is 0 on the code before the fix (it never skipped) and well over zero after, so it also tells the
// two apart when the clock itself is noise. Run by name (Explicit, Tools/abtest.py's own bench): writes
// %TEMP%/tw-simtick-bench.txt and .json, or TW_BENCH_OUT's stem; TW_BENCH_SEEDS picks the seeds.
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class SimTickBenchTests
    {
        public static readonly uint[] Seeds = { 1, 2, 3 };
        const int Men = 48, Settle = 60, Ticks = 600;

        static MatchSim NewMatch(uint seed)
        {
            var cfg = SimConfig.Default; cfg.Seed = seed;
            return MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(seed));
        }

        static void Step(MatchSim m)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(none);
        }

        /// <summary>48 men (a third machine gunners, so AtWreck's own gate has slots to test) in two facing lines
        /// 20 m apart, close enough that most of them see an enemy from the first tick on (a living target always
        /// wins): held in place (no goal), so the fight stays dense for the whole run instead of the two sides
        /// spending most of it still closing. Centred on the field's middle, well clear of its water margins.</summary>
        static void SetDown(MatchSim m)
        {
            var w = m.World; var init = w.Init; var mid = (init.SpawnA + init.SpawnB) * 0.5f;
            for (int side = 0; side < 2; side++)
            {
                float z = mid.z + (side == 0 ? -10f : 10f);
                for (int n = 0; n < Men / 2; n++)
                {
                    byte arch = n % 3 == 0 ? InfantryArchetype.Machinegunner : InfantryArchetype.Rifle;
                    var e = w.Units.Roster[arch];
                    var pos = new float3(mid.x + (n - Men / 4f) * 1.5f, 0f, z);
                    int slot = w.Spawn((byte)side, arch, pos, e.Hp, e.Speed, false);
                    w.GoalId[slot] = -1; w.TrenchId[slot] = -1;
                }
            }
        }

        public static Dictionary<string, float> Run(uint seed)
        {
            using var m = NewMatch(seed);
            SetDown(m);
            for (int t = 0; t < Settle; t++) Step(m);
            var fire = m.World.GetSystem<DirectFireSystem>();
            int skipsBefore = fire.ShelterSkips;
            var sw = Stopwatch.StartNew();
            for (int t = 0; t < Ticks; t++) Step(m);
            sw.Stop();
            return new Dictionary<string, float>
            {
                ["tick_ms_mean"] = (float)sw.Elapsed.TotalMilliseconds / Ticks,
                ["shelter_skips"] = fire.ShelterSkips - skipsBefore,
                ["shots"] = fire.Shots[0] + fire.Shots[1],
                ["men"] = Men,
                ["ticks"] = Ticks,
            };
        }

        [Test, Explicit("a bench, not a claim: 600 ticks of a 48-man ShelledForest match, for Tools/abtest.py bench")]
        public void Report_TheSimTickCost()
        {
            var text = new StringBuilder(); var json = new StringBuilder("{\n");
            var sum = new Dictionary<string, float>();
            var seeds = Seeds;
            string pick = System.Environment.GetEnvironmentVariable("TW_BENCH_SEEDS");
            if (!string.IsNullOrEmpty(pick)) seeds = System.Array.ConvertAll(pick.Split(','), x => uint.Parse(x.Trim()));
            for (int n = 0; n < seeds.Length; n++)
            {
                var mx = Run(seeds[n]);
                text.AppendLine($"seed {seeds[n]}: {mx["tick_ms_mean"]:F4} ms/tick, {mx["shelter_skips"]:F0} shelter skips of {Ticks} ticks, {Men} men");
                json.Append($"  \"seed{seeds[n]}\": {{");
                bool first = true;
                foreach (var kv in mx)
                {
                    json.Append($"{(first ? "" : ", ")}\"{kv.Key}\": {kv.Value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)}");
                    first = false;
                    sum[kv.Key] = (sum.TryGetValue(kv.Key, out var v) ? v : 0f) + kv.Value;
                }
                json.Append("},\n");
            }
            json.Append("  \"mean\": {");
            bool f0 = true;
            foreach (var kv in sum) { json.Append($"{(f0 ? "" : ", ")}\"{kv.Key}\": {(kv.Value / seeds.Length).ToString("R", System.Globalization.CultureInfo.InvariantCulture)}"); f0 = false; }
            json.Append("}\n}\n");
            string stem = System.Environment.GetEnvironmentVariable("TW_BENCH_OUT");
            if (string.IsNullOrEmpty(stem)) stem = Path.Combine(Path.GetTempPath(), "tw-simtick-bench");
            File.WriteAllText(stem + ".txt", text.ToString());
            File.WriteAllText(stem + ".json", json.ToString());
            TestContext.WriteLine(text.ToString());
        }
    }
}
