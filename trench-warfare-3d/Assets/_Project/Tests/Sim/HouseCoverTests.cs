// Phase: A4 (implemented 2026-10-13) — a house gives a man on foot behind it cover from small arms: the footprint
// geometry ("behind" is not "in front" and not "beside"), and the hit counts behind, in front, beside and in the open.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class HouseCoverTests
    {
        static MatchSim NewMatch(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m)
        {
            using var arr = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(arr);
        }

        static List<SimEvent> Run(MatchSim m, int ticks, System.Action<SimWorld> each = null)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                each?.Invoke(m.World);
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        static int Still(MatchSim m, byte team, byte archetype, float x, float z, float hp = -1f)
        {
            var at = new float3(x, 0f, z);
            Assert.AreEqual(0, (int)(m.Map.LayerAt(at) & NavLayer.Trench), $"setup: ({x},{z}) must be open ground");
            var e = RosterEntry.ForArchetype(archetype);
            return m.World.Spawn(team, archetype, at, hp < 0f ? e.Hp : hp, 0f, false);
        }

        static int Hits(List<SimEvent> log, int target)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == SimEventType.Hit && e.B == target && e.Scalar > 0f) n++;
            return n;
        }

        static HouseDef House(float x, float z, float yaw = 0f) =>
            new HouseDef { Pos = new float3(x, 0f, z), Yaw = yaw, Half = new float2(4f, 3f) };

        /// <summary>A long thin wall: at a yaw off the 90s the right frame and its mirror differ by metres.</summary>
        static HouseDef Wall(float x, float z, float yaw) =>
            new HouseDef { Pos = new float3(x, 0f, z), Yaw = yaw, Half = new float2(8f, 1.5f) };

        /// <summary>A hamlet block: wide enough to stand between a whole firing line and one man.</summary>
        static HouseDef Block(float x, float z) =>
            new HouseDef { Pos = new float3(x, 0f, z), Yaw = 0f, Half = new float2(8f, 5f) };

        /// <summary>[H.3] One house of the report's hamlet: half extents 5 x 4 m. Four of them in a row at x 104,
        /// 122, 140, 158 span x 99..109, 117..127, 135..145 and 153..163, so between them stand 8 m wide lanes at
        /// x 109..117, 127..135 and 145..153 that a man can be shot straight through.</summary>
        static HouseDef Hamlet(float x, float z) =>
            new HouseDef { Pos = new float3(x, 0f, z), Yaw = 0f, Half = new float2(5f, 4f) };

        [Test]
        public void TheFootprintTellsBehindFromInFrontAndBeside()
        {
            var h = House(50f, 50f);   // x 46..54, z 47..53
            var shooter = new float3(50f, 0f, 80f);
            Assert.IsTrue(HouseRules.Blocks(h, shooter, new float3(50f, 0f, 45f)), "behind the house: the line crosses it");
            Assert.IsFalse(HouseRules.Blocks(h, shooter, new float3(50f, 0f, 56f)), "in front of it: the line stops short");
            Assert.IsFalse(HouseRules.Blocks(h, shooter, new float3(60f, 0f, 45f)), "beside it: the line is at x 57.7 by z 53");
            Assert.IsTrue(HouseRules.Blocks(h, shooter, new float3(50f, 0f, 50f)), "a man inside the footprint");
            // turned 90 degrees the footprint is x 47..53, z 46..54, so a line down x = 53.5 misses it
            Assert.IsFalse(HouseRules.Blocks(House(50f, 50f, math.PI * 0.5f), new float3(53.5f, 0f, 80f), new float3(53.5f, 0f, 20f)));
            Assert.IsTrue(HouseRules.Blocks(House(50f, 50f, math.PI * 0.5f), new float3(50f, 0f, 80f), new float3(50f, 0f, 20f)));

            // [H.1] a yaw that is not a multiple of 90: the footprint must sit in the sim's own frame, forward
            // (sin, cos) as SimMath.DirFromYaw, right (cos, -sin). Hand-worked: the 16 x 3 m wall at yaw 30 covers
            // z 46.1..47.7 at x 57 (mirrored in z = 50, i.e. at -Yaw, it would cover z 52.3..53.9), and at yaw 120
            // it covers z 52.2..57.5 at x 53 (mirrored: z 42.5..47.8).
            var w30 = Wall(50f, 50f, math.radians(30f));
            Assert.IsFalse(HouseRules.Blocks(w30, new float3(57f, 0f, 80f), new float3(57f, 0f, 50f)),
                "[H.1] yaw 30: the wall is south of z 50 at x 57, so a shot coming to z 50 never crosses it");
            Assert.IsTrue(HouseRules.Blocks(w30, new float3(57f, 0f, 50f), new float3(57f, 0f, 40f)),
                "[H.1] yaw 30: the wall covers z 46.1..47.7 at x 57");
            var w120 = Wall(50f, 50f, math.radians(120f));
            Assert.IsTrue(HouseRules.Blocks(w120, new float3(53f, 0f, 80f), new float3(53f, 0f, 50f)),
                "[H.1] yaw 120: the wall covers z 52.2..57.5 at x 53");
            Assert.IsFalse(HouseRules.Blocks(w120, new float3(53f, 0f, 50f), new float3(53f, 0f, 20f)),
                "[H.1] yaw 120: nothing of the wall lies south of z 50 at x 53");
        }

        // one still man, eight rifles 30 m off, every man held in place, the same seed and the same positions every
        // run: only the house differs. The house is a hamlet block: half extents 8 x 5 m, so it stands between the
        // whole firing line and him.
        static int Fight(HouseDef? house)
        {
            using var m = NewMatch();
            const float z = 30f;
            int man = Still(m, 0, InfantryArchetype.Rifle, 60f, z, 1e6f);
            var held = new List<int> { man };
            for (int k = 0; k < 8; k++) held.Add(Still(m, 1, InfantryArchetype.Rifle, 53f + k * 2f, z + 30f));
            if (house.HasValue) m.Map.AddHouse(house.Value);
            var at = new List<float3>();
            foreach (int slot in held) at.Add(m.World.Position[slot]);
            var log = Run(m, 2400, w =>
            {
                w.FireCooldown[man] = 1000;                      // he never fires: this is about who gets shot, and his
                w.StanceOf[man] = (byte)Stance.Standing;         // own rounds would get the house's cover too
                for (int k = 0; k < held.Count; k++)             // nobody walks out of the geometry
                {
                    w.Position[held[k]] = at[k];
                    w.Velocity[held[k]] = float3.zero;
                }
            });
            return Hits(log, man);
        }

        [Test, Category("Long")]
        public void AManBehindAHouseIsShotLessOftenAndOnlyHim()
        {
            int open = Fight(null);
            int behind = Fight(Block(60f, 45f));     // between him and the firing line
            int inFront = Fight(Block(60f, 15f));    // the house is behind him, he stands in front of it
            int beside = Fight(Block(85f, 45f));     // off to one side of the line of fire

            TestContext.WriteLine($"hits: open {open}, behind {behind}, in front {inFront}, beside {beside}");
            Assert.GreaterOrEqual(open, 150, $"setup: the open run must land plenty of rounds (got {open})");
            Assert.That(behind / (float)open, Is.InRange(0.55f, 0.80f), $"behind the house {behind} hits, in the open {open}");
            Assert.AreEqual(open, inFront, "a man in front of the house is no better off");
            Assert.AreEqual(open, beside, "a man beside the house is no better off");
        }

        /// <summary>Report (no new rule): deaths per man-minute for men crossing a field toward the enemy trench, on the
        /// same eight seeds, with eight houses along the crossing and with none (the old behaviour is exactly "no house
        /// on the map"). Asserts only the direction, after <= before.</summary>
        [Test, Category("Long")]
        public void Report_DeathsPerManMinute()
        {
            // one arm: 24 attackers walk from 90 m short of the enemy trench onto it, twelve defenders stand on it.
            float Arm(uint seed, bool houses)
            {
                using var m = NewMatch(seed);
                var w = m.World;
                float trenchZ = m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[1].CellStart]).z;
                var attackers = new List<int>();
                for (int k = 0; k < 24; k++)
                {
                    var at = new float3(100f + (k % 8) * 7f, 0f, trenchZ - 90f - (k / 8) * 6f);
                    int man = w.Spawn(0, InfantryArchetype.Rifle, at, RosterEntry.ForArchetype(InfantryArchetype.Rifle).Hp, 3f, false);
                    w.GoalId[man] = m.Fields.GetGoal(GoalKey.Trench(1));
                    attackers.Add(man);
                }
                for (int k = 0; k < 12; k++)
                    w.Spawn(1, InfantryArchetype.Rifle, new float3(100f + k * 5f, 0f, trenchZ), 1e6f, 0f, false);
                if (houses)
                    for (int k = 0; k < 8; k++)
                        m.Map.AddHouse(Hamlet(104f + (k % 4) * 18f, trenchZ - 30f - (k / 4) * 25f));

                long deaths = 0, manTicks = 0;
                for (int t = 0; t < 1800; t++)
                {
                    foreach (int man in attackers) if (w.IsAlive(man)) manTicks++;
                    Step(m);
                    var ev = w.Events.Events;
                    for (int e = 0; e < ev.Length; e++)
                        if (ev[e].Type == SimEventType.Death && ev[e].A >= 0 && w.Team[ev[e].A] == 0) deaths++;
                }
                float manMinutes = manTicks * w.Config.TickSeconds / 60f;
                return manMinutes <= 0f ? 0f : deaths / manMinutes;
            }

            var seeds = new uint[] { 1u, 2u, 3u, 5u, 8u, 13u, 21u, 34u };
            var before = new List<float>(); var after = new List<float>();
            int down = 0;
            foreach (uint s in seeds)
            {
                float b = Arm(s, false), a = Arm(s, true);
                before.Add(b); after.Add(a);
                if (a <= b) down++;
                TestContext.WriteLine($"seed {s}: before {b:F3}, after {a:F3} deaths per man-minute ({(a <= b ? "down" : "up")})");
            }
            void Stats(string name, List<float> v)
            {
                var o = new List<float>(v); o.Sort();
                float mean = 0f; foreach (float x in o) mean += x; mean /= o.Count;
                TestContext.WriteLine($"{name}: mean {mean:F3}, min {o[0]:F3}, max {o[o.Count - 1]:F3} deaths per man-minute");
            }
            Stats("before (no houses)", before);
            Stats("after  (8 houses) ", after);
            float mb = 0f, ma = 0f;
            foreach (float x in before) mb += x; mb /= before.Count;
            foreach (float x in after) ma += x; ma /= after.Count;
            TestContext.WriteLine($"seeds down: {down} of {seeds.Length}");
            // [H.3] strictly less: without a house cover rule the two arms are bit-identical (the houses only add
            // to MapData.Hash, and DirectFire leaves cover untouched), so ma == mb and this assert goes red.
            Assert.Less(ma, mb, $"[H.3] houses must get fewer men killed (before {mb:F3}, after {ma:F3})");
        }

        [Test]
        public void AHouselessMatchHashesAsItDid()
        {
            ulong Play(bool house)
            {
                using var m = NewMatch();
                int man = Still(m, 0, InfantryArchetype.Rifle, 60f, 30f, 1e6f);
                for (int k = 0; k < 4; k++) Still(m, 1, InfantryArchetype.Rifle, 57f + k * 2f, 60f);
                if (house) m.Map.AddHouse(House(60f, 45f));
                Run(m, 120, w => w.Suppression[man] = 100f);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(false), Play(false), "the same on every machine");
            Assert.AreNotEqual(Play(false), Play(true), "a house is in the hash");
        }
    }
}
