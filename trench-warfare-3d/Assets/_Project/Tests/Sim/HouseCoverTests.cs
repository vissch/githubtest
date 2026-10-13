// Phase: A4 (implemented 2026-10-13) — a house gives a man on foot behind it cover from small arms: the footprint
// geometry ("behind" is not "in front" and not "beside"), and the hit counts behind, in front, beside and in the open.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
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

        /// <summary>A hamlet block: wide enough to stand between a whole firing line and one man.</summary>
        static HouseDef Block(float x, float z) =>
            new HouseDef { Pos = new float3(x, 0f, z), Yaw = 0f, Half = new float2(8f, 5f) };

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
