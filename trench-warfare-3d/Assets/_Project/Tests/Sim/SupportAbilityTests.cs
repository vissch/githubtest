// Phase: A4 / A5 core (implemented) — the HE barrage costs silver, lands after its warm-up, kills men in the open,
// spares most of a garrison and leaves craters that are cover; chlorine drifts with the wind, empties a trench
// without destroying it and the garrison comes back on "fall back"; both are validated and deterministic.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class SupportAbilityTests
    {
        const int Rifleman = 0;

        static MatchSim NewMatch(int silver = 100000, uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = silver; cfg.Seed = seed;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static SimCommand Support(byte player, OffMapAbilityId id, float x, float z)
            => new SimCommand { Player = player, Type = CommandType.SupportFire, A = (int)id, Pos = new float3(x, 0f, z) };

        static int Alive(SimWorld w, byte team)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == team) n++;
            return n;
        }

        static int Craters(MapData map)
        {
            int n = 0;
            for (int i = 0; i < map.NavLayers.Length; i++) if ((map.NavLayers[i] & (byte)NavLayer.Crater) != 0) n++;
            return n;
        }

        static float TrenchZ(MatchSim m, short trench)
            => m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[trench].CellStart]).z;

        static void Garrison(MatchSim m, byte team, short trench, int count)
        {
            for (int k = 0; k < count; k++) Step(m, SimCommand.Deploy(m.World.Tick, team, Rifleman));
            for (int t = 0; t < 2000 && m.Fields.Trenches[trench].GarrisonCount < count; t++) Step(m);
            Assert.AreEqual(count, m.Fields.Trenches[trench].GarrisonCount, "setup: garrison did not form");
        }

        [Test]
        public void HeBarrage_CostsSilver_LandsAfterWarmup_KillsInTheOpen_AndLeavesCraters()
        {
            using var m = NewMatch();
            var w = m.World;
            for (int k = 0; k < 30; k++) w.Spawn(1, Rifleman, new float3(130f + (k % 6) * 7f, 0f, 380f + (k / 6) * 8f), 100f, 0f, false);
            int silver = w.Silver[0];
            Step(m, Support(0, OffMapAbilityId.HeBarrage, 150f, 400f));
            Assert.LessOrEqual(w.Silver[0], silver - 150 + 1, "the barrage is paid for up front");
            Assert.Greater(m.Abilities.CooldownOf(0, OffMapAbilityId.HeBarrage), 0);

            int explosions = 0;
            for (int t = 0; t < 70; t++) { Step(m); Assert.AreEqual(30, Alive(w, 1), "nothing lands during the 4 s warm-up"); }
            for (int t = 0; t < 200; t++)
            {
                Step(m);
                var ev = w.Events.Events;
                for (int e = 0; e < ev.Length; e++) if (ev[e].Type == SimEventType.Explosion) explosions++;
            }
            Assert.AreEqual(12, explosions, "twelve shells");
            Assert.Less(Alive(w, 1), 30, "men standing in the open under a barrage die");
            Assert.Greater(Craters(m.Map), 20, "every shell leaves a crater in open ground");
            Assert.AreEqual(12, m.Deformation.Applied);
        }

        [Test]
        public void SupportFire_IsRejected_WithoutSilver_OnCooldown_OrOffMap()
        {
            using var m = NewMatch(silver: 200);
            var w = m.World;
            Step(m, Support(0, OffMapAbilityId.HeBarrage, -50f, 400f));
            Assert.AreEqual(0, m.Abilities.Scheduled.Length, "off the map");
            Step(m, Support(0, OffMapAbilityId.HeBarrage, 150f, 400f));
            Assert.AreEqual(12, m.Abilities.Scheduled.Length);
            Step(m, Support(0, OffMapAbilityId.ChlorineGas, 150f, 400f));
            Assert.AreEqual(12, m.Abilities.Scheduled.Length, "50 silver left does not buy gas");
            w.Silver[0] = 1000;
            Step(m, Support(0, OffMapAbilityId.HeBarrage, 150f, 420f));
            Assert.AreEqual(12, m.Abilities.Scheduled.Length, "still cooling down");
            Step(m, Support(1, OffMapAbilityId.HeBarrage, 150f, 420f));
            Assert.AreEqual(24, m.Abilities.Scheduled.Length, "the other player has his own silver and cooldown");
        }

        [Test]
        public void Trench_ShieldsAGarrison_FromShellsThatLandOutside()
        {
            using var m = NewMatch();
            Garrison(m, 1, 1, 12);
            var w = m.World;
            // twelve shells, all placed on open ground 4 m in front of the parapet
            float z = TrenchZ(m, 1) - 5f;
            for (int k = 0; k < 12; k++) m.Blast.Queue(new TW.Sim.Combat.Impact { Pos = new float3(120f + k * 5f, 0f, z), Damage = 150f, Radius = 8f, Suppression = 60f, CraterRadius = 3f, CraterDepth = 1.2f, Player = 0 });
            Step(m);
            Assert.AreEqual(12, Alive(w, 1), "35 % of a near miss does not kill a man at full health");
            bool suppressed = false;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Suppression[i] > 15f) suppressed = true;
            Assert.IsTrue(suppressed, "but it keeps heads down");
        }

        [Test]
        public void Chlorine_EmptiesTheTrench_DriftsDownwind_AndFallBackBringsTheGarrisonHome()
        {
            using var m = NewMatch();
            Garrison(m, 1, 1, 10);
            var w = m.World;
            float z = TrenchZ(m, 1);
            // the map wind blows toward -Z: release upwind of the trench so the cloud rolls over it
            Step(m, Support(0, OffMapAbilityId.ChlorineGas, 150f, z + 14f));
            bool spawned = false;
            for (int t = 0; t < 700; t++)
            {
                Step(m);
                var ev = w.Events.Events;
                for (int e = 0; e < ev.Length; e++) if (ev[e].Type == SimEventType.GasCloudSpawned) spawned = true;
            }
            Assert.IsTrue(spawned);
            Assert.Less(m.Fields.Trenches[1].GarrisonCount, 10, "gas drives the garrison out or kills it");
            Assert.AreEqual(0, Craters(m.Map), "and leaves the ground intact");
            Assert.Greater(m.Gas.ConcentrationAt(new float3(150f, 0f, z - 10f)) + m.Gas.ConcentrationAt(new float3(150f, 0f, z - 20f)) + (m.Gas.Active ? 0f : 1f), 0f,
                "the cloud has moved downwind (or already blown out)");

            int survivors = Alive(w, 1);
            for (int t = 0; t < 1500 && m.Gas.Active; t++) Step(m);
            Assert.IsFalse(m.Gas.Active, "the field switches itself off when the cloud is gone");
            survivors = Alive(w, 1);
            Step(m, new SimCommand { Player = 1, Type = CommandType.TrenchFallback, A = 1 });
            for (int t = 0; t < 1500 && m.Fields.Trenches[1].GarrisonCount < survivors; t++) Step(m);
            Assert.AreEqual(survivors, m.Fields.Trenches[1].GarrisonCount, "fall back re-mans the trench with whoever lived");
        }

        static ulong[] Run(uint seed)
        {
            using var m = NewMatch(seed: seed);
            var hashes = new ulong[900];
            for (int t = 0; t < hashes.Length; t++)
            {
                if (t < 30) Step(m, SimCommand.Deploy(m.World.Tick, 0, t % 3), SimCommand.Deploy(m.World.Tick, 1, t % 3));
                else if (t == 100) Step(m, Support(0, OffMapAbilityId.HeBarrage, 150f, 640f), Support(1, OffMapAbilityId.ChlorineGas, 150f, 140f));
                else if (t == 120) Step(m, Support(1, OffMapAbilityId.HeBarrage, 150f, 100f), Support(0, OffMapAbilityId.ChlorineGas, 140f, 700f));
                else Step(m);
                hashes[t] = m.World.LastHash;
            }
            return hashes;
        }

        [Test]
        public void SupportAbilities_AreDeterministic()
        {
            var a = Run(3);
            var b = Run(3);
            for (int i = 0; i < a.Length; i++) Assert.AreEqual(a[i], b[i], $"hash diverged at tick {i}");
        }
    }
}
