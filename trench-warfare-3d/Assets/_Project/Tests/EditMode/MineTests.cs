// Phase: A5 / docs/21 SIM-D (2026-09-26) — mines and tripwires in the sim: an enemy sets a mine off and the own side
// never does, a mine is not live until it is armed, a tripwire is crossed along its length, a hull sets one off under
// its tracks, a shell's crater cooks off the mines in it, a mine never lies in a trench, and it all hashes and repeats
// exactly. Placed by the system call the sapper will use (units-meta).
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class MineTests
    {
        static readonly float3 Open = new float3(150f, 0f, 240f);

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public SimWorld W => M.World;
            public MineSystem Mines => M.Mines;
            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreatePlaytest(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
            }
            public int Man(float3 at, byte team = 1, float hp = 100f) => W.Spawn(team, 0, at, hp, 0f, false);
            public int Tank(float3 at, byte team = 1) => W.Spawn(team, 4, at, 5000f, 0f, true);
            public int Walker(float3 at, byte archetype, byte team = 1) => W.Spawn(team, archetype, at, 5000f, 0f, true);
            public void Step(int ticks = 1)
            {
                for (int t = 0; t < ticks; t++)
                {
                    using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                    M.Step(none);
                }
            }
            public int Count(SimEventType type, int a = int.MinValue, int b = int.MinValue)
            {
                int n = 0;
                var events = W.Events.Events;
                for (int i = 0; i < events.Length; i++)
                {
                    var e = events[i];
                    if (e.Type != type) continue;
                    if (a != int.MinValue && e.A != a) continue;
                    if (b != int.MinValue && e.B != b) continue;
                    n++;
                }
                return n;
            }
            public void Dispose() => M.Dispose();
        }

        [Test]
        public void AnEnemySetsAMineOffAndTheOwnSideNeverDoes()
        {
            using var r = new Rig();
            Assert.IsTrue(r.Mines.Lies(Open), "the test spot is open ground");
            int mine = r.Mines.Place(r.W, Open, float3.zero, 0f, 0, MineKind.Mine);
            Assert.GreaterOrEqual(mine, 0);
            Assert.AreEqual(1, r.Count(SimEventType.MinePlaced, mine, 0), "MinePlaced (a = index, b = player)");
            r.Step(MineSystem.ArmTicks + 1);
            Assert.AreEqual((int)MineState.Armed, r.Mines.Mines[mine].State);

            int friend = r.Man(Open + new float3(0.3f, 0f, 0f), team: 0);
            r.Step();
            Assert.AreEqual((int)MineState.Armed, r.Mines.Mines[mine].State, "the layer's own man stands on it and nothing happens");
            Assert.AreEqual(0, r.Count(SimEventType.MineTriggered));
            r.W.Despawn(friend);

            int enemy = r.Man(Open + new float3(0.4f, 0f, 0f), team: 1);
            r.Step();
            Assert.AreEqual(1, r.Count(SimEventType.MineTriggered, mine, enemy), "MineTriggered (a = index, b = victim)");
            Assert.AreEqual((int)MineState.Spent, r.Mines.Mines[mine].State);
            r.Step();
            Assert.Greater(r.Count(SimEventType.Explosion, MineSystem.SourceBase + (int)MineKind.Mine), 0, "its burst resolves the next tick, Source = 70 + kind");
            Assert.IsFalse(r.W.IsAlive(enemy), "260 on a 100 hp man at the burst");
            Assert.AreEqual(1, r.Mines.Triggered);
        }

        [Test]
        public void AMineIsNotLiveUntilItIsArmed()
        {
            using var r = new Rig();
            int mine = r.Mines.Place(r.W, Open, float3.zero, 0f, 0, MineKind.Mine);
            int enemy = r.Man(Open, team: 1);
            r.Step(MineSystem.ArmTicks - 2);
            Assert.AreEqual(0, r.Count(SimEventType.MineTriggered), "a man on it while it arms is safe");
            Assert.AreEqual((int)MineState.Arming, r.Mines.Mines[mine].State);
            Assert.IsTrue(r.W.IsAlive(enemy));
            r.Step(3);
            Assert.AreEqual((int)MineState.Spent, r.Mines.Mines[mine].State, "armed, it goes off under him");
        }

        [Test]
        public void ATripwireIsCrossedAlongItsLengthAndNotBesideIt()
        {
            using var r = new Rig();
            int wire = r.Mines.Place(r.W, Open, new float3(1f, 0f, 0f), 10f, 0, MineKind.Tripwire);
            Assert.GreaterOrEqual(wire, 0);
            Assert.AreEqual(10f, r.Mines.Mines[wire].Length, 1e-4f);
            int longer = r.Mines.Place(r.W, Open + new float3(0f, 0f, 20f), new float3(0f, 0f, 1f), 40f, 0, MineKind.Tripwire);
            Assert.AreEqual(MineSystem.MaxTripwireMetres, r.Mines.Mines[longer].Length, 1e-4f, "a tripwire is clamped to its longest");
            r.Step(MineSystem.ArmTicks + 1);
            int beside = r.Man(Open + new float3(5f, 0f, 3f), team: 1);
            r.Step();
            Assert.AreEqual(0, r.Count(SimEventType.MineTriggered), "three metres beside the line: nothing");
            int crossing = r.Man(Open + new float3(7f, 0f, 0.3f), team: 1);
            r.Step();
            Assert.AreEqual(1, r.Count(SimEventType.MineTriggered, wire, crossing), "a man on the line, seven metres along it");
            Assert.IsTrue(r.W.IsAlive(beside), "the one beside it is still standing when it goes off");
        }

        [Test]
        public void AHullSetsAMineOffUnderItsTracks()
        {
            using var r = new Rig();
            int mine = r.Mines.Place(r.W, Open, float3.zero, 0f, 0, MineKind.Mine);
            r.Step(MineSystem.ArmTicks + 1);
            float halfWidth = TW.Sim.Nav.VehicleProfile.ForArchetype(4).HalfWidth;
            int tank = r.Tank(Open + new float3(halfWidth * 0.8f, 0f, 0f), team: 1);
            float hp = r.W.Hp[tank];
            r.Step();
            Assert.AreEqual(1, r.Count(SimEventType.MineTriggered, mine, tank), "the hull is wider than a man: a mine under its track goes off");
            r.Step();
            Assert.Less(r.W.Hp[tank], hp, "and the hull is the worse for it");
        }

        [Test]
        public void AShellsCraterCooksOffTheMinesInIt()
        {
            using var r = new Rig();
            int inside = r.Mines.Place(r.W, Open + new float3(2f, 0f, 0f), float3.zero, 0f, 0, MineKind.Mine);
            int outside = r.Mines.Place(r.W, Open + new float3(30f, 0f, 0f), float3.zero, 0f, 0, MineKind.Mine);
            r.Step(MineSystem.ArmTicks + 1);
            r.M.Blast.Queue(new Impact { Pos = Open, Radius = 8f, Damage = 150f, Suppression = 60f, CraterRadius = 3f, Player = 1, Source = 1 });
            r.Step();
            Assert.AreEqual(1, r.Count(SimEventType.MineCleared, inside), "the one in the crater is cooking (MineCleared, a = index)");
            Assert.AreEqual((int)MineState.Cooking, r.Mines.Mines[inside].State);
            Assert.AreEqual((int)MineState.Armed, r.Mines.Mines[outside].State, "thirty metres off: untouched");
            r.Step(MineSystem.CookMinTicks + 1);
            Assert.AreEqual((int)MineState.Spent, r.Mines.Mines[inside].State, "and it has gone off on its own");
            Assert.Greater(r.Count(SimEventType.Explosion, MineSystem.SourceBase), 0, "its own burst, this tick or the last");
            Assert.AreEqual(1, r.Mines.Cleared);
        }

        [Test]
        public void AMineNeverLiesInATrench()
        {
            using var r = new Rig();
            var map = r.M.Map;
            int cell = -1;
            for (int i = 0; i < map.NavLayers.Length && cell < 0; i++) if ((map.NavLayers[i] & (byte)NavLayer.Trench) != 0) cell = i;
            if (cell < 0) Assert.Ignore("the playtest map has no trench cell");
            var inTrench = new float3((cell % map.NavWidth + 0.5f) * MapData.NavCellSize, 0f, (cell / map.NavWidth + 0.5f) * MapData.NavCellSize);
            Assert.IsFalse(r.Mines.Lies(inTrench));
            Assert.AreEqual(-1, r.Mines.Place(r.W, inTrench, float3.zero, 0f, 0, MineKind.Mine), "refused: a trench is the one place a mine never lies");
            Assert.AreEqual(-1, r.Mines.Place(r.W, Open, float3.zero, 5f, 0, MineKind.Tripwire), "a tripwire needs a heading");
            Assert.AreEqual(0, r.Count(SimEventType.MinePlaced), "nothing was laid");
        }

        [Test]
        public void ASpentMineKeepsItsIndexAndTheNextOneReusesIt()
        {
            using var r = new Rig();
            int first = r.Mines.Place(r.W, Open, float3.zero, 0f, 0, MineKind.Mine);
            r.Step(MineSystem.ArmTicks + 1);
            r.Man(Open, team: 1);
            r.Step();
            Assert.AreEqual((int)MineState.Spent, r.Mines.Mines[first].State);
            Assert.AreEqual(1, r.Mines.Mines.Length, "the spent one stays: the picture holds its marker by index");
            int again = r.Mines.Place(r.W, Open + new float3(0f, 0f, 12f), float3.zero, 0f, 0, MineKind.Mine);
            Assert.AreEqual(first, again, "the next mine takes the spent slot");
            Assert.AreEqual(1, r.Mines.Live);
        }

        [Test]
        public void MinesAreHashedAndRepeatExactly()
        {
            ulong[] Run(bool withMine)
            {
                using var r = new Rig();
                if (withMine) r.Mines.Place(r.W, Open, float3.zero, 0f, 0, MineKind.Mine);
                r.Man(Open + new float3(0.5f, 0f, 0f), team: 1);
                var hashes = new ulong[40];
                for (int t = 0; t < hashes.Length; t++) { r.Step(); hashes[t] = r.W.LastHash; }
                return hashes;
            }
            var a = Run(true); var b = Run(true); var none = Run(false);
            for (int t = 0; t < a.Length; t++) Assert.AreEqual(a[t], b[t], "tick " + t + ": the same mine, the same hash");
            Assert.AreNotEqual(a[0], none[0], "a mine lying there is in the hash from the first tick");
        }

        [Test]
        public void AMinesOwnBurstCooksOffItsNeighbour()
        {
            using var r = new Rig();
            int first = r.Mines.Place(r.W, Open, float3.zero, 0f, 0, MineKind.Mine);
            int second = r.Mines.Place(r.W, Open + new float3(2f, 0f, 0f), float3.zero, 0f, 0, MineKind.Mine);
            r.Step(MineSystem.ArmTicks + 1);
            r.Man(Open, team: 1);
            r.Step();
            Assert.AreEqual((int)MineState.Spent, r.Mines.Mines[first].State, "the first goes off under him");
            r.Step();
            Assert.AreEqual(1, r.Count(SimEventType.MineCleared, second), "its crater, dug by the next tick's Blast, cooks off the neighbour two metres away");
            Assert.AreEqual((int)MineState.Cooking, r.Mines.Mines[second].State);
            r.Step(MineSystem.CookMinTicks + 1);
            Assert.AreEqual((int)MineState.Spent, r.Mines.Mines[second].State, "and it goes off on its own");
        }

        [Test]
        public void ATripwireMayNotCrossATrench()
        {
            using var r = new Rig();
            var map = r.M.Map;
            int cell = -1;
            for (int i = 0; i < map.NavLayers.Length && cell < 0; i++) if ((map.NavLayers[i] & (byte)NavLayer.Trench) != 0) cell = i;
            if (cell < 0) Assert.Ignore("the playtest map has no trench cell");
            var inTrench = new float3((cell % map.NavWidth + 0.5f) * MapData.NavCellSize, 0f, (cell / map.NavWidth + 0.5f) * MapData.NavCellSize);
            var from = inTrench - new float3(0f, 0f, 6f);
            Assert.IsTrue(r.Mines.Lies(from) && r.Mines.Lies(from + new float3(0f, 0f, 12f)), "both ends of the wire stand on open ground either side of the trench");
            Assert.AreEqual(-1, r.Mines.Place(r.W, from, new float3(0f, 0f, 1f), 12f, 0, MineKind.Tripwire), "refused: the wire would run across the trench");
            Assert.IsFalse(r.Mines.LiesAlong(from, new float3(0f, 0f, 1f), 12f));
        }

        [Test]
        public void AMineAheadOfAHullGoesOffBeforeTheBowPassesItAndOneBesideItNever()
        {
            using var r = new Rig();
            var prof = TW.Sim.Nav.VehicleProfile.ForArchetype(4);
            Assert.IsTrue(prof.HalfLength * 0.95f > prof.HalfWidth, "the Maw is longer than it is wide, so the bow reaches past a disc of its half width");
            int ahead = r.Mines.Place(r.W, Open, float3.zero, 0f, 0, MineKind.Mine);
            var beside = Open + new float3(0f, 0f, 60f);
            int aside = r.Mines.Place(r.W, beside, float3.zero, 0f, 0, MineKind.Mine);
            r.Step(MineSystem.ArmTicks + 1);
            int bow = r.Tank(Open + new float3(0f, 0f, prof.HalfLength * 0.95f), team: 1);      // team 1 faces -z: the mine is under its bow
            int flank = r.Tank(beside + new float3(prof.HalfWidth * 1.1f, 0f, 0f), team: 1);    // a mine just outside the hull's side
            r.Step();
            Assert.AreEqual(1, r.Count(SimEventType.MineTriggered, ahead, bow), "under the bow: it goes off before the hull is over it");
            Assert.AreEqual(0, r.Count(SimEventType.MineTriggered, aside), "beside the hull: nothing");
            Assert.AreEqual((int)MineState.Armed, r.Mines.Mines[aside].State);
        }

        [Test]
        public void OnlyTheBlastSystemClearsItsResolvedList()
        {
            // the list is filled at 720 and read at 725, 730, 1000 and 1130: whoever drained it (Deformation once did) starved the readers after it
            string simDir = System.IO.Path.Combine(UnityEngine.Application.dataPath, "_Project", "Sim");
            var clearers = new System.Collections.Generic.List<string>();
            foreach (var file in System.IO.Directory.GetFiles(simDir, "*.cs", System.IO.SearchOption.AllDirectories))
                if (System.IO.File.ReadAllText(file).Contains("Resolved.Clear(")) clearers.Add(System.IO.Path.GetFileName(file));
            Assert.AreEqual(1, clearers.Count, "one owner: " + string.Join(", ", clearers));
            Assert.AreEqual("Blast.cs", clearers[0]);
        }

        [Test]
        public void AMineUnderALongHullsBowIsTakenOnTheHullNotAsANearMiss()
        {
            using var r = new Rig();
            const byte banner = 10;   // VehicleArchetype.Banner: a walker longer than it is wide
            var prof = TW.Sim.Nav.VehicleProfile.ForArchetype(banner);
            Assert.IsTrue(prof.HalfLength > prof.HalfWidth + 0.6f, "the Banner's bow reaches past the disc a burst calls direct");
            int mine = r.Mines.Place(r.W, Open, float3.zero, 0f, 0, MineKind.Mine);
            r.Step(MineSystem.ArmTicks + 1);
            int hull = r.Walker(Open + new float3(0f, 0f, prof.HalfLength - 0.25f), banner, team: 1);   // team 1 faces -z: the mine is 4 m under its bow
            float hp = r.W.Hp[hull];
            r.Step();
            Assert.AreEqual(1, r.Count(SimEventType.MineTriggered, mine, hull), "under the bow, outside the old disc: it goes off");
            r.Step();
            Assert.GreaterOrEqual(hp - r.W.Hp[hull], MineSystem.MineDamage * TW.Sim.Units.VehicleModulesSystem.MineHullShare - 1e-3f, "and the hull takes the mine's share, not a near miss's five points");
        }

        [Test]
        public void TheTriggersBoxAndTheBurstsGateHoldForEveryHull()
        {
            // two margins the code once held by arithmetic nobody asserted (critique round 5): the trigger's box pad against
            // every hull's corner plus a tripwire's reach, and the burst's gate against the corner a covered mine can lie at
            for (byte a = 4; a <= 11; a++)
            {
                var prof = TW.Sim.Nav.VehicleProfile.ForArchetype(a);
                Assert.LessOrEqual(prof.Reach + MineSystem.TripwireReach, MineSystem.WidestHull, "archetype " + a + ": the box pad covers its corner and a tripwire's reach");
                Assert.LessOrEqual(prof.Reach, MineSystem.MineRadius + System.Math.Max(prof.HalfWidth, prof.Reach), "archetype " + a + ": the burst's gate reaches its corner");
                Assert.IsTrue(prof.Covers(0f, new Unity.Mathematics.float3(0f, 0f, 0f), new Unity.Mathematics.float3(prof.HalfWidth + 0.5f, 0f, 0f), TW.Sim.Units.VehicleModulesSystem.MineHullMargin), "archetype " + a + ": the burst's margin reaches half a metre past the flank");
                Assert.IsFalse(prof.Covers(0f, new Unity.Mathematics.float3(0f, 0f, 0f), new Unity.Mathematics.float3(prof.HalfWidth + 0.5f, 0f, 0f)), "archetype " + a + ": the trigger's footprint does not");
            }
        }
    }
}
