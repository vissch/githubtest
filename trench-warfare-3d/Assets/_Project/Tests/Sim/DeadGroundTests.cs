// Phase: A2 (implemented 2026-09-29) — dead ground: a man on foot in the open well behind his own front trench is out
// of sight of the far side's small arms (TargetAcquisition, CombatTables.DeadGroundMetres). The same man out in no
// man's land is seen, and a man in his own trench is what he always was. Before it, a machine gun in the enemy's
// front trench shot the player's reinforcements dead on the way up from the spawn (MatchLoopTests).
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
    public class DeadGroundTests
    {
        static void Step(MatchSim m)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(none);
        }

        static float TrenchZ(MatchSim m, short t, float x)
        {
            // the trench's z at this column, as TargetAcquisition measures it
            float sum = 0f; int n = 0;
            var def = m.Map.Trenches[t];
            int col = (int)(x / MapData.NavCellSize);
            for (int c = 0; c < def.CellCount; c++)
            {
                int cell = m.Map.TrenchCells[def.CellStart + c];
                if (cell % m.Map.NavWidth != col) continue;
                sum += m.Map.NavCellCenter(cell).z; n++;
            }
            Assert.Greater(n, 0, "setup: the trench crosses this column");
            return sum / n;
        }

        /// <summary>The battle scene's ground with an enemy machine gunner in his front trench at x 45, and one
        /// unkillable, unmoving rifleman of ours at <paramref name="behindOurs"/> metres behind our front trench
        /// (negative: in front of it). Returns how many ticks of 200 the gunner had him as his target.</summary>
        static int Watched(float behindOurs)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var field = BattlefieldParams.ShelledForest(1917u); field.Bombardment = 0f;
            using var m = MatchSim.CreateBattlefield(cfg, field);
            var w = m.World;
            short ours = m.Fields.FrontTrench(0), theirs = m.Fields.FrontTrench(1);
            var e = w.Roster[1 * RosterEntry.SlotCount + 0];
            int gunner = w.Spawn(1, InfantryArchetype.Machinegunner, new float3(45f, 0f, TrenchZ(m, theirs, 45f) + 6f), 100000f, e.Speed, false);
            w.MaxHp[gunner] = 100000f;
            w.GoalId[gunner] = m.Fields.GetGoal(GoalKey.Trench(theirs));
            for (int k = 0; k < 600 && w.TrenchId[gunner] < 0; k++) Step(m);
            Assert.GreaterOrEqual(w.TrenchId[gunner], 0, "setup: the gunner is in his trench");
            float x = w.Position[gunner].x;
            int man = w.Spawn(0, InfantryArchetype.Rifle, new float3(x, 0f, TrenchZ(m, ours, x) - behindOurs), 100000f, 0f, false);
            w.MaxHp[man] = 100000f;
            float range = math.distance(w.Position[man].xz, w.Position[gunner].xz);
            Assert.Less(range, CombatTables.WeaponFor(InfantryArchetype.Machinegunner).RangeMax, "setup: he is inside the gun's range");
            int seen = 0;
            for (int k = 0; k < 200; k++) { Step(m); if (w.TargetSlot[gunner] == man) seen++; }
            return seen;
        }

        [Test]
        public void AManComingUpBehindHisLines_IsOutOfTheEnemyGunnersSight()
        {
            Assert.AreEqual(0, Watched(30f), "thirty metres behind his own front trench he is on the approaches");
        }

        [Test]
        public void TheSameManInNoMansLand_IsSeen()
        {
            Assert.Greater(Watched(-20f), 100, "twenty metres in front of his trench he is fair game");
        }
    }
}
