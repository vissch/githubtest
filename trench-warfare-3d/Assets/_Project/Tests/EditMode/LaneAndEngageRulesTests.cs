// Phase: A3 (2026-09-28) — the rules behind SpreadAndEngageTests: the lane a man keeps to (Lane), where a trench
// wall may be crossed (FlowField), and who goes after whom and where he stops (EngageSystem).
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
    public class LaneAndEngageRulesTests
    {
        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        // ---- lanes ---------------------------------------------------------------------------------------------

        [Test]
        public void Lanes_OfOneDeployment_CoverTheWidthBetweenThem()
        {
            const float width = 90f;
            foreach (int first in new[] { 0, 17, 300, 3000 })
            {
                var strips = new int[9];   // 10 m each
                for (int slot = first; slot < first + 24; slot++)
                {
                    float x = Lane.Of(slot, 1, width);
                    Assert.GreaterOrEqual(x, Lane.Margin, $"slot {slot}");
                    Assert.LessOrEqual(x, width - Lane.Margin, $"slot {slot}");
                    strips[math.min(8, (int)(x / 10f))]++;
                }
                for (int s = 0; s < 9; s++)
                    Assert.GreaterOrEqual(strips[s], 1, $"24 men from slot {first}: nobody's lane lies between {s * 10} and {s * 10 + 10} m");
            }
        }

        [Test]
        public void ALane_IsTheSameEveryTime_AndTheNextTenantOfASlotGetsAnother()
        {
            Assert.AreEqual(Lane.Of(41, 3, 180f), Lane.Of(41, 3, 180f));
            Assert.Greater(math.abs(Lane.Of(41, 3, 180f) - Lane.Of(41, 4, 180f)), 20f, "a slot's next tenant walks another line");
            Assert.Greater(math.abs(Lane.Of(41, 3, 180f) - Lane.Of(42, 3, 180f)), 20f, "neighbouring slots walk far apart");
        }

        [Test]
        public void TheTurn_MakesForTheLane_AndNeverAgainstTheFlow()
        {
            var ahead = new float2(0f, 1f);   // the field runs up the map (team 0); its left-hand normal is -X
            Assert.Less(Lane.Turn(ahead, 40f, 60f), 0f, "his lane is to his right: he turns right");
            Assert.Greater(Lane.Turn(ahead, 60f, 40f), 0f, "his lane is to his left: he turns left");
            Assert.AreEqual(0f, Lane.Turn(ahead, 50f, 50f), "on his lane he walks straight");
            Assert.AreEqual(-Lane.Pull, Lane.Turn(ahead, 0f, 60f), 1e-6f, "far off it the turn is whole, and no more");
            Assert.Less(math.abs(Lane.Turn(ahead, 49f, 50f)), Lane.Pull * 0.2f, "near it the turn eases off");
            var back = new float2(0f, -1f);   // team 1 walks down the map: the same lanes, the other hand
            Assert.Greater(Lane.Turn(back, 40f, 60f), 0f);
            // where the field itself runs across the map (along a wire belt to its gap), the lane has no say
            Assert.AreEqual(0f, Lane.Turn(new float2(1f, 0f), 40f, 60f), 1e-6f);
            Assert.AreEqual(0f, Lane.Turn(new float2(-1f, 0f), 40f, 60f), 1e-6f);
            // a turn of Pull leaves him going forward: the flow's own share of his way is never less than 0.74
            float2 mixed = math.normalize(ahead + new float2(-ahead.y, ahead.x) * Lane.Pull);
            Assert.Greater(math.dot(mixed, ahead), 0.74f);
        }

        // ---- the parapet ---------------------------------------------------------------------------------------

        [Test]
        public void AManCrossesATrenchWallAnywhere_ButNothingBlocked()
        {
            byte ground = (byte)NavLayer.Surface, trench = (byte)NavLayer.Trench, ladder = (byte)(NavLayer.Trench | NavLayer.Link);
            Assert.IsTrue(FlowField.CanStepInfantry(ground, trench), "down into a trench");
            Assert.IsTrue(FlowField.CanStepInfantry(trench, ground), "over the top");
            Assert.IsTrue(FlowField.CanStepInfantry(ground, ladder));
            Assert.IsTrue(FlowField.CanStepInfantry(ground, (byte)(NavLayer.Surface | NavLayer.Wire)), "wire is slow, not shut");
            Assert.IsFalse(FlowField.CanStepInfantry(ground, (byte)NavLayer.Blocked));
            Assert.IsFalse(FlowField.CanStepInfantry(ground, (byte)(NavLayer.Trench | NavLayer.Blocked)));
            Assert.IsFalse(FlowField.CanStepInfantry(ground, (byte)NavLayer.Bunker), "a bunker's inside still needs its door");
            Assert.IsTrue(FlowField.OverTheParapet(ground, trench));
            Assert.IsTrue(FlowField.OverTheParapet(trench, ground));
            Assert.IsFalse(FlowField.OverTheParapet(ground, ladder), "a ladder is not the wall");
            Assert.IsFalse(FlowField.OverTheParapet(trench, trench));
            Assert.IsFalse(FlowField.OverTheParapet(ground, ground));
        }

        /// <summary>The wall costs ParapetCost in the field, so a field still counts a trench as something in the way:
        /// the cell behind a trench is further from a goal in front of it than the width of the trench alone.</summary>
        [Test]
        public void TheFieldCountsTheWall()
        {
            using var map = GreyboxMapGenerator.Create(Allocator.Persistent);
            var field = new FlowField(map.NavWidth, map.NavLength, Allocator.Persistent);
            int x = 17;   // not a ladder column (every tenth from 5)
            int zTrench = map.NavCellOf(map.NavCellCenter(map.TrenchCells[map.Trenches[0].CellStart])).y;
            int before = zTrench - 1, z = zTrench;
            while ((map.NavLayers[map.NavIndex(x, z)] & (byte)NavLayer.Trench) != 0) z++;
            int goal = map.NavIndex(x, z), start = map.NavIndex(x, before);
            var goals = new NativeArray<int>(new[] { goal }, Allocator.Temp);
            field.Build(map, goals);
            int cells = z - before;
            Assert.AreEqual(cells * 10 + 2 * FlowField.ParapetCost, field.Integration[start],
                $"{cells} cells straight across the trench, and its two walls");
            field.Dispose();
        }

        // ---- who goes after whom -------------------------------------------------------------------------------

        [Test]
        public void HoldDistance_FollowsTheWeapon()
        {
            var rifle = CombatTables.WeaponFor(InfantryArchetype.Rifle);
            var smg = CombatTables.WeaponFor(InfantryArchetype.Assault);
            var mg = CombatTables.WeaponFor(InfantryArchetype.Machinegunner);
            var man = InfantrySpec.For(InfantryArchetype.Rifle);
            var gunner = InfantrySpec.For(InfantryArchetype.Machinegunner);
            Assert.AreEqual(rifle.RangeMax * 0.5f, EngageSystem.HoldDistance(man, rifle, exposed: false), 1e-4f, "half his rifle's range, where its accuracy is whole");
            Assert.AreEqual(CombatTables.AdvanceFireRange * 0.75f, EngageSystem.HoldDistance(man, rifle, exposed: true), 1e-4f, "under orders he only engages within 60 m");
            Assert.Less(EngageSystem.HoldDistance(InfantrySpec.For(InfantryArchetype.Assault), smg, true), EngageSystem.HoldDistance(man, rifle, true), "an assault man goes in closer");
            Assert.Greater(EngageSystem.HoldDistance(gunner, mg, false), EngageSystem.HoldDistance(man, rifle, false), "a machine gun is set up further off");
            Assert.IsTrue(gunner.Braced, "setup: the machine gun is the braced weapon");
        }

        [Test]
        public void WhoFights()
        {
            Assert.IsTrue(EngageSystem.Fights(InfantrySpec.For(InfantryArchetype.Rifle), CombatTables.WeaponFor(InfantryArchetype.Rifle)));
            Assert.IsTrue(EngageSystem.Fights(InfantrySpec.For(InfantryArchetype.Officer), CombatTables.WeaponFor(InfantryArchetype.Officer)));
            Assert.IsFalse(EngageSystem.Fights(InfantrySpec.For(InfantryArchetype.Medic), CombatTables.WeaponFor(InfantryArchetype.Medic)), "a medic has no weapon");
            Assert.IsFalse(EngageSystem.Fights(InfantrySpec.For(InfantryArchetype.Repair), CombatTables.WeaponFor(InfantryArchetype.Repair)), "an engineer has a machine to mend");
        }

        /// <summary>One man of each side in the open on the greybox field, the enemy standing still.</summary>
        static MatchSim Pair(byte archetype, float apart, out int man, out int foe, bool exposed = true)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var m = MatchSim.CreateGreybox(cfg);
            var w = m.World;
            man = w.Spawn(0, archetype, new float3(150f, 0f, 300f), 100f, 3f, false);
            w.GoalId[man] = m.Fields.GetGoal(GoalKey.Trench(1));
            if (exposed) w.Flags[man] |= (uint)UnitFlags.Exposed;
            foe = w.Spawn(1, InfantryArchetype.Medic, new float3(150f + apart, 0f, 300f), 100000f, 0.001f, false);   // unarmed, and he lasts
            return m;
        }

        [Test]
        public void AMan_LeavesHisWay_ForAnEnemyInTheOpen_AndStopsAtHisDistance()
        {
            // the enemy is 5 m inside the hunt radius, off to his right; his goal is 380 m straight up the field
            using var m = Pair(InfantryArchetype.Rifle, EngageSystem.HuntRadius - 5f, out int man, out int foe);
            var w = m.World;
            bool closed = false, held = false;
            for (int t = 0; t < 400; t++)
            {
                Step(m);
                byte mode = m.Movement.Engage[man];
                if (mode == MovementSystem.EngageClose) closed = true;
                if (mode == MovementSystem.EngageHold) held = true;
            }
            Assert.IsTrue(closed, "he went after him");
            Assert.IsTrue(held, "and stopped to shoot");
            Assert.AreEqual(foe, m.Engage.Hunt[man] >= 0 ? m.Engage.Hunt[man] : w.TargetSlot[man], "the man he is after");
            float dist = math.distance(w.Position[man].xz, w.Position[foe].xz);
            float hold = EngageSystem.HoldDistance(InfantrySpec.For(InfantryArchetype.Rifle), CombatTables.WeaponFor(InfantryArchetype.Rifle), true);
            Assert.LessOrEqual(dist, hold * EngageSystem.HoldSlack, $"he stands {dist:F1} m from him");
            Assert.Greater(dist, hold * 0.8f, $"he stands {dist:F1} m from him: he did not run on into him");
            Assert.Less(math.length(w.Velocity[man].xz), 0.05f, "he stands still");
            Assert.AreEqual((byte)Stance.Crouch, w.StanceOf[man], "on one knee");
            Assert.Less(w.Position[man].z, 310f, "he has not gone on up the field");
            float2 facing = SimMath.DirFromYaw(w.Yaw[man]).xz;
            Assert.Greater(math.dot(facing, math.normalize(w.Position[foe].xz - w.Position[man].xz)), 0.95f, "facing him");
        }

        [Test]
        public void BeyondTheHuntRadius_HeKeepsToHisWay()
        {
            using var m = Pair(InfantryArchetype.Rifle, EngageSystem.HuntRadius + 25f, out int man, out int foe);
            for (int t = 0; t < 60; t++)
            {
                Step(m);
                Assert.AreEqual(MovementSystem.EngageNone, m.Movement.Engage[man], $"tick {t}");
            }
            Assert.Greater(m.World.Position[man].z, 305f, "he went on up the field");
        }

        [Test]
        public void AMedic_GoesAfterNobody()
        {
            using var m = Pair(InfantryArchetype.Medic, 30f, out int man, out int foe);
            for (int t = 0; t < 60; t++)
            {
                Step(m);
                Assert.AreEqual(MovementSystem.EngageNone, m.Movement.Engage[man], $"tick {t}");
            }
        }

        [Test]
        public void NobodyIsHuntedAcrossWire()
        {
            // he would have 20 m to go to his distance, and 8 m along it the wire begins
            using var m = Pair(InfantryArchetype.Rifle, 65f, out int man, out int foe);
            var map = m.Map;
            var a = map.NavCellOf(m.World.Position[man]);
            for (int dz = -6; dz <= 6; dz++)
                for (int dx = 4; dx <= 5; dx++)
                    map.SetLayer(a.x + dx, a.y + dz, NavLayer.Surface | NavLayer.Wire);
            map.RebuildCost();
            for (int t = 0; t < 100; t++)
            {
                Step(m);
                Assert.AreNotEqual(MovementSystem.EngageClose, m.Movement.Engage[man], $"tick {t}: he set off through the wire");
            }
        }
    }
}
