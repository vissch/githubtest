// Phase: A2 (implemented 2026-09-28) — the assault ladder: on the ground the battle scene plays (ShelledForest 1917,
// 90 m wide), a garrison of ten riflemen holds the enemy's front trench and an attack of N times its number climbs out
// of ours on ">>", bare or behind support (smoke on the enemy's parapet, an HE line on the enemy trench). A bare attack
// at three to one takes the trench, at two to one it is beaten off but bleeds the garrison, support makes two to one
// enough, and an equal attack with nothing behind it takes nothing. It is the tug of war the game is about: a front
// that cannot move is not one.
using System.Collections.Generic;
using System.IO;
using System.Text;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class AssaultLadderTests
    {
        const int Rifleman = 0, Assault = 1;
        public static uint Field = 1917;   // GreyboxCorridor's BattlefieldSeed

        public enum Support { None, Smoke, Barrage, Both }

        public struct Rung
        {
            public bool Taken; public int TakenTick;
            public int Attackers, Defenders, AttackersLost, DefendersLost;
            public float Closest;          // metres short of the enemy trench the nearest attacker came
            public int ReachedTrench;      // attackers who stood in the enemy trench at any time
            public int ShotsA, HitsA, ShotsD, HitsD, PinnedA;   // the attack's and the garrison's shots and hits; attackers ever pinned
            public string Stray;           // men who never got into their trench before the attack, and where
            public override string ToString()
                => $"{(Taken ? "TAKEN at " + TakenTick : "held      ")}  lost {AttackersLost,2}/{Attackers,2} att  {DefendersLost,2}/{Defenders,2} def  closest {Closest,5:F1} m  inTrench {ReachedTrench}  shots att {ShotsA}/{HitsA} def {ShotsD}/{HitsD}  pinned {PinnedA}  stray {Stray}";
        }

        static void Step(MatchSim m, List<SimCommand> cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds.ToArray(), Allocator.Temp);
            m.Step(arr);
        }

        static void SetHoldFire(MatchSim m, short trench, byte hold)
        {
            var ts = m.Fields.Trenches[trench]; ts.HoldFire = hold; m.Fields.Trenches[trench] = ts;
        }

        static float TrenchZ(MatchSim m, short t)
            => m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[t].CellStart + m.Map.Trenches[t].CellCount / 2]).z;

        /// <summary>One assault: <paramref name="defenders"/> riflemen of team 1 walk into their front trench, then
        /// <paramref name="attackers"/> of team 0 (three riflemen to one assault man) into ours, and ">>" sends ours at
        /// theirs. The garrison gets no reinforcement and no order: what is measured is the assault.</summary>
        public static Rung Run(int attackers, int defenders, Support support, uint seed, int ticks = 3600, int gunners = 0)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 1000000; cfg.Seed = seed;
            var field = BattlefieldParams.ShelledForest(Field); field.Bombardment = 0f;   // the battle scene's ground; shells are nobody's decision
            using var m = MatchSim.CreateBattlefield(cfg, field);
            var w = m.World;
            short own = m.Fields.FrontTrench(0), theirs = m.Fields.FrontTrench(1);
            float theirZ = TrenchZ(m, theirs);
            var cmds = new List<SimCommand>();
            // each side is put down just behind its front trench, spread along it, with the roster's own men, and
            // walks in: how reinforcements get there (boats, the rear trench, a slot's cooldown) is not what is measured
            int goalOwn = m.Fields.GetGoal(GoalKey.Trench(own)), goalTheirs = m.Fields.GetGoal(GoalKey.Trench(theirs));
            float ownZ = TrenchZ(m, own), width = m.Map.SizeMeters.x;
            for (int k = 0; k < defenders; k++)
            {
                int every = gunners > 0 ? math.max(1, defenders / gunners) : 0;   // gunners spread evenly along the line
                var e = gunners > 0 && k % every == every / 2 && k / every < gunners ? RosterEntry.Machinegunner : w.Roster[1 * RosterEntry.SlotCount + Rifleman];
                int s = w.Spawn(1, e.Archetype, new float3((k + 0.5f) * width / defenders, 0f, theirZ + 8f), e.Hp, e.Speed, false);
                w.GoalId[s] = goalTheirs;
            }
            for (int k = 0; k < attackers; k++)
            {
                var e = w.Roster[0 * RosterEntry.SlotCount + (k % 4 == 3 ? Assault : Rifleman)];
                int s = w.Spawn(0, e.Archetype, new float3((k + 0.5f) * width / attackers, 0f, ownZ - 8f), e.Hp, e.Speed, false);
                w.GoalId[s] = goalOwn;
            }
            cmds.Clear();
            // both lines hold their fire while they form up: 110 m apart they are in each other's rifle range, and a
            // garrison thinned before the attack is not the rung being measured
            for (int t = 0; t < 1260; t++)
            {
                for (short tr = 0; tr < m.Map.Trenches.Length; tr++) SetHoldFire(m, tr, 1);
                Step(m, cmds);
                if (t >= 60 && m.Fields.Trenches[own].GarrisonCount >= attackers && m.Fields.Trenches[theirs].GarrisonCount >= defenders) break;
            }
            for (short tr = 0; tr < m.Map.Trenches.Length; tr++) SetHoldFire(m, tr, 0);
            var r = new Rung { Closest = 999f, Stray = "" };
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i)) continue;
                if (w.Team[i] == 0) r.Attackers++; else r.Defenders++;
                if (w.TrenchId[i] < 0) r.Stray += (w.Team[i] == 0 ? "a" : "d") + $"({w.Position[i].x:F0},{w.Position[i].z:F0}) ";
            }

            if (support == Support.Smoke || support == Support.Both)
                // one screen, 40 m of the 90 m front (a second call the same tick is refused by the ability's cooldown)
                cmds.Add(new SimCommand { Tick = w.Tick, Player = 0, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.SmokeScreen,
                                          Pos = new float3(5f, 0f, theirZ - 12f), B = AbilityArgs.Pack(90, 0, 40) });
            if (support == Support.Barrage || support == Support.Both)
                cmds.Add(new SimCommand { Tick = w.Tick, Player = 0, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.HeBarrage,
                                          Pos = new float3(15f, 0f, theirZ), B = AbilityArgs.Pack(90, AbilityPattern.Line, 60) });
            Step(m, cmds); cmds.Clear();
            // over the top as the support comes down: smoke takes two seconds, the barrage four and lasts six
            int wait = support == Support.Barrage || support == Support.Both ? 120 : support == Support.Smoke ? 40 : 0;
            for (int t = 0; t < wait; t++) Step(m, cmds);
            // ">>" on every trench of ours that holds men (a front line may be several trenches), and a man who never
            // got into one goes with them
            for (short tr = 0; tr < m.Map.Trenches.Length; tr++)
                if (m.Fields.Trenches[tr].OwnerTeam == 0 && m.Fields.Trenches[tr].GarrisonCount > 0)
                    cmds.Add(new SimCommand { Tick = w.Tick, Player = 0, Type = CommandType.TrenchAdvance, A = tr });
            for (int i = 0; i < w.HighWater; i++)
                if (w.IsAlive(i) && w.Team[i] == 0 && w.TrenchId[i] < 0) { w.GoalId[i] = goalTheirs; w.Flags[i] |= (uint)UnitFlags.Exposed; }
            Step(m, cmds); cmds.Clear();

            var inTrench = new HashSet<int>();
            int start = (int)w.Tick;
            for (int t = 0; t < ticks; t++)
            {
                Step(m, cmds);
                var ev = w.Events.Events;
                for (int e = 0; e < ev.Length; e++)
                {
                    if (ev[e].Type == SimEventType.TrenchCaptured && ev[e].B == 0 && !r.Taken) { r.Taken = true; r.TakenTick = (int)w.Tick - start; }
                    if (ev[e].Type == SimEventType.Shot && ev[e].A >= 0) { if (w.Team[ev[e].A] == 0) r.ShotsA++; else r.ShotsD++; }
                    if (ev[e].Type == SimEventType.Hit && ev[e].A >= 0 && ev[e].Scalar > 0f) { if (w.Team[ev[e].A] == 0) r.HitsA++; else r.HitsD++; }
                    if (ev[e].Type == SimEventType.Pinned && w.Team[ev[e].A] == 0) r.PinnedA++;
                    if (ev[e].Type == SimEventType.Death && (w.Flags[ev[e].A] & (uint)UnitFlags.Vehicle) == 0)
                    { if (w.Team[ev[e].A] == 0) r.AttackersLost++; else r.DefendersLost++; }
                }
                int alive0 = 0, alive1 = 0;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (!w.IsAlive(i) || (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) continue;
                    if (w.Team[i] == 1) { alive1++; continue; }
                    alive0++;
                    r.Closest = math.min(r.Closest, math.max(0f, theirZ - w.Position[i].z));
                    if (w.TrenchId[i] == theirs || (w.Position[i].z > theirZ - 2f && w.Position[i].z < theirZ + 2f)) inTrench.Add(i);
                }
                if (r.Taken || alive0 == 0) break;
            }
            r.ReachedTrench = inTrench.Count;
            return r;
        }

        /// <summary>Some seeds of one rung: how many took the trench, and the share of the garrison it cost.</summary>
        static (int taken, float bled, string said) Rung4(int ratio, Support support, uint seeds = 4)
        {
            int taken = 0, lost = 0, held = 0; var sb = new StringBuilder();
            for (uint s = 0; s < seeds; s++)
            {
                var r = Run(10 * ratio, 10, support, 0xA55A0000u + s);
                if (r.Taken) taken++;
                lost += r.DefendersLost; held += r.Defenders;
                sb.AppendLine($"{support} {ratio}:1 seed {s}  {r}");
            }
            return (taken, held > 0 ? (float)lost / held : 0f, sb.ToString());
        }

        // The ladder the game is balanced on (2026-09-28, the agent's choice: decisions.md, Open). Before the running
        // man and the bomb, a bare attack at three to one never took the trench and support at two to one took it two
        // times in eight; an attack that failed cost the garrison nobody, so a front could not move.

        [Test]
        public void ABareAttackAtThreeToOne_TakesTheTrench()
        {
            var (taken, _, said) = Rung4(3, Support.None);
            Assert.GreaterOrEqual(taken, 3, said);
        }

        [Test]
        public void ABareAttackAtTwoToOne_IsBeatenOff_ButBleedsTheGarrison()
        {
            // eight seeds: the rung sits on the edge (one seed where eight men got in used to decide it), so four were
            // noise; measured 2026-09-29 with dead ground, 1 of 8 taken and the garrison 22 % down (38 % without it)
            var (taken, bled, said) = Rung4(2, Support.None, 8);
            Assert.LessOrEqual(taken, 3, said);
            Assert.GreaterOrEqual(bled, 0.15f, "the next wave finds a thinner line: " + said);
        }

        [Test]
        public void SmokeOrABarrage_MakesTwoToOneEnough()
        {
            var smoke = Rung4(2, Support.Smoke);
            var barrage = Rung4(2, Support.Barrage);
            Assert.GreaterOrEqual(smoke.taken, 3, smoke.said);
            Assert.GreaterOrEqual(barrage.taken, 3, barrage.said);
        }

        [Test]
        public void AnEqualAttack_WithNothingBehindIt_TakesNothing()
        {
            var (taken, _, said) = Rung4(1, Support.None);
            Assert.AreEqual(0, taken, said);
        }

        [Test, Explicit("the whole ladder, for tuning: nine rungs of four seeds")]
        public void Report_TheAssaultLadder()
        {
            var sb = new StringBuilder();
            int defenders = 10;
            foreach (var support in new[] { Support.None, Support.Smoke, Support.Barrage })
                foreach (int ratio in new[] { 1, 2, 3 })
                {
                    int taken = 0, attLost = 0, defLost = 0, att = 0, def = 0;
                    for (uint s = 0; s < 4; s++)
                    {
                        var r = Run(defenders * ratio, defenders, support, 0xA55A0000u + s);
                        sb.AppendLine($"{support,-8} {ratio}:1 seed {s}  {r}");
                        if (r.Taken) taken++;
                        attLost += r.AttackersLost; defLost += r.DefendersLost; att += r.Attackers; def += r.Defenders;
                    }
                    sb.AppendLine($"== {support,-8} {ratio}:1  taken {taken}/4  attackers lost {attLost}/{att}  defenders lost {defLost}/{def}");
                }
            File.WriteAllText(Path.Combine(Path.GetTempPath(), "tw-assault-ladder.txt"), sb.ToString());
            TestContext.WriteLine(sb.ToString());
        }
    }
}
