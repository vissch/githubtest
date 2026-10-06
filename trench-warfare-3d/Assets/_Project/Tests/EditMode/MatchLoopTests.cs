// Phase: A6 (implemented 2026-09-29) — the match as the game plays it: the battle scene's ground and economy
// (ShelledForest 1917, 300 silver and 2 a second, ambient shelling), the scripted enemy exactly as GreyboxCorridor sets
// it up, and a player who follows one plain policy. It reports whose trenches changed hands and when, and how the
// armies stood minute by minute: whether the front moves, and whether the enemy is a threat at all. The enemy keeps
// deploying, attacks only with the odds (two to one behind a barrage, three bare) and breaks a player who only sits in
// his trench. Before 2026-09-29 it stopped deploying after two minutes (every coin went on harassing fire) and went
// over the top with eight men whatever stood in front of them.
using System.Collections.Generic;
using System.IO;
using System.Text;
using NUnit.Framework;
using Unity.Mathematics;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class MatchLoopTests
    {
        /// <summary>What the player does. Defend: deploy the armed men he can afford and hold his front trench.
        /// Mirror: the same, and go over the top once his front trench holds <see cref="Report.AttackAt"/> men, as the
        /// enemy does. Patient: attack only with twice the enemy's front garrison. Script: the enemy's own script on the
        /// player's seat (ScriptedEnemy.Side 0), so both sides think alike.</summary>
        public enum Policy { Defend, Mirror, Patient, Script }

        public struct Report
        {
            public int CapturedByPlayer, CapturedByEnemy, Winner, EndTick;
            public int AttackAt;
            public int[] Deployed, ByFire, ByBlast, ByGas, ByOther;   // per team: men deployed, and how the dead died
            public string Timeline, Deaths;
            public override string ToString()
                => $"captures player {CapturedByPlayer} enemy {CapturedByEnemy}  winner {(Winner < 0 ? "none" : Winner.ToString())} at tick {EndTick}\n"
                 + $"  deployed player {Deployed[0]} enemy {Deployed[1]}; dead player by fire {ByFire[0]} blast {ByBlast[0]} gas {ByGas[0]} other {ByOther[0]}; "
                 + $"enemy by fire {ByFire[1]} blast {ByBlast[1]} gas {ByGas[1]} other {ByOther[1]}\n{Timeline}{Deaths}";
        }

        /// <summary>The seat a policy issues to: the player's lockstep driver.</summary>
        sealed class Seat : TW.Net.ICommandSink
        {
            readonly TW.Net.LockstepDriver driver;
            public Seat(TW.Net.LockstepDriver d) { driver = d; }
            public int Player => 0;
            public void Issue(SimCommand c) => driver.Issue(c);
        }

        static int ArmedSlot(SimWorld w, int n)
        {
            var slots = new List<int>();
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                var e = w.Roster[s];
                if (!e.IsVehicle && e.Archetype != InfantryArchetype.Medic && e.Archetype != InfantryArchetype.Repair && e.Archetype != InfantryArchetype.Para) slots.Add(s);
            }
            return slots.Count == 0 ? -1 : slots[n % slots.Count];
        }

        public static Report Play(Policy policy, int minutes, uint seed = 0xC0FFEE, ScriptedEnemy ai = null, int attackAt = 8, System.Action<MatchSim> each = null)
        {
            var cfg = SimConfig.Default; cfg.Seed = seed; cfg.StartingSilver = 300; cfg.SilverPerSecond = 2;   // GreyboxCorridor's
            var field = BattlefieldParams.ShelledForest(1917u); field.Bombardment = 8f;
            using var session = new LockstepSession(() => MatchSim.CreateBattlefield(cfg, field), false, 0, 0, 0f, seed);
            ai ??= new ScriptedEnemy();   // the scene's: every 40 ticks, attacks at 8, support with 180 in reserve
            var said = new StringBuilder();
            var callers = ai.Said;   // a caller's own listener hears it too
            ai.Said = x => { said.AppendLine("  enemy: " + x); callers?.Invoke(x); };
            var mirror = policy == Policy.Script ? new ScriptedEnemy { Side = 0, Said = x => said.AppendLine("  player: " + x) } : null;
            var seat = new Seat(session.LocalDriver);
            var m = session.Local; var w = m.World;
            var r = new Report { Winner = -1, AttackAt = attackAt, Deployed = new int[2], ByFire = new int[2], ByBlast = new int[2], ByGas = new int[2], ByOther = new int[2] };
            var sb = new StringBuilder();
            int ticks = minutes * 60 * 20, deployed = 0;
            uint last = uint.MaxValue;
            int guard = ticks * 40;
            while (w.Tick < ticks && guard-- > 0)
            {
                uint t = w.Tick;
                if (t != last)
                {
                    last = t;
                    // the player's policy, once a tick, as a player at the deploy bar and the trench buttons
                    if (mirror != null) mirror.Think(m, m, seat, null);
                    else if (t % 40 == 0)
                    {
                        int slot = ArmedSlot(w, deployed);
                        if (slot >= 0 && w.Silver[0] >= w.Roster[slot].Cost && w.SlotCooldown[slot] == 0) { seat.Issue(SimCommand.Deploy(t, 0, slot)); deployed++; }
                    }
                    if (mirror == null && t % 100 == 20)
                    {
                        short front = m.Fields.FrontTrench(0);
                        for (short k = 0; k < m.Fields.Trenches.Length; k++)
                        {
                            var ts = m.Fields.Trenches[k];
                            if (ts.OwnerTeam != 0) continue;
                            byte want = (byte)(k != front ? 1 : 0);
                            if (ts.Locked != want) seat.Issue(new SimCommand { Tick = t, Player = 0, Type = CommandType.TrenchLock, A = k, B = want });
                            if (k != front && ts.GarrisonCount > 0) seat.Issue(new SimCommand { Tick = t, Player = 0, Type = CommandType.TrenchAdvance, A = k });
                        }
                    }
                    if (mirror == null && policy != Policy.Defend && t % 100 == 60)
                    {
                        short front = m.Fields.FrontTrench(0), theirs = m.Fields.FrontTrench(1);
                        int mine = front >= 0 ? m.Fields.Trenches[front].GarrisonCount : 0;
                        int them = theirs >= 0 ? m.Fields.Trenches[theirs].GarrisonCount : 0;
                        bool go = policy == Policy.Mirror ? mine >= attackAt : mine >= math.max(attackAt, 2 * them);
                        if (front >= 0 && go) { seat.Issue(new SimCommand { Tick = t, Player = 0, Type = CommandType.TrenchAdvance, A = front }); sb.AppendLine($"  player: {t / 20} s over the top, {mine} against {them}"); }
                    }
                }
                session.StepOnce(ai);
                each?.Invoke(m);
                var ev = w.Events.Events;
                for (int k = 0; k < ev.Length; k++)
                {
                    if (ev[k].Type == SimEventType.TrenchCaptured)
                    {
                        if (ev[k].B == 0) r.CapturedByPlayer++; else r.CapturedByEnemy++;
                        sb.AppendLine($"  {w.Tick / 20,4} s  trench {ev[k].A} taken by {(ev[k].B == 0 ? "player" : "enemy")}");
                    }
                    if (ev[k].Type == SimEventType.MatchEnded) { r.Winner = ev[k].A; }
                    if (ev[k].Type == SimEventType.UnitDeployed) r.Deployed[(int)ev[k].Dir.y & 1]++;
                    if (ev[k].Type == SimEventType.Death && (w.Flags[ev[k].A] & (uint)UnitFlags.Vehicle) == 0)
                    {
                        int team = w.Team[ev[k].A] & 1, why = ev[k].B;
                        string killer = why >= 0 ? (w.TrenchId[why] >= 0 ? $"garrison z{(int)w.Position[why].z} arch{w.Archetype[why]}" : $"open z{(int)w.Position[why].z} arch{w.Archetype[why]}") : ((DeathCause)why).ToString();
                        r.Deaths += $"    {w.Tick / 20,4} s team{team} {(w.TrenchId[ev[k].A] >= 0 ? "garrison" : "open")} z{(int)w.Position[ev[k].A].z} by {killer}\n";
                        if (why >= 0) r.ByFire[team]++;
                        else if (why == (int)DeathCause.Blast) r.ByBlast[team]++;
                        else if (why == (int)DeathCause.Gas) r.ByGas[team]++;
                        else r.ByOther[team]++;
                    }
                }
                if (w.Tick % 1200 == 0 && w.Tick > 0 && w.Tick != r.EndTick)
                {
                    r.EndTick = (int)w.Tick;
                    int a = 0, b = 0, ga = 0, gb = 0;
                    for (int i = 0; i < w.HighWater; i++)
                    {
                        if (!w.IsAlive(i) || (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) continue;
                        if (w.Team[i] == 0) { a++; if (w.TrenchId[i] >= 0) ga++; } else { b++; if (w.TrenchId[i] >= 0) gb++; }
                    }
                    sb.AppendLine($"  {w.Tick / 20,4} s  player {a,3} men ({ga} in trenches)  enemy {b,3} ({gb})  silver {w.Silver[0]}/{w.Silver[1]}  fronts {m.Fields.FrontTrench(0)}/{m.Fields.FrontTrench(1)}  kills {m.Fire.Kills[0]}/{m.Fire.Kills[1]}");
                }
                if (r.Winner >= 0) break;
            }
            r.EndTick = (int)w.Tick;
            r.Timeline = sb.ToString() + said.ToString();
            return r;
        }

        /// <summary>Ten minutes against a player who only holds his trench, on three seeds: played once and shared by the
        /// tests below (each playing its own three cost the gate twelve matches; EditMode runs near its 600 s limit).</summary>
        sealed class Defence
        {
            public readonly List<Report> Reports = new List<Report>();
            public readonly List<string> Decisions = new List<string>();
            public int Hoarded, Short;   // samples short of men for the odds, and those sitting on the reserve
        }
        static Defence defended;

        static Defence Defended()
        {
            if (defended != null) return defended;
            var d = new Defence();
            for (uint s = 1; s <= 3; s++)
            {
                var ai = new ScriptedEnemy { Said = x => d.Decisions.Add(x) };
                d.Reports.Add(Play(Policy.Defend, 10, s, ai, 8, m =>
                {
                    var w = m.World;
                    if (w.Tick < 600 || w.Tick % 100 != 0) return;
                    short theirs = m.Fields.FrontTrench(0);
                    int held = theirs >= 0 ? m.Fields.Trenches[theirs].GarrisonCount : 0, men = 0;
                    for (int i = 0; i < w.HighWater; i++)
                        if (w.IsAlive(i) && w.Team[i] == 1 && (w.Flags[i] & (uint)UnitFlags.Vehicle) == 0) men++;
                    if (men >= math.max(8, 2 * held)) return;
                    d.Short++;
                    if (w.Silver[1] >= 180) d.Hoarded++;   // the reserve, and more than any man it deploys costs
                }));
            }
            return defended = d;
        }

        [Test]
        public void TheEnemy_KeepsItsArmyComing()
        {
            // a rate, not a count: a match it wins in four minutes deploys fewer (the old script: 8 in ten minutes)
            foreach (var r in Defended().Reports)
                Assert.GreaterOrEqual(r.Deployed[1] * 1200f / math.max(1, r.EndTick), 1.5f, "men a minute: it spent its silver on men, not only on shells: " + r);
        }

        [Test]
        public void TheEnemy_AttacksOnlyWithTheOdds()
        {
            var said = Defended().Decisions;
            Assert.IsNotEmpty(said, "it attacked at all");
            foreach (var d in said)
            {
                if (!d.Contains("barrage called") && !d.Contains("bare")) continue;
                var parts = d.Split(' ');
                int mine = int.Parse(parts[parts.Length - 3]), held = int.Parse(parts[parts.Length - 1]);
                // patience lowers the odds behind a barrage, never below even, and says so
                int at = d.IndexOf("odds ", System.StringComparison.Ordinal);
                float want = d.Contains("bare") ? 3f : at >= 0 ? float.Parse(d.Substring(at + 5, 3), System.Globalization.CultureInfo.InvariantCulture) : 2f;
                Assert.GreaterOrEqual(want, 1f, d);
                Assert.GreaterOrEqual(mine, want * held, d);
                Assert.GreaterOrEqual(mine, 8, d);
            }
        }

        [Test]
        public void TheEnemy_BreaksAPlayerWhoOnlySitsInHisTrench()
        {
            int broke = 0;
            foreach (var r in Defended().Reports) if (r.CapturedByEnemy > 0) broke++;
            Assert.GreaterOrEqual(broke, 2, "in ten minutes, on two seeds of three");
        }

        [Test]
        public void TheEnemy_BuysMenUntilItHasTheOdds_BeforeItSavesForTheBarrage()
        {
            // Saving at parity (2026-09-29, seen in Play) held it at the player's count: 18 men to 6 with only ten in its
            // front trench and 200 silver it never spent, so it never reached two to one there and sat for four minutes
            var d = Defended();
            Assert.Greater(d.Short, 20, "it was short of men for a while");
            Assert.LessOrEqual(d.Hoarded, d.Short / 20, $"short of men, it sat on its silver {d.Hoarded} times in {d.Short}");
        }

        [Test]
        public void TheScriptAgainstItself_TakesTrenches()
        {
            // With the trench mouth hidden from far guns (v23) nobody died between attacks, both sides grew alike, and
            // the script waiting for two to one never went over: six ten-minute matches against itself took no trench.
            // Patience lowers the odds it wants behind a barrage every two minutes without an attack.
            int decided = 0;
            for (uint s = 0; s < 3; s++)
            {
                var r = Play(Policy.Script, 10, 0xC0FFEEu + s * 7919u);
                if (r.CapturedByPlayer + r.CapturedByEnemy > 0) decided++;
            }
            Assert.GreaterOrEqual(decided, 2, "a trench changed hands in ten minutes, on two seeds of three");
        }

        [Test, Explicit("ten minutes of each policy against the scene's enemy, for tuning")]
        public void Report_TheMatchLoop()
        {
            var sb = new StringBuilder();
            foreach (var p in new[] { Policy.Defend, Policy.Mirror, Policy.Patient, Policy.Script })
                sb.AppendLine($"== {p}\n{Play(p, 10)}");
            File.WriteAllText(Path.Combine(Path.GetTempPath(), "tw-match-loop.txt"), sb.ToString());
            TestContext.WriteLine(sb.ToString());
        }
    }
}
