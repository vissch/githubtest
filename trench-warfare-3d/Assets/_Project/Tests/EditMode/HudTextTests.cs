// Phase: tooling (2026-09-23) — the HUD's factual claims are checked against the simulation they describe: every
// deployable machine has a name, a tooltip and an icon of its own, no tooltip outgrows the hint line, and every
// number quoted in one (the Kettle's minimum range, the Pavise's maximum and its claim to the longest reach on the
// field, the Pincer's blind arc, the Banner's standard, the barrage's shells, radius and delay) is the sim's number.
//
// Why this file exists. Three tooltip claims were checked by hand and two were wrong: the Pincer's guns were called
// turrets when they are sponson mounts, and a comment asserted no walker is crewed when TankSpec.Unmanned only
// means nobody climbs out of the wreck. Worse than being wrong, the file SAID it had been checked — there was a
// doc comment claiming the specifics were confirmed against RosterEntry and the vehicle systems, written before
// anybody confirmed anything. Prose cannot be trusted to stay true to code it merely sits near, so the claims are
// now assertions: change the sim and the tooltip fails until somebody rewrites it.
using NUnit.Framework;
using Unity.Collections;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;

namespace TW.Tests
{
    public class HudTextTests
    {
        /// <summary>Player 0's roster as the sim fills it: the archetypes the deploy bar will actually be asked to
        /// draw. Reading it from FillDefault rather than listing them here is the point — a roster that grows finds
        /// these tests, which is how the Banner and the Redoubt were caught with no name and no tooltip.</summary>
        static byte[] DeployableArchetypes()
        {
            var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            RosterEntry.FillDefault(roster, 0);
            var found = new System.Collections.Generic.List<byte>();
            for (int s = 0; s < RosterEntry.SlotCount; s++)
                if (roster[s].IsVehicle) found.Add(roster[s].Archetype);
            roster.Dispose();
            return found.ToArray();
        }

        [Test]
        public void EveryDeployableMachineHasANameOfItsOwn()
        {
            foreach (byte a in DeployableArchetypes())
                Assert.That(BattleHud.VehicleName(a), Is.Not.EqualTo("Vehicle"),
                    $"archetype {a} is in the default roster but falls through to the generic name, so its deploy " +
                    "button is labelled \"Vehicle\"");
        }

        [Test]
        public void EveryDeployableMachineHasATooltipOfItsOwn()
        {
            string generic = BattleHud.VehicleTip(200);   // an archetype that cannot exist: the default branch
            foreach (byte a in DeployableArchetypes())
                Assert.That(BattleHud.VehicleTip(a), Is.Not.EqualTo(generic),
                    $"archetype {a} is deployable but falls through to the generic tooltip, so the player is told " +
                    "nothing about what they are buying");
        }

        /// <summary>One banner per event (CombatFx drew a second, IMGUI one over the HUD's until a player build's shot showed
        /// both, 2026-09-27): every support card fired by either side names itself, in a banner the plate can hold.</summary>
        [Test]
        public void EverySupportAbilityHasABannerForEitherSide()
        {
            foreach (var id in TW.UI.HudView.SupportAbilities)
                foreach (bool mine in new[] { true, false })
                {
                    string text = TW.UI.HudText.AbilityBanner(id, mine);
                    StringAssert.Contains(mine ? TW.UI.HudText.Support(id).Name : TW.UI.HudText.Support(id).Name.ToUpperInvariant(), text, id + " is named");
                    Assert.LessOrEqual(text.Length, 32, id + ": '" + text + "' fits the banner plate");
                }
            // a card's nameplate shows seven capitals whole ("BARRAGE", "ASSAULT"); "CREEPING" drew as "CREEPI..." in the
            // player build's shot
            foreach (var id in TW.UI.HudView.SupportAbilities)
                Assert.LessOrEqual(TW.UI.HudText.Support(id).Card.Length, 7, id + ": '" + TW.UI.HudText.Support(id).Card + "' fits its card");
            Assert.AreNotEqual(TW.UI.HudText.AbilityBanner(OffMapAbilityId.StrafeRun, true), TW.UI.HudText.AbilityBanner(OffMapAbilityId.StrafeRun, false), "yours and theirs read differently");
        }

        /// <summary>What ObjectiveTracker.OnEvent shows for each event (it calls BannerFor and nothing else): every support
        /// ability fired by either side raises its banner, not only the enemy's barrages as before 2026-09-27.</summary>
        [Test]
        public void TheHudRaisesABannerForEveryAbilityTrenchAndTheEnd()
        {
            foreach (var id in TW.UI.HudView.SupportAbilities)
                foreach (int player in new[] { 0, 1 })
                {
                    Assert.IsTrue(TW.UI.ObjectiveTracker.BannerFor(new SimEvent { Type = SimEventType.AbilityFired, A = (int)id, B = player }, out var text, out _, out var cls, out int rank), id + " by player " + player);
                    Assert.AreEqual(TW.UI.HudText.AbilityBanner(id, player == 0), text);
                    Assert.AreEqual(player == 0 ? null : "tw-banner--defeat", cls, "the enemy's is a warning");
                    Assert.AreEqual(TW.Presentation.BannerRules.Rank(SimEventType.AbilityFired, player == 0), rank);
                }
            Assert.IsTrue(TW.UI.ObjectiveTracker.BannerFor(new SimEvent { Type = SimEventType.TrenchCaptured, A = 2, B = 1 }, out var lost, out _, out var lostCls, out _));
            Assert.AreEqual("Trench 2 lost", lost); Assert.AreEqual("tw-banner--defeat", lostCls);
            Assert.IsTrue(TW.UI.ObjectiveTracker.BannerFor(new SimEvent { Type = SimEventType.MatchEnded, A = 0 }, out var won, out float stays, out _, out int endRank));
            Assert.AreEqual(TW.UI.HudText.VictoryBanner, won); Assert.GreaterOrEqual(stays, 600f, "the end stays up"); Assert.AreEqual(TW.Presentation.BannerRules.MatchEnd, endRank);
            Assert.IsFalse(TW.UI.ObjectiveTracker.BannerFor(new SimEvent { Type = SimEventType.Shot }, out _, out _, out _, out _), "a shot raises nothing");
        }

        /// <summary>The aim hint offers Tab only for an ability with patterns to cycle.</summary>
        [Test]
        public void TheAimHintOffersTabOnlyWhereThereArePatterns()
        {
            foreach (var id in TW.UI.HudView.SupportAbilities)
            {
                Assert.IsTrue(OffMapAbilitySystem.TryGetStats((int)id, out var s), id.ToString());
                bool cycles = (s.Patterns & ~1) != 0;
                foreach (bool line in new[] { false, true })
                    Assert.AreEqual(cycles, TW.UI.HudText.AimHintFor(line, cycles).Contains("Tab"), id + (line ? " (line)" : " (point)"));
            }
            StringAssert.Contains("Esc", TW.UI.HudText.AimHintFor(false, false), "the hint still says how to cancel");
            foreach (var id in new[] { OffMapAbilityId.SmokeScreen, OffMapAbilityId.StrafeRun, OffMapAbilityId.Beam, OffMapAbilityId.CreepingBarrage })
            {
                OffMapAbilitySystem.TryGetStats((int)id, out var s);
                Assert.AreEqual(0, s.Patterns & ~1, id + " has nothing to cycle, so its hint must not offer Tab");
            }
        }

        /// <summary>Both HUDs' banners go through BannerRules (ObjectiveTracker, CombatFx.OnGUI): a stream of ability
        /// banners must not wipe a trench changing hands or the match's end off the plate, and a finished banner gives way.</summary>
        [Test]
        public void ABannerIsNotCoveredByOneThatMattersLess()
        {
            int own = TW.Presentation.BannerRules.Rank(SimEventType.AbilityFired, true), enemy = TW.Presentation.BannerRules.Rank(SimEventType.AbilityFired, false);
            int trench = TW.Presentation.BannerRules.Rank(SimEventType.TrenchCaptured, false), end = TW.Presentation.BannerRules.Rank(SimEventType.MatchEnded, true);
            Assert.That(own < enemy && enemy < trench && trench < end, "own ability < enemy ability < trench < match end");
            Assert.AreEqual(0, TW.Presentation.BannerRules.Rank(SimEventType.Shot, true), "a shot raises no banner");
            Assert.IsFalse(TW.Presentation.BannerRules.Replaces(own, trench, true), "your barrage does not cover 'Trench 2 lost'");
            Assert.IsFalse(TW.Presentation.BannerRules.Replaces(trench, end, true), "nothing covers the match's end");
            Assert.IsTrue(TW.Presentation.BannerRules.Replaces(enemy, own, true), "the enemy's barrage covers your own");
            Assert.IsTrue(TW.Presentation.BannerRules.Replaces(trench, trench, true), "the newer of two trench banners shows");
            Assert.IsTrue(TW.Presentation.BannerRules.Replaces(own, end, false), "a finished banner gives way to anything");
            Assert.IsFalse(TW.Presentation.BannerRules.Replaces(0, 0, false), "an event with no banner shows nothing");
        }

        /// <summary>The hint line is one GUI.Label with a fixed rect: it clips rather than wraps, so a long tooltip
        /// is silently cut off. At the narrowest window that is about 100 characters.</summary>
        [Test]
        public void NoTooltipOutgrowsTheHintLine()
        {
            foreach (byte a in DeployableArchetypes())
            {
                string tip = BattleHud.VehicleTip(a);
                Assert.That(tip.Length, Is.LessThanOrEqualTo(100),
                    $"{BattleHud.VehicleName(a)}'s tooltip is {tip.Length} characters and will be clipped: \"{tip}\"");
            }
            foreach (string tip in new[] { BattleHud.BarrageTip, BattleHud.GasTip })
                Assert.That(tip.Length, Is.LessThanOrEqualTo(100), $"support tooltip is {tip.Length} characters: \"{tip}\"");
        }

        [Test]
        public void TheRosterNeverOutgrowsTheIcons()
        {
            Assert.That(BattleHud.UnitIcons, Is.GreaterThanOrEqualTo(RosterEntry.SlotCount),
                $"there are {BattleHud.UnitIcons} icons for {RosterEntry.SlotCount} slots, so the last slots share " +
                "the icon of the one before and two different machines look identical on the bar");
        }

        // ---- the numbers quoted in the tooltips are the sim's numbers ----------------------------------------

        [Test]
        public void KettleTooltipQuotesItsRealMinimumRange()
        {
            StringAssert.Contains($"{TankSpec.Kettle.Gun0.RangeMin:0} m", BattleHud.VehicleTip(VehicleArchetype.Kettle));
        }

        [Test]
        public void PaviseTooltipQuotesItsRealMaximumRange()
        {
            StringAssert.Contains($"{TankSpec.Pavise.Gun0.RangeMax:0} m", BattleHud.VehicleTip(VehicleArchetype.Pavise));
        }

        /// <summary>A superlative is the most fragile claim a tooltip can make: it goes stale when some OTHER
        /// machine changes, which is exactly where nobody looks.</summary>
        [Test]
        public void PaviseReallyHasTheLongestReachOnTheField()
        {
            float longest = 0f; byte holder = 0;
            foreach (byte a in AllArchetypes)
            {
                var spec = TankSpec.For(a);
                for (int g = 0; g < spec.GunCount; g++)
                    if (spec.Gun(g).RangeMax > longest) { longest = spec.Gun(g).RangeMax; holder = a; }
            }
            Assert.That(holder, Is.EqualTo(VehicleArchetype.Pavise),
                $"the Pavise tooltip claims the longest reach on the field, but archetype {holder} reaches {longest} m");
        }

        /// <summary>The Pincer's guns are sponson mounts, not turrets. The tooltip says it cannot reach behind
        /// itself, which is a real tactical fact — get behind a crab — and it must stay true of the arcs.</summary>
        [Test]
        public void PincerCannotReachBehindItself()
        {
            var spec = TankSpec.Pincer;
            Assert.That(Covered(spec, 0f), Is.True, "the Pincer should cover dead ahead");
            Assert.That(Covered(spec, 180f), Is.False,
                "the Pincer's tooltip says it cannot reach behind it, but its gun arcs now cover dead astern");
        }

        [Test]
        public void TheMachinesWithNoGunReallyHaveNone()
        {
            Assert.That(TankSpec.Censer.GunCount, Is.Zero, "the Censer's tooltip says it has no gun");
            Assert.That(TankSpec.Redoubt.GunCount, Is.Zero, "the Redoubt's tooltip says it has no gun");
        }

        [Test]
        public void BannerTooltipQuotesItsRealStandardRadius()
        {
            StringAssert.Contains($"{TankSpec.Banner.StandardRadius:0} m", BattleHud.VehicleTip(VehicleArchetype.Banner));
        }

        [Test]
        public void BarrageTooltipQuotesItsRealShellsRadiusAndDelay()
        {
            Assert.That(OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.HeBarrage, out var s), Is.True);
            string tip = BattleHud.BarrageTip;
            StringAssert.Contains($"{s.Shells} shells", tip);
            StringAssert.Contains($"{s.Radius:0} m", tip);
            StringAssert.Contains($"{s.WarmupTicks * SimConfig.Default.TickSeconds:0} s", tip);
        }

        // ---- helpers -----------------------------------------------------------------------------------------

        static readonly byte[] AllArchetypes =
        {
            VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Pincer, VehicleArchetype.Kettle,
            VehicleArchetype.Censer, VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Redoubt,
        };

        /// <summary>Whether any of a machine's guns can bear on a bearing, in degrees from dead ahead.</summary>
        static bool Covered(in TankSpec spec, float yawDegrees)
        {
            for (int g = 0; g < spec.GunCount; g++)
            {
                var gun = spec.Gun(g);
                float rest = gun.RestYaw * Mathf.Rad2Deg, half = gun.ArcHalf * Mathf.Rad2Deg;
                if (Mathf.Abs(Mathf.DeltaAngle(rest, yawDegrees)) <= half + 0.001f) return true;
            }
            return false;
        }
    }
}
