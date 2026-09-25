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
using UnityEngine.InputSystem;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.UI;
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
        static byte[] DeployableArchetypes() => Deployable(vehicles: true);

        /// <summary>Everything either faction fields, machine or man. Both rosters, because a slot means nothing across
        /// factions: Iron's officer sits where Brass's sniper does.</summary>
        static byte[] AllDeployableArchetypes() => Deployable(vehicles: false);

        static byte[] Deployable(bool vehicles)
        {
            var found = new System.Collections.Generic.List<byte>();
            var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            for (int f = 0; f < Factions.Count; f++)
            {
                FactionRoster.Fill(roster, 0, Factions.Of((byte)f));
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                {
                    if (vehicles && !roster[s].IsVehicle) continue;
                    if (roster[s].Hp <= 0f) continue;   // an empty slot, if a faction ever fields fewer than ten
                    if (!found.Contains(roster[s].Archetype)) found.Add(roster[s].Archetype);
                }
            }
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
            foreach (string tip in new[] { BattleHud.BarrageTip, BattleHud.GasTip, BattleHud.DropTip })
                Assert.That(tip.Length, Is.LessThanOrEqualTo(100), $"support tooltip is {tip.Length} characters: \"{tip}\"");
        }

        [Test]
        public void EveryUnitEitherFactionFieldsHasWordsAndAPictureOfItsOwn()
        {
            var seenName = new System.Collections.Generic.Dictionary<string, byte>();
            foreach (byte a in AllDeployableArchetypes())
            {
                Assert.That(HudText.Name(a), Is.Not.EqualTo("Vehicle"), $"archetype {a} has no name of its own");
                Assert.That(HudText.Tip(a), Is.Not.EqualTo(HudText.Tip(200)), $"archetype {a} has no tooltip of its own");
                Assert.That(HudText.Tip(a).Length, Is.LessThanOrEqualTo(100), $"archetype {a}'s tooltip is clipped");
                Assert.That(HudText.PortraitName(a), Is.Not.Empty, $"archetype {a} has no portrait stem");
                Assert.That(a, Is.LessThan(BattleHud.UnitIcons),
                    $"archetype {a} is fielded but past the end of the icon table, so it draws another unit's picture");
                // two units sharing one name would put two identical cards on the bar, which is the bug the icon
                // table had for a year: the player is told two different men are the same man
                Assert.That(seenName.ContainsKey(HudText.Name(a)), Is.False,
                    $"archetypes {a} and {(seenName.TryGetValue(HudText.Name(a), out var b) ? b : (byte)0)} are both called \"{HudText.Name(a)}\"");
                seenName[HudText.Name(a)] = a;
            }
        }

        [Test]
        public void ThePortraitsCoverEveryArchetypeTheIconsDo()
        {
            Assert.That(HudText.PortraitCount, Is.EqualTo(BattleHud.UnitIcons),
                "the Toolkit HUD's portraits and the old bar's icons are indexed by the same archetype id");
        }

        /// <summary>Ten roster slots take the digit row, so the support cards are on F5-F7. A slot past the roster has
        /// no key at all rather than borrowing slot 1's, which is what an unguarded modulo did.</summary>
        [Test]
        public void TheKeysCoverTenSlotsAndThreeSupportCards()
        {
            for (int s = 0; s < RosterEntry.SlotCount; s++)
                Assert.That(HudText.Hotkey(s), Is.EqualTo(((s + 1) % 10).ToString()), $"slot {s}");
            Assert.That(HudText.Hotkey(RosterEntry.SlotCount), Is.Empty, "a slot the roster does not have");
            Assert.That(HudText.Hotkey(-1), Is.Empty);
            Assert.That(HudText.SupportHotkey(0), Is.EqualTo("F5"));
            Assert.That(HudText.SupportHotkey(1), Is.EqualTo("F6"));
            Assert.That(HudText.SupportHotkey(2), Is.EqualTo("F7"));
            Assert.That(HudText.SupportHotkey(3), Is.Empty);
            Assert.That(KeyMap.Defaults().Primary[(int)GameAction.Deploy10], Is.EqualTo(Key.Digit0), "the tenth slot is 0");
            Assert.That(KeyMap.Defaults().Primary[(int)GameAction.ArmBarrage], Is.EqualTo(Key.F5));
            Assert.That(KeyMap.Defaults().Primary[(int)GameAction.ArmGas], Is.EqualTo(Key.F6));
            Assert.That(KeyMap.Defaults().Primary[(int)GameAction.ArmDrop], Is.EqualTo(Key.F7));
        }

        /// <summary>Every roster slot and every support card has a key of its own: two cards on one key means one of
        /// them cannot be reached from the keyboard, and the bar would not say so.</summary>
        [Test]
        public void NoTwoCardsShareAKey()
        {
            var b = KeyMap.Defaults();
            var keys = new System.Collections.Generic.List<Key>();
            for (int s = 0; s < RosterEntry.SlotCount; s++) keys.Add(b.Primary[(int)GameAction.Deploy1 + s]);
            keys.Add(b.Primary[(int)GameAction.ArmBarrage]);
            keys.Add(b.Primary[(int)GameAction.ArmGas]);
            keys.Add(b.Primary[(int)GameAction.ArmDrop]);
            foreach (var k in keys)
            {
                Assert.That(k, Is.Not.EqualTo(Key.None), "a card with no key");
                Assert.That(keys.FindAll(x => x == k).Count, Is.EqualTo(1), $"{k} is bound to two cards");
            }
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
