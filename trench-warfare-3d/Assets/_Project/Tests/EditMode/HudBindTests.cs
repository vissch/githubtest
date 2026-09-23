// Phase: B6 (implemented) — every number the Toolkit HUD shows equals the sim's number, and the words it uses are
// the words the IMGUI HUD was held to (HudTextTests, kept as they are for BattleHud until it goes).
// The cards are bound with plain values and read back: cost, cooldown seconds, the locked / poor / cooling / armed
// states, the in-transit badge; the gauges with silver, income, men, time and speed. Then the text tables: every
// deployable archetype from EITHER side's default roster has its own name, portrait and tooltip, no tooltip
// outgrows the plate, and the quoted numbers are the sim constants (Kettle's minimum range, the Pavise's maximum
// and its claim to the longest reach, the Banner's radius, the barrage's shells, radius and delay).
using NUnit.Framework;
using Unity.Collections;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.UI;

namespace TW.Tests
{
    public class HudBindTests
    {
        static RosterEntry[] Roster(int player)
        {
            var na = new NativeArray<RosterEntry>(RosterEntry.SlotCount * 2, Allocator.Temp);
            RosterEntry.FillDefault(na, 0); RosterEntry.FillDefault(na, RosterEntry.SlotCount);
            var r = new RosterEntry[RosterEntry.SlotCount];
            for (int s = 0; s < r.Length; s++) r[s] = na[player * RosterEntry.SlotCount + s];
            na.Dispose(); return r;
        }

        static HudRefs BuildBare(RosterEntry[] roster)
        {
            // no UXML: HudView.Build queries what it can and falls back to code-built cards, so this runs without assets
            var root = new VisualElement { name = HudView.RootName };
            foreach (var n in HudView.RequiredNames) if (n != HudView.RootName) root.Add(n.EndsWith("-value") || n.StartsWith("tooltip") || n == "hint" || n == "banner-text" || n.StartsWith("caption") ? new Label { name = n } : n.StartsWith("speed-") && n != "speed-value" && n != "speed-bar" ? (VisualElement)new Button { name = n } : new VisualElement { name = n });
            return HudView.Build(root, null, roster, new[] { 150, 120 });
        }

        [Test]
        public void ACardShowsItsCostThenItsCooldownThenItsCostAgain()
        {
            var refs = BuildBare(Roster(0));
            var c = refs.Cards[0];
            float tick = SimConfig.Default.TickSeconds;
            HudView.BindCard(c, silver: 1000, cooldownTicks: 0, cooldownTotalTicks: 1, unlocked: true, over: false, tickSeconds: tick, inTransit: 0);
            Assert.That(c.Cost.text, Is.EqualTo(c.BaseCost.ToString()));
            Assert.That(c.Root.enabledSelf, Is.True);
            HudView.BindCard(c, 1000, cooldownTicks: 200, cooldownTotalTicks: 200, unlocked: true, over: false, tickSeconds: tick, inTransit: 0);
            Assert.That(c.Cost.text, Is.EqualTo("10s"), "200 ticks at 20 Hz is ten seconds");
            Assert.That(c.Root.ClassListContains("is-cooling"));
            Assert.That(c.Root.enabledSelf, Is.False);
            Assert.That(c.Cooldown.style.height.value.value, Is.GreaterThan(80f), "the cooldown mask fills the card at the start");
            HudView.BindCard(c, 1000, 0, 1, true, false, tick, 0);
            Assert.That(c.Cost.text, Is.EqualTo(c.BaseCost.ToString()));
            Assert.That(c.Root.ClassListContains("is-cooling"), Is.False);
        }

        [Test]
        public void LockedPoorOverAndInTransitStates()
        {
            var refs = BuildBare(Roster(0));
            var c = refs.Cards[4];   // the Maw
            float tick = SimConfig.Default.TickSeconds;
            HudView.BindCard(c, silver: 0, cooldownTicks: 0, cooldownTotalTicks: 1, unlocked: false, over: false, tickSeconds: tick, inTransit: 0);
            Assert.That(c.Root.ClassListContains("is-locked"));
            Assert.That(c.Root.enabledSelf, Is.False);
            HudView.BindCard(c, silver: c.BaseCost - 1, 0, 1, unlocked: true, over: false, tick, 0);
            Assert.That(c.Root.ClassListContains("is-locked"), Is.False);
            Assert.That(c.Root.ClassListContains("is-poor"), "one silver short is unaffordable");
            Assert.That(c.Root.enabledSelf, Is.False);
            HudView.BindCard(c, silver: c.BaseCost, 0, 1, true, false, tick, 3);
            Assert.That(c.Root.ClassListContains("is-poor"), Is.False);
            Assert.That(c.Root.enabledSelf, Is.True);
            Assert.That(c.Badge.text, Is.EqualTo("3"), "three men riding the boats in");
            Assert.That(c.Badge.style.display.value, Is.EqualTo(DisplayStyle.Flex));
            HudView.BindCard(c, 9999, 0, 1, true, over: true, tick, 0);
            Assert.That(c.Root.ClassListContains("is-over"));
            Assert.That(c.Root.enabledSelf, Is.False, "the match is over");
            Assert.That(c.Badge.style.display.value, Is.EqualTo(DisplayStyle.None));
        }

        [Test]
        public void SupportCardArmsAndSaysAim()
        {
            var refs = BuildBare(Roster(0));
            var c = refs.SupportCards[0];
            float tick = SimConfig.Default.TickSeconds;
            Assert.That(c.Ability, Is.EqualTo(OffMapAbilityId.HeBarrage));
            HudView.BindSupportCard(c, silver: 1000, cooldownTicks: 0, cooldownTotalTicks: 1, armed: false, over: false, tickSeconds: tick);
            Assert.That(c.Name.text, Is.EqualTo(c.Title));
            HudView.BindSupportCard(c, 0, 0, 1, armed: true, over: false, tick);
            Assert.That(c.Name.text, Is.EqualTo(HudText.Aim));
            Assert.That(c.Root.ClassListContains("is-armed"));
            Assert.That(c.Root.enabledSelf, Is.True, "an armed card stays clickable so it can be disarmed with no silver");
            HudView.BindSupportCard(c, 0, 0, 1, armed: false, over: false, tick);
            Assert.That(c.Root.ClassListContains("is-poor"));
            Assert.That(c.Root.enabledSelf, Is.False);
        }

        [Test]
        public void GaugesShowSilverIncomeMenTimeAndSpeed()
        {
            var refs = BuildBare(Roster(0));
            HudView.BindGauges(refs, silver: 345, income: 2f, men: 24, enemy: 31, seconds: 754, speed: 4f, paused: false);
            Assert.That(refs.SilverValue.text, Is.EqualTo("345"));
            Assert.That(refs.IncomeValue.text, Is.EqualTo("+2/s"));
            Assert.That(refs.MenValue.text, Is.EqualTo("24"));
            Assert.That(refs.EnemyValue.text, Is.EqualTo("31"));
            Assert.That(refs.TimeValue.text, Is.EqualTo("12:34"));
            Assert.That(refs.SpeedValue.text, Is.EqualTo("4x"));
            Assert.That(refs.SpeedButtons[3].ClassListContains("tw-btn--on"));
            HudView.BindGauges(refs, 345, 2f, 24, 31, 754, 4f, paused: true);
            Assert.That(refs.SpeedValue.text, Is.EqualTo(HudText.Paused));
            Assert.That(refs.SpeedButtons[0].ClassListContains("tw-btn--on"), "the pause button lights while paused");
            Assert.That(refs.PausePlate.ClassListContains("is-visible"));
        }

        [Test]
        public void BindingTheSameValuesTwiceChangesNothing()
        {
            var refs = BuildBare(Roster(0));
            var c = refs.Cards[1];
            float tick = SimConfig.Default.TickSeconds;
            HudView.BindCard(c, 500, 40, 200, true, false, tick, 1);
            Assert.That(HudView.BindCard(c, 500, 40, 200, true, false, tick, 1), Is.False, "a steady frame must touch nothing");
            Assert.That(HudView.BindCard(c, 500, 39, 200, true, false, tick, 1), Is.False, "39 vs 40 ticks is the same second and the same percent");
            Assert.That(HudView.BindCard(c, 500, 20, 200, true, false, tick, 1), Is.True);
        }

        // ---- the words, held to the sim (the assertions HudTextTests made of BattleHud) ----------------------------

        static byte[] DeployableArchetypes()
        {
            var found = new System.Collections.Generic.List<byte>();
            foreach (int p in new[] { 0, 1 })
                foreach (var e in Roster(p)) if (e.IsVehicle && !found.Contains(e.Archetype)) found.Add(e.Archetype);
            return found.ToArray();
        }

        [Test]
        public void EveryDeployableMachineHasItsOwnNamePortraitAndTooltipOnBothSides()
        {
            string generic = HudText.VehicleTip(200);
            foreach (byte a in DeployableArchetypes())
            {
                Assert.That(HudText.VehicleName(a), Is.Not.EqualTo("Vehicle"), $"archetype {a} falls through to the generic name");
                Assert.That(HudText.VehicleTip(a), Is.Not.EqualTo(generic), $"archetype {a} falls through to the generic tooltip");
                Assert.That(System.Array.IndexOf(SkinSpec.PortraitNames, HudText.PortraitName(a)), Is.GreaterThanOrEqualTo(0), $"archetype {a} has no portrait entry");
                Assert.That(a, Is.LessThan(HudText.PortraitCount), $"archetype {a} is past HudText.PortraitCount");
            }
        }

        [Test]
        public void NoTooltipOutgrowsThePlate()
        {
            foreach (byte a in DeployableArchetypes())
                Assert.That(HudText.VehicleTip(a).Length, Is.LessThanOrEqualTo(100), $"{HudText.VehicleName(a)}'s tooltip is too long");
            for (byte a = 0; a < 4; a++) Assert.That(HudText.Tip(a).Length, Is.LessThanOrEqualTo(100));
            Assert.That(HudText.BarrageTip.Length, Is.LessThanOrEqualTo(100));
            Assert.That(HudText.GasTip.Length, Is.LessThanOrEqualTo(100));
        }

        [Test]
        public void TooltipNumbersAreTheSimsNumbers()
        {
            StringAssert.Contains($"{TankSpec.Kettle.Gun0.RangeMin:0} m", HudText.VehicleTip(VehicleArchetype.Kettle));
            StringAssert.Contains($"{TankSpec.Pavise.Gun0.RangeMax:0} m", HudText.VehicleTip(VehicleArchetype.Pavise));
            StringAssert.Contains($"{TankSpec.Banner.StandardRadius:0} m", HudText.VehicleTip(VehicleArchetype.Banner));
            Assert.That(TankSpec.Censer.GunCount, Is.Zero, "the Censer's tooltip says it has no gun");
            Assert.That(TankSpec.Redoubt.GunCount, Is.Zero, "the Redoubt's tooltip says it has no gun");
            Assert.That(OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.HeBarrage, out var s), Is.True);
            StringAssert.Contains($"{s.Shells} shells", HudText.BarrageTip);
            StringAssert.Contains($"{s.Radius:0} m", HudText.BarrageTip);
            StringAssert.Contains($"{s.WarmupTicks * SimConfig.Default.TickSeconds:0} s", HudText.BarrageTip);
        }

        [Test]
        public void PaviseReallyHasTheLongestReach()
        {
            float longest = 0f; byte holder = 0;
            foreach (byte a in new[] { VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Pincer, VehicleArchetype.Kettle, VehicleArchetype.Censer, VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Redoubt })
            {
                var spec = TankSpec.For(a);
                for (int g = 0; g < spec.GunCount; g++) if (spec.Gun(g).RangeMax > longest) { longest = spec.Gun(g).RangeMax; holder = a; }
            }
            Assert.That(holder, Is.EqualTo(VehicleArchetype.Pavise), $"the Pavise tooltip claims the longest reach, but archetype {holder} reaches {longest} m");
        }

        [Test]
        public void IntTextCachesAndFormats()
        {
            Assert.That(IntText.Get(0), Is.EqualTo("0"));
            Assert.That(ReferenceEquals(IntText.Get(345), IntText.Get(345)), "the same value yields the same string instance: no per-frame garbage");
            Assert.That(IntText.Seconds(7), Is.EqualTo("7s"));
            Assert.That(IntText.Clock(0), Is.EqualTo("00:00"));
            Assert.That(IntText.Clock(3599), Is.EqualTo("59:59"));
            Assert.That(IntText.Clock(7261), Is.EqualTo("121:01"));
        }
    }
}
