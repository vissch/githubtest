// Phase: gas-smoke-night-g1 (the owner, 2026-10-20: the ability clouds were tuned by day; at night black or grey
// smoke vanishes). The guard on the two knobs that keep the gas and the smoke screen readable on a moonlit field,
// and on the day look not moving with them. The ground it measures against is the night field's own, measured off
// the pixels: docs/evidence/gas-smoke-night-g1/probe.json, ground_median_night, written by probe.py beside the
// reshot stills (three captures a field, from one locked camera). 0.12 is the phase's threshold.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class GasSmokeNightTests
    {
        const float GroundNight = 0.22895f;   // docs/evidence/gas-smoke-night-g1/probe.json ground_median_night (zoom 30, NightMud)
        const float Threshold = 0.12f;        // the phase's: contrast_median_night >= 0.12
        const float GroundDay = 0.78017f;     // the same file, smoke_winter_0.ground_median: the dimmest of the three measured day grounds, so the day guard takes the least favourable one

        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        [Test]
        public void GasCloudReadsAtNight()
        {
            // FlipbookFx.NightTint is built so the value it is given IS the luma at the drawing's middle ink, so the
            // knob's value is the cloud's modelled luma; the gas bank is lifted off GasBankNight instead.
            float screen = CombatFx.ReadSmokeScreenNight();
            float gas = CombatFx.BankLuma(CombatFx.GasBankNight) * CombatFx.ReadGasNight();
            Assert.GreaterOrEqual(CombatFx.BankContrast(screen, GroundNight), Threshold, "the smoke screen vanishes at night");
            Assert.GreaterOrEqual(CombatFx.BankContrast(gas, GroundNight), Threshold, "the gas bank vanishes at night");

            Assert.AreEqual("fx.smokeScreenNight", CombatFx.SmokeScreenNightKnob);
            Assert.AreEqual("fx.gasNight", CombatFx.GasNightKnob);
            Assert.AreEqual(CombatFx.BankLuma(CombatFx.SmokeBankNight), CombatFx.DefaultSmokeScreenNight, 1e-6f,
                "the screen's default is the light warm grey it was tuned to");
            Assert.AreEqual(1.08f, CombatFx.DefaultGasNight, 1e-6f);
        }

        [Test]
        public void SmokeScreenReadsByDay()
        {
            // Phase gas-smoke-night-g1-g1: by day the bank was painted with the biome's near-white dust and lit, so the
            // cloud landed at the snow's own luma (contrast 0.020-0.023). FlipbookFx.DayBanks paints it flat, so the
            // knob's value IS the cloud's modelled luma, as at night.
            Assert.GreaterOrEqual(CombatFx.BankContrast(CombatFx.ReadSmokeScreenDay(), GroundDay), Threshold,
                "the smoke screen vanishes into the snow by day");

            // and the guard bites: the old look's own measured day cloud (0.75079, the same probe.json
            // smoke_winter_0.cloud_median) is under the bar — a white field, which is what this unit fixes.
            Assert.Less(CombatFx.BankContrast(0.75079f, GroundDay), Threshold,
                "the old day cloud must be under the bar, or this guard proves nothing");

            Assert.AreEqual("fx.smokeScreenDay", CombatFx.SmokeScreenDayKnob);
            Assert.AreEqual(CombatFx.BankLuma(CombatFx.SmokeBankDay), CombatFx.DefaultSmokeScreenDay, 1e-6f,
                "the screen's day default is the grey it is painted at");
            Assert.AreEqual(0f, CombatFx.OldSmokeScreenDay, 1e-6f, "the old day value must paint nothing");
            Assert.IsFalse(CombatFx.OldSmokeScreenDay > 0f, "DayBanks' guard is exactly screen > 0");

            Knobs.Clear();
            Knobs.Set(CombatFx.SmokeScreenDayKnob, "0");
            Assert.AreEqual(0f, CombatFx.ReadSmokeScreenDay(), 1e-6f);
        }

        [Test]
        public void TheOldValuesSetNothing_AndTheGasDayColourIsUnchanged()
        {
            // NightBanks' two guards are exactly these: screen > 0 paints, gas > 1 lifts.
            Assert.IsFalse(CombatFx.OldSmokeScreenNight > 0f, "the old screen value must paint nothing");
            Assert.IsFalse(CombatFx.OldGasNight > 1f, "the old gas value must lift nothing");
            Assert.AreEqual(0f, CombatFx.OldSmokeScreenNight, 1e-6f);
            Assert.AreEqual(1f, CombatFx.OldGasNight, 1e-6f);

            // the knobs are read on a moonlit field only, so no day path can reach them
            Assert.IsFalse(FlipbookFx.MoonLit(false, false), "a day field is never moonlit");
            Assert.IsFalse(FlipbookFx.MoonLit(true, true), "a molten-lit field is not moonlit either");
            // and the day colours they sit next to have not moved
            Assert.AreEqual(new Color(0.50f, 0.64f, 0.12f), CombatFx.GasBankDay, "the gas bank's day colour moved");
            Assert.AreEqual(new Color(0.74f, 0.80f, 0.34f), CombatFx.GasBankNight, "the gas bank's night colour is the lift's base");

            Knobs.Clear();
            Knobs.Set(CombatFx.SmokeScreenNightKnob, "0");
            Knobs.Set(CombatFx.GasNightKnob, "1");
            Assert.AreEqual(0f, CombatFx.ReadSmokeScreenNight(), 1e-6f);
            Assert.AreEqual(1f, CombatFx.ReadGasNight(), 1e-6f);
        }
    }
}
