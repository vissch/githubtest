// Phase: water-entry-splash-r4 (2026-10-19, implemented) — CombatFx.DeathLanding / Wet: every death in water makes
// exactly one splash, on every path a death can take (plain, body fallback, flung, a gag with arcs, a gag with none).
// Pure arithmetic on hand-authored plans: no scene, no GPU.
using System.IO;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Presentation.Units;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class WaterDeathSplashTests
    {
        [Test]
        public void EveryWaterDeathSplashesOnce()
        {
            var fell = new Vector3(10f, 0f, 10f);

            // plain death and the body-fallback path: never planned, so the splash sits where he fell, with no delay.
            {
                var plain = CombatFx.DeathLanding(default, false, fell, out float delay);
                Assert.AreEqual(fell, plain, "plain death: splashes where he fell");
                Assert.AreEqual(0f, delay, "plain death: no delay");
                Assert.IsTrue(CombatFx.Wet(0f, 0.15f, false));
            }

            // a gag with a flight (the flung case folds into this: only the FIRST arc's landing counts, later arcs are dry
            // ground by construction). Delay 0.2, the throw starts at 0 and lasts 0.8s, so the water is met at 0.2 + 0 + 0.8 = 1.0s.
            {
                var path = new FallenFlight.Plan { Delay = 0.2f, Arcs = 1, A0 = new FallenFlight.Arc { To = new Vector3(14f, 0f, 10f), Start = 0f, Dur = 0.8f } };
                var landing = CombatFx.DeathLanding(path, true, fell, out float delay);
                Assert.AreEqual(new Vector3(14f, 0f, 10f), landing, "gag with arcs: the first arc's landing");
                Assert.AreEqual(1.0f, delay, 1e-5f, "0.2 delay + 0 start + 0.8 dur");
                Assert.IsTrue(CombatFx.Wet(0f, 0.15f, false));
            }

            // flung: the same shape but two arcs and no hold delay. The FIRST landing is still the one that counts.
            {
                var path = new FallenFlight.Plan
                {
                    Delay = 0f,
                    Arcs = 2,
                    A0 = new FallenFlight.Arc { To = new Vector3(20f, 0f, 10f), Start = 0f, Dur = 0.6f },
                    A1 = new FallenFlight.Arc { To = new Vector3(22f, 0f, 10f), Start = 0.6f, Dur = 0.3f },
                };
                var landing = CombatFx.DeathLanding(path, true, fell, out float delay);
                Assert.AreEqual(new Vector3(20f, 0f, 10f), landing, "flung: the first landing, not the bounce");
                Assert.AreEqual(0.6f, delay, 1e-5f, "0 delay + 0 start + 0.6 dur, the first arc's end");
                Assert.IsTrue(CombatFx.Wet(0f, 0.15f, false));
            }

            // a gag with no flight at all (a skid with no throw first): the skid's start is where the water is met.
            {
                var path = new FallenFlight.Plan { Delay = 0.1f, Arcs = 0, SkidDur = 0.5f, SkidStart = 0.25f, SkidFrom = new Vector3(16f, 0f, 10f) };
                var landing = CombatFx.DeathLanding(path, true, fell, out float delay);
                Assert.AreEqual(new Vector3(16f, 0f, 10f), landing, "a gag's skid: where the skid starts");
                Assert.AreEqual(0.35f, delay, 1e-5f, "0.1 delay + 0.25 skid start");
                Assert.IsTrue(CombatFx.Wet(0f, 0.15f, false));
            }

            // a gag with neither a flight nor a skid: he never leaves where he fell.
            {
                var path = new FallenFlight.Plan { Delay = 0.35f, Arcs = 0, SkidDur = 0f, Origin = new Vector3(12f, 0f, 12f) };
                var landing = CombatFx.DeathLanding(path, true, fell, out float delay);
                Assert.AreEqual(new Vector3(12f, 0f, 12f), landing, "a gag with no flight: his origin");
                Assert.AreEqual(0.35f, delay, 1e-5f, "just his delay");
                Assert.IsTrue(CombatFx.Wet(0f, 0.15f, false));
            }
        }

        [Test]
        public void ADryFieldSplashesNothing()
        {
            Assert.IsFalse(CombatFx.Wet(0f, MapData.NoWater, false), "no water table and no hook: nothing is wet");
        }

        [Test]
        public void OneSplashSiteOnly()
        {
            string camera = Path.Combine("Assets", "_Project", "Presentation", "Camera");
            string deaths = File.ReadAllText(Path.Combine(camera, "CombatFx.Deaths.cs"));
            string gags = File.ReadAllText(Path.Combine(camera, "CombatFx.Gags.cs"));
            string water = File.ReadAllText(Path.Combine(camera, "CombatFx.Water.cs"));

            Assert.AreEqual(1, Occurrences(deaths, "WaterDeath("), "CombatFx.Deaths.cs: exactly one call site");
            Assert.AreEqual(1, Occurrences(water, "FlipbookFx.Book.Splash"), "CombatFx.Water.cs: exactly one splash draw");
            Assert.AreEqual(0, Occurrences(deaths, "FlipbookFx.Book.Splash"), "CombatFx.Deaths.cs: no splash of its own");
            Assert.AreEqual(0, Occurrences(gags, "FlipbookFx.Book.Splash"), "CombatFx.Gags.cs: no splash of its own");
        }

        static int Occurrences(string text, string needle)
        {
            int count = 0, i = 0;
            while ((i = text.IndexOf(needle, i, System.StringComparison.Ordinal)) >= 0) { count++; i += needle.Length; }
            return count;
        }
    }
}
