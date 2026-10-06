// Phase: tooling (the gym, 2026-09-28) — the gym's catalogue (Perf/GymCatalogue) keeps up with the game by itself: every
// clip on every figure, every off-map ability with the answer the sim will give, every death kind with its staging,
// every sim event with a row saying how the gym shows it (no default: a new event fails here until someone decides),
// and six zoom bands, closest first.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using TW.Perf;
using TW.Presentation;
using TW.Presentation.Units;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class GymCatalogueTests
    {
        [Test]
        public void EveryClipIsOnEveryFigure()
        {
            var clips = GymCatalogue.Clips();
            Assert.AreEqual(VATRenderer.FigureNames.Length * ((int)Clip.Count - 1), clips.Count);
            var seen = new HashSet<string>();
            foreach (var e in clips) Assert.IsTrue(seen.Add(e.Name), "one entry per clip and figure: " + e.Name);
        }

        [Test]
        public void EverySimEventSaysHowTheGymShowsIt()
        {
            var missing = new List<string>();
            foreach (SimEventType t in Enum.GetValues(typeof(SimEventType)))
            {
                if (t == SimEventType.None) continue;
                if (string.IsNullOrEmpty(GymCatalogue.EventHow(t).note)) missing.Add(t.ToString());
            }
            Assert.IsEmpty(missing, "new SimEventType values need a row in GymCatalogue.EventHow: " + string.Join(", ", missing));
            Assert.AreEqual(Enum.GetValues(typeof(SimEventType)).Length - 1, GymCatalogue.Events().Count);
        }

        [Test]
        public void EveryAbilityExpectsWhatTheSimWillSay()
        {
            var list = GymCatalogue.Abilities();
            Assert.AreEqual(Enum.GetValues(typeof(OffMapAbilityId)).Length - 1, list.Count);
            foreach (var e in list)
            {
                bool known = OffMapAbilitySystem.TryGetStats(e.Id, out _);
                Assert.AreEqual(!known, e.Expect == GymExpect.Rejected, e.Name + ": Rejected exactly when the sim has no stats for it");
            }
            Assert.IsTrue(list.Exists(e => e.Id == (int)OffMapAbilityId.ParaDrop && e.Expect == GymExpect.FactionSeat), "ParaDrop is called from the Brass seat");
        }

        [Test]
        public void EveryDeathKindHasItsStaging()
        {
            var list = GymCatalogue.Deaths();
            Assert.AreEqual(Enum.GetValues(typeof(DeathKind)).Length, list.Count);
            foreach (var e in list) Assert.IsFalse(string.IsNullOrEmpty(e.Note), e.Name + " says how the gym kills a man that way");
        }

        [Test]
        public void TheBandsGoFromCloseToFar()
        {
            var b = GymCatalogue.Bands;
            Assert.AreEqual(6, b.Length);
            for (int i = 1; i < b.Length; i++) Assert.Greater(b[i].Zoom, b[i - 1].Zoom, b[i].Name);
            Assert.AreEqual(30f, b[2].Zoom, "T1 is the standard view");
        }

        [Test]
        public void TheSniperClipsArePinnedOnTheSniperFigure()
        {
            Assert.AreEqual(1, VATRenderer.FigureOfArchetype(GymCatalogue.ArchetypeForFigure(1)));
            Assert.AreEqual(0, VATRenderer.FigureOfArchetype(GymCatalogue.ArchetypeForFigure(0)));
        }
    }
}
