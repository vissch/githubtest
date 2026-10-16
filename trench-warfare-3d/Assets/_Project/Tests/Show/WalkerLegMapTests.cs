// Phase: review-fix (rv-37-p2-u16, 2026-10-13) — code review 2026-10-04, unit 16: Q2 (TankModel.cs:185,
// VehicleKinematics.cs:147, VehicleModules.cs:297).
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class WalkerLegMapTests
    {
        /// <summary>[Q2] VehicleModules numbers a walker's legs over the sim's own Legs config (Redoubt: 6, so 3 a
        /// side), but the renderer's model geometry can have fewer: Redoubt is drawn on 4 legs, 2 a side
        /// (TankModel.NumberLegs). Reading the sim's bitmask straight as a model leg index threw the wrong leg (a
        /// sim bit past the model's own leg count did nothing at all), so losing one side showed the wrong leg
        /// flying off, or none. ModelLegMask carries each side over by the FRACTION of that side's sim legs lost.</summary>
        [Test]
        public void ARedoubtsSimLegsLandOnItsOwnLegs()
        {
            var model = TankModel.Load("Redoubt", VehicleArchetype.Redoubt, "Body", VehicleSize.Walker);
            Assert.NotNull(model, "Redoubt did not load");
            Assert.AreEqual(4, model.LegCount, "[Q2] premise: Redoubt's model is drawn on 4 legs");
            Assert.AreEqual(2, model.LegsPerSide, "[Q2] premise: Redoubt's model numbers 2 legs a side");

            // losing the whole left side on the sim's 6-leg, 3-a-side numbering (bits 0,1,2) must throw both LEFT
            // model legs and no right one; losing the whole right side (bits 3,4,5) must throw both right legs.
            byte leftLost = TankRenderer.ModelLegMask(0b000111, 6, 2);
            byte rightLost = TankRenderer.ModelLegMask(0b111000, 6, 2);
            Assert.AreEqual((byte)0b0011, leftLost, "[Q2] losing the sim's left side did not throw both LEFT model legs");
            Assert.AreEqual((byte)0b1100, rightLost, "[Q2] losing the sim's right side did not throw both RIGHT model legs");

            AssertSideMatches(model, leftLost, "left");
            AssertSideMatches(model, rightLost, "right");

            // a walker whose model has the same leg count as the sim (every other walker: 4 and 4) maps to itself.
            Assert.AreEqual((byte)0b0011, TankRenderer.ModelLegMask(0b0011, 4, 2), "[Q2] identity broke for a 1:1 walker");
        }

        /// <summary>[Q2.1] A PART of a side lost, which the whole-side test above never saw. The rule: a leg lost
        /// in the sim must never show as no leg lost, so a side's lost fraction rounds UP (TankRenderer.cs,
        /// ModelLegMask). Banker's rounding (Mathf.RoundToInt) drew 1 sim leg lost of 4 as a whole walker.</summary>
        [Test]
        public void ARedoubtShowsALegDownForEveryPartOfASideItLoses()
        {
            var model = TankModel.Load("Redoubt", VehicleArchetype.Redoubt, "Body", VehicleSize.Walker);
            Assert.NotNull(model, "Redoubt did not load");

            // the Redoubt's own numbers: 6 sim legs (3 a side) on 2 model legs a side
            byte oneOfThree = TankRenderer.ModelLegMask(0b000001, 6, 2);
            Assert.AreEqual((byte)0b0001, oneOfThree, "[Q2.1] 1 of 3 left sim legs lost must show one left model leg down");
            Assert.AreEqual((byte)0b0011, TankRenderer.ModelLegMask(0b000011, 6, 2), "[Q2.1] 2 of 3 left sim legs lost must show both left model legs down");
            byte oneOfThreeRight = TankRenderer.ModelLegMask(0b001000, 6, 2);
            Assert.AreEqual((byte)0b0100, oneOfThreeRight, "[Q2.1] 1 of 3 RIGHT sim legs lost must show one right model leg down");
            AssertSideMatches(model, oneOfThree, "left");
            AssertSideMatches(model, oneOfThreeRight, "right");

            // 4 sim legs a side on 2 model legs: the case banker's rounding dropped altogether
            Assert.AreEqual((byte)0b0001, TankRenderer.ModelLegMask(0b00000001, 8, 2), "[Q2.1] 1 of 4 sim legs lost showed no leg lost at all");
            Assert.AreEqual((byte)0b0011, TankRenderer.ModelLegMask(0b00000111, 8, 2), "[Q2.1] 3 of 4 sim legs lost must show both model legs down");
        }

        /// <summary>Every model leg number in a mapped mask must belong to a part whose Outward.x sign is the side
        /// the mask claims: bits 0,1 are LEFT (Outward.x < 0), bits 2,3 (modelPerSide 2) are RIGHT (Outward.x > 0).</summary>
        static void AssertSideMatches(TankModel model, byte mask, string label)
        {
            var parts = model.Lods[0].Parts;
            for (int k = 0; k < 8; k++)
            {
                if ((mask & (1 << k)) == 0) continue;
                bool found = false;
                foreach (var p in parts)
                {
                    if (p.Leg != k || (p.Role != TankPartRole.Leg && p.Role != TankPartRole.Thigh)) continue;
                    found = true;
                    bool rightSide = k >= model.LegsPerSide;
                    if (rightSide) Assert.Greater(p.Outward.x, 0f, $"[Q2] model leg {k} ({label}) is not on the right");
                    else Assert.Less(p.Outward.x, 0f, $"[Q2] model leg {k} ({label}) is not on the left");
                }
                Assert.IsTrue(found, $"[Q2] model leg {k} ({label}) has no part");
            }
        }
    }
}
