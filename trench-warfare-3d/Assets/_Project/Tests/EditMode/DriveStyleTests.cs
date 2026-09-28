// Phase: A5c (2026-09-28) — each machine drives in a way of its own (TankRenderer.DriveStyle). These hold the table to
// what it claims: no two machines ride alike, the heavy ones are the slow and deep ones, a cushion banks in where
// springs lean out, and a walker's step stays a share of the gait GaitTests prove rather than a new one.
using System.Collections.Generic;
using NUnit.Framework;
using TW.Presentation.Tactical;
using TW.Sim;

namespace TW.Tests
{
    public class DriveStyleTests
    {
        static readonly byte[] Machines =
        {
            VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Breaker, VehicleArchetype.Brute, VehicleArchetype.Salvo,
            VehicleArchetype.Mercy, VehicleArchetype.Skimmer, VehicleArchetype.Hopper, VehicleArchetype.Pincer, VehicleArchetype.Kettle,
            VehicleArchetype.Censer, VehicleArchetype.Pavise, VehicleArchetype.Banner, VehicleArchetype.Redoubt, VehicleArchetype.Croaker,
        };

        static TankRenderer.DriveStyle S(byte a) => TankRenderer.StyleFor(a);

        [Test]
        public void NoTwoMachinesRideAlikeAndNoneRidesThePlainStyle()
        {
            var seen = new List<TankRenderer.DriveStyle>();
            foreach (byte a in Machines)
            {
                var s = S(a);
                Assert.AreNotEqual(TankRenderer.PlainStyle, s, $"archetype {a} has a ride of its own");
                foreach (var o in seen) Assert.AreNotEqual(o, s, $"archetype {a} rides as another machine does");
                seen.Add(s);
            }
        }

        [Test]
        public void TheHeavyMachinesAreTheSlowDeepOnes()
        {
            Assert.Less(S(VehicleArchetype.Maw).Omega, S(VehicleArchetype.Tusk).Omega, "the landship settles slower than the light tank");
            Assert.Less(S(VehicleArchetype.Maw).RumbleRate, S(VehicleArchetype.Tusk).RumbleRate, "and its engine's note is lower");
            Assert.Greater(S(VehicleArchetype.Maw).ClatterEvery, S(VehicleArchetype.Tusk).ClatterEvery, "its running gear beats slower for the ground covered");
            Assert.Greater(S(VehicleArchetype.Redoubt).Stomp, S(VehicleArchetype.Pincer).Stomp, "the blockhouse's footfall is felt, the scuttler's barely");
            Assert.Greater(S(VehicleArchetype.Redoubt).Swing, S(VehicleArchetype.Censer).Swing, "and it swings its legs slower");
        }

        [Test]
        public void ACushionBanksIntoATurnWhereSpringsLeanOut()
        {
            foreach (byte a in new[] { VehicleArchetype.Skimmer, VehicleArchetype.Hopper })
            {
                Assert.Less(S(a).Lean, 0f, $"archetype {a} banks in");
                Assert.Less(S(a).Squat, 0f, $"archetype {a} noses down into the way it gathers");
            }
            foreach (byte a in new[] { VehicleArchetype.Maw, VehicleArchetype.Tusk, VehicleArchetype.Salvo, VehicleArchetype.Mercy })
            {
                Assert.Greater(S(a).Lean, 0f, $"archetype {a} leans out on its springs");
                Assert.Greater(S(a).Squat, 0f, $"archetype {a} squats as it pulls away");
            }
        }

        [Test]
        public void EveryRideIsStableAndEveryStepAShareOfTheProvenGait()
        {
            foreach (byte a in Machines)
            {
                var s = S(a);
                Assert.That(s.Zeta, Is.InRange(0.25f, 1f), $"archetype {a}: rocks, but settles");
                Assert.That(s.Omega, Is.InRange(2f, 14f), $"archetype {a}");
                Assert.That(s.HeaveOmega, Is.InRange(4f, 14f), $"archetype {a}");
                Assert.That(s.Swing, Is.InRange(0.7f, 1.5f), $"archetype {a}: a swing WalkerGait can still keep up with");
                Assert.That(s.Arc, Is.InRange(0.65f, 1.6f), $"archetype {a}");
                Assert.That(s.Puff, Is.InRange(0.5f, 1.5f), $"archetype {a}");
            }
        }
    }
}
