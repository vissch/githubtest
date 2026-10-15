// Phase: A5c (2026-09-28) — each machine drives in a way of its own (TankRenderer.DriveStyle). These hold the table to
// what it claims: no two machines ride alike, the heavy ones are the slow and deep ones, a cushion banks in where
// springs lean out, and a walker's step stays a share of the gait GaitTests prove rather than a new one.
using System.Collections.Generic;
using NUnit.Framework;
using TW.Presentation.Tactical;
using TW.Sim;

namespace TW.Tests
{
    /// <summary>[TS10] id-to-name pairs reflected off VehicleArchetype's own public const byte fields, so a test that
    /// names a machine cannot drift from the ids the sim actually declares.</summary>
    public static class VehicleIds
    {
        public static readonly Dictionary<byte, string> ById = Build();

        static Dictionary<byte, string> Build()
        {
            var map = new Dictionary<byte, string>();
            foreach (var f in typeof(VehicleArchetype).GetFields(
                         System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Public))
            {
                if (f.FieldType != typeof(byte)) continue;
                byte id = (byte)f.GetValue(null);
                if (!map.ContainsKey(id)) map[id] = f.Name;
            }
            return map;
        }
    }

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
        public void EveryExplicitMachineIsARealArchetype()
        {
            foreach (byte a in Machines)
                Assert.That(VehicleIds.ById.ContainsKey(a), $"[TS10] {a} is not a const on VehicleArchetype");
        }

        [Test]
        public void NoMachineWithARideOfItsOwnIsLeftOffTheList()
        {
            var listed = new HashSet<byte>(Machines);
            foreach (var kv in VehicleIds.ById)
            {
                if (listed.Contains(kv.Key)) continue;
                Assert.AreEqual(TankRenderer.PlainStyle, S(kv.Key),
                    $"[TS10] {kv.Value} ({kv.Key}) has a ride of its own in StyleFor but is missing from Machines");
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
