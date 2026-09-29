// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — MachineSockets. The walkers' exporter writes one
// Socket_Exhaust and one Socket_Fire where TankRenderer asks for Socket_Exhaust0/1 and Socket_Fire0/1: with the plain
// lookup every walker had no exhaust and no fire flame, and its burning smoke started under the ground.
using NUnit.Framework;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class MachineSocketTests
    {
        static readonly (string Name, byte Archetype)[] Walkers =
        {
            ("Pincer", VehicleArchetype.Pincer), ("Kettle", VehicleArchetype.Kettle), ("Censer", VehicleArchetype.Censer),
            ("Pavise", VehicleArchetype.Pavise), ("Banner", VehicleArchetype.Banner), ("Redoubt", VehicleArchetype.Redoubt),
        };

        [Test]
        public void EveryWalkerFindsItsExhaustAndItsFire_WhichThePlainLookupMissed()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = TankModel.Load(name, archetype, "Body", VehicleSize.Walker);
                Assert.IsNotNull(m, name);
                Assert.IsFalse(m.Sockets.ContainsKey("Socket_Exhaust0"), $"{name}: the plain lookup misses it (why walkers never smoked)");
                Assert.IsTrue(MachineSockets.TryResolve(m, "Socket_Exhaust0", out var ex), $"{name}: exhaust");
                Assert.IsTrue(MachineSockets.TryResolve(m, "Socket_Fire0", out var fire), $"{name}: fire");
                Assert.AreEqual(m.Sockets["Socket_Exhaust"], ex, $"{name}: the one exhaust it has");
                Assert.AreEqual(m.Sockets["Socket_Fire"], fire, $"{name}: the one fire socket it has");
                Assert.IsFalse(MachineSockets.TryResolve(m, "Socket_Exhaust1", out _), $"{name}: a second exhaust is never invented");
                Assert.IsFalse(MachineSockets.TryResolve(m, "Socket_Fire1", out _), $"{name}: nor a second fire");
            }
        }

        [Test]
        public void TheTanksKeepTheirOwnPairs()
        {
            var maw = TankModel.Load("Maw", VehicleArchetype.Maw, "Hull", VehicleSize.Tank);
            Assert.IsNotNull(maw);
            foreach (var s in new[] { "Socket_Exhaust0", "Socket_Exhaust1", "Socket_Fire0" })
            {
                if (!maw.Sockets.TryGetValue(s, out var own)) continue;
                Assert.IsTrue(MachineSockets.TryResolve(maw, s, out var found), s);
                Assert.AreEqual(own, found, $"the Maw's own {s}, unchanged");
            }
            Assert.IsFalse(MachineSockets.TryResolve(maw, "Socket_Nowhere", out _));
            Assert.IsFalse(MachineSockets.TryResolve(null, "Socket_Exhaust0", out _));
        }
    }
}
