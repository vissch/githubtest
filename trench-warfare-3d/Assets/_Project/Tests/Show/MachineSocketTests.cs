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
            // [TS20] the three numbered sockets Maw_LOD0.fbx really declares: a `continue` here skipped a name the Maw
            // does not have (Socket_Exhaust1) and would have skipped all three in silence had the fbx lost them.
            int hits = 0;
            foreach (var s in new[] { "Socket_Exhaust0", "Socket_Fire0", "Socket_Fire1" })
            {
                Assert.IsTrue(maw.Sockets.TryGetValue(s, out var own), $"[TS20] the Maw declares {s} in its fbx");
                Assert.IsTrue(MachineSockets.TryResolve(maw, s, out var found), s);
                Assert.AreEqual(own, found, $"the Maw's own {s}, unchanged");
                hits++;
            }
            Assert.AreEqual(3, hits, "[TS20] the Maw's own numbered sockets were all checked, not skipped");
            Assert.IsFalse(MachineSockets.TryResolve(maw, "Socket_Nowhere", out _));
            Assert.IsFalse(MachineSockets.TryResolve(null, "Socket_Exhaust0", out _));
        }

        // [K4] Burning() asked for "Socket_Fire" + k, so a burning hull built two strings every frame and a wreck
        // burns up to 40 s: that breaks the 0 B/frame rule. TankRenderer.FireSocket names them once instead.
        [Test]
        public void TheFireSocketNamesAreNamedOnceAndNotBuilt()
        {
            for (int k = 0; k < 2; k++)
                Assert.AreEqual("Socket_Fire" + k, TankRenderer.FireSocket(k), $"[K4] fire socket {k}");
            Assert.AreSame(TankRenderer.FireSocket(0), TankRenderer.FireSocket(0), "[K4] the same string, not a new one");
            TankRenderer.FireSocket(0); TankRenderer.FireSocket(1);        // warm
            long before = System.GC.GetAllocatedBytesForCurrentThread();
            string last = null;
            for (int i = 0; i < 1000; i++) last = TankRenderer.FireSocket(i & 1);
            long grew = System.GC.GetAllocatedBytesForCurrentThread() - before;
            Assert.IsNotNull(last);
            Assert.Less(grew, 64L, "[K4] 1000 fire-socket lookups must allocate nothing (1000 built names are ~30 KB)");
        }
    }
}
