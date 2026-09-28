// Phase: A5c (2026-09-28) — a walker's legs hang together (owner: the Redoubt and the Kettle "are bugged":
// a Kettle leg walked a body-width beside the machine, a Redoubt leg came apart in three). TankModel.Assemble puts every
// leg joint on the piece it hangs from; these hold that on the joined walkers at both LODs, and that the two LODs agree, so
// a rebake that splits the legs apart again, or a loader that stops joining them, fails here and not in a playtest.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class WalkerJointTests
    {
        // the machines TankModel.JoinedLegs joins (Pavise and Banner are not joined yet: see TankModel.Assemble)
        static readonly (string name, byte archetype)[] Walkers = { ("Kettle", VehicleArchetype.Kettle), ("Redoubt", VehicleArchetype.Redoubt) };

        [Test]
        public void TheJoinedWalkersAreTheOnesListed()
        {
            CollectionAssert.AreEquivalent(TankModel.JoinedLegs, System.Array.ConvertAll(Walkers, w => w.name));
        }

        static bool Limb(TankPartRole r) => r == TankPartRole.Leg || r == TankPartRole.Thigh || r == TankPartRole.Shin || r == TankPartRole.Foot;

        [Test]
        public void EveryLegJointLiesOnThePieceItHangsFrom()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = TankModel.Load(name, archetype, "Body", VehicleSize.Walker);
                Assert.IsNotNull(m, name);
                for (int lod = 0; lod < 2; lod++)
                {
                    var parts = m.Lods[lod].Parts;
                    int limbs = 0;
                    foreach (var p in parts)
                    {
                        if (!Limb(p.Role) || p.Parent < 0) continue;
                        limbs++;
                        var q = parts[p.Parent].Mesh;
                        float gap = (TankModel.NearestOnSurface(q, p.Local) - p.Local).magnitude;   // on the parent's surface, wherever the rule put it
                        // LOD1 took LOD0's moves, and its decimated surface is a little off LOD0's
                        Assert.Less(gap, lod == 0 ? TankModel.JointSlack : 0.35f, $"{name} LOD{lod} {p.Name}: its joint stands {gap:0.00} m off {parts[p.Parent].Name}");
                    }
                    Assert.Greater(limbs, 0, $"{name} LOD{lod} has legs");
                }
            }
        }

        [Test]
        public void BothLodsHoldTheirLegsInTheSamePlace()
        {
            foreach (var (name, archetype) in Walkers)
            {
                var m = TankModel.Load(name, archetype, "Body", VehicleSize.Walker);
                if (m.Lods[1] == m.Lods[0]) continue;
                foreach (var p in m.Lods[0].Parts)
                {
                    if (!Limb(p.Role)) continue;
                    int k = m.Lods[1].Find(p.Name);
                    if (k < 0) continue;
                    Assert.Less((m.Lods[1].Parts[k].Local - p.Local).magnitude, 0.05f, $"{name} {p.Name}: the far form's joint is not the near form's");
                }
            }
        }
    }
}
