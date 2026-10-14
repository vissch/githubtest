// Phase: the Banner, its fire (owner, 2026-10-14: "its visually still off, line it up properly"). The cause was not
// the Banner's alone: MuzzleWorld had a last-ditch line, v.Pos + up * 2.5, for a gun with neither a muzzle socket nor
// a gun part of its own, and a shot drawn from that point is drawn in open air beside the machine. The Banner's two
// stacked barrels are both gun 0 and only Gun_Lower carried Socket_Muzzle, so gun 1 fell to it and the flame sat
// 290.5 cm off the mouth of Gun_Upper. This file holds the rule for EVERY machine in the roster, so no second machine
// can quietly draw its fire off itself: a barrel that can fire has a mouth of its own, the renderer draws from that
// mouth, and it is nowhere near the last-ditch point. The machines whose art gives them their sockets must draw
// exactly where they did before: the Pincer and the Skimmer are measured against their own Sockets entry.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class MachineMuzzleTests
    {
        const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;
        const System.Reflection.BindingFlags Stat = System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;

        /// <summary>The fire may start this far off the mouth of the barrel and no further: BannerFireFitTests' own
        /// 10 cm, at the size a machine is drawn.</summary>
        const float MouthTol = 0.10f;

        /// <summary>TankRenderer's own roster: name, archetype, root part and scale, read off the private table so a
        /// machine added there is measured here without this file being touched.</summary>
        static System.Collections.Generic.List<(string Name, byte Archetype, string Root, float Scale)> Roster()
        {
            var field = typeof(TankRenderer).GetField("Machines", Stat);
            Assert.NotNull(field, "TankRenderer has no Machines table");
            var rows = (System.Array)field.GetValue(null);
            var list = new System.Collections.Generic.List<(string, byte, string, float)>();
            foreach (var row in rows)
            {
                // a ValueTuple's fields are Item1.. at runtime: the names in the table are the compiler's only
                var t = row.GetType();
                list.Add(((string)t.GetField("Item1").GetValue(row), (byte)t.GetField("Item2").GetValue(row),
                          (string)t.GetField("Item3").GetValue(row), (float)t.GetField("Item4").GetValue(row)));
            }
            Assert.Greater(list.Count, 0, "TankRenderer's Machines table is empty");
            return list;
        }

        static (TankRenderer r, object v) Rig(TankModel m, byte archetype)
        {
            var go = new GameObject("MachineMuzzleUnderTest");
            var r = go.AddComponent<TankRenderer>();
            var view = r.GetType().GetNestedType("View", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
            Assert.NotNull(view, "TankRenderer has no View");
            object v = System.Activator.CreateInstance(view, true);
            view.GetField("Model", Any).SetValue(v, m);
            view.GetField("Archetype", Any).SetValue(v, archetype);
            return (r, v);
        }

        static Matrix4x4[] World(object v) => (Matrix4x4[])v.GetType().GetField("World", Any).GetValue(v);

        /// <summary>Poses every LOD0 part the way the renderer does and writes the matrices into the view, so the
        /// renderer's own MuzzleWorld reads the drawn pose (BannerFireFitTests' rig).</summary>
        static void PoseWorld(TankRenderer r, object v, TankModel m)
        {
            var mi = r.GetType().GetMethod("PartLocal", Any);
            Assert.NotNull(mi, "TankRenderer has no PartLocal");
            var parts = m.Lods[0].Parts;
            var world = new Matrix4x4[parts.Count];
            for (int i = 0; i < parts.Count; i++)
            {
                var local = (Matrix4x4)mi.Invoke(r, new object[] { v, parts[i], i });
                int up = parts[i].Parent;
                world[i] = up >= 0 ? world[up] * local : local;
            }
            v.GetType().GetField("World", Any).SetValue(v, world);
        }

        static Vector3 DrawnMuzzle(TankRenderer r, object v, int k)
        {
            var mi = r.GetType().GetMethod("MuzzleWorld", Any);
            Assert.NotNull(mi, "TankRenderer has no MuzzleWorld");
            var args = new object[] { v, k, Vector3.forward };
            return (Vector3)mi.Invoke(r, args);
        }

        /// <summary>The mouth of a barrel as cut: the point TankImport puts Socket_Muzzle on, posed.</summary>
        static Vector3 Mouth(object v, TankModel m, int part)
        {
            var b = m.Lods[0].Parts[part].Mesh.bounds;
            return World(v)[part].MultiplyPoint3x4(new Vector3(b.center.x, b.center.y, b.max.z));
        }

        /// <summary>The k-th Role==Gun part of LOD0 in part order, or -1: the barrel that fires gun k.</summary>
        static int Barrel(TankModel m, int k)
        {
            var parts = m.Lods[0].Parts;
            int seen = 0;
            for (int i = 0; i < parts.Count; i++)
                if (parts[i].Role == TankPartRole.Gun && parts[i].Mesh != null && seen++ == k) return i;
            return -1;
        }

        /// <summary>True when the art gives gun k a muzzle socket of its own, so its mouth is not derived from a mesh
        /// (the names TankModel.Load looks for, in its order).</summary>
        static bool Socketed(TankModel m, int k) =>
            k == 0 ? m.Sockets.ContainsKey("Socket_Muzzle") || m.Sockets.ContainsKey("Socket_Muzzle_L")
                   : m.Sockets.ContainsKey("Socket_Muzzle_R");

        /// <summary>No gun of any machine in the roster draws its fire from MuzzleWorld's last-ditch point
        /// (v.Pos + up * 2.5, a spot in open air beside the machine): every barrel that can fire has a mouth of its
        /// own and the renderer draws out of that mouth. Before 2026-10-14 the Banner's gun 1 drew from the last-ditch
        /// point, 290.5 cm off the mouth of Gun_Upper, which is the daylight the owner saw between the flame and the
        /// gun. Every machine and gun is written out with its own number, so the next one to slip says so.</summary>
        [Test]
        public void NoMachineDrawsItsFireFromTheLastDitchPoint()
        {
            foreach (var row in Roster())
            {
                var m = TankModel.Load(row.Name, row.Archetype, row.Root, row.Scale);
                Assert.NotNull(m, $"{row.Name} did not load");
                var (r, v) = Rig(m, row.Archetype);
                try
                {
                    PoseWorld(r, v, m);
                    Vector3 pos = (Vector3)v.GetType().GetField("Pos", Any).GetValue(v);
                    Vector3 lastDitch = pos + Vector3.up * 2.5f;
                    for (int k = 0; k < m.MuzzlePart.Length; k++)
                    {
                        int barrel = Barrel(m, k);
                        if (barrel < 0) { TestContext.Out.WriteLine($"{row.Name} gun {k}: no barrel of its own, nothing to fire"); continue; }
                        Assert.GreaterOrEqual(m.MuzzlePart[k], 0,
                            $"{row.Name} gun {k} fires {m.Lods[0].Parts[barrel].Name} but has no mouth of its own, so its fire is drawn off the machine");
                        Vector3 drawn = DrawnMuzzle(r, v, k), mouth = Mouth(v, m, barrel);
                        float off = (drawn - mouth).magnitude, air = (drawn - lastDitch).magnitude;
                        bool own = Socketed(m, k);
                        TestContext.Out.WriteLine($"{row.Name} gun {k}: fires {m.Lods[0].Parts[barrel].Name}, {(own ? "its own socket" : "its barrel's mesh")}, drawn {off * 100f:0.0} cm off that barrel's mouth, {air * 100f:0.0} cm from the last-ditch point");
                        // only a derived mouth must sit on the k-th gun part: where the art gives the gun a socket, the
                        // socket is the truth and may hang elsewhere (the Kettle's is 1.92 m off the mouth of its
                        // Mortar part), and EveryMachineWithItsOwnMuzzleSocketDrawsWhereItDid holds that one still
                        if (!own)
                            Assert.Less(off, MouthTol,
                                $"{row.Name} gun {k} draws its fire {off * 100f:0.0} cm off the mouth of {m.Lods[0].Parts[barrel].Name}");
                        Assert.Greater(air, 0.01f,
                            $"{row.Name} gun {k} draws its fire from MuzzleWorld's last-ditch point, which is open air beside the machine");
                    }
                }
                finally { Object.DestroyImmediate(r.gameObject); }
            }
        }

        /// <summary>Every machine whose art gives it its own Socket_Muzzle draws exactly where it drew before this
        /// change: on the socket, carried through the part it hangs on. Read off the Sockets dictionary, which the fix
        /// does not touch, so these stand for every machine that was already right. At least two must be measured, or
        /// the test proves nothing (the Pincer has no muzzle socket of its own: it is one of the derived ones).</summary>
        [Test]
        public void EveryMachineWithItsOwnMuzzleSocketDrawsWhereItDid()
        {
            int measured = 0;
            foreach (var row in Roster())
            {
                var m = TankModel.Load(row.Name, row.Archetype, row.Root, row.Scale);
                Assert.NotNull(m, $"{row.Name} did not load");
                if (!m.Sockets.TryGetValue("Socket_Muzzle", out var s)) { TestContext.Out.WriteLine($"{row.Name}: no Socket_Muzzle of its own"); continue; }
                var (r, v) = Rig(m, row.Archetype);
                try
                {
                    PoseWorld(r, v, m);
                    Vector3 want = World(v)[s.part].MultiplyPoint3x4(s.local);
                    Vector3 drawn = DrawnMuzzle(r, v, 0);
                    TestContext.Out.WriteLine($"{row.Name} gun 0: drawn {drawn}, its socket {want}, {(drawn - want).magnitude * 1000f:0.000} mm apart");
                    Assert.AreEqual(0f, (drawn - want).magnitude, 1e-4f, $"{row.Name}'s fire moved off its own muzzle socket");
                    measured++;
                }
                finally { Object.DestroyImmediate(r.gameObject); }
            }
            Assert.GreaterOrEqual(measured, 2, "fewer than two machines in the roster own a Socket_Muzzle, so nothing was held still");
        }
    }
}
