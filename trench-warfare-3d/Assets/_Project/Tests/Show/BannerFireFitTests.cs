// Phase: the Banner, its fire (owner, 2026-10-13, on the film of its redone legs, shot and recoil: "the vfx is still
// off. look at the fire it clips over the weapon"). The muzzle fire was measured before anything was changed, on the
// real posed meshes, and these are the numbers that came out of this tree:
//   * the drawn muzzle: Socket_Muzzle sits on the mouth of ONE of the Banner's two barrels (TankImport puts it on
//     (centre.x, centre.y, max.z) of that gun's mesh), but MuzzleWorld applied its offset in GunPart[0]'s frame, and
//     both of the Banner's guns are gun 0, so the frame could be the OTHER barrel's: the fire then starts off the
//     barrel, over the gun's body.
//   * every card's near edge along the barrel (its centre pushed out, less half its width): the plain Flash is 3.2 m
//     wide 0.4 m out, so its near edge sat 1.20 m BEHIND the muzzle plane, inside the barrel and under the top; smoke
//     card 0 at 0.05 m behind it.
//   * a card's slip: the 1 s GunBlast card stood still in the world while the Banner walked at 2.4 m/s, so it slid
//     2.4 m back over the barrel and the top before it died.
// The claims below are what the fire must do instead: start at the mouth of the barrel that fired, go forward from
// there, and travel with the machine while it lives. The sizes themselves are GunMuzzleLookTests'.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class BannerFireFitTests
    {
        const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;

        /// <summary>The fire may start this far off the mouth of the barrel and no further: 10 cm at the size a
        /// machine is drawn.</summary>
        const float MouthTol = 0.10f;

        static TankModel Load()
        {
            var m = TankModel.Load("Banner", VehicleArchetype.Banner, "Hull", VehicleSize.Walker);
            Assert.NotNull(m, "the Banner did not load");
            return m;
        }

        /// <summary>A TankRenderer and a view of its own kind holding this model (ModelCutTests' rig: Awake never runs
        /// in EditMode, so the renderer's knobs keep the battle's defaults).</summary>
        static (TankRenderer r, object v) Rig(TankModel m)
        {
            var go = new GameObject("BannerFireUnderTest");
            var r = go.AddComponent<TankRenderer>();
            var view = r.GetType().GetNestedType("View", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
            Assert.NotNull(view, "TankRenderer has no View");
            object v = System.Activator.CreateInstance(view, true);
            view.GetField("Model", Any).SetValue(v, m);
            view.GetField("Archetype", Any).SetValue(v, VehicleArchetype.Banner);
            return (r, v);
        }

        static float[] Arr(object v, string field) => (float[])v.GetType().GetField(field, Any).GetValue(v);

        static Matrix4x4[] World(object v) => (Matrix4x4[])v.GetType().GetField("World", Any).GetValue(v);

        /// <summary>The part's matrix in its parent's frame, as TankRenderer draws it this instant.</summary>
        static Matrix4x4 Posed(TankRenderer r, object v, TankModel m, int part)
        {
            var mi = r.GetType().GetMethod("PartLocal", Any);
            Assert.NotNull(mi, "TankRenderer has no PartLocal");
            return (Matrix4x4)mi.Invoke(r, new object[] { v, m.Lods[0].Parts[part], part });
        }

        /// <summary>Poses every LOD0 part the way the renderer does and writes the matrices into the view, so the
        /// renderer's own MuzzleWorld reads the drawn pose. The machine stands on the origin facing +z.</summary>
        static void PoseWorld(TankRenderer r, object v, TankModel m)
        {
            var parts = m.Lods[0].Parts;
            var world = new Matrix4x4[parts.Count];
            for (int i = 0; i < parts.Count; i++)
            {
                var local = Posed(r, v, m, i);
                int up = parts[i].Parent;
                world[i] = up >= 0 ? world[up] * local : local;
            }
            v.GetType().GetField("World", Any).SetValue(v, world);
        }

        /// <summary>Where the renderer draws the fire of gun k, and which way.</summary>
        static Vector3 DrawnMuzzle(TankRenderer r, object v, int k, out Vector3 dir)
        {
            var mi = r.GetType().GetMethod("MuzzleWorld", Any);
            Assert.NotNull(mi, "TankRenderer has no MuzzleWorld");
            var args = new object[] { v, k, Vector3.forward };
            var at = (Vector3)mi.Invoke(r, args);
            dir = (Vector3)args[2];
            return at;
        }

        /// <summary>The mouth of a barrel as cut: the point TankImport puts Socket_Muzzle on, posed.</summary>
        static Vector3 Mouth(object v, TankModel m, int part)
        {
            var b = m.Lods[0].Parts[part].Mesh.bounds;
            return World(v)[part].MultiplyPoint3x4(new Vector3(b.center.x, b.center.y, b.max.z));
        }

        /// <summary>The fire of the Banner's shot starts at the mouth of the barrel that fired - the point the mesh
        /// gives, not a number the renderer works out - at the first frame of the shot and again once the barrel is
        /// back in battery, which is where the blast card still is at its last frame.</summary>
        [Test]
        public void TheBannersFireStartsAtTheMouthOfTheBarrelThatFired()
        {
            var m = Load();
            var (r, v) = Rig(m);
            Assert.IsTrue(m.Sockets.TryGetValue("Socket_Muzzle", out var socket), "the Banner has no Socket_Muzzle");
            string barrel = m.Lods[0].Parts[socket.part].Name;
            var recoil = Arr(v, "Recoil");
            foreach (float at in new[] { 1f, 0f })   // the first frame of the shot, and the blast card's last
            {
                recoil[0] = at;
                PoseWorld(r, v, m);
                Vector3 drawn = DrawnMuzzle(r, v, 0, out Vector3 dir), mouth = Mouth(v, m, socket.part);
                Vector3 root = World(v)[socket.part].GetColumn(3);
                float off = (drawn - mouth).magnitude;
                TestContext.Out.WriteLine($"recoil {at:0}: the Banner's fire starts {off * 100f:0.0} cm from the mouth of {barrel} ({drawn} against {mouth})");
                Assert.Less(off, MouthTol, $"with recoil {at:0} the Banner's fire starts {off * 100f:0.0} cm off the mouth of {barrel}: it is drawn on the gun, not out of it");
                Assert.Greater(Vector3.Dot(dir, (mouth - root).normalized), 0.9f, "the fire does not point out along the barrel it leaves");
            }
            Object.DestroyImmediate(r.gameObject);
        }

        /// <summary>No card of the Banner's shot sits behind the mouth of the barrel (drawn through the barrel and the
        /// top), and none of them leaves a gap in front of it.</summary>
        [Test]
        public void NoCardOfTheBannersShotSitsInsideTheWeapon()
        {
            var cards = new TankRenderer.FireCard[TankRenderer.MaxFireCards];
            int n = TankRenderer.FireCards(VehicleArchetype.Banner, cards);
            Assert.Greater(n, 0, "the Banner's shot draws no cards at all");
            for (int i = 0; i < n; i++)
            {
                var c = cards[i];
                TestContext.Out.WriteLine($"{c.Book}: {c.Width:0.00} m wide, pushed {c.Push:0.00} m, near edge {c.NearEdge:0.00} m, {c.Life:0.00} s");
                Assert.GreaterOrEqual(c.NearEdge, -TankRenderer.FireBehindMax,
                    $"the {c.Book} card is {c.Width:0.00} m wide {c.Push:0.00} m out, so it starts {-c.NearEdge:0.00} m behind the muzzle: it is drawn inside the weapon");
                Assert.LessOrEqual(c.NearEdge, TankRenderer.FireGapMax,
                    $"the {c.Book} card starts {c.NearEdge:0.00} m in front of the muzzle: the fire does not touch the barrel");
            }
        }

        /// <summary>A card that outlives the walker's next step travels with it: a world-static card slides back over
        /// the barrel and the top while the machine walks through it.</summary>
        [Test]
        public void TheBannersLongFireCardsAreCarriedWithTheMachine()
        {
            float speed = RosterEntry.Banner.Speed;
            var cards = new TankRenderer.FireCard[TankRenderer.MaxFireCards];
            int n = TankRenderer.FireCards(VehicleArchetype.Banner, cards);
            for (int i = 0; i < n; i++)
            {
                var c = cards[i];
                float slip = c.Carried ? 0f : c.Life * speed;
                TestContext.Out.WriteLine($"{c.Book}: {c.Life:0.00} s, carried {c.Carried}, slip {slip:0.00} m at {speed:0.0} m/s");
                if (c.Life > TankRenderer.CarriedOver)
                    Assert.IsTrue(c.Carried, $"the {c.Book} card lives {c.Life:0.00} s and stands still, so the Banner walks {slip:0.00} m through it");
            }
        }
    }
}
