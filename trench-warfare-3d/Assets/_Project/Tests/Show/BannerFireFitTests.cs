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

        /// <summary>The fire may start this far off the mouth ON SCREEN, measured in metres at the muzzle's depth, and
        /// its long side may lie this far off the barrel's own axis on screen (the owner's words, 2026-10-14: the fire
        /// "starts exactly at the mouth of the barrel that fired, lies along that barrel's own axis, and stays there
        /// from every camera angle").</summary>
        const float LineTol = 0.15f, AngleTol = 5f;

        /// <summary>A camera the owner looks from: where it stands round the machine (bearing from the way it faces,
        /// degrees), how high it is lifted (pitch, degrees) and how far off it is (m).</summary>
        struct Eye { public string Name; public float Bearing, Pitch, Dist; }

        static readonly Eye[] Eyes =
        {
            new Eye { Name = "front quarter", Bearing = 35f, Pitch = 12f, Dist = 22f },
            new Eye { Name = "side",          Bearing = 90f, Pitch = 0f,  Dist = 22f },
            new Eye { Name = "rear quarter",  Bearing = 145f, Pitch = 12f, Dist = 22f },
            new Eye { Name = "tactical",      Bearing = 0f,  Pitch = 25f, Dist = 30f },   // TacticalCamera's own Pitch 25 at Zoom 30
        };

        /// <summary>A real Camera standing where that eye is, looking at the muzzle.</summary>
        static Camera Look(Eye e, Vector3 at)
        {
            var go = new GameObject("BannerFireEye");
            var cam = go.AddComponent<Camera>();
            cam.fieldOfView = 42f; cam.aspect = 16f / 9f; cam.enabled = false;
            float b = e.Bearing * Mathf.Deg2Rad, p = e.Pitch * Mathf.Deg2Rad;
            Vector3 away = new Vector3(Mathf.Sin(b) * Mathf.Cos(p), Mathf.Sin(p), Mathf.Cos(b) * Mathf.Cos(p));
            go.transform.position = at + away * e.Dist;
            go.transform.LookAt(at);
            return cam;
        }

        /// <summary>A point's place on the screen in METRES at the muzzle's depth: its offset from the muzzle measured
        /// on the camera's own plane, which is the plane TW/Flipbook builds every card in (UNITY_MATRIX_V rows 0 and 1
        /// are the camera's right and up), so this is the same screen the card is drawn on.</summary>
        static Vector2 OnScreen(Camera cam, Vector3 from, Vector3 to)
        {
            Vector3 d = to - from;
            return new Vector2(Vector3.Dot(d, cam.transform.right), Vector3.Dot(d, cam.transform.up));
        }

        static Vector3 Corner(Vector3 at, Vector3 right, Vector3 up, float x, float y, float sr, float cr, float w, float h)
        {
            // the shader rotates the unit card, THEN scales it by (width, height)
            float cx = (x * cr - y * sr) * w, cy = (x * sr + y * cr) * h;
            return at + right * cx + up * cy;
        }

        /// <summary>The drawn quad of a live card, rebuilt exactly as Flipbook_URP.shader's vert does: the art root (the
        /// midpoint of its left edge, where the fire books start their drawing) and the far end of its long side.
        /// A laid card is placed, rolled and shortened from the eye the way FlipbookFx.Draw does it.</summary>
        static void DrawnQuad(Camera cam, Vector3 pos, Vector3 axis, float width, float height, float storedRoll, bool mirrored,
                              out Vector3 root, out Vector3 tipEnd)
        {
            Vector3 right = cam.transform.right, up = cam.transform.up;
            Vector3 at = pos; float roll = storedRoll, w = width;
            if (axis.sqrMagnitude > 0f) FlipbookFx.LayAlong(right, up, pos, axis, width, height, out at, out roll, out w);
            float mirror = mirrored ? -1f : 1f, sr = Mathf.Sin(roll), cr = Mathf.Cos(roll);
            root = Corner(at, right, up, -0.5f * mirror, 0f, sr, cr, w, height);
            tipEnd = Corner(at, right, up, 0.5f * mirror, 0f, sr, cr, w, height);
        }

        static System.Collections.IList Cards(FlipbookFx fx) =>
            (System.Collections.IList)typeof(FlipbookFx).GetField("cards", Any).GetValue(fx);

        static object Field(object card, string name) => card.GetType().GetField(name, Any).GetValue(card);

        /// <summary>The Banner's muzzle fire, measured on what is DRAWN: for every barrel that can fire, two yaws of the
        /// top and four eyes (front quarter, side, rear quarter and the game's own tactical view), the flame's art root
        /// sits on the mouth of that barrel's mesh and its long side lies along that barrel's own axis, both measured on
        /// screen. (Owner, 2026-10-14, on the close frame 5-5-new-close-0332.png: "its visually still off, line it up
        /// properly". 5a4b834b moved the muzzle POINT and pushed each card out along the bore IN THE WORLD; the cards are
        /// camera-facing, so that push foreshortens on screen while the card's own half-width does not, and the 6 m blast
        /// slid up to 3 m off the mouth at a quarter view. The plain Flash was rolled at random on top of that.)</summary>
        [Test]
        public void TheBannersFireLiesAlongTheBarrelFromEveryEye()
        {
            var m = Load();
            var (r, v) = Rig(m);
            var fx = new FlipbookFx();
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");
                r.GetType().GetField("books", Any).SetValue(r, fx);
                var draw = r.GetType().GetMethod("DrawFireCards", Any);
                Assert.NotNull(draw, "TankRenderer has no DrawFireCards");
                var cards = Cards(fx);
                var yaw = Arr(v, "GunYaw");
                var recoil = Arr(v, "Recoil");
                int measured = 0;
                for (int k = 0; k < m.MuzzlePart.Length; k++)
                {
                    if (m.MuzzlePart[k] < 0) continue;
                    int part = m.MuzzlePart[k];
                    string barrel = m.Lods[0].Parts[part].Name;
                    foreach (float top in new[] { 0f, 55f * Mathf.Deg2Rad })
                    {
                        yaw[0] = top; yaw[1] = top;
                        recoil[k] = 1f;                  // the first frame of the shot, which is the frame the flash is on
                        PoseWorld(r, v, m);
                        Vector3 muzzle = DrawnMuzzle(r, v, k, out Vector3 dir);
                        var b = m.Lods[0].Parts[part].Mesh.bounds;
                        var world = World(v)[part];
                        Vector3 tip = world.MultiplyPoint3x4(new Vector3(b.center.x, b.center.y, b.max.z));
                        Vector3 baseAt = world.MultiplyPoint3x4(new Vector3(b.center.x, b.center.y, b.min.z));
                        Vector3 bore = tip - baseAt;
                        float boreOff = Vector3.Angle(dir, bore);
                        TestContext.Out.WriteLine($"barrel {barrel}, top yaw {top * Mathf.Rad2Deg:0}: the renderer's direction is {boreOff:0.0} deg off the barrel's drawn axis");
                        Assert.Less(boreOff, AngleTol, $"the fire of {barrel} is sent {boreOff:0.0} deg off the barrel's own drawn axis: it follows the aim or the hull, not the bore");
                        foreach (var e in Eyes)
                        {
                            var cam = Look(e, tip);
                            try
                            {
                                cards.Clear();
                                draw.Invoke(r, new object[] { v, muzzle, dir, FlipbookFx.ScreenRoll(cam, dir), Vector3.forward });
                                Vector2 boreScreen = OnScreen(cam, baseAt, tip);
                                int fire = 0;
                                for (int i = 0; i < cards.Count; i++)
                                {
                                    var c = cards[i];
                                    var book = (FlipbookFx.Book)Field(c, "Book");
                                    if (book != FlipbookFx.Book.GunBlast && book != FlipbookFx.Book.Flash) continue;
                                    fire++;
                                    DrawnQuad(cam, (Vector3)Field(c, "Pos"), (Vector3)Field(c, "Axis"), (float)Field(c, "Width"), (float)Field(c, "Height"),
                                        (float)Field(c, "Roll"), ((FlipbookFx.Kind)Field(c, "Kind") & FlipbookFx.Kind.Mirror) != 0, out Vector3 root, out Vector3 end);
                                    float off = OnScreen(cam, tip, root).magnitude;
                                    float ang = Vector2.Angle(OnScreen(cam, root, end), boreScreen);
                                    TestContext.Out.WriteLine($"  {e.Name}: the {book} card starts {off * 100f:0.0} cm off the mouth of {barrel} and lies {ang:0.0} deg off its axis");
                                    Assert.Less(off, LineTol, $"from the {e.Name} the Banner's {book} starts {off * 100f:0.0} cm off the mouth of {barrel}: the fire is not on the gun");
                                    Assert.Less(ang, AngleTol, $"from the {e.Name} the Banner's {book} lies {ang:0.0} deg off the axis of {barrel}: the flame is not along the bore");
                                    measured++;
                                }
                                Assert.AreEqual(2, fire, $"the Banner's shot drew {fire} fire cards from the {e.Name}, not the blast and the flash");
                            }
                            finally { Object.DestroyImmediate(cam.gameObject); }
                        }
                        recoil[k] = 0f;
                    }
                }
                Assert.Greater(measured, 0, "no barrel of the Banner has a muzzle to measure");
            }
            finally
            {
                // not fx.Dispose(): it Destroys its materials, which EditMode forbids (they are HideAndDontSave)
                Cards(fx).Clear();
                r.GetType().GetField("books", Any).SetValue(r, null);
                Object.DestroyImmediate(r.gameObject);
            }
        }
    }
}
