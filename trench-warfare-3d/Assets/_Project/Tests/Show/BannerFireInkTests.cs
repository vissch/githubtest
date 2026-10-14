// Phase: the Banner, its fire, third pass (owner, 2026-10-14, on the side-on close frame of its shot: "its visually
// still off, line it up properly"). Two passes before this one measured the ANCHOR POINT of the fire and read 0.0 cm
// while the picture was plainly wrong: the tracer leaves the mouth of the barrel correctly, the flame does not. What
// his frame shows is the flame's bright wide body hanging below and ahead of the mouth, with only its thin tip on the
// bore line a barrel-mouth out - a quarter turn plus an offset of about the card's own half size.
//
// The cause, measured off the PNGs (the leg's ink.md): FlipbookFx.LayAlong put the QUAD's left-edge midpoint on the
// mouth and lined +u up with the bore, but the FireGunBlast book does not draw its flame out of the left edge's
// middle. Its flame grows out of the cell's LOWER-LEFT corner, at UV (0.070, 0.276), and runs up and to the right at
// about +40 degrees. So the quad was laid perfectly and the ink inside it was 40 degrees and 0.22 of a card-height
// off. The Flash is a round bloom with no direction at all (centroid (0.500, 0.537)), and the left-edge rule carried
// its bright middle half a card - 1.0 m of the Banner's 2.0 m card - out in front of the mouth.
//
// BannerFireFitTests asserts where the quad's CORNER lands; that is what let a wrong picture pass. These tests assert
// where the DRAWING lands, and they take the drawing's root, direction and thickness FROM THE PNG, not from the code's
// own arithmetic. The three quantities are the owner's: (1) the drawing's root sits on the mouth, within a quarter of
// the barrel mouth's own on-screen width, (2) the drawing's root-to-tip axis is within 10 degrees of the projected
// bore, (3) the wide bright end is the end at the mouth.
//
// Measured on the quad's four drawn corners and the sheet frame's opaque ink, not on rendered pixels: an EditMode test
// has no frame to read, and the quad plus the ink inside it is exactly what a frame would show.
//
// Over the whole roster (2026-10-14): the Banner is the ONLY machine that lays a card along its bore, so it was the
// only one drawn wrong and the only one this fix moves. EveryLaidFlameRootsOnTheMouthAndPointsDownTheBore runs over
// every row of TankRenderer.Machines, so a machine that starts laying a flame is measured here without a new test.
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class BannerFireInkTests
    {
        const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;
        const System.Reflection.BindingFlags Stat = System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;

        /// <summary>The drawing's root may sit this share of the barrel mouth's own on-screen width off the mouth, and
        /// its axis this many degrees off the projected bore (the owner's two figures, 2026-10-14).</summary>
        const float RootShare = 0.25f, AxisTol = 10f;

        /// <summary>A sheet frame's ink, read off its pixels: the ink-weighted centre, the principal axis turned so
        /// that the THICK end is the near one, the point that end sits on, how thick each end is, and how elongated
        /// the whole drawing is.</summary>
        struct Ink
        {
            public Vector2 Centre, ThickEnd, Axis;   // cell UV, v up; Axis is a unit direction in the same UV
            public float ThickRoot, ThickTip, Aspect, Length;
        }

        /// <summary>A drawing this much longer than it is wide, or more, has a direction worth laying along; below it
        /// the two spreads are too close to call and the book is a bloom. Measured: the GunBlast flame 1.75, the
        /// Flash 1.57 - so this threshold does NOT decide which is which. The sheet declares that (InkPoint zero is a
        /// bloom) and the measurement only has to agree that a declared bloom is not a long streak.</summary>
        const float StreakAspect = 2.5f;

        static readonly Dictionary<FlipbookFx.Book, Ink> inkCache = new Dictionary<FlipbookFx.Book, Ink>();
        static readonly Dictionary<FlipbookFx.Book, string> inkReport = new Dictionary<FlipbookFx.Book, string>();

        static Ink InkOf(FlipbookFx.Book book, out string report)
        {
            if (!inkCache.TryGetValue(book, out var ink))
            {
                ink = Measure(book, 0, out string made);
                inkCache[book] = ink; inkReport[book] = made;
            }
            report = inkReport[book];
            return ink;
        }

        /// <summary>The ink of one frame of a book, in that frame's own cell UV. The cell is picked exactly as
        /// Flipbook_URP.shader's vert picks it: column f % cols, and the row rows-1-f/cols counted from the BOTTOM,
        /// which is where v = 0 is in a Unity texture.</summary>
        static Ink Measure(FlipbookFx.Book book, int frame, out string report)
        {
            FlipbookFx.Grid(book, out string name, out int cols, out int rows, out int frames);
            Assert.Less(frame, frames, $"{name} has only {frames} frames");
            string path = Path.Combine(Application.dataPath, "_Project/Resources/VFX/" + name + ".png");
            Assert.IsTrue(File.Exists(path), $"the sheet {name}.png is not in Resources/VFX");
            var tex = new Texture2D(2, 2, TextureFormat.RGBA32, false);
            try
            {
                // read off the file, not the imported asset: an importer with Read/Write off would refuse GetPixels
                Assert.IsTrue(tex.LoadImage(File.ReadAllBytes(path)), $"{name}.png could not be decoded");
                int cw = tex.width / cols, ch = tex.height / rows;
                var px = tex.GetPixels((frame % cols) * cw, (rows - 1 - frame / cols) * ch, cw, ch);
                var ink = new Ink();
                float su = 0f, sv = 0f, sw = 0f;
                int count = 0;
                for (int i = 0; i < px.Length; i++)
                {
                    float w = Opaque(px[i]); if (w <= 0f) continue;
                    count++; sw += w;
                    su += w * ((i % cw + 0.5f) / cw); sv += w * ((i / cw + 0.5f) / ch);
                }
                Assert.Greater(count, 0, $"{name} frame {frame} has no ink at all");
                ink.Centre = new Vector2(su / sw, sv / sw);
                float xx = 0f, yy = 0f, xy = 0f;
                for (int i = 0; i < px.Length; i++)
                {
                    float w = Opaque(px[i]); if (w <= 0f) continue;
                    float du = (i % cw + 0.5f) / cw - ink.Centre.x, dv = (i / cw + 0.5f) / ch - ink.Centre.y;
                    xx += w * du * du; yy += w * dv * dv; xy += w * du * dv;
                }
                xx /= sw; yy /= sw; xy /= sw;
                float half = Mathf.Sqrt(Mathf.Max(0f, (xx - yy) * (xx - yy) * 0.25f + xy * xy));
                float big = (xx + yy) * 0.5f + half, small = Mathf.Max(1e-9f, (xx + yy) * 0.5f - half);
                ink.Aspect = Mathf.Sqrt(big / small);
                float th = 0.5f * Mathf.Atan2(2f * xy, xx - yy);
                var axis = new Vector2(Mathf.Cos(th), Mathf.Sin(th));
                Reach(px, cw, ch, ink.Centre, axis, out float tMin, out float tMax);
                float band = 0.15f * (tMax - tMin);
                float nearThick = Thick(px, cw, ch, ink.Centre, axis, tMin, tMin + band);
                float farThick = Thick(px, cw, ch, ink.Centre, axis, tMax - band, tMax);
                if (nearThick < farThick)   // the thick end is the root: turn the axis round so it points away from it
                {
                    axis = -axis;
                    Reach(px, cw, ch, ink.Centre, axis, out tMin, out tMax);
                    float swap = nearThick; nearThick = farThick; farThick = swap;
                }
                ink.Axis = axis; ink.ThickRoot = nearThick; ink.ThickTip = farThick; ink.Length = tMax - tMin;
                // the thick end's own POINT: the ink-weighted centre of the near eighth of the drawing
                float cut = tMin + 0.08f * ink.Length, ru = 0f, rv = 0f, rw = 0f;
                for (int i = 0; i < px.Length; i++)
                {
                    float w = Opaque(px[i]); if (w <= 0f) continue;
                    float u = (i % cw + 0.5f) / cw, v = (i / cw + 0.5f) / ch;
                    if ((u - ink.Centre.x) * axis.x + (v - ink.Centre.y) * axis.y > cut) continue;
                    rw += w; ru += w * u; rv += w * v;
                }
                ink.ThickEnd = new Vector2(ru / rw, rv / rw);
                report = $"{name} frame {frame}: thick end at UV ({ink.ThickEnd.x:0.000}, {ink.ThickEnd.y:0.000}), " +
                         $"centre ({ink.Centre.x:0.000}, {ink.Centre.y:0.000}), axis {Mathf.Atan2(axis.y, axis.x) * Mathf.Rad2Deg:0.0} deg, " +
                         $"aspect {ink.Aspect:0.00}, length {ink.Length:0.000}, thickness root {ink.ThickRoot:0.000} tip {ink.ThickTip:0.000}";
                return ink;
            }
            finally { Object.DestroyImmediate(tex); }
        }

        /// <summary>The ink a premultiplied fire card actually shows: its alpha times its own brightest channel.</summary>
        static float Opaque(Color c)
        {
            float v = c.a * Mathf.Max(c.r, Mathf.Max(c.g, c.b));
            return v > 0.08f ? v : 0f;
        }

        static void Reach(Color[] px, int cw, int ch, Vector2 centre, Vector2 axis, out float tMin, out float tMax)
        {
            tMin = float.MaxValue; tMax = float.MinValue;
            for (int i = 0; i < px.Length; i++)
            {
                if (Opaque(px[i]) <= 0f) continue;
                float t = ((i % cw + 0.5f) / cw - centre.x) * axis.x + ((i / cw + 0.5f) / ch - centre.y) * axis.y;
                if (t < tMin) tMin = t;
                if (t > tMax) tMax = t;
            }
        }

        static float Thick(Color[] px, int cw, int ch, Vector2 centre, Vector2 axis, float a, float b)
        {
            float lo = float.MaxValue, hi = float.MinValue;
            for (int i = 0; i < px.Length; i++)
            {
                if (Opaque(px[i]) <= 0f) continue;
                float du = (i % cw + 0.5f) / cw - centre.x, dv = (i / cw + 0.5f) / ch - centre.y;
                float t = du * axis.x + dv * axis.y;
                if (t < a || t > b) continue;
                float per = du * -axis.y + dv * axis.x;
                if (per < lo) lo = per;
                if (per > hi) hi = per;
            }
            return hi > lo ? hi - lo : 0f;
        }

        /// <summary>Every book that is laid along a direction declares the ink it actually draws. This is the link the
        /// two earlier passes never made: the lay now reads these two numbers off the sheet, so if they drift from the
        /// drawing the picture goes wrong again with every other test still green.</summary>
        [Test]
        public void TheLaidBooksDeclareTheInkTheyActuallyDraw()
        {
            foreach (var book in LaidBooks())
            {
                var ink = InkOf(book, out string report);
                FlipbookFx.InkLay(book, out Vector2 root, out Vector2 point);
                TestContext.Out.WriteLine($"{book}: {report}");
                TestContext.Out.WriteLine($"  declared root ({root.x:0.000}, {root.y:0.000}), point ({point.x:0.000}, {point.y:0.000})");
                bool directed = point.sqrMagnitude > 1e-12f;
                Vector2 want = directed ? ink.ThickEnd : ink.Centre;
                Assert.Less((root - want).magnitude, 0.03f,
                    $"{book} declares its root at ({root.x:0.000}, {root.y:0.000}) in its cell and draws it at ({want.x:0.000}, {want.y:0.000}): the lay puts the flame where the sheet says, so the sheet must say where the flame is. {report}");
                if (directed)
                    Assert.Less(Vector2.Angle(point.normalized, ink.Axis), 6f,
                        $"{book} declares it points {Mathf.Atan2(point.y, point.x) * Mathf.Rad2Deg:0.0} deg in its cell and draws {Mathf.Atan2(ink.Axis.y, ink.Axis.x) * Mathf.Rad2Deg:0.0} deg. {report}");
                else
                    Assert.Less(ink.Aspect, StreakAspect,
                        $"{book} is declared a bloom with no direction but is drawn {ink.Aspect:0.00} times longer than it is wide: it has a direction and should declare it. {report}");
            }
        }

        /// <summary>The owner's three figures, for every machine that lays a flame on its gun: from four eyes and two
        /// yaws of the top, the DRAWING's root sits on the mouth of the barrel that fired, its axis lies along the
        /// projected bore, and the wide bright end is the end at the mouth.</summary>
        [Test]
        public void EveryLaidFlameRootsOnTheMouthAndPointsDownTheBore([ValueSource(nameof(LayingMachines))] string machine)
        {
            var row = RowOf(machine);
            var m = TankModel.Load(row.Name, row.Archetype, row.Root, row.Scale);
            Assert.NotNull(m, $"{machine} did not load");
            var (r, v) = Rig(m, row.Archetype);
            var fx = new FlipbookFx();
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");
                r.GetType().GetField("books", Any).SetValue(r, fx);
                var draw = r.GetType().GetMethod("DrawFireCards", Any);
                Assert.NotNull(draw, "TankRenderer has no DrawFireCards");
                var cards = (System.Collections.IList)typeof(FlipbookFx).GetField("cards", Any).GetValue(fx);
                var yaw = Arr(v, "GunYaw");
                var recoil = Arr(v, "Recoil");
                int guns = Mathf.Max(1, TankSpec.For(row.Archetype).GunCount);   // a barrel the sim never fires draws nothing
                int measured = 0;
                for (int k = 0; k < m.MuzzlePart.Length && k < guns; k++)
                {
                    Assert.GreaterOrEqual(m.MuzzlePart[k], 0, $"gun {k} of the {machine} has no mouth of its own, so its fire is drawn off the machine");
                    int part = m.MuzzlePart[k];
                    string barrel = m.Lods[0].Parts[part].Name;
                    var bounds = m.Lods[0].Parts[part].Mesh.bounds;
                    foreach (float top in new[] { 0f, 55f * Mathf.Deg2Rad })
                    {
                        for (int i = 0; i < yaw.Length; i++) yaw[i] = top;
                        recoil[k] = 1f;                      // the first frame of the shot, the frame the flash is on
                        PoseWorld(r, v, m);
                        Vector3 muzzle = DrawnMuzzle(r, v, k, out Vector3 dir);
                        var world = World(v)[part];
                        Vector3 mouth = world.MultiplyPoint3x4(new Vector3(bounds.center.x, bounds.center.y, bounds.max.z));
                        Vector3 heel = world.MultiplyPoint3x4(new Vector3(bounds.center.x, bounds.center.y, bounds.min.z));
                        Vector3 lipL = world.MultiplyPoint3x4(new Vector3(bounds.min.x, bounds.center.y, bounds.max.z));
                        Vector3 lipR = world.MultiplyPoint3x4(new Vector3(bounds.max.x, bounds.center.y, bounds.max.z));
                        Vector3 lipD = world.MultiplyPoint3x4(new Vector3(bounds.center.x, bounds.min.y, bounds.max.z));
                        Vector3 lipU = world.MultiplyPoint3x4(new Vector3(bounds.center.x, bounds.max.y, bounds.max.z));
                        foreach (var e in Eyes)
                        {
                            var cam = Look(e, mouth);
                            try
                            {
                                cards.Clear();
                                draw.Invoke(r, new object[] { v, muzzle, dir, FlipbookFx.ScreenRoll(cam, dir), Vector3.forward });
                                Vector2 bore = OnScreen(cam, heel, mouth);
                                // how wide the mouth itself reads on screen: the wider of its two diameters, since
                                // the one along the eye foreshortens to nothing (side on, the barrel's own x axis
                                // points at the camera and reads 0.0000 cm)
                                float wide = Mathf.Max(OnScreen(cam, lipL, lipR).magnitude, OnScreen(cam, lipD, lipU).magnitude);
                                Assert.Greater(wide, 1e-3f, $"the mouth of {barrel} has no width on screen from the {e.Name}");
                                for (int i = 0; i < cards.Count; i++)
                                {
                                    var c = cards[i];
                                    var axis = (Vector3)Field(c, "Axis");
                                    if (axis.sqrMagnitude <= 0f) continue;      // not a laid card: the smoke keeps its world push
                                    var book = (FlipbookFx.Book)Field(c, "Book");
                                    var ink = InkOf(book, out string report);
                                    FlipbookFx.InkLay(book, out Vector2 inkRoot, out Vector2 inkPoint);
                                    // the card is LAID by what the sheet declares, and the drawing is then wherever
                                    // the PIXELS put it: that is the whole measurement. Reading the drawing back out
                                    // of the declaration instead would pass whatever the code believed.
                                    bool directed = inkPoint.sqrMagnitude > 1e-12f;
                                    Vector2 drawnRoot = directed ? ink.ThickEnd : ink.Centre;
                                    Vector2 drawnPoint = directed ? ink.Axis * ink.Length : Vector2.zero;
                                    DrawnInk(cam, (Vector3)Field(c, "Pos"), axis, (float)Field(c, "Width"), (float)Field(c, "Height"),
                                             inkRoot, inkPoint, drawnRoot, drawnPoint, out Vector3 root, out Vector3 tip);
                                    float off = OnScreen(cam, mouth, root).magnitude;
                                    TestContext.Out.WriteLine($"{machine} {barrel} top {top * Mathf.Rad2Deg:0} deg, {e.Name}: the {book}'s root is {off * 100f:0.0} cm off the mouth, {off / wide:0.00} of the mouth's own {wide * 100f:0.0} cm on screen");
                                    Assert.Less(off, RootShare * wide,
                                        $"from the {e.Name} the {machine}'s {book} has the root of its DRAWING {off * 100f:0.0} cm off the mouth of {barrel}, which is {off / wide:0.00} of the mouth's own on-screen width: the flame does not touch the gun. {report}");
                                    if (inkPoint.sqrMagnitude > 1e-12f)
                                    {
                                        float ang = Vector2.Angle(OnScreen(cam, root, tip), bore);
                                        TestContext.Out.WriteLine($"    it lies {ang:0.0} deg off the projected bore, thickness root {ink.ThickRoot:0.000} tip {ink.ThickTip:0.000}");
                                        Assert.Less(ang, AxisTol,
                                            $"from the {e.Name} the {machine}'s {book} drawing lies {ang:0.0} deg off the bore of {barrel}: the flame does not point away along the barrel. {report}");
                                        Assert.Greater(ink.ThickRoot, ink.ThickTip,
                                            $"the {book}'s end at the mouth is the thin one ({ink.ThickRoot:0.000} against {ink.ThickTip:0.000}): the flame is laid tip first. {report}");
                                    }
                                    measured++;
                                }
                            }
                            finally { Object.DestroyImmediate(cam.gameObject); }
                        }
                        recoil[k] = 0f;
                    }
                }
                Assert.Greater(measured, 0, $"the {machine} laid no flame to measure");
            }
            finally
            {
                ((System.Collections.IList)typeof(FlipbookFx).GetField("cards", Any).GetValue(fx)).Clear();
                r.GetType().GetField("books", Any).SetValue(r, null);
                Object.DestroyImmediate(r.gameObject);
            }
        }

        // ---- the rig: BannerFireFitTests' and MachineMuzzleTests' own, over the whole roster ----

        /// <summary>Every book any machine's shot lays along its bore, so a book that starts being laid is measured
        /// here without this file being touched.</summary>
        static List<FlipbookFx.Book> LaidBooks()
        {
            var books = new List<FlipbookFx.Book>();
            var cards = new TankRenderer.FireCard[TankRenderer.MaxFireCards];
            foreach (var row in Roster())
            {
                int n = TankRenderer.FireCards(row.Archetype, cards);
                for (int i = 0; i < n; i++)
                    if (cards[i].AlongAxis && !books.Contains(cards[i].Book)) books.Add(cards[i].Book);
            }
            Assert.Greater(books.Count, 0, "no machine lays a card along its bore, so there is no directed flame at all");
            return books;
        }

        /// <summary>Every machine of the roster whose shot lays a card along its bore: the three figures must hold for
        /// each of them, not for the Banner alone.</summary>
        public static string[] LayingMachines()
        {
            var names = new List<string>();
            var cards = new TankRenderer.FireCard[TankRenderer.MaxFireCards];
            foreach (var row in Roster())
            {
                int n = TankRenderer.FireCards(row.Archetype, cards);
                for (int i = 0; i < n; i++)
                    if (cards[i].AlongAxis) { names.Add(row.Name); break; }
            }
            return names.Count > 0 ? names.ToArray() : new[] { "Banner" };
        }

        /// <summary>TankRenderer's own roster (MachineMuzzleTests' reader): a ValueTuple's fields are Item1.. at
        /// runtime, so the names in the table are the compiler's only.</summary>
        static List<(string Name, byte Archetype, string Root, float Scale)> Roster()
        {
            var field = typeof(TankRenderer).GetField("Machines", Stat);
            Assert.NotNull(field, "TankRenderer has no Machines table");
            var list = new List<(string, byte, string, float)>();
            foreach (var row in (System.Array)field.GetValue(null))
            {
                var t = row.GetType();
                list.Add(((string)t.GetField("Item1").GetValue(row), (byte)t.GetField("Item2").GetValue(row),
                          (string)t.GetField("Item3").GetValue(row), (float)t.GetField("Item4").GetValue(row)));
            }
            Assert.Greater(list.Count, 0, "TankRenderer's Machines table is empty");
            return list;
        }

        static (string Name, byte Archetype, string Root, float Scale) RowOf(string machine)
        {
            foreach (var row in Roster()) if (row.Name == machine) return row;
            Assert.Fail($"{machine} is not a row of TankRenderer.Machines");
            return default;
        }

        static (TankRenderer r, object v) Rig(TankModel m, byte archetype)
        {
            var go = new GameObject("FlameInkUnderTest");
            var r = go.AddComponent<TankRenderer>();
            var view = r.GetType().GetNestedType("View", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
            Assert.NotNull(view, "TankRenderer has no View");
            object v = System.Activator.CreateInstance(view, true);
            view.GetField("Model", Any).SetValue(v, m);
            view.GetField("Archetype", Any).SetValue(v, archetype);
            return (r, v);
        }

        static float[] Arr(object v, string field) => (float[])v.GetType().GetField(field, Any).GetValue(v);

        static Matrix4x4[] World(object v) => (Matrix4x4[])v.GetType().GetField("World", Any).GetValue(v);

        static object Field(object card, string name) => card.GetType().GetField(name, Any).GetValue(card);

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

        static Vector3 DrawnMuzzle(TankRenderer r, object v, int k, out Vector3 dir)
        {
            var mi = r.GetType().GetMethod("MuzzleWorld", Any);
            Assert.NotNull(mi, "TankRenderer has no MuzzleWorld");
            var args = new object[] { v, k, Vector3.forward };
            var at = (Vector3)mi.Invoke(r, args);
            dir = (Vector3)args[2];
            return at;
        }

        struct Eye { public string Name; public float Bearing, Pitch, Dist; }

        /// <summary>The four eyes BannerFireFitTests uses, which are the owner's: the side-on and the two quarters of
        /// the films, and the game's own tactical view (TacticalCamera's Pitch 25 at Zoom 30).</summary>
        static readonly Eye[] Eyes =
        {
            new Eye { Name = "front quarter", Bearing = 35f,  Pitch = 12f, Dist = 22f },
            new Eye { Name = "side",          Bearing = 90f,  Pitch = 0f,  Dist = 22f },
            new Eye { Name = "rear quarter",  Bearing = 145f, Pitch = 12f, Dist = 22f },
            new Eye { Name = "tactical",      Bearing = 0f,   Pitch = 25f, Dist = 30f },
        };

        static Camera Look(Eye e, Vector3 at)
        {
            var go = new GameObject("FlameInkEye");
            var cam = go.AddComponent<Camera>();
            cam.fieldOfView = 42f; cam.aspect = 16f / 9f; cam.enabled = false;
            float b = e.Bearing * Mathf.Deg2Rad, p = e.Pitch * Mathf.Deg2Rad;
            Vector3 away = new Vector3(Mathf.Sin(b) * Mathf.Cos(p), Mathf.Sin(p), Mathf.Cos(b) * Mathf.Cos(p));
            go.transform.position = at + away * e.Dist;
            go.transform.LookAt(at);
            return cam;
        }

        /// <summary>A point's place on the screen in METRES at the muzzle's depth: its offset from the muzzle on the
        /// camera's own plane, which is the plane TW/Flipbook builds every card in (UNITY_MATRIX_V rows 0 and 1 are the
        /// camera's right and up), so this is the same screen the card is drawn on.</summary>
        static Vector2 OnScreen(Camera cam, Vector3 from, Vector3 to)
        {
            Vector3 d = to - from;
            return new Vector2(Vector3.Dot(d, cam.transform.right), Vector3.Dot(d, cam.transform.up));
        }

        /// <summary>Where the DRAWING's root and its tip are drawn in the world, through exactly the transform the
        /// shader uses: the unit card's point (uv - 0.5) is rotated by the roll, THEN scaled by (width, height), then
        /// placed at `at + right * cx + up * cy` (Flipbook_URP.shader, vert). The card is laid by FlipbookFx.LayAlong
        /// by the sheet's DECLARED ink, which is what FlipbookFx.Draw does; the root and tip handed back are the
        /// MEASURED ink's, so a declaration that does not match the drawing shows up as the drawing being off the
        /// mouth, which is precisely the fault the owner saw.</summary>
        static void DrawnInk(Camera cam, Vector3 pos, Vector3 axis, float width, float height,
                             Vector2 layRoot, Vector2 layPoint, Vector2 drawnRoot, Vector2 drawnPoint,
                             out Vector3 root, out Vector3 tip)
        {
            Vector3 right = cam.transform.right, up = cam.transform.up;
            // laid by the sheet's declaration, exactly as FlipbookFx.Draw lays a card of that book
            FlipbookFx.LayAlong(right, up, pos, axis, width, height, layRoot, layPoint, out Vector3 at, out float roll, out float w);
            float sr = Mathf.Sin(roll), cr = Mathf.Cos(roll);
            // and the drawing is where the PIXELS of the sheet sit inside that quad
            Vector2 p = drawnRoot - new Vector2(0.5f, 0.5f);
            root = Place(at, right, up, p, sr, cr, w, height);
            tip = Place(at, right, up, drawnPoint.sqrMagnitude > 1e-12f ? p + drawnPoint : p, sr, cr, w, height);
        }

        static Vector3 Place(Vector3 at, Vector3 right, Vector3 up, Vector2 p, float sr, float cr, float w, float h)
        {
            float cx = (p.x * cr - p.y * sr) * w, cy = (p.x * sr + p.y * cr) * h;
            return at + right * cx + up * cy;
        }
    }
}
