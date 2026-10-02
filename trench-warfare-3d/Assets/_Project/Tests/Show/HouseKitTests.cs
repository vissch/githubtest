// Phase: B5 (implemented) — the buildings and sliced kit props (HouseKit): what rests on what, the chunks drawn as one mesh a building, and a sliced prop put back where it stood
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class HouseKitTests
    {
        static HouseKit.Chunk C(float x0, float y0, float z0, float x1, float y1, float z1)
        {
            var b = new Bounds(); b.SetMinMax(new Vector3(x0, y0, z0), new Vector3(x1, y1, z1));
            return new HouseKit.Chunk { Local = b, Offset = new Vector3(b.center.x, b.min.y, b.center.z) };
        }

        [Test]
        public void A_Mask_Holds_Every_Chunk_A_House_May_Have_And_Each_Word_Survives_A_Float()
        {
            // The mask reaches the shader as a float4 and a float carries a whole number exactly only to 24 bits, so
            // the bits sit 24 to a component. This is the test that fails first if anyone widens a word: the high
            // chunks come back wrong, and in the game that reads as a distant house growing its roof back.
            Assert.AreEqual(96, HouseKit.MaxChunks, "four words of 24");
            var all = HouseKit.ChunkMask.Filled(HouseKit.MaxChunks);
            for (int i = 0; i < HouseKit.MaxChunks; i++) Assert.IsTrue(all.Has(i), "chunk " + i + " is in a full mask");
            var packed = all.Packed;
            foreach (var w in new[] { packed.x, packed.y, packed.z, packed.w })
            {
                Assert.AreEqual((1 << HouseKit.ChunkMask.BitsPerWord) - 1, (int)w, "a full word");
                Assert.AreEqual(w, (float)(int)w, "the word is a whole number a float keeps exactly");
                Assert.That(w, Is.LessThan(16777216f), "inside a float's 24-bit whole-number range");
            }

            // one chunk out of a full house, which is how a loose chunk is drawn
            var but = HouseKit.ChunkMask.Filled(HouseKit.MaxChunks);
            but.Clear(95);
            Assert.IsFalse(but.Has(95)); Assert.IsTrue(but.Has(94)); Assert.IsTrue(but.Has(0));
            Assert.AreNotEqual(all, but, "clearing the top chunk changes the mask the shader is given");

            // and a mask only ever says what it was told: no neighbour is set by setting one
            var one = default(HouseKit.ChunkMask);
            one.Set(72);
            Assert.IsTrue(one.Has(72));
            for (int i = 0; i < HouseKit.MaxChunks; i++) if (i != 72) Assert.IsFalse(one.Has(i), "only chunk 72, not " + i);
        }

        [Test]
        public void A_Wall_On_A_Wall_Rests_On_It_And_The_Roof_On_The_Wall_That_Reaches_It()
        {
            // two ground walls, an upper wall on the left one, a roof across both
            var house = new HouseKit.House { Chunks = new[]
            {
                C(-2f, 0f, -.2f, 0f, 2.4f, .2f),      // 0 ground left
                C(0f, 0f, -.2f, 2f, 2.4f, .2f),       // 1 ground right
                C(-2f, 2.45f, -.2f, 0f, 4.6f, .2f),   // 2 upper left (a seam's gap above 0)
                C(-2f, 4.5f, -1f, 2f, 5.5f, 1f),      // 3 roof over 1 and 2
            } };
            HouseKit.Solve(house);
            var c = house.Chunks;
            Assert.IsTrue(c[0].Grounded && c[1].Grounded);
            Assert.IsFalse(c[2].Grounded); CollectionAssert.AreEquivalent(new[] { 0 }, c[2].RestsOn);
            Assert.IsFalse(c[3].Grounded); CollectionAssert.AreEquivalent(new[] { 2 }, c[3].RestsOn);
            CollectionAssert.Contains(c[0].Carries, 2);
            CollectionAssert.Contains(c[2].Carries, 3);
        }

        [Test]
        public void A_Piece_With_Nothing_Within_Reach_Rests_On_The_Highest_Thing_Under_It()
        {
            var house = new HouseKit.House { Chunks = new[]
            {
                C(-1f, 0f, -1f, 1f, 1f, 1f),        // 0 low stub
                C(-1f, 0f, -1f, 1f, 2f, 1f),        // 1 taller stub
                C(-1f, 3.5f, -1f, 1f, 4f, 1f),      // 2 a beam 1.5 m over the taller
                C(5f, 3f, 5f, 6f, 4f, 6f),          // 3 over nothing at all
            } };
            HouseKit.Solve(house);
            CollectionAssert.AreEqual(new[] { 1 }, house.Chunks[2].RestsOn);
            Assert.IsTrue(house.Chunks[3].Grounded, "a piece over nothing stands, rather than falling for no reason");
        }

        [Test]
        public void A_Ruin_Is_Cut_Into_Courses_So_It_Comes_Down_From_The_Top()
        {
            // The whole point of cutting the ruins on their own floor lines (Tools/housesplit.py, TW_FLOORS=1). A blind
            // grid leaves chunks straddling two storeys, and HouseKit reads support from bounds alone, so a straddling
            // chunk is held by the storey below and the roof cannot come off until the ground floor does. What this
            // checks is the shape of the support graph, because that is what PropDestruction's Shaken/Settle cascade
            // walks: knock out a course and everything above it is orphaned and follows, a course at a time.
            var houses = HouseKit.Load("Ruins", _ => null, 0);
            Assert.AreEqual(4, houses.Length, "the ruined-building sheet has four buildings");
            foreach (var house in houses)
            {
                Assert.LessOrEqual(house.Chunks.Length, HouseKit.MaxChunks, house.Name + " fits a mask");
                Assert.Greater(house.Chunks.Length, 24, house.Name + " is cut past what the old 24-bit mask allowed");
                Assert.Greater(house.Bounds.size.y, 8f, house.Name + " is a building, not a hut");
                Assert.Less(house.Bounds.size.y, 12f, house.Name + " is a building, not a tower block");

                // how many courses deep the support chain runs: the longest path up from the ground
                var depth = new int[house.Chunks.Length];
                for (int pass = 0; pass < house.Chunks.Length; pass++)
                    for (int i = 0; i < house.Chunks.Length; i++)
                    {
                        if (house.Chunks[i].Grounded) { depth[i] = 0; continue; }
                        int d = 0;
                        foreach (int below in house.Chunks[i].RestsOn) d = Mathf.Max(d, depth[below] + 1);
                        depth[i] = d;
                    }
                int courses = 0, top = 0;
                for (int i = 0; i < depth.Length; i++)
                    if (depth[i] > courses) { courses = depth[i]; top = i; }

                Assert.GreaterOrEqual(courses + 1, 3, house.Name + " stands in at least three courses, so it can lose its top without losing its feet");
                // and the tallest chain really is the top of the building, not some stack off to one side
                Assert.Greater(house.Chunks[top].Local.min.y, house.Bounds.size.y * 0.45f,
                    house.Name + ": the deepest chain ends high up, which is what makes the roof go first");

                // nothing is held up by something level with it or above it, or a fall would never reach the ground
                foreach (var chunk in house.Chunks)
                {
                    if (!chunk.Grounded) Assert.Greater(chunk.RestsOn.Length, 0, chunk.Index + " in " + house.Name + " rests on something");
                    foreach (int below in chunk.RestsOn)
                        Assert.Less(house.Chunks[below].Local.min.y, chunk.Local.min.y, house.Name + ": a chunk rests only on lower ones, so a fall always ends");
                }

                // the ground course carries the building: knocking it out has to orphan everything above it
                bool anyGround = false;
                foreach (var chunk in house.Chunks) anyGround |= chunk.Grounded;
                Assert.IsTrue(anyGround, house.Name + " touches the ground");
            }
        }

        [Test]
        public void The_Imported_Houses_Stand_And_Every_Chunk_Is_Held_From_The_Ground()
        {
            var houses = HouseKit.Load(_ => null);
            Assert.AreEqual(6, houses.Length, "the owner's sheet has six houses");
            int chunks = 0;
            foreach (var house in houses)
            {
                Assert.Greater(house.Bounds.size.y, 3f, house.Name + " is drawn at house scale");
                Assert.Less(house.Bounds.size.y, 9f, house.Name + " is drawn at house scale");
                bool anyGround = false;
                foreach (var chunk in house.Chunks)
                {
                    chunks++;
                    anyGround |= chunk.Grounded;
                    Assert.LessOrEqual(Mathf.Max(chunk.Local.size.x, chunk.Local.size.y, chunk.Local.size.z), 4.0f, chunk.Index + " in " + house.Name + " is a chunk, not a wall (the cut is 2.4 m; merged slivers add to it)");
                    if (!chunk.Grounded) Assert.Greater(chunk.RestsOn.Length, 0, chunk.Index + " in " + house.Name + " rests on something");
                    foreach (int j in chunk.RestsOn) Assert.Less(house.Chunks[j].Local.min.y, chunk.Local.min.y, "a chunk rests only on lower ones, so a fall always ends");
                }
                Assert.IsTrue(anyGround, house.Name + " touches the ground");
            }
            Assert.Greater(chunks, 40);
            var rear = HouseKit.Load("Military", _ => null, 6);
            Assert.AreEqual(4, rear.Length, "the watchtower sheet has four buildings");
            Assert.AreEqual(6, rear[0].Index, "a second set is numbered on from the first");
            Assert.AreEqual(6, rear[0].Chunks[0].House);
        }

        [Test]
        public void A_Chunk_Is_Drawn_At_Its_Offset_In_A_Turned_House()
        {
            var chunk = C(1f, 2f, 0f, 2f, 3f, 1f);
            var house = Matrix4x4.TRS(new Vector3(10f, 1f, 20f), Quaternion.Euler(0f, 90f, 0f), Vector3.one);
            var drawn = HouseKit.Place(house, chunk);
            var expected = house.MultiplyPoint3x4(chunk.Offset);
            Assert.That(Vector3.Distance(drawn.GetPosition(), expected), Is.LessThan(1e-4f));
            Assert.That(Vector3.Distance(HouseKit.HouseOf(drawn, chunk).GetPosition(), house.GetPosition()), Is.LessThan(1e-4f));
        }

        [Test]
        public void Each_Building_Of_Each_Set_Draws_As_One_Mesh_With_Every_Vertex_Tagged_By_Its_Chunk()
        {
            foreach (var set in new[] { "Houses", "Military", "Siege", "Stones", "Weapons" })
            foreach (var house in HouseKit.Load(set, name => new BattlefieldKit.Module { Mesh = Resources.Load<Mesh>("Env/" + set + "/" + name) }, 0))
            {
                Assert.AreEqual(set, house.Set);
                Assert.LessOrEqual(house.Chunks.Length, HouseKit.MaxChunks, house.Name + " fits a mask");
                var whole = HouseKit.BuildWhole(house);
                Assert.IsNotNull(whole, house.Name + " combines (its chunks import readable)");
                int verts = 0; foreach (var c in house.Chunks) verts += c.Module.Mesh.vertexCount;
                Assert.AreEqual(verts, whole.vertexCount, house.Name + " keeps every chunk's vertices");
                Assert.AreEqual(1, whole.subMeshCount);
                var ids = new System.Collections.Generic.List<Vector2>(); whole.GetUVs(1, ids);
                Assert.AreEqual(verts, ids.Count);
                var smooth = new System.Collections.Generic.List<Vector3>(); whole.GetUVs(3, smooth);
                Assert.AreEqual(verts, smooth.Count, "the outline's smoothed normals come through the combine");
                // the chunks sit at their offsets: the whole mesh spans the house
                Assert.That(Vector3.Distance(whole.bounds.center, house.Bounds.center), Is.LessThan(0.05f), house.Name + " is put together where its chunks say");
                // ChunkMask.Set ignores an id it has no bit for, so the range is checked here rather than left to it
                var seen = default(HouseKit.ChunkMask);
                foreach (var id in ids)
                {
                    int c = Mathf.RoundToInt(id.x);
                    Assert.That(c, Is.InRange(0, house.Chunks.Length - 1), house.Name + ": chunk id " + c + " is one of its " + house.Chunks.Length);
                    seen.Set(c);
                }
                Assert.AreEqual(house.AllBits, seen, house.Name + ": every chunk has vertices");
                Object.DestroyImmediate(whole);
            }
        }

        [Test]
        public void A_Sliced_Kit_Prop_Puts_Every_Vertex_Of_The_Whole_Prop_Back_Where_It_Was()
        {
            // the chunks are the prop cut up, not moved: the cuts add vertices, but every original one is still there
            foreach (var (set, name) in new[] { ("Siege", "Well"), ("Stones", "WallStub"), ("Weapons", "Biplane"), ("Weapons", "FieldGun") })
            {
                var house = System.Array.Find(HouseKit.Load(set, chunk => new BattlefieldKit.Module { Mesh = Resources.Load<Mesh>("Env/" + set + "/" + chunk) }, 0), h => h.Name == name);
                Assert.IsNotNull(house, name + " is sliced into Resources/Env/" + set + "/Chunks");
                Assert.LessOrEqual(house.Chunks.Length, HouseKit.MaxChunks, name + " fits a mask");
                Assert.GreaterOrEqual(house.Chunks.Length, 6, name + " comes apart in pieces");
                var whole = HouseKit.BuildWhole(house);
                var original = Resources.Load<Mesh>("Env/" + set + "/" + name);
                var cut = whole.vertices; float worst = 0f;
                foreach (var v in original.vertices)
                {
                    float best = float.MaxValue;
                    foreach (var w in cut) best = Mathf.Min(best, (w - v).sqrMagnitude);
                    worst = Mathf.Max(worst, Mathf.Sqrt(best));
                }
                Assert.Less(worst, 0.005f, name + ": a vertex of the whole prop is " + worst + " m from any of its chunks' (turned or mirrored?)");
                Object.DestroyImmediate(whole);
            }
        }
    }
}
