// Phase: B7 (docs/21 phase 2) — the scatter rules on a hand-built field: no grass on a trench floor or a beaten path;
// denser against a post; flowers only in grass, and few; camp items only in the trenches, the dugouts and the rear;
// winter grows frost, not grass; the same seed lays the same field and another seed another; the caps hold.
// No kit, no map, no scene: the input is filled by hand and the fields and layers are plain arithmetic.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Terrain;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class ScatterRulesTests
    {
        const int N = 24;   // a 48 x 48 m field

        /// <summary>Two trench rows with ladders, the way the map lays them: side A near, side B far.</summary>
        static ScatterInput Field(uint seed = 1917)
        {
            var input = ScatterInput.Blank(N, N, seed);
            input.TrenchRow(4, new[] { 6, 18 }, sideA: true);
            input.TrenchRow(19, new[] { 6, 18 }, sideA: false);
            return input;
        }

        static ScatterField Grown(ScatterInput input)
        {
            var field = ScatterField.Build(input);
            for (int i = 0; i < field.Patch.Length; i++) field.Patch[i] = 1f;   // grass wants to grow everywhere: the other rules decide
            return field;
        }

        static List<ScatterInstance> Laid(ScatterInput input, ScatterField field)
        {
            var list = new List<ScatterInstance>();
            ScatterLayers.Place(input, field, list);
            return list;
        }

        static int CellOf(ScatterInput input, in ScatterInstance s) => input.Index(Mathf.Clamp((int)(s.X / ScatterInput.Cell), 0, input.W - 1), Mathf.Clamp((int)(s.Z / ScatterInput.Cell), 0, input.L - 1));

        [Test]
        public void NoGrassOnATrenchFloorALadderOrABeatenPath()
        {
            var input = Field();
            var field = Grown(input);
            var laid = Laid(input, field);
            int grass = 0;
            foreach (var s in laid)
            {
                if (s.Kind != ScatterKind.Grass) continue;
                grass++;
                int cell = CellOf(input, s);
                Assert.IsFalse(input.Is(cell, NavLayer.Trench | NavLayer.Link), $"grass on a trench cell at {s.X:0.0}, {s.Z:0.0}");
                Assert.LessOrEqual(field.Traffic[cell], ScatterLayers.TrafficBare, $"grass on a beaten path at {s.X:0.0}, {s.Z:0.0}");
            }
            Assert.Greater(grass, 200, "the open ground between the lines is grassed");
            // the corridor between the ladders at x = 6: bare down its middle line, grassed six metres off it
            for (int z = 6; z < 18; z++)
            {
                Assert.Greater(field.Traffic[input.Index(6, z)], ScatterLayers.TrafficBare, $"the corridor at row {z} is walked bare");
                Assert.AreEqual(0f, ScatterLayers.GrassDensity(field, input.Index(6, z)));
            }
            Assert.Less(field.Traffic[input.Index(12, 12)], 0.2f, "the middle of the field, off the corridors and the ladders, is not");
            Assert.Greater(ScatterLayers.GrassDensity(field, input.Index(12, 12)), 0.5f);
            Assert.AreEqual(1f, field.Traffic[input.Index(6, 4)], "a ladder is walked to nothing");
            Assert.AreEqual(1f, field.Traffic[input.Index(3, 19)], "and so is a trench floor");
        }

        [Test]
        public void TheCorridorBendsThroughTheWireGap()
        {
            var input = Field();
            input.WireRow(11, 9); input.WireRow(12, 9);   // a two-row belt across the field, one gap at x = 9
            var way = new List<Vector2Int>();
            ScatterField.Route(input, new Vector2Int(6, 4), new Vector2Int(6, 19), way);
            Assert.AreEqual(3, way.Count, "ladder, the gap, ladder");
            Assert.AreEqual(new Vector2Int(9, 11), way[1], "the waypoint sits in the gap, in the belt's middle row");
            var field = Grown(input);
            Assert.Greater(field.Traffic[input.Index(9, 11)], ScatterLayers.TrafficBare, "the men file through the gap");
            Assert.Less(field.Traffic[input.Index(6, 11)], 0.2f, "and not through the wire on the straight line");
            Assert.Greater(field.Traffic[input.Index(9, 12)], 0.4f, "and on through the belt's second row");
            // no gap within reach: the corridor crosses the wire where the straight line does
            var cut = Field();
            cut.WireRow(12, 200);
            ScatterField.Route(cut, new Vector2Int(6, 4), new Vector2Int(6, 19), way);
            Assert.AreEqual(new Vector2Int(6, 12), way[1]);
            // no wire at all: straight
            ScatterField.Route(Field(), new Vector2Int(6, 4), new Vector2Int(18, 19), way);
            Assert.AreEqual(2, way.Count);
            // wire beside the line but not on it is no belt of this corridor: straight
            var beside = Field();
            for (int x = 15; x < 24; x++) beside.Nav[beside.Index(x, 11)] |= (byte)NavLayer.Wire;
            ScatterField.Route(beside, new Vector2Int(6, 4), new Vector2Int(6, 19), way);
            Assert.AreEqual(2, way.Count, "a belt that ends nine cells off the corridor is not crossed");
            // a breach two rows deep on the corridor's own rows is a gap like any other, and the nearest one wins
            var breached = Field();
            breached.WireRow(11, 7, 2); breached.WireRow(12, 7, 2);
            ScatterField.Route(breached, new Vector2Int(6, 4), new Vector2Int(6, 19), way);
            Assert.AreEqual(new Vector2Int(7, 11), way[1], "through the hole a cell off the line");
            // two belts: the second is searched along the line from the first waypoint, so the path never bends back
            var two = Field();
            two.WireRow(8, 9); two.WireRow(14, 9);
            ScatterField.Route(two, new Vector2Int(6, 4), new Vector2Int(6, 19), way);
            Assert.AreEqual(4, way.Count);
            Assert.AreEqual(new Vector2Int(9, 8), way[1]); Assert.AreEqual(new Vector2Int(9, 14), way[2], "the second gap is straight ahead of the first");
            // a short belt beside the line, under the bend to the first gap: the second pass finds it on the painted segment
            var under = Field();
            under.WireRow(11, 9); under.WireRow(12, 9);
            for (int x = 9; x < 14; x++) under.Nav[under.Index(x, 8)] |= (byte)NavLayer.Wire;
            ScatterField.Route(under, new Vector2Int(6, 4), new Vector2Int(6, 19), way);
            Assert.AreEqual(4, way.Count, "the bend to the gap at (9, 11) crosses the short belt at row 8: a waypoint of its own");
            Assert.AreEqual(new Vector2Int(8, 8), way[1], "through the clear column beside the short belt, not its wire");
            Assert.AreEqual(new Vector2Int(9, 11), way[2]);
        }

        [Test]
        public void GrassGathersAgainstAPost()
        {
            var input = ScatterInput.Blank(N, N, 7);
            input.Obstructions.Add(new ScatterInput.Obstruction { X = 25f, Z = 25f, Radius = 0.1f, Height = 1.5f });
            var field = Grown(input);
            int at = input.Index(12, 12), off = input.Index(12, 15);   // the post's cell, and six metres away
            Assert.GreaterOrEqual(ScatterLayers.GrassDensity(field, at) / ScatterLayers.GrassDensity(field, off), 1.5f, "denser within 2 m of the post than 6 m off it");
            var laid = Laid(input, field);
            int near = 0, far = 0;
            foreach (var s in laid)
            {
                if (s.Kind != ScatterKind.Grass) continue;
                int c = CellOf(input, s);
                if (c == at) near++; else if (c == off) far++;
            }
            Assert.GreaterOrEqual(near, far * 1.5f, $"{near} tufts against the post, {far} six metres off");
        }

        [Test]
        public void FlowersOnlyWhereThereIsGrassAndFew()
        {
            var input = Field();
            var field = Grown(input);
            var laid = Laid(input, field);
            var grassCells = new HashSet<int>();
            int grass = 0, flowers = 0;
            foreach (var s in laid) if (s.Kind == ScatterKind.Grass) { grass++; grassCells.Add(CellOf(input, s)); }
            foreach (var s in laid)
            {
                if (s.Kind != ScatterKind.Flower) continue;
                flowers++;
                Assert.IsTrue(grassCells.Contains(CellOf(input, s)), $"a flower with no grass round it at {s.X:0.0}, {s.Z:0.0}");
            }
            Assert.Greater(flowers, 0, "some poppies");
            Assert.LessOrEqual(flowers, grass * ScatterLayers.FlowerShare + grassCells.Count, "a child layer of the grass, not a carpet");
        }

        [Test]
        public void CampItemsOnlyInTrenchesDugoutsAndTheRear()
        {
            var input = Field();
            input.Occupied.Add(new ScatterInput.Footprint { X = 25f, Z = 25f, HalfX = 2f, HalfZ = 2f, Yaw = 30f, Dugout = true });
            input.RearA = 3f;   // z below three metres is side A's rear
            var field = Grown(input);
            var laid = Laid(input, field);
            int kit = 0, crates = 0, rear = 0, inDugout = 0;
            foreach (var s in laid)
            {
                bool camp = s.Kind == ScatterKind.CampKit || s.Kind == ScatterKind.Crate || s.Kind == ScatterKind.Lantern || s.Kind == ScatterKind.WallDebris || s.Kind == ScatterKind.ShellStack;
                if (!camp) continue;
                if (s.Kind == ScatterKind.CampKit) kit++;
                if (s.Kind == ScatterKind.Crate) crates++;
                int cell = CellOf(input, s);
                bool trench = input.Is(cell, NavLayer.Trench) && !input.Is(cell, NavLayer.Link);
                bool dugout = input.Taken(s.X, s.Z, out bool isDugout) && isDugout;
                bool behind = input.InRear(s.Z);
                if (dugout) inDugout++;
                if (behind && !trench) rear++;
                Assert.IsTrue(trench || dugout || behind, $"{s.Kind} out in the open at {s.X:0.0}, {s.Z:0.0}");
            }
            Assert.Greater(kit, 0, "tins and kit along the trench walls");
            Assert.Greater(crates, 0, "a crate somewhere");
            Assert.GreaterOrEqual(inDugout, 2, "the dugout is stocked");
            foreach (var s in laid)
                if (s.Kind == ScatterKind.Grass || s.Kind == ScatterKind.Flower)
                {
                    int cell = CellOf(input, s);
                    int cx = cell % input.W, cz = cell / input.W;
                    Assert.IsFalse(input.Taken(ScatterInput.CentreX(cx), ScatterInput.CentreZ(cz), out _), "nothing grows through a shelter's floor");
                }
        }

        [Test]
        public void WinterGrowsFrostNotGrass()
        {
            var input = Field();
            input.Frozen = true;
            var laid = Laid(input, Grown(input));
            int frost = 0;
            foreach (var s in laid)
            {
                Assert.AreNotEqual(ScatterKind.Grass, s.Kind); Assert.AreNotEqual(ScatterKind.Flower, s.Kind); Assert.AreNotEqual(ScatterKind.GrassAccent, s.Kind);
                if (s.Kind == ScatterKind.FrostTuft) frost++;
            }
            Assert.Greater(frost, 100, "frozen stalks where the grass would have been");
        }

        [Test]
        public void TheSameSeedLaysTheSameFieldAndAnotherSeedAnother()
        {
            var a = Laid(Field(1917), ScatterField.Build(Field(1917)));
            var b = Laid(Field(1917), ScatterField.Build(Field(1917)));
            Assert.AreEqual(a.Count, b.Count);
            for (int i = 0; i < a.Count; i++)
            {
                Assert.AreEqual(a[i].Kind, b[i].Kind); Assert.AreEqual(a[i].X, b[i].X); Assert.AreEqual(a[i].Z, b[i].Z); Assert.AreEqual(a[i].Yaw, b[i].Yaw); Assert.AreEqual(a[i].Scale, b[i].Scale);
            }
            var c = Laid(Field(1918), ScatterField.Build(Field(1918)));
            bool same = a.Count == c.Count;
            for (int i = 0; same && i < a.Count; i++) same = a[i].Kind == c[i].Kind && a[i].X == c[i].X && a[i].Z == c[i].Z;
            Assert.IsFalse(same, "another seed is another field");
        }

        [Test]
        public void TheCapsHold()
        {
            var input = ScatterInput.Blank(150, 240, 3);   // a 300 x 480 m field, grass everywhere
            var field = Grown(input);
            var laid = Laid(input, field);
            int grass = 0, accents = 0, flowers = 0;
            foreach (var s in laid)
            {
                if (s.Kind == ScatterKind.Grass) grass++;
                else if (s.Kind == ScatterKind.GrassAccent) accents++;
                else if (s.Kind == ScatterKind.Flower) flowers++;
            }
            Assert.AreEqual(ScatterLayers.MaxGrass, grass, "the grass fills its budget and no more");
            Assert.LessOrEqual(accents, ScatterLayers.MaxAccent);
            Assert.LessOrEqual(flowers, ScatterLayers.MaxFlowers);
        }
    }
}
