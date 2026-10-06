// Phase: deaths (2026-09-28, implemented) — the parts a man loses are cut from his own figure (DebrisRenderer.Figure):
// every part but the pack cuts from the baked soldier; a limb, a chest, hips and a half lie along +Z as the debris shader
// rests them, a boot and a helmet stand; the rifle is in none of them (it flies as its own piece); the uniform carries
// the cloth mask the side's tint dyes and the skin does not; the helmet is the helmet and the head is bare; every cut
// is capped (a wound, a boot's charred top). Needs the
// baked figure (Resources/Units/FigureSoldier), so it runs in the editor, not offline.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using Piece = TW.Presentation.Tactical.DebrisRenderer.Piece;

namespace TW.Tests
{
    public class FigurePartsTests
    {
        static (List<Vector3> v, List<int> t, List<Color> c) Cut(Piece piece)
        {
            var v = new List<Vector3>(); var t = new List<int>(); var c = new List<Color>();
            Assert.IsTrue(DebrisRenderer.FigurePart(piece, v, t, c), $"{piece} cuts from the soldier");
            return (v, t, c);
        }

        static Bounds BoundsOf(List<Vector3> v)
        {
            var b = new Bounds(v[0], Vector3.zero);
            foreach (var p in v) b.Encapsulate(p);
            return b;
        }

        static bool Near(Color a, Color b) => Mathf.Abs(a.r - b.r) < 0.02f && Mathf.Abs(a.g - b.g) < 0.02f && Mathf.Abs(a.b - b.b) < 0.02f;

        /// <summary>Every part written as an OBJ (with its vertex colours) to TW_STILLS_DIR, for a look outside the game.</summary>
        [Test, Explicit("Writes files: run by name.")]
        public void WritesThePartsForALook()
        {
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "tw-parts");
            System.IO.Directory.CreateDirectory(dir);
            foreach (var piece in new[] { Piece.Head, Piece.Helm, Piece.Torso, Piece.Pelvis, Piece.Arm, Piece.Leg, Piece.Boot, Piece.UpperHalf, Piece.LowerHalf })
            {
                var (v, t, c) = Cut(piece);
                var sb = new System.Text.StringBuilder();
                var inv = System.Globalization.CultureInfo.InvariantCulture;
                for (int i = 0; i < v.Count; i++) sb.AppendLine(string.Format(inv, "v {0} {1} {2} {3} {4} {5}", v[i].x, v[i].y, v[i].z, c[i].r, c[i].g, c[i].b));
                for (int k = 0; k < t.Count; k += 3) sb.AppendLine($"f {t[k] + 1} {t[k + 1] + 1} {t[k + 2] + 1}");
                System.IO.File.WriteAllText(System.IO.Path.Combine(dir, piece + ".obj"), sb.ToString());
            }
            TestContext.Out.WriteLine("parts written to " + dir);
        }

        [Test]
        public void EveryPartOfAManCutsFromHisFigure()
        {
            foreach (var piece in new[] { Piece.Head, Piece.Helm, Piece.Torso, Piece.Pelvis, Piece.Arm, Piece.Leg, Piece.Boot, Piece.UpperHalf, Piece.LowerHalf })
            {
                var (v, t, c) = Cut(piece);
                Assert.AreEqual(0, t.Count % 3, piece.ToString());
                Assert.AreEqual(v.Count, c.Count, piece.ToString());
                Assert.GreaterOrEqual(t.Count / 3, 4, $"{piece}: a part, not a sliver");
                foreach (int i in t) Assert.Less(i, v.Count);
            }
            var none = new List<Vector3>(); var nt = new List<int>(); var nc = new List<Color>();
            Assert.IsFalse(DebrisRenderer.FigurePart(Piece.Pack, none, nt, nc), "he carries no pack to cut: the stand-in");
            Assert.AreEqual(0, none.Count + nt.Count + nc.Count, "and nothing is left behind by a part that did not cut");
        }

        [Test]
        public void ALimbLiesAlongZAndABootStands()
        {
            foreach (var piece in new[] { Piece.Arm, Piece.Leg, Piece.UpperHalf, Piece.LowerHalf })
            {
                var b = BoundsOf(Cut(piece).v);
                Assert.Greater(b.size.z, b.size.y, $"{piece} lies along +Z (the shader rests a piece yaw-only)");
            }
            var boot = BoundsOf(Cut(Piece.Boot).v);
            var leg = BoundsOf(Cut(Piece.Leg).v);
            Assert.Less(boot.size.y, leg.size.z * 0.4f, "a boot is the foot of the leg, not the leg");
        }

        [Test]
        public void TheRifleIsInNoPartAndTheHelmetOnlyInItsOwn()
        {
            foreach (var piece in new[] { Piece.Head, Piece.Torso, Piece.Pelvis, Piece.Arm, Piece.Leg, Piece.Boot, Piece.UpperHalf, Piece.LowerHalf })
                foreach (var col in Cut(piece).c) Assert.IsFalse(Near(col, new Color(0.27f, 0.18f, 0.11f)) && col.a < 0.5f, $"{piece} carries the rifle");
            int helmetOnHead = 0, head = 0;
            foreach (var col in Cut(Piece.Head).c) { head++; if (DebrisRenderer.IsHelmet(col)) helmetOnHead++; }
            Assert.Less(helmetOnHead, head / 4, "the head comes off without its helmet (a stray olive vertex on a seam aside)");
            int painted = 0, helm = 0;
            foreach (var col in Cut(Piece.Helm).c) { helm++; if (DebrisRenderer.IsHelmet(col) || Near(col, DebrisRenderer.SteelInside)) painted++; }
            Assert.Greater(painted, helm * 3 / 4, "the helmet is the helmet (its paint and its dark underside; a seam vertex or two of the hood aside)");
        }

        [Test]
        public void ANeckIsCappedWithAWoundAndABootWithCharredLeather()
        {
            // a small round hole is capped; a wide ragged one (a waist, a shoulder) is left for the shader to draw its
            // inside as flesh (Debris_URP): fanned to one middle, the first renders showed spikes
            var head = Cut(Piece.Head).c;
            Assert.IsTrue(head.Exists(col => Near(col, DebrisRenderer.Wound)), "the neck is capped, wound red");
            Assert.IsTrue(head.Exists(col => Near(col, DebrisRenderer.Bone)), "with the bone in the middle");
            Assert.IsFalse(Cut(Piece.Boot).c.Exists(col => Near(col, DebrisRenderer.Wound)), "a boot's top is charred, not a wound: it flies at any gore");
        }

        [Test]
        public void TheUniformTakesTheSideAndTheSkinDoesNot()
        {
            int cloth = 0, bare = 0;
            foreach (var col in Cut(Piece.Torso).c) { if (col.a > 0.5f) cloth++; else bare++; }
            Assert.Greater(cloth, 0, "a chest is uniform: the side's tint dyes it (vertex alpha 1)");
            int headCloth = 0;
            foreach (var col in Cut(Piece.Head).c) if (col.a > 0.5f) headCloth++;
            Assert.AreEqual(0, headCloth, "a head keeps its own colour");
        }
    }
}
