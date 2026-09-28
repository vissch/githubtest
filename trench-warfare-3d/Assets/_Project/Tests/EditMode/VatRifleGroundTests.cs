// Phase: C2 (the troops' clip review, 2026-09-28) — VatRifleGround, which keeps the baked rifle out of the ground: every
// death and every prone, crawling and rolling clip stood a man's rifle 20-80 cm deep in the mud (Tools/vatcheck.py). A
// shallow dip only lifts and a deep one pitches the rifle about the grip, smoothly as the pose moves; a rifle already
// clear is untouched; a death drops the rifle to lie level on the ground without a jump on the way; the muzzle socket
// goes with the rifle. The grip moves between the hands without a jump (VATBaker.BlendGrip: hard switches popped the
// rifle up to 0.87 m in a frame). With a bake present, no baked muzzle is under the ground.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Editor;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class VatRifleGroundTests
    {
        const int R0 = 3;   // a few body vertices before the rifle, as in a figure

        /// <summary>One frame: the rifle box as VATBaker lays it (1.15 m, centre 0.30 m ahead of the grip, faces +x -x +y -y
        /// +z -z), gripped at `grip` and turned by `rot`; sockets muzzle, barrel, chest.</summary>
        static (Vector3[] p, Vector3[] n, Vector3[] s) Frame(Vector3 grip, Quaternion rot)
        {
            var p = new Vector3[R0 + VatRifleGround.Corners]; var n = new Vector3[p.Length];
            Vector3 centre = new Vector3(0f, 0f, 0.30f), size = new Vector3(0.06f, 0.10f, 1.15f);
            Vector3[] axes = { Vector3.right, Vector3.left, Vector3.up, Vector3.down, Vector3.forward, Vector3.back };
            int v = R0;
            foreach (var ax in axes)
            {
                Vector3 a = Mathf.Abs(ax.y) > 0.5f ? Vector3.right : Vector3.up, b = Vector3.Cross(ax, a);
                for (int k = 0; k < 4; k++)
                {
                    float sa = (k == 0 || k == 3) ? -1f : 1f, sb = k < 2 ? -1f : 1f;
                    p[v] = grip + rot * (centre + Vector3.Scale(ax + a * sa + b * sb, size * 0.5f)); n[v] = rot * ax; v++;
                }
            }
            for (int i = 0; i < R0; i++) p[i] = grip + Vector3.up * 0.5f;
            var s = new[] { grip + rot * new Vector3(0f, 0f, 0.875f), rot * Vector3.forward, grip + Vector3.up * 0.4f };
            return (p, n, s);
        }

        static float Low(Vector3[] p) { float l = float.MaxValue; for (int i = R0; i < p.Length; i++) l = Mathf.Min(l, p[i].y); return l; }
        static Vector3 MuzzleEnd(Vector3[] p) { var c = Vector3.zero; for (int i = 16; i < 20; i++) c += p[R0 + i]; return c / 4f; }

        [Test]
        public void ARiflePointedIntoTheGroundComesOutAboutItsGrip()
        {
            var grip = new Vector3(0f, 0.3f, 0f);
            var (p, n, s) = Frame(grip, Quaternion.Euler(50f, 0f, 0f));   // 50 degrees down: the muzzle end 0.4 m under
            Assert.Less(Low(p), -0.3f, "the case is real");
            VatRifleGround.Pitch(p, n, R0, s, Vector3.forward);
            Assert.GreaterOrEqual(Low(p), -1e-4f, "out of the ground");
            Assert.Greater(s[VatAsset.Barrel].y, -Mathf.Sin(50f * Mathf.Deg2Rad) + 0.1f, "raised about the grip, not only lifted");
            Assert.AreEqual(0f, Vector3.Distance(MuzzleEnd(p), s[VatAsset.Muzzle]), 0.02f, "the muzzle socket went with the rifle");
            for (int i = 0; i < R0; i++) Assert.AreEqual(grip + Vector3.up * 0.5f, p[i], "the body is not touched");
        }

        [Test]
        public void AShallowDipOnlyLifts()
        {
            var grip = new Vector3(0f, 0.25f, 0f);
            var (p, n, s) = Frame(grip, Quaternion.Euler(-80f, 0f, 0f));   // near upright, the butt a few cm under: a kneeling man's rifle
            float dip = -Low(p);
            Assert.That(dip, Is.InRange(0.005f, 0.05f), "the case is real");
            var barrel = s[VatAsset.Barrel];
            VatRifleGround.Pitch(p, n, R0, s, Vector3.forward);
            Assert.AreEqual(0f, Low(p), 1e-4f, "rests on the ground");
            Assert.AreEqual(0f, Vector3.Angle(barrel, s[VatAsset.Barrel]), 0.01f, "not turned");
        }

        [Test]
        public void ARifleClearOfTheGroundIsUntouched()
        {
            var (p, n, s) = Frame(new Vector3(0f, 1.2f, 0f), Quaternion.Euler(10f, 20f, 0f));
            var before = (Vector3[])p.Clone(); var muzzle = s[VatAsset.Muzzle];
            VatRifleGround.Pitch(p, n, R0, s, Vector3.forward);
            for (int i = 0; i < p.Length; i++) Assert.AreEqual(before[i], p[i]);
            Assert.AreEqual(muzzle, s[VatAsset.Muzzle]);
        }

        [Test]
        public void ThePitchMovesSmoothlyAsThePoseDoes()
        {
            // a man going down with the rifle: it dips a degree a frame from level to 80 degrees down; the drawn muzzle may not jump
            var grip = new Vector3(0f, 0.25f, 0f);
            Vector3 last = default;
            for (int deg = 0; deg <= 80; deg++)
            {
                var (p, n, s) = Frame(grip, Quaternion.Euler(deg, 0f, 0f));
                VatRifleGround.Pitch(p, n, R0, s, Vector3.forward);
                Assert.GreaterOrEqual(Low(p), -1e-4f, "frame " + deg);
                if (deg > 0) Assert.Less(Vector3.Distance(last, s[VatAsset.Muzzle]), 0.05f, "frame " + deg);
                last = s[VatAsset.Muzzle];
            }
        }

        [Test]
        public void ADeathDropsTheRifleToLieLevelWithoutAJump()
        {
            // the source: he falls on his face, the rifle driven 0.6 m into the ground and spinning about the vertical at the end
            var frames = new List<Vector3[]>(); var normals = new List<Vector3[]>(); var sockets = new List<Vector3[]>();
            const int count = 40;
            for (int f = 0; f < count; f++)
            {
                float u = f / (count - 1f);
                var grip = new Vector3(0f, Mathf.Lerp(1.1f, 0.1f, u), u * 0.8f);
                var (p, n, s) = Frame(grip, Quaternion.Euler(Mathf.Lerp(0f, 80f, u), u * 120f, 0f));
                frames.Add(p); normals.Add(n); sockets.Add(s);
            }
            Assert.Less(Low(frames[count - 1]), -0.4f, "the case is real");
            var start = (Vector3[])frames[0].Clone();
            var sourceMuzzle = new Vector3[count]; for (int f = 0; f < count; f++) sourceMuzzle[f] = sockets[f][VatAsset.Muzzle];
            VatRifleGround.Settle(frames, normals, sockets, 0, count, R0, death: true);
            for (int f = 0; f < count; f++)
            {
                Assert.GreaterOrEqual(Low(frames[f]), -1e-4f, "frame " + f);
                if (f > 0)
                {
                    float moved = Vector3.Distance(sockets[f][VatAsset.Muzzle], sockets[f - 1][VatAsset.Muzzle]);
                    float source = Vector3.Distance(sourceMuzzle[f], sourceMuzzle[f - 1]);
                    Assert.Less(moved, source + 0.1f, "no jump the source did not have, frame " + f);
                }
            }
            Assert.AreEqual(0f, sockets[count - 1][VatAsset.Barrel].y, 0.01f, "it ends lying level");
            Assert.Less(Low(frames[count - 1]), 0.02f, "on the ground, not above it");
            Assert.AreEqual(0f, Vector3.Distance(MuzzleEnd(frames[count - 1]), sockets[count - 1][VatAsset.Muzzle]), 0.02f, "the muzzle socket went with it");
            for (int i = 0; i < start.Length; i++) Assert.AreEqual(0f, Vector3.Distance(start[i], frames[0][i]), 1e-4f, "still in his hands at the start");
        }

        [Test]
        public void TheGripMovesBetweenTheHandsWithoutAJump()
        {
            var right = Matrix4x4.TRS(new Vector3(0.1f, 1.2f, 0.3f), Quaternion.Euler(0f, 10f, 0f), Vector3.one);
            var left = Matrix4x4.TRS(new Vector3(-0.4f, 1.5f, 0.1f), Quaternion.Euler(70f, -60f, 20f), Vector3.one);
            Assert.AreEqual(right, VATBaker.BlendGrip(right, left, 0f));
            Assert.AreEqual(left, VATBaker.BlendGrip(right, left, 1f));
            Vector3 last = right.MultiplyPoint3x4(new Vector3(0f, 0f, 0.875f));
            for (int k = 1; k <= 20; k++)
            {
                var g = VATBaker.BlendGrip(right, left, k / 20f);
                Assert.AreEqual(1f, g.determinant, 1e-4f, "rigid: the rifle keeps its shape part way");
                Vector3 muzzle = g.MultiplyPoint3x4(new Vector3(0f, 0f, 0.875f));
                Assert.Less(Vector3.Distance(last, muzzle), 0.15f, "a twentieth of the way moves the muzzle a little, step " + k);
                last = muzzle;
            }
        }

        [TestCase("Soldier")]
        [TestCase("Sniper")]
        public void NoBakedMuzzleIsUnderTheGround(string figure)
        {
            var data = Resources.Load<VatAssetData>("Units/Figure" + figure);
            if (data == null || !data.Valid) Assert.Ignore("no bake of " + figure);
            var asset = data.ToAsset();   // the mesh stays the Resources asset's (OwnsMesh false)
            try
            {
                if (asset.Sockets == null) Assert.Ignore("an atlas without sockets");
                float low = float.MaxValue; int at = -1;
                for (int f = 0; f < asset.TotalFrames; f++)
                {
                    float y = asset.Sockets[f * asset.SocketsPerFrame + VatAsset.Muzzle].y;
                    if (y < low) { low = y; at = f; }
                }
                Assert.GreaterOrEqual(low, -0.01f, figure + ": the muzzle " + Mathf.RoundToInt(-low * 100f) + " cm in the ground at atlas frame " + at);
            }
            finally { asset.Release(); }
        }
    }
}
