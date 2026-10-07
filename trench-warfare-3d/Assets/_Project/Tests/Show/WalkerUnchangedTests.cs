// Phase: A5c (2026-10-07, the Banner's re-cut) — the leg code learned to hold a foot (WalkerGait.Solve, TankModel.HeldFeet)
// for the one walker cut to the standard, and every other walker must walk exactly as it did. These are the sums of two
// walks of each of them (every foot, the body's ride and tilt, every solved part's matrix, tick by tick), recorded
// from the code as it was the day before the change. A sum that moves is a walker that walks differently: if that is
// meant, record it again with what was seen on film; if it is not, the change leaked.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class WalkerUnchangedTests
    {
        // (model, archetype, root part, scale, then the recorded sums: ride, feet, parts for the flat walk and for the
        // turning walk on a slope with a leg lost)
        static readonly (string name, byte archetype, string root, float scale, double[] sums)[] Others =
        {
            ("Pincer", VehicleArchetype.Pincer, "Body", VehicleSize.Walker, new double[] { -164.40025472640991, 407341.25906741474, 25994.633529398609, 657.882347260369, 382182.56200966926, 25081.942529075881 }),
            ("Kettle", VehicleArchetype.Kettle, "Body", VehicleSize.Walker, new double[] { 188.28250765800476, 199834.1468753106, 25087.710774108047, 998.50677515883581, 187438.25457045835, 16270.124439312827 }),
            ("Censer", VehicleArchetype.Censer, "Body", VehicleSize.Walker, new double[] { -97.2072958946228, 197014.0437695444, 17384.069950718105, 678.26185631945555, 182298.79012455765, 15223.990588995051 }),
            ("Pavise", VehicleArchetype.Pavise, "Body", VehicleSize.Walker, new double[] { -103.1882643699646, 195368.41093394579, 27045.211311743264, 647.43527054910373, 180164.95268752371, 23930.035606487785 }),
            ("Redoubt", VehicleArchetype.Redoubt, "Body", VehicleSize.Walker, new double[] { -139.03507590293884, 193162.81531432408, 31039.780812851128, 721.07928962214646, 183108.81594177449, 24633.109622364518 }),
            ("Croaker", VehicleArchetype.Croaker, "Hull", 1f, new double[] { 6.158832460641861, 58941.239806711732, 8803.6169522428045, 909.823678699322, 56262.745074033752, 2117.9419546969211 }),
        };

        /// <summary>One walk, summed: how the body rode, where every foot was, and what Solve made of every leg part
        /// at both levels of detail.</summary>
        static void Walk(TankModel m, System.Func<float, float, float> ground, float speed, float yawRate, byte lost, float seconds,
                         out double ride, out double feet, out double parts)
        {
            ride = feet = parts = 0.0;
            var gait = new WalkerGait();
            const float dt = 1f / 60f;
            Vector3 pos = new Vector3(20f, 0f, 20f);
            float yaw = 0.3f;
            gait.Plant(m, pos, yaw, ground);
            var local = new Matrix4x4[64];
            var solved = new bool[64];
            for (float t = 0f; t < seconds; t += dt)
            {
                yaw += yawRate * dt;
                Vector3 vel = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw)) * speed;
                pos += vel * dt;
                gait.Step(m, pos, yaw, vel, yawRate, lost, false, dt, ground);
                ride += gait.Height + gait.Pitch * 3.0 + gait.Roll * 5.0;
                for (int i = 0; i < gait.Feet.Length; i++)
                    feet += (gait.Feet[i].At.x * 1.1 + gait.Feet[i].At.y * 1.3 + gait.Feet[i].At.z * 1.7) * (i + 1);
                var root = Matrix4x4.TRS(new Vector3(pos.x, gait.Height, pos.z), gait.Carriage(yaw), Vector3.one);
                for (int lod = 0; lod < 2; lod++)
                {
                    var l = m.Lods[lod];
                    if (l == null || l.Legs == null || (lod == 1 && l == m.Lods[0])) continue;
                    gait.Solve(l, root, local, solved);
                    for (int p = 0; p < l.Parts.Count; p++)
                    {
                        if (!solved[p]) continue;
                        for (int k = 0; k < 16; k++) parts += local[p][k] * (k + 1) * 0.01 * (p + 1) * (lod + 1);
                    }
                }
            }
        }

        [Test]
        public void EveryOtherWalkerWalksExactlyAsItDid()
        {
            var report = new System.Text.StringBuilder();
            bool same = true;
            foreach (var (name, archetype, root, scale, sums) in Others)
            {
                var m = TankModel.Load(name, archetype, root, scale);
                Assert.IsNotNull(m, name);
                var got = new double[6];
                Walk(m, (x, z) => 0f, 2.0f, 0f, 0, 5f, out got[0], out got[1], out got[2]);
                Walk(m, (x, z) => z * 0.12f, 1.2f, 0.4f, 2, 5f, out got[3], out got[4], out got[5]);
                report.Append($"\n(\"{name}\": ");
                for (int k = 0; k < 6; k++)
                {
                    report.Append(got[k].ToString("R", System.Globalization.CultureInfo.InvariantCulture)).Append(k < 5 ? ", " : ")");
                    if (System.Math.Abs(got[k] - sums[k]) > 1e-4 * System.Math.Max(1.0, System.Math.Abs(sums[k]))) same = false;
                }
                foreach (var rig in m.Lods[0].Legs)
                    if (rig != null) Assert.IsFalse(rig.Held, $"{name}: only a walker cut to the standard holds its feet");
            }
            Assert.IsTrue(same, "a walker that was not re-cut walks differently. The sums now:" + report);
        }

        [Test]
        public void OnlyTheWalkersCutToTheStandardHoldTheirFeet()
        {
            CollectionAssert.AreEquivalent(new[] { "Banner" }, TankModel.HeldFeet);
        }
    }
}
