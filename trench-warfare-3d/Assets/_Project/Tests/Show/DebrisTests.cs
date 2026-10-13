// Phase: B5 (implemented) — the arithmetic a thrown piece is drawn by. TW/Debris integrates every fragment on the GPU
// from a record written once; DebrisMath is the same arithmetic in C#, used at the throw to solve the landing and here
// to pin the contract down: a piece lands where and when the solve says, on sloped ground too, never passes below its
// rest height, comes to rest after one bounce, and the record is the 96 bytes the shader declares.
using System.Reflection;
using System.Runtime.InteropServices;
using NUnit.Framework;
using Unity.Collections;
using UnityEngine;
using TW.Perf;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class DebrisTests
    {
        const float Tolerance = 1e-3f;

        /// <summary>[X4] Throw's call to Ground converted a method group to a Func every call (CombatFx already caches
        /// the equivalent, groundAt). Fails on old code (one allocation for the delegate), passes once DebrisRenderer
        /// caches it in groundFn (set once in Awake).</summary>
        [Test]
        public void Throw_DoesNotAllocateTheGroundDelegate()
        {
            var go = new GameObject("DebrisRenderer");
            NativeArray<DebrisRenderer.Record> records = default;
            try
            {
                var dr = go.AddComponent<DebrisRenderer>();   // runs Awake (Instance)
                var t = typeof(DebrisRenderer);
                // groundFn is cached in Awake from the instance method Ground, which needs no Host here: set the same
                // shape by hand so the test does not depend on Awake's timing in an edit-mode test
                t.GetField("groundFn", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(dr, (System.Func<float, float, float>)((x, z) => 0f));
                records = new NativeArray<DebrisRenderer.Record>(8, Allocator.Persistent);
                t.GetField("records", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(dr, records);
                var poolType = t.GetNestedType("Pool", BindingFlags.NonPublic);
                var pool = System.Activator.CreateInstance(poolType, nonPublic: true);
                poolType.GetField("Start").SetValue(pool, 0);
                poolType.GetField("Capacity").SetValue(pool, 8);
                var pools = (System.Array)t.GetField("pools", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(dr);
                pools.SetValue(pool, (int)DebrisRenderer.Piece.Clod);
                t.GetField("<Ready>k__BackingField", BindingFlags.NonPublic | BindingFlags.Instance).SetValue(dr, true);

                var rng = new DebrisRng(Vector3.one, 3u);
                Assume.That(AllocProbe.Available, "no profiler Recorder in the editor: the probe cannot run");
                double perCall = AllocProbe.PerCall(() => dr.Throw(DebrisRenderer.Piece.Clod, Vector3.zero, Vector3.up, 1f, Color.white, ref rng), 2, 20);
                Assert.That(perCall, Is.EqualTo(0), "[X4] Throw allocated a Func<float,float,float> from the Ground method group every call");
            }
            finally
            {
                if (records.IsCreated) records.Dispose();
                Object.DestroyImmediate(go);
            }
        }

        [Test]
        public void TimeToHeight_IsTheFallOfAThrow()
        {
            // straight up at 9.8 m/s from the ground: back at the ground after 2 s
            Assert.AreEqual(2f, DebrisMath.TimeToHeight(0f, DebrisMath.Gravity, 0f), Tolerance);
            // dropped from 4.9 m with no speed: down in 1 s
            Assert.AreEqual(1f, DebrisMath.TimeToHeight(4.9f, 0f, 0f), Tolerance);
            // already at rest height and not going up: no flight
            Assert.AreEqual(0f, DebrisMath.TimeToHeight(0f, -1f, 0f));
            // below the rest height and going up too slowly to reach it: no flight rather than a negative time
            Assert.AreEqual(0f, DebrisMath.TimeToHeight(-10f, 1f, 0f));
        }

        [Test]
        public void Landing_OnFlatGround_MatchesTheClosedForm()
        {
            var p0 = new Vector3(10f, 2f, 10f); var v0 = new Vector3(3f, 6f, -2f);
            DebrisMath.Landing(p0, v0, 0.1f, (x, z) => 1f, out float t, out float y);
            Assert.AreEqual(1.1f, y, Tolerance, "rests half its thickness above the ground");
            Assert.AreEqual(DebrisMath.TimeToHeight(2f, 6f, 1.1f), t, Tolerance);
            var at = DebrisMath.PositionAt(p0, v0, t, y, t);
            Assert.AreEqual(y, at.y, Tolerance, "at landT the arc is on the rest height");
        }

        [Test]
        public void Landing_OnAStep_SolvesForTheGroundWhereItComesDown()
        {
            // ground rises by 3 m past x = 15: a piece thrown that way lands earlier and higher than the flat solve says
            var p0 = new Vector3(10f, 1f, 0f); var v0 = new Vector3(8f, 7f, 0f);
            System.Func<float, float, float> ground = (x, z) => x > 15f ? 3f : 0f;
            DebrisMath.Landing(p0, v0, 0f, ground, out float t, out float y);
            Assert.AreEqual(3f, y, Tolerance);
            float x = p0.x + v0.x * t;
            Assert.Greater(x, 15f, "it does come down on the high ground");
            Assert.AreEqual(y, DebrisMath.PositionAt(p0, v0, t, y, t).y, Tolerance);
            DebrisMath.Landing(p0, v0, 0f, (a, b) => 0f, out float flatT, out _);
            Assert.Less(t, flatT, "hitting the step cuts the flight short");
        }

        [Test]
        public void PositionAt_NeverGoesBelowTheRestHeight_AndComesToRest()
        {
            var p0 = new Vector3(0f, 1.5f, 0f); var v0 = new Vector3(4f, 5f, 1f);
            DebrisMath.Landing(p0, v0, 0.2f, (x, z) => 0f, out float landT, out float landY);
            float vy1 = v0.y - DebrisMath.Gravity * landT;
            float bounce = DebrisMath.BounceSeconds(vy1);
            Assert.Greater(bounce, 0f, "it lands with a fall, so it bounces");
            Vector3 last = default;
            for (float t = 0f; t < landT + bounce + 2f; t += 0.01f)
            {
                var at = DebrisMath.PositionAt(p0, v0, landT, landY, t);
                Assert.GreaterOrEqual(at.y, landY - Tolerance, $"below the ground at t = {t}");
                if (t > landT + bounce + 0.05f) Assert.AreEqual(last, at, "at rest it stays put");
                last = at;
            }
            Assert.AreEqual(landY, last.y, Tolerance);
            // the bounce carries it on a little, in the direction it was going
            Assert.Greater(last.x, p0.x + v0.x * landT);
        }

        [Test]
        public void PositionAt_IsContinuousAtTheLanding()
        {
            var p0 = new Vector3(3f, 2f, -1f); var v0 = new Vector3(-2f, 4f, 3f);
            DebrisMath.Landing(p0, v0, 0.1f, (x, z) => 0.5f, out float landT, out float landY);
            var before = DebrisMath.PositionAt(p0, v0, landT, landY, landT - 1e-4f);
            var after = DebrisMath.PositionAt(p0, v0, landT, landY, landT + 1e-4f);
            Assert.Less((before - after).magnitude, 0.01f);
        }

        [Test]
        public void Share_SpendsThePoolsUnderTheEye()
        {
            Assert.AreEqual(1f, DebrisMath.Share(0f));
            Assert.AreEqual(1f, DebrisMath.Share(54f));
            Assert.AreEqual(0.5f, DebrisMath.Share(80f));
            Assert.AreEqual(0.25f, DebrisMath.Share(200f));
        }

        [Test]
        public void Rng_IsSeededByPlace_AndStaysInRange()
        {
            var a = new DebrisRng(new Vector3(12.3f, 0.4f, 55.1f), 7u);
            var b = new DebrisRng(new Vector3(12.3f, 0.4f, 55.1f), 7u);
            var c = new DebrisRng(new Vector3(12.3f, 0.4f, 55.1f), 8u);
            bool differs = false;
            for (int k = 0; k < 64; k++)
            {
                float x = a.Next(), y = b.Next(), z = c.Next();
                Assert.AreEqual(x, y, "the same place and salt replay the same throw");
                Assert.GreaterOrEqual(x, 0f); Assert.Less(x, 1f);
                if (x != z) differs = true;
                var dir = a.OnSphere(); b.OnSphere(); c.OnSphere();
                Assert.AreEqual(1f, dir.magnitude, 1e-3f);
            }
            Assert.IsTrue(differs, "a different salt is a different throw");
        }

        [Test]
        public void Record_IsTheShaderStride_AndEveryPieceHasAPool()
        {
            Assert.AreEqual(DebrisRenderer.RecordBytes, Marshal.SizeOf<DebrisRenderer.Record>(), "TW/Debris declares a 96-byte record");
            int total = 0;
            for (int k = 0; k < (int)DebrisRenderer.Piece.Count; k++)
            {
                int n = DebrisRenderer.CapacityOf((DebrisRenderer.Piece)k);
                Assert.Greater(n, 0, ((DebrisRenderer.Piece)k) + " has a pool");
                total += n;
            }
            Assert.LessOrEqual(total * DebrisRenderer.RecordBytes, 512 * 1024, "the whole field's pieces fit in half a megabyte");
        }
    }
}
