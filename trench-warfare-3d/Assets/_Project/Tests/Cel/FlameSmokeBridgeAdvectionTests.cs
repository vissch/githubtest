// Phase: CelFx bridge entrainment record/owner contracts (2026-11-17).
using System;
using System.Reflection;
using System.Runtime.InteropServices;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class FlameSmokeBridgeAdvectionTests
    {
        static byte[] Bytes(CelRecord r)
        {
            IntPtr p = Marshal.AllocHGlobal(CelRecord.Bytes);
            try { Marshal.StructureToPtr(r, p, false); var b = new byte[CelRecord.Bytes]; Marshal.Copy(p, b, 0, b.Length); return b; }
            finally { Marshal.FreeHGlobal(p); }
        }

        [TestCase(FxTier.High, 1f, 0f, 8)]
        [TestCase(FxTier.Epic, 1.7f, .37f, 10)]
        [TestCase(FxTier.Epic, .6f, .2f, 10)]
        public void ActualMake_OnlyBridgeMetadataChanges_NoRandomOrMotionChanges(FxTier tier, float scale, float delay, int expectedCount)
        {
            var recipe = FlamethrowerRecipe.TimeScaled(FlamethrowerRecipe.Recipe, scale, delay);
            Vector3 origin = new Vector3(3, 1, 2), target = new Vector3(-4, 3, 10);
            CelFx.Frame(target - origin, out var f, out var right, out var up);
            int marked = 0;
            for (int slot = 0; slot < recipe.Layers.Count; slot++)
            {
                var layer = recipe.Layers[slot];
                if (layer.Kind != "FlameSmoke") continue;
                bool bridge = layer.Name == "smoke bridge";
                var control = JsonUtility.FromJson<CelLayer>(JsonUtility.ToJson(layer));
                if (bridge) control.Stretch = 0f;
                int count = layer.CountAt(0f, CelFx.QualityShare(tier));
                if (bridge) Assert.That(count, Is.EqualTo(expectedCount));
                var rng = new CelRng(1917u * 31u + (uint)slot * 7919u + 1u); var baselineRng = rng;
                for (int j = 0; j < count; j++)
                {
                    CelFx.Make(layer, j, count, origin, f, right, up, target, 7f / 24f, ref rng, out var raw);
                    CelFx.Make(control, j, count, origin, f, right, up, target, 7f / 24f, ref baselineRng, out var baseline);
                    var normalized = raw; normalized.Size.w = baseline.Size.w;
                    CollectionAssert.AreEqual(Bytes(baseline), Bytes(normalized), layer.Name + " RNG/layout changed");
                    var prepared = raw;
                    new FlameSmokeKind().PrepareRecord(ref prepared, origin, target, 7f / 24f);
                    Assert.That(prepared.Tone.z, Is.EqualTo(7f / 24f + delay).Within(1e-6f));
                    var expected = raw; expected.Tone.z = prepared.Tone.z;
                    if (bridge)
                    {
                        marked++; expected.Size.w = prepared.Size.w;
                        Assert.That(prepared.Size.w, Is.InRange(-4195327f, -1024f));
                        Assert.That(Vector3.Angle(FlameSmokeKind.DecodeBridgeDirection(prepared.Size.w), right + .215f * up), Is.LessThan(.2f));
                        // Real prepared clock: delay/scale must map the protected boundary and full entrainment consistently.
                        Assert.That((prepared.Tone.z + 4f * scale - prepared.Tone.z) / -prepared.Extra.w, Is.EqualTo(4f).Within(1e-5f));
                        Assert.That((prepared.Tone.z + 4.7f * scale - prepared.Tone.z) / -prepared.Extra.w, Is.EqualTo(4.7f).Within(1e-5f));
                    }
                    CollectionAssert.AreEqual(Bytes(expected), Bytes(prepared), "Axis/Seed/Pos/Motion/size or optics changed");
                }
                Assert.That(rng.Next(), Is.EqualTo(baselineRng.Next()), "RNG stream advanced differently");
            }
            Assert.That(marked, Is.EqualTo(expectedCount));
        }

        [Test]
        public void OrientationPacking_UsesEmitterFrame_IncludingVerticalAndZeroFallback()
        {
            Vector3[] directions = { Vector3.forward, Vector3.back, Vector3.left, Vector3.right,
                Vector3.up, Vector3.down, Vector3.zero, new Vector3(1e-5f, 1f, 0f), new Vector3(-7, 2, 4), new Vector3(3, -9, -2) };
            foreach (var dir in directions)
            {
                CelFx.Frame(dir, out _, out var right, out var up);
                var r = new CelRecord { Size = new Vector4(1, 2, .6f, FlameSmokeKind.BridgeMarker),
                    Extra = new Vector4(1, .12f, -.8f, -1), Tone = new Vector4(0, 1, 1, 1), Axis = new Vector3(.3f, -.4f, .5f) };
                var axis = r.Axis;
                new FlameSmokeKind().PrepareRecord(ref r, Vector3.one, Vector3.one + dir, 0);
                Vector3 drift = FlameSmokeKind.DecodeBridgeDirection(r.Size.w) * FlameSmokeKind.BridgeDriftLength;
                Assert.That(Vector3.Distance(drift, right + .215f * up), Is.LessThan(.003f));
                Assert.That(r.Axis, Is.EqualTo(axis));
                Assert.That(r.Size.w, Is.EqualTo(Mathf.Round(r.Size.w)), "float payload must be an exact integer");
            }
        }

        [Test]
        public void PackedWorldDirection_PolesSeamsAndSeededDirections_HaveBoundedError()
        {
            var rng = new CelRng(90817u);
            Vector3[] poles = { Vector3.forward, Vector3.back, Vector3.up, Vector3.down,
                Vector3.left, Vector3.right, new Vector3(1e-6f, 0, -1), new Vector3(-1e-6f, 0, -1) };
            for (int i = 0; i < 4096 + poles.Length; i++)
            {
                Vector3 n = i < poles.Length ? poles[i] : rng.OnSphere();
                float packed = FlameSmokeKind.PackBridgeDirection(n);
                Vector3 decoded = FlameSmokeKind.DecodeBridgeDirection(packed);
                Assert.That(Vector3.Angle(n, decoded), Is.LessThan(.2f));
                Assert.That(Vector3.Distance(n.normalized, decoded), Is.LessThan(.003f));
                Assert.That(packed, Is.InRange(-4195327f, -1024f));
            }
        }

        [Test]
        public void OrdinaryAndUnstagedRecords_DoNotAcquireAdvection()
        {
            foreach (float marker in new[] { 0f, .6f, -1f, -2f, FlameSmokeKind.BridgeMarker })
            {
                var r = new CelRecord { Size = new Vector4(1, 2, .6f, marker), Extra = Vector4.zero, Tone = new Vector4(0, 1, 1, 1) };
                var expected = r; expected.Tone.z = 2;
                new FlameSmokeKind().PrepareRecord(ref r, Vector3.zero, Vector3.forward, 2);
                CollectionAssert.AreEqual(Bytes(expected), Bytes(r));
            }
        }
        static CelRecord Stored(CelFxRenderer owner, int kind)
        {
            const BindingFlags flags = BindingFlags.Instance | BindingFlags.NonPublic;
            var pools = (Array)typeof(CelFxRenderer).GetField("pools", flags).GetValue(owner);
            var pool = pools.GetValue(kind);
            int start = (int)pool.GetType().GetField("Start").GetValue(pool);
            object records = typeof(CelFxRenderer).GetField("records", flags).GetValue(owner);
            return (CelRecord)records.GetType().GetProperty("Item").GetValue(records, new object[] { start });
        }

        [Test]
        public void PreparationIsPure_AndOwningRendererMirrorsAdjustedRecordOnce()
        {
            SceneStatics.ResetSession();
            var a = new GameObject("bridge-global"); var b = new GameObject("bridge-owner");
            CelFxRenderer global = null, owner = null;
            try
            {
                global = a.AddComponent<CelFxRenderer>(); global.Init(); CelFx.Adopt(global);
                owner = b.AddComponent<CelFxRenderer>(); owner.Init();
                int opaque = owner.KindId("FlameSmoke"), soft = owner.KindId("FlameSmokeSoft");
                var layer = FlamethrowerRecipe.Recipe.Layers.Find(l => l.Name == "smoke bridge");
                var rng = new CelRng(1917u * 31u + 26u * 7919u + 1u); CelRecord raw = default;
                for (int j = 0; j < 10; j++) CelFx.Make(layer, j, 10, Vector3.zero, Vector3.forward, Vector3.right, Vector3.up, Vector3.forward * 10f, 7f / 24f, ref rng, out raw);
                var prepared = raw;
                new FlameSmokeKind().PrepareRecord(ref prepared, Vector3.zero, Vector3.one, 7f / 24f);
                Assert.That(owner.SpawnedOf(opaque), Is.Zero); Assert.That(owner.SpawnedOf(soft), Is.Zero);
                Assert.That(global.SpawnedOf(global.KindId("FlameSmokeSoft")), Is.Zero);
                owner.Spawn(opaque, raw, Vector3.zero, Vector3.one, 7f / 24f);
                Assert.That(owner.SpawnedOf(opaque), Is.EqualTo(1)); Assert.That(owner.SpawnedOf(soft), Is.EqualTo(1));
                Assert.That(Stored(owner, opaque), Is.EqualTo(prepared));
                Assert.That(Stored(owner, soft), Is.EqualTo(prepared));
                Assert.That(global.SpawnedOf(global.KindId("FlameSmokeSoft")), Is.Zero);
            }
            finally
            {
                if (owner != null) owner.Shutdown(); if (global != null) global.Shutdown();
                UnityEngine.Object.DestroyImmediate(b); UnityEngine.Object.DestroyImmediate(a); SceneStatics.ResetSession();
            }
        }
    }
}
