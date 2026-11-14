// Phase: CelFx artillery - Only the Close ground fan record may differ from the selected compound recipe.
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.InteropServices;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class ArtilleryGroundFanTests
    {
        CelFxRenderer renderer;
        GameObject holder;
        FxTier previousQuality;

        [SetUp]
        public void SetUp()
        {
            previousQuality = FxQuality.Tier;
            FxQuality.Set(FxTier.High);
            SceneStatics.ResetSession();
            holder = new GameObject("ArtilleryGroundFanTests") { hideFlags = HideFlags.HideAndDontSave };
            renderer = holder.AddComponent<CelFxRenderer>();
            renderer.Init();
            CelFx.Adopt(renderer);
            CelFx.ZoomOverride = 10f;
            CelFx.Clock = 100f;
            SceneHooks.Flash = (at, colour, power, reach, life) => { };
        }

        [TearDown]
        public void TearDown()
        {
            try { renderer.Shutdown(); UnityEngine.Object.DestroyImmediate(holder); }
            finally { SceneStatics.ResetSession(); FxQuality.Set(previousQuality); }
        }

        static CelFxRecipe Recipe(string name, bool close) => (CelFxRecipe)typeof(ArtilleryRecipe)
            .GetField(name + (close ? "Close" : ""), BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic).GetValue(null);

        Dictionary<float, (int kind, CelRecord record)> Records(int[] starts)
        {
            const BindingFlags flags = BindingFlags.Instance | BindingFlags.NonPublic;
            var pools = (Array)typeof(CelFxRenderer).GetField("pools", flags).GetValue(renderer);
            object records = typeof(CelFxRenderer).GetField("records", flags).GetValue(renderer);
            var item = records.GetType().GetProperty("Item");
            var result = new Dictionary<float, (int, CelRecord)>();
            for (int kind = 0; kind < renderer.KindCount; kind++)
            {
                object pool = pools.GetValue(kind);
                int first = (int)pool.GetType().GetField("Start").GetValue(pool);
                Assert.That(renderer.SpawnedOf(kind), Is.LessThanOrEqualTo(renderer.CapacityOf(kind)), "no ring wrap in fixture");
                for (int slot = starts[kind]; slot < renderer.SpawnedOf(kind); slot++)
                {
                    var record = (CelRecord)item.GetValue(records, new object[] { first + slot });
                    result.Add(record.Seed, (kind, record));
                }
            }
            return result;
        }

        static byte[] Bytes(CelRecord record)
        {
            IntPtr memory = Marshal.AllocHGlobal(CelRecord.Bytes);
            try
            {
                Marshal.StructureToPtr(record, memory, false);
                var bytes = new byte[CelRecord.Bytes];
                Marshal.Copy(memory, bytes, 0, bytes.Length);
                return bytes;
            }
            finally { Marshal.FreeHGlobal(memory); }
        }

        [Test]
        public void Background_Changes_One_Record_And_Preserves_Other_63_Bytes() => CheckRecords("Background");

        [Test]
        public void Foreground_Changes_One_Record_And_Preserves_Other_63_Bytes() => CheckRecords("Foreground");

        [Test]
        public void Target_Changes_One_Record_And_Preserves_Other_63_Bytes() => CheckRecords("Target");

        void CheckRecords(string name)
        {
            var close = Recipe(name, true);
            var control = JsonUtility.FromJson<CelFxRecipe>(JsonUtility.ToJson(close));
            var outer = Recipe(name, false);
            Assert.That(close.Layers.Count, Is.EqualTo(9));
            for (int i = 2; i < 3; i++)
            {
                control.Layers[i] = JsonUtility.FromJson<CelLayer>(JsonUtility.ToJson(outer.Layers[i]));
                Assert.That(close.Layers[i].Start, Is.EqualTo(control.Layers[i].Start));
                Assert.That(close.Layers[i].Duration, Is.EqualTo(control.Layers[i].Duration));
                Assert.That(close.Layers[i].TimeJitter, Is.EqualTo(control.Layers[i].TimeJitter));
                Assert.That(close.Layers[i].Close, Is.EqualTo(1));
                Assert.That(control.Layers[i].Close, Is.EqualTo(1));
            }
            foreach (var layer in control.Layers)
            {
                typeof(CelLayer).GetField("kindId", BindingFlags.Instance | BindingFlags.NonPublic).SetValue(layer, int.MinValue);
                typeof(CelLayer).GetField("paletteId", BindingFlags.Instance | BindingFlags.NonPublic).SetValue(layer, -1);
                typeof(CelLayer).GetField("coolId", BindingFlags.Instance | BindingFlags.NonPublic).SetValue(layer, -1);
            }
            var starts = new int[renderer.KindCount];
            CelFx.Play(control, Vector3.up, Vector3.forward, Vector3.up, 1917u, 1.25f);
            var expected = Records(starts);
            Assert.That(expected.Count, Is.EqualTo(64), "real High/Close GPU records; queued CPU light excluded");
            for (int kind = 0; kind < starts.Length; kind++) starts[kind] = renderer.SpawnedOf(kind);
            ArtilleryRecipe.PlayAt(outer, Vector3.up, Vector3.forward, 1917u, 1.25f);
            var actual = Records(starts);
            Assert.That(actual.Count, Is.EqualTo(64));
            int fixedRecords = 0, impactRecords = 0;
            foreach (var pair in expected)
            {
                Assert.That(actual.ContainsKey(pair.Key), Is.True, "exact seeded record set");
                var before = pair.Value; var after = actual[pair.Key];
                Assert.That(after.kind, Is.EqualTo(before.kind));
                string kindName = renderer.KindName(before.kind);
                bool impact = kindName == ArtilleryFxKind.KindName && before.record.Extra.x == ArtilleryFxKind.Pool;
                if (!impact)
                {
                    Assert.That(Bytes(after.record), Is.EqualTo(Bytes(before.record)), "all 128 bytes for seed " + pair.Key);
                    fixedRecords++;
                    continue;
                }
                Assert.That(Bytes(after.record), Is.Not.EqualTo(Bytes(before.record)), "ground record must actually differ");
                Assert.That(after.record.Born, Is.EqualTo(before.record.Born));
                Assert.That(after.record.Seed, Is.EqualTo(before.record.Seed));
                Assert.That(after.record.Pos, Is.EqualTo(before.record.Pos));
                Assert.That(after.record.Vel, Is.EqualTo(before.record.Vel));
                Assert.That(after.record.Axis, Is.EqualTo(before.record.Axis));
                Assert.That(after.record.Motion, Is.EqualTo(before.record.Motion));
                Assert.That(after.record.Life, Is.EqualTo(1.50f));
                Assert.That(after.record.Tone.z, Is.EqualTo(1.15f));
                Assert.That(after.record.Extra.x, Is.EqualTo(ArtilleryFxKind.GroundFan));
                var normalized = after.record;
                normalized.Life = before.record.Life;
                normalized.Tone.z = before.record.Tone.z;
                normalized.Extra.x = before.record.Extra.x;
                Assert.That(Bytes(normalized), Is.EqualTo(Bytes(before.record)), "only Life, Tone.z and Extra.x may differ");
                TestContext.WriteLine(name + " ground " + kindName + ": Born=" + after.record.Born.ToString("R") +
                    " Seed=" + after.record.Seed.ToString("R") + " old/new life=" + before.record.Life.ToString("R") + "/" + after.record.Life.ToString("R"));
                impactRecords++;
            }
            Assert.That(fixedRecords, Is.EqualTo(63));
            Assert.That(impactRecords, Is.EqualTo(1));
            Assert.That(CelFx.PendingCount, Is.EqualTo(2), "one unchanged surface-light event per play remains queued");
            TestContext.WriteLine("64 actual records per play; 63 byte-identical; only ground Life/Tone.z/Extra.x differ; CPU lights unchanged and queued.");
        }
    }
}
