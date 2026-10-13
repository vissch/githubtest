// Phase: Playground (2026-10-14, lane/show/idea-the-playground-camera-shoots-on-the-bang) — the host's own bang detector
// The schedule had a worked example (PlaygroundBangTests) but the host's BangStep, which decides WHEN a bang counts, had
// none (second read P3.5). It compared the last bang's name, so a cook followed by a flame before the host's Update was
// lost for good (P3.1). This drives the real BangStep by reflection, with the host's clock turned by hand: the pngs are
// the grabbing's business (and there is no camera to grab from in edit mode), the schedule and the manifest are here.
using System.IO;
using System.Reflection;
using NUnit.Framework;
using TW.Playground;
using UnityEngine;
using UnityEngine.TestTools;

namespace TW.Tests.Playground
{
    public sealed class PlaygroundBangHostTests
    {
        const BindingFlags Priv = BindingFlags.Instance | BindingFlags.NonPublic;
        static readonly MethodInfo Step = typeof(PlaygroundHost).GetMethod("BangStep", Priv);
        static readonly FieldInfo Pending = typeof(PlaygroundHost).GetField("pending", Priv);
        static readonly FieldInfo FxField = typeof(PlaygroundHost).GetField("<Fx>k__BackingField", Priv);
        static readonly MethodInfo RaiseBang = typeof(PlaygroundFx).GetMethod("Bang", Priv);
        static readonly FieldInfo Clock = typeof(PlaygroundHost).GetField("Now", BindingFlags.Static | BindingFlags.NonPublic);

        GameObject go;
        PlaygroundHost host;
        PlaygroundFx fx;
        string dir, stem;
        float now;
        readonly System.Collections.Generic.List<Camera> off = new System.Collections.Generic.List<Camera>();

        [SetUp]
        public void SetUp()
        {
            go = new GameObject("host");
            host = go.AddComponent<PlaygroundHost>();   // edit mode: no Awake, so no stage and no Fx of its own
            fx = go.AddComponent<PlaygroundFx>();
            FxField.SetValue(host, fx);
            dir = Path.Combine(Path.GetTempPath(), "tw_bang_host_" + System.Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(dir);
            stem = Path.Combine(dir, "t").Replace(Path.DirectorySeparatorChar, '/');
            now = 0f;
            Clock.SetValue(null, (System.Func<float>)(() => now));
            // a camera left in the edit-mode scene would make Shoot really render, and its Destroy of the texture is
            // an error in edit mode. The schedule is what is under test, not the grabbing: switch the camera off.
            while (Camera.main != null) { var c = Camera.main; c.enabled = false; off.Add(c); }
            LogAssert.ignoreFailingMessages = true;
        }

        [TearDown]
        public void TearDown()
        {
            foreach (var c in off) if (c != null) c.enabled = true;
            off.Clear();
            LogAssert.ignoreFailingMessages = false;
            Clock.SetValue(null, (System.Func<float>)(() => Time.time));
            Object.DestroyImmediate(go);
            if (Directory.Exists(dir)) Directory.Delete(dir, true);
        }

        void Arm(string kind, float wait = 20f)
        {
            string r = host.Do("bang " + stem + ".png " + kind + " 8 8 0.2 1 " + wait.ToString("0.##", System.Globalization.CultureInfo.InvariantCulture));
            StringAssert.StartsWith("ok ", r);
        }

        BangShot Shot() => (BangShot)Pending.GetValue(host);

        [Test]
        public void AWantedBangFollowedByAnotherKindStillFires()
        {
            Arm("cook");
            RaiseBang.Invoke(fx, new object[] { "cook" });
            RaiseBang.Invoke(fx, new object[] { "flame" });   // the last bang's name is no longer "cook"
            now = 0.3f;
            Step.Invoke(host, null);
            Assert.IsNotNull(Shot(), "the shot is still running, four frames owed");
            Assert.IsTrue(Shot().Fired, "the cook counted, though a flame came after it");
        }

        [Test]
        public void AKindNobodyRaisesNeverFires()
        {
            Arm("burst");
            RaiseBang.Invoke(fx, new object[] { "flame" });
            now = 0.3f;
            Step.Invoke(host, null);
            Assert.IsFalse(Shot().Fired, "a flame is not a burst");
        }

        [Test]
        public void FiveStepsAfterTheBangFinishTheShotAndWriteItsManifest()
        {
            Arm("cook");
            Step.Invoke(host, null);                          // the pre-roll tick at t = 0
            RaiseBang.Invoke(fx, new object[] { "cook" });
            foreach (float t in new[] { 0.21f, 0.5f, 0.8f, 1.1f, 1.4f })   // well past each step, never on it
            { now = t; Step.Invoke(host, null); }
            Assert.IsNull(Pending.GetValue(host), "the shot is finished and forgotten");
            Assert.AreEqual("bang cook: 5 frames", host.Status);
            string json = File.ReadAllText(stem + ".json");
            StringAssert.Contains("\"bang\": true", json);
            StringAssert.Contains("t_f5.png", json);
        }

        [Test]
        public void NoBangByTheWaitWritesAManifestThatSaysSo()
        {
            Arm("cook", 0f);
            Step.Invoke(host, null);
            Assert.IsNull(Pending.GetValue(host));
            StringAssert.Contains("timed out", host.Status);
            StringAssert.Contains("\"bang\": false", File.ReadAllText(stem + ".json"));
        }
    }
}
