// Phase: Playground (2026-10-16, lane/show/review-fixes) — the P3 fixes of the 2026-10-04 review, guarded
// Y15: "model v -1" was taken and answered "ok"; every later scene threw on Library.Vehicles[index % Length].
// Y17: an HE shell on a flying machine drew its burst on the ground under it, and "cookdelay -1" ("never cooks off")
// still cooked a machine off on overkill, because Mathf.Max(1.5f, CookDelay * 0.5f) turns -1 into 1.5 s.
using System.Globalization;
using System.Linq;
using System.Reflection;
using NUnit.Framework;
using TW.Playground;
using TW.Playground.Editor;
using UnityEditor;
using UnityEngine;
using UnityEngine.TestTools;

namespace TW.Tests.Playground
{
    public sealed class PlaygroundFixTests
    {
        static PlaygroundLibrary Lib()
        {
            var l = AssetDatabase.LoadAssetAtPath<PlaygroundLibrary>(PlaygroundSetup.LibraryPath);
            Assert.IsNotNull(l, "the committed library is missing; run TW/Playground/Build");
            return l;
        }

        [Test]   // [Y15]
        public void AModelIndexBelowZeroIsRefused()
        {
            var go = new GameObject("host");
            try
            {
                var host = go.AddComponent<PlaygroundHost>();
                Assert.That(host.Do("model v -1"), Does.StartWith("bad"), "[Y15] a negative vehicle index was taken");
                Assert.That(host.Do("model u -2"), Does.StartWith("bad"), "[Y15] a negative figure index was taken");
                Assert.That(host.Do("model v 1"), Does.StartWith("ok"), "[Y15] a sound index must still be taken");
            }
            finally { Object.DestroyImmediate(go); }
        }

        [Test]   // [Y17]
        public void AFlyersBurstStaysInTheAir()
        {
            var e = Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).flyer);
            if (e == null) Assert.Ignore("no flyer in the library");
            var fxGo = new GameObject("fx");
            var parent = new GameObject("test stage").transform;
            try
            {
                var fx = fxGo.AddComponent<PlaygroundFx>();
                fx.ClearCards();        // edit mode has no Awake: this is what gives it its flipbooks
                var r = VehicleRig.Build(e, fx, parent, Vector3.zero, 0f, 1.4f, 3);
                r.CookDelay = -1f; r.GroundY = 0f;
                for (int f = 0; f < 60; f++) r.Advance(1f / 60f);
                var hull = r.Find("Hull");
                float height = hull.T.position.y;
                Assert.That(height, Is.GreaterThan(5f), "the flyer is not in the air, so this test proves nothing");
                r.HitLocal(r.transform.InverseTransformPoint(hull.T.position) + new Vector3(2.6f, 0f, 0f), Vector3.down, 35f, VehicleRig.HitKind.HE);
                var lights = fxGo.GetComponentsInChildren<Light>();
                Assert.That(lights.Length, Is.GreaterThan(0), "[Y17] the burst lit nothing at all");
                float top = lights.Max(l => l.transform.position.y);
                Assert.That(top, Is.GreaterThan(height - 2f), "[Y17] the burst was drawn on the ground under the flyer, not at the hit");
            }
            finally { Object.DestroyImmediate(parent.gameObject); Object.DestroyImmediate(fxGo); }
        }

        [Test]   // [Y17]
        public void ACookDelayOfMinusOneNeverCooksOff()
        {
            var e = Lib().Vehicles.FirstOrDefault();
            Assert.IsNotNull(e, "no vehicle in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, 1.7f, 3);
                r.CookDelay = -1f;
                r.HitPart(r.Find("Hull"), 500f);        // overkill: Hp well below -0.6 * MaxHp
                Assert.That(r.State, Is.EqualTo(VehicleRig.Stage.KnockedOut), "the big hit did not knock it out");
                // put the knock-out ten seconds into the past, so any cook-off timer has long run out
                var knockedAt = typeof(VehicleRig).GetField("knockedAt", BindingFlags.Instance | BindingFlags.NonPublic);
                Assert.IsNotNull(knockedAt, "VehicleRig.knockedAt is gone; this test needs rewriting");
                knockedAt.SetValue(r, Time.time - 10f);
                r.HitPart(r.Find("Hull"), 1f);
                Assert.That(r.State, Is.EqualTo(VehicleRig.Stage.KnockedOut), "[Y17] cookdelay -1 means never, but it cooked off");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }
        [Test]   // [Y18]
        public void TheBakedMeshGoesWithTheFigure()
        {
            var l = Lib();
            if (l.Units.Length == 0) Assert.Ignore("no unit in the library");
            var parent = new GameObject("test stage").transform;
            ClipDeck deck = null;
            try
            {
                deck = ClipDeck.Make(l, parent);
                if (deck == null) Assert.Ignore("the library has no ClipSource");
                var u = UnitRig.Build(l.Units[0], deck, null, parent, Vector3.zero, 0f, 1f);
                u.Drift(out _, out _);          // this is what bakes the mesh
                var field = typeof(UnitRig).GetField("baked", BindingFlags.Instance | BindingFlags.NonPublic);
                Assert.IsNotNull(field, "UnitRig.baked is gone; this test needs rewriting");
                var baked = field.GetValue(u) as Mesh;
                Assert.IsNotNull(baked, "nothing was baked, so this test proves nothing");
                // OnDestroy is called directly: inside an edit-mode DestroyImmediate chain Unity swallows the rig's own
                // DestroyImmediate calls, so even the materials the rig has always cleaned up survive there
                var onDestroy = typeof(UnitRig).GetMethod("OnDestroy", BindingFlags.Instance | BindingFlags.NonPublic);
                Assert.IsNotNull(onDestroy, "UnitRig.OnDestroy is gone; this test needs rewriting");
                onDestroy.Invoke(u, null);
                Assert.IsTrue(baked == null, "[Y18] the baked mesh outlived the figure: one leaks per figure per report");
            }
            finally { if (deck != null) deck.Destroy(); Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]   // [Y18]
        public void ForgettingTheCutsDestroysTheMeshesItBuilt()
        {
            var parent = new GameObject("test stage").transform;
            try
            {
                BuildingRig.ForgetCuts();
                BuildingRig.Build("Houses", "House5", null, parent, Vector3.zero, 0f, 11);
                var field = typeof(BuildingRig).GetField("recut", BindingFlags.Static | BindingFlags.NonPublic);
                Assert.IsNotNull(field, "BuildingRig.recut is gone; this test needs rewriting");
                var cache = (System.Collections.Generic.Dictionary<Mesh, Mesh>)field.GetValue(null);
                var cut = cache.Where(kv => kv.Value != null && kv.Value != kv.Key).Select(kv => kv.Value).ToList();
                var kept = cache.Where(kv => kv.Value != null && kv.Value == kv.Key).Select(kv => kv.Value).ToList();
                if (cut.Count == 0) Assert.Ignore("this house has no cut faces, so this test proves nothing");
                BuildingRig.ForgetCuts();
                Assert.IsTrue(cut.All(m => m == null), "[Y18] the cut-face meshes outlived the cache: one per module per scene");
                Assert.IsTrue(kept.All(m => m != null), "[Y18] an uncut mesh is the kit's own and must not be destroyed");
            }
            finally { Object.DestroyImmediate(parent.gameObject); BuildingRig.ForgetCuts(); }
        }

        [Test]   // [Y18]
        public void ClearingDebrisKeepsTheOneRenderer()
        {
            var go = new GameObject("fx");
            bool ignoring = LogAssert.ignoreFailingMessages;
            LogAssert.ignoreFailingMessages = true;   // the old tree's Destroy() in edit mode complains
            try
            {
                var fx = go.AddComponent<PlaygroundFx>();
                // read through reflection: DebrisRenderer lives in TW.Presentation.Camera, which this test assembly
                // does not reference (and the reference list is a seam)
                var debris = typeof(PlaygroundFx).GetProperty("Debris");
                Assert.IsNotNull(debris, "PlaygroundFx.Debris is gone; this test needs rewriting");
                fx.ClearDebris();
                object first = debris.GetValue(fx);
                Assert.IsNotNull(first, "no debris renderer at all");
                fx.ClearDebris();
                Assert.AreSame(first, debris.GetValue(fx), "[Y18] a new DebrisRenderer per scene: it is not Ready until its Start, so debris in the same batch is dropped");
            }
            finally { LogAssert.ignoreFailingMessages = ignoring; Object.DestroyImmediate(go); }
        }

        [Test]   // [Y19]
        public void ThePoseSignatureReadsTheSameInEveryCulture()
        {
            var e = Lib().Vehicles.FirstOrDefault();
            Assert.IsNotNull(e, "no vehicle in the library");
            var parent = new GameObject("test stage").transform;
            var was = CultureInfo.CurrentCulture;
            try
            {
                CultureInfo.CurrentCulture = new CultureInfo("de-DE");
                var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, 1.7f, 3);
                var sig = r.PoseSignature();
                Assert.That(sig, Does.Contain("."), "[Y19] the signature was written in the machine's own culture, so a comma-decimal machine compared nonsense");
            }
            finally { CultureInfo.CurrentCulture = was; Object.DestroyImmediate(parent.gameObject); }
        }
    }
}
