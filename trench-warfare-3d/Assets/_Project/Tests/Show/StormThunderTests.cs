// Phase: found-storm-makethunder-s-cost-at-every-start-is-u-20261016-1 — Storm.Start synthesizes its three thunder
// clips (1.7 M Mathf.Exp calls plus a Tanh pass, measured 87.5/92.2 ms in a release player, see tasks.md). This
// guards the fix: the clips are made once per session and shared by every Storm instance, not resynthesized at
// every Start.
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class StormThunderTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
        const BindingFlags AnyStatic = BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public;

        static void CallStart(Storm s)
        {
            var m = typeof(Storm).GetMethod("Start", Any);
            Assert.IsNotNull(m, "Storm.Start (renamed? this test calls it by name)");
            m.Invoke(s, null);
        }

        static AudioClip[] ThunderOf(Storm s)
        {
            var f = typeof(Storm).GetField("thunder", Any);
            Assert.IsNotNull(f, "Storm.thunder (renamed? this test reads it by name)");
            return (AudioClip[])f.GetValue(s);
        }

        static void ClearShared()
        {
            var f = typeof(Storm).GetField("shared", AnyStatic);
            Assert.IsNotNull(f, "Storm.shared (renamed? this test resets it by name)");
            f.SetValue(null, null);
        }

        [Test]
        public void TheSecondStormInASession_ReusesTheFirstOnesThunderClips_AndSynthesizesMuchFaster()
        {
            ClearShared();
            var goA = new GameObject("storm A under test") { hideFlags = HideFlags.DontSave };
            var goB = new GameObject("storm B under test") { hideFlags = HideFlags.DontSave };
            try
            {
                var a = goA.AddComponent<Storm>();
                var swA = System.Diagnostics.Stopwatch.StartNew();
                CallStart(a);
                swA.Stop();
                var thunderA = ThunderOf(a);
                Assert.AreEqual(3, thunderA.Length);
                foreach (var clip in thunderA) Assert.IsNotNull(clip);

                var b = goB.AddComponent<Storm>();
                var swB = System.Diagnostics.Stopwatch.StartNew();
                CallStart(b);
                swB.Stop();
                var thunderB = ThunderOf(b);

                for (int i = 0; i < 3; i++)
                    Assert.AreSame(thunderA[i], thunderB[i], $"clip {i}: the second Storm reuses the first one's clips, it does not resynthesize");
                Assert.Less(swB.Elapsed.TotalMilliseconds, swA.Elapsed.TotalMilliseconds / 10.0 + 1.0,
                    $"second Start ({swB.Elapsed.TotalMilliseconds:F2} ms) should be far cheaper than the first ({swA.Elapsed.TotalMilliseconds:F2} ms): it must not resynthesize");
            }
            finally
            {
                Object.DestroyImmediate(goA);
                Object.DestroyImmediate(goB);
                ClearShared();
            }
        }

        // [L1.2]
        [Test]
        public void SceneStatics_DestroysTheSharedThunderClips_SoAPlaySessionLeaksNone()
        {
            ClearShared();
            var go = new GameObject("storm under test") { hideFlags = HideFlags.DontSave };
            try
            {
                var s = go.AddComponent<Storm>();
                CallStart(s);
                var clips = ThunderOf(s);
                Assert.AreEqual(3, clips.Length);

                TW.Presentation.SceneStatics.ResetSession();

                for (int i = 0; i < clips.Length; i++)
                    Assert.IsTrue(clips[i] == null, "[L1.2] the session reset destroys thunder clip " + i + ": the editor must not leak three AudioClips per Play session");

                var f = typeof(Storm).GetField("shared", AnyStatic);
                Assert.IsNull(f.GetValue(null), "[L1.2] the session reset also drops the shared cache");
            }
            finally
            {
                Object.DestroyImmediate(go);
                ClearShared();
            }
        }

        [Test]
        public void SceneStatics_DropsTheSharedClips_SoADeadSessionsClipsAreNeverReused()
        {
            ClearShared();
            var go = new GameObject("storm under test") { hideFlags = HideFlags.DontSave };
            try
            {
                var s = go.AddComponent<Storm>();
                CallStart(s);
                var f = typeof(Storm).GetField("shared", AnyStatic);
                Assert.IsNotNull(f.GetValue(null), "Start filled the shared cache");

                TW.Presentation.SceneStatics.ResetSession();
                Assert.IsNull(f.GetValue(null), "a gym/session reset drops the shared clips (they were destroyed with the dead scene)");
            }
            finally
            {
                Object.DestroyImmediate(go);
                ClearShared();
            }
        }
    }
}
