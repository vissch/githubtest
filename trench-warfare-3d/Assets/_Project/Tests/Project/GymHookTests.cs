// Phase: tooling (the gym, 2026-10-11) — Gym's Play-mode hook used to return only on a null request, and SessionState
// hands back "" for a key nobody set whatever default is passed (MachineStudy, seen 2026-10-05), so every ordinary Play
// in a scene with a SimHost started a full unattended gym run. The hook now ignores an empty request and one without
// Gym.Run's "run:" marker. The test also logs what GetString really answers for an unset key in this editor, for [N8.1].
using System.Reflection;
using NUnit.Framework;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;
using TW.Editor;
using TW.Presentation;

namespace TW.Tests
{
    public class GymHookTests
    {
        const string RequestKey = "TW.Gym.Request";
        GameObject host;

        [Test]
        public void OnlyAMarkedGymRequestStartsARun()
        {
            string probe = SessionState.GetString("TW.Gym.Probe.Unset", null);
            Debug.Log("[N8.1] GetString(an unset key, null) -> " + (probe == null ? "null" : "\"" + probe + "\""));

            host = new GameObject("n81-host");
            host.AddComponent<SimHost>();   // edit mode runs no Awake, so nothing starts a match

            SessionState.SetString(RequestKey, "");
            EnterPlayMode();
            Assert.AreEqual(0, Object.FindObjectsByType<GymRun>(FindObjectsSortMode.None).Length,
                "[N8.1] an empty gym request started a gym run");

            SessionState.SetString(RequestKey, "run:max=1");
            EnterPlayMode();
            Assert.AreEqual(1, Object.FindObjectsByType<GymRun>(FindObjectsSortMode.None).Length,
                "[N8.1] a marked gym request started no run");
        }

        [Test]
        public void AnErasedGymRequestStartsNoRun()   // [N8.1b]
        {
            var open = EditorSceneManager.GetActiveScene();
            Debug.Log("[N8.1b] active scene: path \"" + open.path + "\", name \"" + open.name + "\"");

            host = new GameObject("n81b-erased-host");
            host.AddComponent<SimHost>();

            SessionState.EraseString(RequestKey);
            EnterPlayMode();
            Assert.AreEqual(0, Object.FindObjectsByType<GymRun>(FindObjectsSortMode.None).Length,
                "[N8.1b] an erased gym request attached a GymRun");
        }

        [Test]
        public void GymsOwnMarkerAloneStartsExactlyOneRun()   // [N8.1b]
        {
            host = new GameObject("n81b-marker-host");
            host.AddComponent<SimHost>();

            SessionState.SetString(RequestKey, GymConst("Marker"));   // what Gym.Run("") stores
            EnterPlayMode();
            var runs = Object.FindObjectsByType<GymRun>(FindObjectsSortMode.None);
            Assert.AreEqual(1, runs.Length, "[N8.1b] Gym.Run(\"\")'s own value attached " + runs.Length + " GymRun, not one");
            Assert.AreEqual("", runs[0].Options, "[N8.1b] the marker was not stripped from the options");
        }

        // the marker comes from Gym itself, so no test hand-writes "run:"
        static string GymConst(string name)
        {
            var f = typeof(Gym).GetField(name, BindingFlags.Static | BindingFlags.NonPublic);
            Assert.IsNotNull(f, "[N8.1b] Gym." + name + " is gone");
            return (string)f.GetRawConstantValue();
        }

        static void EnterPlayMode()
        {
            var hook = typeof(Gym).GetMethod("OnPlayMode", BindingFlags.Static | BindingFlags.NonPublic);
            Assert.IsNotNull(hook, "[N8.1] Gym.OnPlayMode is gone");
            hook.Invoke(null, new object[] { PlayModeStateChange.EnteredPlayMode });
        }

        [TearDown]
        public void Clean()
        {
            SessionState.EraseString(RequestKey);
            // only the component: StartRunner attaches it to FindFirstObjectByType<SimHost>(), which in the Test Runner
            // window with GreyboxCorridor open is the scene's own host object, not the one this test made. [N8.1b]
            foreach (var run in Object.FindObjectsByType<GymRun>(FindObjectsSortMode.None)) Object.DestroyImmediate(run);
            if (host != null) Object.DestroyImmediate(host);
            host = null;
        }
    }
}
