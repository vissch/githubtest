// Phase: tooling (the gym, 2026-10-11) — Gym's Play-mode hook used to return only on a null request, and SessionState
// hands back "" for a key nobody set whatever default is passed (MachineStudy, seen 2026-10-05), so every ordinary Play
// in a scene with a SimHost started a full unattended gym run. The hook now ignores an empty request and one without
// Gym.Run's "run:" marker. The test also logs what GetString really answers for an unset key in this editor, for [N8.1].
using System.Reflection;
using NUnit.Framework;
using UnityEditor;
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
            foreach (var run in Object.FindObjectsByType<GymRun>(FindObjectsSortMode.None)) Object.DestroyImmediate(run.gameObject);
            if (host != null) Object.DestroyImmediate(host);
            host = null;
        }
    }
}
