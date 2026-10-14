// Phase: review fix (2026-10-14) — SimHost.WriteWorlds is how every tool changes the match. It used to leave the
// presenter holding the last tick's slot count, so a unit spawned while the host was paused was never drawn.
using System.Collections;
using NUnit.Framework;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;

namespace TW.Tests
{
    public class ToolWritePlayTests
    {
        GameObject go;
        bool? canaryWas;

        [SetUp]
        public void SetUp() { canaryWas = SimHost.CanaryOverride; SimHost.CanaryOverride = false; }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            Time.captureDeltaTime = 0f;
            SimHost.CanaryOverride = canaryWas;
            if (go != null) Object.Destroy(go);
            yield return null;
        }

        SimHost MakeHost()
        {
            go = new GameObject("tool-write-test");
            go.SetActive(false);
            var host = go.AddComponent<SimHost>();
            host.GeneratedBattlefield = false;
            host.PlaytestMap = true;
            host.StartingSilver = 3000;
            go.SetActive(true);
            return host;
        }

        // [M4] a tool spawning a unit through WriteWorlds while the host is paused: the presenter must draw it at once.
        [UnityTest]
        public IEnumerator AUnitSpawnedWhilePausedIsDrawnWhereItWasPut()
        {
            var host = MakeHost();
            host.TimeScale = 0f;
            yield return null;

            var want = new float3(12f, 0f, -34f);
            int slot = -1;
            Assert.IsTrue(host.WriteWorlds(m => slot = m.World.Spawn(0, 0, want, 100f, 2f, false)), "[M4] WriteWorlds refused the spawn");
            Assert.GreaterOrEqual(slot, 0, "[M4] the spawn gave no slot");

            var drawn = host.Presenter.Drawn(slot);
            Assert.Less(math.distance(drawn, want), 0.01f, $"[M4] a unit spawned while paused is not drawn: Drawn({slot}) is {drawn}, wanted {want}");
        }
    }
}
