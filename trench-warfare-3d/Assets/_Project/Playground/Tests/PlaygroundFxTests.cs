// Phase: Playground (2026-09-28, lane/show/maint-2026-09) — the lamp pool survives a light destroyed behind its back
// A repaired machine destroyed its fire light itself; the lamp's entry stayed (life 99999 s, following a machine that is
// still there) and PlaygroundFx.LateUpdate threw MissingReferenceException every frame, over 1,100 times in one session.
using System.Reflection;
using NUnit.Framework;
using TW.Playground;
using UnityEngine;

namespace TW.Tests.Playground
{
    public sealed class PlaygroundFxTests
    {
        static readonly MethodInfo LateUpdate = typeof(PlaygroundFx).GetMethod("LateUpdate", BindingFlags.Instance | BindingFlags.NonPublic);

        GameObject host, machine;
        PlaygroundFx fx;

        [SetUp]
        public void SetUp()
        {
            host = new GameObject("fx");
            fx = host.AddComponent<PlaygroundFx>();   // edit mode: no Awake, so no books or debris, only the lamps
            machine = new GameObject("machine");
        }

        [TearDown]
        public void TearDown()
        {
            Object.DestroyImmediate(host);
            Object.DestroyImmediate(machine);
        }

        [Test]
        public void A_held_lamp_whose_light_was_destroyed_elsewhere_is_dropped_without_throwing()
        {
            var light = fx.Lamp(Vector3.up, Color.red, 8f, 14f, 99999f, true, machine.transform);
            Object.DestroyImmediate(light.gameObject);
            Assert.DoesNotThrow(() => LateUpdate.Invoke(fx, null));
            Assert.AreEqual(0, fx.LampsAlive);
        }

        [Test]
        public void EndLamp_forgets_the_lamp_and_destroys_its_light()
        {
            var light = fx.Lamp(Vector3.up, Color.red, 8f, 14f, 99999f, true, machine.transform);
            var other = fx.Lamp(Vector3.zero, Color.white, 4f, 10f, 99999f);
            fx.EndLamp(light);
            Assert.AreEqual(1, fx.LampsAlive);
            Assert.IsTrue(light == null, "the light's GameObject is destroyed");
            Assert.IsTrue(other != null, "the other lamp is untouched");
            Assert.DoesNotThrow(() => LateUpdate.Invoke(fx, null));
        }
    }
}
