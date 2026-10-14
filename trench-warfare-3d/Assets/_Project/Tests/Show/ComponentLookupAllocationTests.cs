// Phase: tooling (2026-09-23) — what a per-event component lookup costs, measured, because the per-tick allocation
// hunt came down to "something in an event subscriber allocates about 45 bytes per event" and these are the two
// calls every Shot and every Death event makes before it does anything else.
//
// CombatFx.OnSimEvent lines 367 and 445 both open with:
//     var cam = Camera.main;
//     float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
//
// `TryGetComponent<T>` has a fast path only when T is a concrete component type; an INTERFACE cannot use it, so
// the call falls back to a managed search. With hundreds of Shot events a tick and 1,997 men on the field, a small
// allocation there is multiplied by the busiest event in the game. This file measures the three variants side by
// side so the claim is a number rather than folklore, and stays afterwards so nobody reintroduces it.
//
// The answer, measured on Unity 6000.0.50f1, is that all three are FREE: 0 bytes a call, interface lookup and
// concrete lookup and Camera.main alike. The hypothesis was wrong. That is worth a file of its own rather than a
// deleted branch, for two reasons: it stops the next person "optimising" a per-event lookup that costs nothing,
// which is work that would have been done on the strength of a widely repeated piece of folklore about
// GetComponent and interfaces; and if a Unity upgrade ever makes them allocate, this goes red and tells us, at
// which point hundreds of Shot events a tick really would matter.
//
// The per-tick allocation is therefore still unattributed. Ruled out so far, all measured at 0 bytes:
// LockstepDriver.TryStep, SimPresenter.Capture, AnimationController.Tick, EventPump.Collect, and now these three
// lookups. What remains is the bodies of the nine OnSimEvent subscribers, and SimPresenter.Interpolate /
// AnimationController.Advance, which run per frame rather than per tick.
using System;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Perf;

namespace TW.Tests
{
    public class ComponentLookupAllocationTests
    {
        sealed class StubZoom : MonoBehaviour, IZoomSource
        {
            public float CurrentZoom => 30f;
        }

        // Allocations per call (TW.Perf.AllocProbe). This file first measured bytes with
        // GC.GetAllocatedBytesForCurrentThread, a stub under Unity's Boehm GC that reads 0 whatever the code does, so its
        // "all three are free" finding was void until re-measured with a counter that sees allocations (2026-09-23).
        static double PerCall(Action call, int warm, int reps) => AllocProbe.PerCall(call, warm, reps);

        /// <summary>[TS4] The Camera.main measurement here used to run with no MainCamera-tagged camera in the scene,
        /// so it timed the NOT-FOUND path: a miss, which is the one path CombatFx.OnSimEvent never takes in a battle.
        /// The found path — the one that walks the tagged objects and hands back a Camera — had never been measured at
        /// all. Both are measured now, and both must be free.</summary>
        [Test]
        public void PerEventComponentLookupsAreFree()
        {
            var go = new GameObject("lookup probe");
            try
            {
                var cam = go.AddComponent<Camera>();
                go.AddComponent<StubZoom>();

                double byInterface = PerCall(() => { cam.TryGetComponent<IZoomSource>(out _); }, 100, 2000);
                double byConcrete = PerCall(() => { cam.TryGetComponent<StubZoom>(out _); }, 100, 2000);

                // untagged still: this is Camera.main when it finds nothing, which is what this file used to measure
                double cameraMainMissing = PerCall(() => { var c = Camera.main; }, 100, 2000);

                // and now the path a running battle takes: a tagged, enabled camera that Camera.main hands back
                // (which camera it hands back is the scene's business — a battle scene loaded by another test in the
                //  same run has its own Main Camera — so what matters here is only that it finds one)
                go.tag = "MainCamera";
                Assert.IsNotNull(Camera.main, "[TS4] Camera.main still finds nothing with a tagged, enabled camera in " +
                                              "the scene, so the measurement below would be the not-found path again");
                double cameraMainFound = PerCall(() => { var c = Camera.main; }, 100, 2000);

                string line = $"allocations per call — TryGetComponent<IZoomSource> {byInterface:0.###}, " +
                              $"TryGetComponent<StubZoom> {byConcrete:0.###}, " +
                              $"Camera.main (not found) {cameraMainMissing:0.###}, " +
                              $"Camera.main (found) {cameraMainFound:0.###}";
                TestContext.WriteLine(line);

                // One allocation a call, at hundreds of Shot events a tick, is the scale that started this hunt.
                Assert.That(byConcrete, Is.EqualTo(0), $"concrete TryGetComponent allocates. {line}");
                Assert.That(byInterface, Is.EqualTo(0),
                    "an interface-typed TryGetComponent now allocates where it used to be free. CombatFx.OnSimEvent " +
                    "calls one on every Shot and every Death event, so this is suddenly a real per-event cost and " +
                    $"the callers need the lookup hoisted out of the event handler. {line}");
                Assert.That(cameraMainMissing, Is.EqualTo(0),
                    $"[TS4] Camera.main now allocates when it finds nothing; CombatFx.OnSimEvent reads it per event. {line}");
                Assert.That(cameraMainFound, Is.EqualTo(0),
                    $"[TS4] Camera.main now allocates when it DOES find the camera, which is the path every battle " +
                    $"takes; CombatFx.OnSimEvent reads it per event. {line}");
            }
            finally
            {
                UnityEngine.Object.DestroyImmediate(go);
            }
        }
    }
}
