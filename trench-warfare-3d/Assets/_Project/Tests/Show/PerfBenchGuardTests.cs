// Phase: tooling (code review 2026-10-04, F6 and F7). Two faults of the bench that no report ever showed: only the
// settle stage had a deadline and a throw left the stage machine stuck, so a run that stopped ticking held the editor
// or the player for ever with no report; and an armed run left the previous report in place (a hang read as the last
// run) and wrote hash_start as zeros when the window never opened, so two failed runs compared as the same battle.
using System;
using System.IO;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Perf;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class PerfBenchGuardTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public;

        GameObject go;
        PerfBench bench;
        string outPath;

        [SetUp]
        public void Arm()
        {
            outPath = Path.Combine(Path.GetTempPath(), "tw-f6f7-" + Guid.NewGuid().ToString("N") + ".json");
            go = new GameObject("PerfBenchGuardTests");
            bench = go.AddComponent<PerfBench>();
            bench.Options = BenchOptions.Parse("quit=0");
            bench.Options.Out = outPath;
        }

        [TearDown]
        public void Clean()
        {
            if (go != null) UnityEngine.Object.DestroyImmediate(go);
            if (File.Exists(outPath)) File.Delete(outPath);
            CameraShake.Strength = 1f;
            CombatFx.ShowOverlays = true;
        }

        static FieldInfo Field(string name)
        {
            var f = typeof(PerfBench).GetField(name, Any);
            Assert.IsNotNull(f, "[F6] PerfBench." + name + " is gone");
            return f;
        }

        static void Call(object target, string name, params object[] args)
        {
            var m = typeof(PerfBench).GetMethod(name, Any);
            Assert.IsNotNull(m, "[F6] PerfBench." + name + " is gone");
            try { m.Invoke(target, args); }
            catch (TargetInvocationException e) { throw e.InnerException; }
        }

        /// <summary>Puts the bench in a stage by name, reflecting on the private Stage enum.</summary>
        void SetStage(string name)
        {
            var stage = typeof(PerfBench).GetNestedType("Stage", BindingFlags.NonPublic | BindingFlags.Public);
            Assert.IsNotNull(stage, "[F6] PerfBench.Stage is gone");
            Field("stage").SetValue(bench, Enum.Parse(stage, name));
        }

        string Stage() => Field("stage").GetValue(bench).ToString();

        [Test]
        public void EveryStageHasADeadline()   // [F6]
        {
            SetStage("Warm");
            Field("stageDeadline").SetValue(bench, Time.realtimeSinceStartup - 1f);
            Call(bench, "LateUpdate");
            Assert.IsTrue(File.Exists(outPath), "[F6] a stage that ran past its deadline wrote no report at all");
            Assert.AreEqual(3, PerfBench.LastExitCode, "[F6] a stage that ran past its deadline did not end the run");
            StringAssert.Contains("\"ended_in\": \"Warm\"", File.ReadAllText(outPath),
                "[F6] the report does not say which stage timed out");
        }

        [Test]
        public void AThrowInAStageEndsTheRun()   // [F6]
        {
            SetStage("Warm");                                   // Warm dereferences host, which is null here
            Field("stageDeadline").SetValue(bench, Time.realtimeSinceStartup + 600f);
            UnityEngine.TestTools.LogAssert.ignoreFailingMessages = true;
            Call(bench, "LateUpdate");                           // must not throw out of LateUpdate
            UnityEngine.TestTools.LogAssert.ignoreFailingMessages = false;
            Assert.IsTrue(File.Exists(outPath), "[F6] a stage that threw wrote no report");
            Assert.AreNotEqual(0, PerfBench.LastExitCode, "[F6] a stage that threw reported a good run");
            Assert.AreEqual("Done", Stage(), "[F6] a stage that threw left the stage machine where it was");
        }

        [Test]
        public void ANeverOpenedWindowWritesHashStartNull()   // [F7]
        {
            Call(bench, "Finish", 2, "[F7] the bad-option path");
            string json = File.ReadAllText(outPath);
            StringAssert.Contains("\"hash_start\": null", json,
                "[F7] a run whose window never opened wrote a hash_start it never took");
            StringAssert.DoesNotContain("0000000000000000", json,
                "[F7] a run whose window never opened wrote zeros as its hash");
        }

        [Test]
        public void TheShakeComesBackAsItWasFound()   // [F14]
        {
            CameraShake.Strength = 0.7f;
            Call(bench, "TakeCamera");
            Call(bench, "TakeCamera");   // stands for Approach's own call: keepShake must not be overwritten with 0
            Assert.AreEqual(0f, CameraShake.Strength, "[F14] the shake should be off while the bench holds the camera");
            Call(bench, "Finish", 0, "...");
            Assert.AreEqual(0.7f, CameraShake.Strength, "[F14] a second save overwrote the shake that was there with 0");
        }

        [Test]
        public void TheOverlaysComeBackWhenTheRunStays()   // [F14]
        {
            bench.Options = BenchOptions.Parse("quit=0 shot_tick=0 shot_hud=0");
            bench.Options.Out = outPath;
            Call(bench, "HideHud");
            Assert.IsFalse(CombatFx.ShowOverlays, "[F14] HideHud did not hide the overlays");
            Call(bench, "Finish", 0, "...");
            Assert.IsTrue(CombatFx.ShowOverlays,
                "[F14] a run that stayed (quit=0) left the overlays hidden: nothing ever put them back");
        }

        [Test]
        public void ArmingARunRemovesTheOldReport()   // [F7]
        {
            File.WriteAllText(outPath, "{ \"this\": \"is the run before\" }");
            PerfBench.LastResultPath = outPath;
            PerfBench.LastExitCode = 0;
            Call(null, "ClearStaleReport", bench.Options);
            Assert.IsFalse(File.Exists(outPath), "[F7] arming a run left the previous report where a reader finds it");
            Assert.AreEqual("", PerfBench.LastResultPath, "[F7] the last report's path still points at the run before");
            Assert.AreEqual(-1, PerfBench.LastExitCode, "[F7] the last exit code is still the run before's");
        }
    }
}
