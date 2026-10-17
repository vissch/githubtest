// Phase: found-syncenemy-re-copies-scriptedenemy-s-fields-o-20261017-1 - the frame's catch-up loop called SyncEnemy
// once per stepped tick instead of once a frame; SimHost.StepFrame pulls the copy out of the loop.
using System.IO;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;

namespace TW.Tests
{
    public class SimHostCatchUpTests
    {
        [Test]
        public void Knobs_Are_Copied_Once_However_Many_Ticks_The_Frame_Steps()
        {
            float tick = 1f / 30f;
            float acc = 5.5f / 30f;
            int syncs = 0, steps = 0;
            SimHost.StepFrame(ref acc, tick, 8, () => syncs++, () => { steps++; return true; });
            Assert.AreEqual(1, syncs);
            Assert.AreEqual(5, steps);
            Assert.AreEqual(0.5f * tick, acc, 1e-4f);
        }

        [Test]
        public void A_Quiet_Frame_Still_Copies_Them_Once()
        {
            float tick = 1f / 30f;
            float acc = 0f;
            int syncs = 0, steps = 0;
            SimHost.StepFrame(ref acc, tick, 8, () => syncs++, () => { steps++; return true; });
            Assert.AreEqual(1, syncs);
            Assert.AreEqual(0, steps);
        }

        [Test]
        public void The_Guard_Caps_The_Catch_Up()
        {
            float tick = 1f / 30f;
            float acc = 20f * tick;
            int syncs = 0, steps = 0;
            SimHost.StepFrame(ref acc, tick, 8, () => syncs++, () => { steps++; return true; });
            Assert.AreEqual(1, syncs);
            Assert.AreEqual(8, steps);
        }

        [Test]
        public void A_Stall_Leaves_The_Backlog_Alone()
        {
            float tick = 1f / 30f;
            float acc = 5f * tick;
            float startAcc = acc;
            int syncs = 0, steps = 0;
            SimHost.StepFrame(ref acc, tick, 8, () => syncs++, () => false);
            Assert.AreEqual(1, syncs);
            Assert.AreEqual(0, steps);
            Assert.AreEqual(startAcc, acc, 1e-6f);
        }

        [Test]
        public void The_Catch_Up_Loop_Does_Not_Sync_The_Enemy()
        {
            string src = File.ReadAllText(Path.Combine(Application.dataPath, "_Project/Presentation/Core/SimHost.cs"));
            int from = src.IndexOf("public static int StepFrame(", System.StringComparison.Ordinal);
            Assert.GreaterOrEqual(from, 0, "SimHost.cs has no StepFrame (renamed? this test reads it by name)");
            int to = src.IndexOf("\n        }", from, System.StringComparison.Ordinal);
            Assert.Greater(to, from, "StepFrame has no end");
            string body = src.Substring(from, to - from);
            Assert.Less(body.IndexOf("sync()", System.StringComparison.Ordinal), body.IndexOf("while (", System.StringComparison.Ordinal),
                "the enemy's knobs are copied before the loop, once a frame");
        }
    }
}
