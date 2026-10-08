// Phase: tooling (review fix T34, 2026-10-09) — the PlayMode canary with units on the field.
// [T34] The gate's PlayMode canary only ever compared two EMPTY worlds for a few frames: no PlayMode test ran
// SimHost.Update / SyncEnemy / AlignWorlds with units fighting on the field, and none read host.Desync. These two do:
// one real two-world match that must stay in sync, and one that nudges a hashed field in a single world so the
// detector itself is proven live.
using System.Collections;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;
using TW.Sim;

namespace TW.Tests
{
    public class CanaryPlayTests
    {
        GameObject go;
        bool? canaryWas;

        [SetUp]
        public void SetUp()
        {
            canaryWas = SimHost.CanaryOverride;
            SimHost.CanaryOverride = true;   // two worlds over the loopback, the gate's canary
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            Time.captureDeltaTime = 0f;   // left set, every later PlayMode test runs on a fake frame time
            SimHost.CanaryOverride = canaryWas;
            if (go != null) Object.Destroy(go);
            yield return null;
        }

        SimHost MakeHost()
        {
            go = new GameObject("canary-test");
            go.SetActive(false);
            var host = go.AddComponent<SimHost>();
            host.GeneratedBattlefield = false;
            host.PlaytestMap = true;        // trenches a side, 200 m of no man's land
            host.ScriptedPeer = true;
            host.PeerAttacks = true;
            host.StartingSilver = 3000;
            host.SilverPerSecond = 10f;
            go.SetActive(true);
            return host;
        }

        int deployed;

        /// <summary>Runs the match to <paramref name="targetTick"/>: the player deploys riflemen for the first 40 ticks
        /// and sends the front garrison over the top once it has a crowd. 16 ticks a frame (the guard's ceiling).</summary>
        IEnumerator Fight(SimHost host, uint targetTick, int frameCap)
        {
            host.TimeScale = 8f;
            Time.captureDeltaTime = 0.5f;
            bool ordered = false;
            for (int f = 0; f < frameCap && host.Local.World.Tick < targetTick; f++)
            {
                uint t = host.Local.World.Tick;
                if (t < 40)
                    for (int k = 0; k < 3; k++, deployed++) host.Issue(SimCommand.Deploy(t, 0, 0));
                if (!ordered && host.Local.World.AliveCount > 20)
                {
                    short tr = host.Local.Fields.FrontTrench(0);
                    if (tr >= 0)
                    {
                        host.Issue(new SimCommand { Tick = t, Type = CommandType.TrenchAdvance, A = tr, B = 0 });
                        ordered = true;
                    }
                }
                yield return null;
            }
        }

        static int AliveOnTeam(SimWorld w, byte team)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == team) n++;
            return n;
        }

        // [T34] The path the report says nothing covers: both worlds full of units of both sides, a few hundred ticks
        // of fighting, and host.Desync read at the end.
        [UnityTest, Category("Long")]
        public IEnumerator TwoWorldsFightingStayInSync()
        {
            var host = MakeHost();
            yield return null;
            Assert.IsTrue(host.CanaryActive, "[T34] the PlayMode canary must stand up two worlds");
            Assert.IsNotNull(host.Peer, "[T34] the canary's second world is missing");

            yield return Fight(host, 600, 600);
            Assert.That(host.Local.World.Tick, Is.GreaterThanOrEqualTo(600u), "[T34] the match never reached tick 600");

            foreach (var (name, sim) in new[] { ("local", host.Local), ("peer", host.Peer) })
            {
                Assert.That(AliveOnTeam(sim.World, 0), Is.GreaterThan(0), "[T34] no player units in the " + name + " world");
                Assert.That(AliveOnTeam(sim.World, 1), Is.GreaterThan(0), "[T34] no enemy units in the " + name + " world");
            }
            Assert.That(host.Local.World.AliveCount, Is.LessThan(deployed),
                "[T34] nothing died: the two worlds never fought (deployed " + deployed + ", alive " + host.Local.World.AliveCount + ")");

            Assert.IsFalse(host.Desync, "[T34] the two worlds desynced while fighting");
        }

        // [T34] And the detector is live: one hashed field nudged in one world only must be seen. Silver accumulates,
        // so the worlds can never re-agree.
        [UnityTest, Category("Long")]
        public IEnumerator ANudgedWorldIsSeenAsADesync()
        {
            var host = MakeHost();
            yield return null;
            yield return Fight(host, 200, 400);

            LogAssert.Expect(LogType.Error, new Regex("^DESYNC at tick"));   // Compare logs it; an unexpected error fails a test
            while (!host.AlignWorlds()) yield return null;
            host.Peer.World.Silver[1] += 1;   // hashed (SimWorld.Hash), peer world only

            for (int f = 0; f < 200 && !host.Desync; f++) yield return null;
            Assert.IsTrue(host.Desync, "[T34] a hashed field changed in one world only was never seen as a desync");
        }
    }
}
