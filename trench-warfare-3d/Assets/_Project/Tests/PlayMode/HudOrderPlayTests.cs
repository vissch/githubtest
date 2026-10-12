// Phase: A3 (implemented) — an unscoped over-the-top order finds the men. Every deploy garrisons the rear trench, so
// the key must aim there and not at the empty front line (review 3, [Z011.1]).
using System.Collections;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Sim;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class HudOrderPlayTests
    {
        GameObject go, hudGo;

        [UnitySetUp]
        public IEnumerator SetUp()
        {
            HudBootstrap.Disabled = true; ShellBoot.Disabled = true;
            MatchLaunch.Current = null;
            yield return null;
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            MatchLaunch.Current = null;
            if (hudGo != null) Object.Destroy(hudGo);
            if (go != null) Object.Destroy(go);
            HudBootstrap.Disabled = false; ShellBoot.Disabled = false;
            yield return null;
        }

        /// <summary>
        /// [Z011.1] G with nothing selected must order the trench the men are in. On the playtest map seat 0 holds
        /// trench 0 at z = 60 (the reserve) and trench 1 at z = 140 (the front); deploys stop in the rear one, so an
        /// order aimed at the front trench moves nobody and the sim reports OrderFoundNoOne.
        /// </summary>
        [UnityTest]
        public IEnumerator GWithNothingSelectedSendsTheMannedTrenchOverTheTop()
        {
            go = new GameObject("hud-order-test"); go.SetActive(false);
            var host = go.AddComponent<SimHost>();
            host.GeneratedBattlefield = false; host.PlaytestMap = true;
            host.ScriptedPeer = false; host.DeterminismCanary = false;
            host.StartingSilver = 1000; host.TimeScale = 20f;
            go.SetActive(true);
            yield return null;
            Assert.That(host.Local, Is.Not.Null, "[Z011.1] the match must be built");

            short rear = host.Local.Fields.RearTrench(0), front = host.Local.Fields.FrontTrench(0);
            Assert.That(rear, Is.Not.EqualTo(front), "[Z011.1] the playtest map gives seat 0 two fire trenches");

            // one deploy a tick: SlotCooldown rejects a second in the same tick
            int deployed = 0;
            for (int f = 0; f < 600 && deployed < 5; f++)
            {
                host.Issue(SimCommand.Deploy(host.Local.World.Tick, 0, 0));
                deployed = Alive(host.Local.World);
                yield return null;
            }
            Assert.That(Alive(host.Local.World), Is.GreaterThanOrEqualTo(5), "[Z011.1] five riflemen must reach the field");

            for (int f = 0; f < 600 && host.Local.Fields.Trenches[rear].GarrisonCount < 5; f++) yield return null;
            Assert.That(host.Local.Fields.Trenches[rear].GarrisonCount, Is.EqualTo(5), "[Z011.1] every reinforcement garrisons the rear trench");
            Assert.That(host.Local.Fields.Trenches[front].GarrisonCount, Is.EqualTo(0), "[Z011.1] the front trench holds nobody until a player orders an advance");

            hudGo = new GameObject("hud-order-test-hud"); hudGo.SetActive(false);
            var hud = hudGo.AddComponent<HudController>();   // inactive: no OnEnable, no UI tree, no selection
            hud.Host = host;
            Assert.That(hud.Interactive, Is.True, "[Z011.1] no modal must hold the input focus");

            int left = 0, foundNoOne = 0;
            System.Action<SimEvent> watch = e =>
            {
                if (e.Type == SimEventType.UnitLeftTrench && e.B == rear) left++;
                else if (e.Type == SimEventType.OrderFoundNoOne) foundNoOne++;
            };
            host.Events.OnEvent += watch;
            try
            {
                hud.OrderFront(CommandType.TrenchAdvance);   // no selection: the unscoped branch
                for (int f = 0; f < 120 && left == 0 && foundNoOne == 0; f++) yield return null;
            }
            finally { host.Events.OnEvent -= watch; }

            Assert.That(foundNoOne, Is.EqualTo(0), "[Z011.1] the order must not go to an empty trench (OrderFoundNoOne)");
            Assert.That(left, Is.GreaterThan(0), "[Z011.1] men of the manned trench must go over the top");
        }

        static int Alive(SimWorld w)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == 0) n++;
            return n;
        }
    }
}
