// Phase: in-game effects review (2026-10-13) — which effects a player actually sees in an ordinary match. An instrument,
// not a gate check: it opens GreyboxCorridor, enters Play with the match as it ships (scripted peer on, nothing disabled),
// listens to the very events the picture draws from (SimHost.Events.OnEvent, the EventPump) and tallies them by type and
// by who made them over four minutes of match time, writing TW_STILLS_DIR/effect-census.txt.
// The game clock is stepped by Time.captureDeltaTime so four match minutes take about one wall minute in batch.
// Its ranking is cross-checked against TW.Tests.VfxEventCensusTests (a headless bench battle): the shapes must agree.
// Env: TW_STILLS_DIR the folder (default %TEMP%/tw-vfxstills), TW_CENSUS_TICKS the match ticks (default 4800 = 4 min).
// Run by name: -runTests -testFilter TW.Tests.EffectCensus
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.UI;

namespace TW.Tests
{
    public class EffectCensus
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => UnityEngine.Object.FindFirstObjectByType<SimHost>();

        /// <summary>Who an event's slot belongs to, by class name; "" when the event names no unit. The same reading as
        /// VfxEventCensusTests.Who, kept to the events this review films.</summary>
        static string Who(SimWorld w, SimEvent e)
        {
            switch (e.Type)
            {
                case SimEventType.Shot: case SimEventType.Hit: case SimEventType.Death:
                case SimEventType.VehicleFired: case SimEventType.UnitAlight:
                    return e.A >= 0 && e.A < w.Archetype.Length ? UnitLook.Name(w.Archetype[e.A]) : "?";
                case SimEventType.Explosion: case SimEventType.AbilityFired:
                    return e.A > 0 && Enum.IsDefined(typeof(OffMapAbilityId), (short)e.A)
                        ? ((OffMapAbilityId)e.A).ToString() : "weapon " + e.A;
                default: return "";
            }
        }

        // The tally lives in statics, not in locals of the iterator: an EditMode test that enters Play is serialised
        // between frames, and a captured lambda does not survive that.
        static readonly Dictionary<SimEventType, int> ByType = new Dictionary<SimEventType, int>();
        static readonly Dictionary<string, int> ByWho = new Dictionary<string, int>();
        static SimHost tallyHost;

        static void Tally(SimEvent e)
        {
            ByType[e.Type] = ByType.TryGetValue(e.Type, out int n) ? n + 1 : 1;
            if (tallyHost == null || tallyHost.Local == null) return;
            string who = Who(tallyHost.Local.World, e);
            if (who.Length == 0) return;
            string key = e.Type + " | " + who;
            ByWho[key] = ByWho.TryGetValue(key, out int m) ? m + 1 : 1;
        }

        [UnityTest, Explicit("a measurement in a real match: run by name, with a graphics device.")]
        public IEnumerator CountsWhatFiresInAMatch()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");

            int wantTicks = 4800;
            if (int.TryParse(Environment.GetEnvironmentVariable("TW_CENSUS_TICKS"), out int fromEnv) && fromEnv > 0) wantTicks = fromEnv;
            ByType.Clear(); ByWho.Clear();
            tallyHost = Host;
            tallyHost.Events.OnEvent += Tally;

            Time.captureDeltaTime = 1f / 20f;   // one tick of game clock a frame: four match minutes in about one
            long startTick = tallyHost.Local.World.Tick;
            long ticks = 0;
            for (int f = 0; f < wantTicks * 6 + 600; f++)
            {
                yield return null;
                if (tallyHost == null || tallyHost.Local == null) break;
                ticks = (long)tallyHost.Local.World.Tick - startTick;
                if (ticks >= wantTicks) break;
            }
            Time.captureDeltaTime = 0f;
            int overrun = tallyHost != null ? tallyHost.Events.OverrunTotal : 0;
            int subs = tallyHost != null ? tallyHost.Events.SubscriberCount : 0;
            if (tallyHost != null) tallyHost.Events.OnEvent -= Tally;
            Assert.That(ticks, Is.GreaterThan(0), "the match never stepped");

            var sb = new StringBuilder();
            float minutes = ticks / 20f / 60f;
            sb.AppendLine("Effect census: counted from the sim's own events dispatched to the picture in one GreyboxCorridor");
            sb.AppendLine($"match, {ticks} ticks ({minutes:0.0} min of match time), the match as it ships (scripted peer on).");
            sb.AppendLine($"overrun {overrun}, subscribers {subs}");
            sb.AppendLine("-- by type (count, per minute) --");
            foreach (var kv in ByType.OrderByDescending(k => k.Value))
                sb.AppendLine($"{kv.Key,-22} {kv.Value,9} {kv.Value / Mathf.Max(0.0001f, minutes),10:0}");
            sb.AppendLine("-- by type and source --");
            foreach (var kv in ByWho.OrderByDescending(k => k.Value))
                sb.AppendLine($"{kv.Key,-48} {kv.Value,9} {kv.Value / Mathf.Max(0.0001f, minutes),10:0}");
            string text = sb.ToString();
            TestContext.Out.WriteLine(text);
            Debug.Log(text);

            string dir = Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-vfxstills");
            Directory.CreateDirectory(dir);
            File.WriteAllText(Path.Combine(dir, "effect-census.txt"), text, new UTF8Encoding(false));

            yield return new ExitPlayMode();
            Assert.That(ByType.Count, Is.GreaterThan(0), "no events at all");
        }
    }
}
