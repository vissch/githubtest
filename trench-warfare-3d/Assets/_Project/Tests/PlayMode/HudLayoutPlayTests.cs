// Phase: B6 (implemented) — the HUD laid out on a real panel: the bar fits, the cards are big enough to read, and the
// click mask answers yes over chrome and no over the field. Layout needs a panel, so this is PlayMode; it makes its
// own UIDocument from the shipped PanelSettings and UXML, with no SimHost and no scene beyond the test's own.
// Also the allocation check claude-68 asked for: binding changing values into the cards for many frames must not
// grow the heap by more than a fixed small amount (GC.GetAllocatedBytesForCurrentThread deltas, warm-up first).
using System.Collections;
using NUnit.Framework;
using Unity.Collections;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.UIElements;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class HudLayoutPlayTests
    {
        GameObject go;

        [UnitySetUp]
        public IEnumerator SetUp() { HudBootstrap.Disabled = true; yield return null; }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            if (go != null) Object.Destroy(go);
            HudBootstrap.Disabled = false;
            yield return null;
        }

        static RosterEntry[] DefaultRoster()
        {
            var na = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            RosterEntry.FillDefault(na, 0);
            var r = na.ToArray(); na.Dispose(); return r;
        }

        HudRefs MakeHud(out UIDocument doc)
        {
            var panel = Resources.Load<PanelSettings>(HudBootstrap.PanelResource);
            var tree = Resources.Load<VisualTreeAsset>(HudBootstrap.HudResource);
            Assert.That(panel, Is.Not.Null, "DustFrontPanel.asset missing from Resources (TW/UI/Create Panel Settings)");
            Assert.That(tree, Is.Not.Null, "Hud/BattleHud.uxml missing from Resources");
            go = new GameObject("hud-test");
            doc = go.AddComponent<UIDocument>();
            doc.panelSettings = panel; doc.visualTreeAsset = tree;
            return HudView.Build(doc.rootVisualElement, Resources.Load<VisualTreeAsset>("Hud/UnitCard"), DefaultRoster(), new[] { 150, 120 });
        }

        [UnityTest]
        public IEnumerator TheBarFitsAndTheCardsAreReadable()
        {
            var refs = MakeHud(out var doc);
            yield return null; yield return null;
            var root = doc.rootVisualElement;
            if (SystemInfo.graphicsDeviceType == UnityEngine.Rendering.GraphicsDeviceType.Null && root.resolvedStyle.width <= 0f)
                Assert.Ignore("no layout without a graphics device in this batch run");
            float w = root.resolvedStyle.width, h = root.resolvedStyle.height;
            Assert.That(w, Is.GreaterThan(0f)); Assert.That(h, Is.GreaterThan(0f));
            var bar = refs.Bar.worldBound;
            var diag = new System.Text.StringBuilder();
            foreach (var c in refs.Cards) { var hh = c.Root.parent; diag.Append("[").Append(hh.GetType().Name).Append(" w=").Append(hh.worldBound.width).Append(" mr=").Append(hh.resolvedStyle.marginRight).Append(" cls=").Append(string.Join(",", hh.GetClasses())).Append(" par=").Append(hh.parent != null ? hh.parent.name : "-").Append("] "); }
            Assert.That(bar.width, Is.GreaterThanOrEqualTo(HudLayout.BarWidth() - 1f), "the bar is at least what its cards consume: " + diag);
            Assert.That(bar.width, Is.LessThanOrEqualTo(w), "the bar fits the reference width");
            Assert.That(bar.xMax, Is.LessThanOrEqualTo(w + 0.5f)); Assert.That(bar.yMax, Is.LessThanOrEqualTo(h + 0.5f));
            foreach (var c in refs.Cards) Assert.That(c.Root.worldBound.width, Is.GreaterThanOrEqualTo(70f), $"{c.Title} card is {c.Root.worldBound.width} px wide");
            var mm = refs.MinimapBezel.worldBound;
            Assert.That(mm.xMax, Is.LessThanOrEqualTo(w + 0.5f)); Assert.That(mm.y, Is.GreaterThanOrEqualTo(0f));
        }

        [UnityTest]
        public IEnumerator PickAnswersForChromeAndNotForTheField()
        {
            var refs = MakeHud(out var doc);
            yield return null; yield return null;
            var root = doc.rootVisualElement;
            var panel = root.panel;
            Assert.That(panel, Is.Not.Null);
            if (root.resolvedStyle.width <= 0f) Assert.Ignore("no layout without a graphics device in this batch run");
            var barCentre = refs.Bar.worldBound.center;
            var hit = panel.Pick(barCentre);
            Assert.That(hit, Is.Not.Null.And.Not.SameAs(root), "the bar is chrome: a click on it must be the HUD's");
            var field = new Vector2(root.resolvedStyle.width * 0.5f, root.resolvedStyle.height * 0.4f);
            var miss = panel.Pick(field);
            Assert.That(miss == null || miss == root, Is.True, $"the middle of the screen must reach the field, but Pick returned '{miss?.name}'");
        }

        [UnityTest]
        public IEnumerator BindingChangingValuesAllocatesAlmostNothing()
        {
            var refs = MakeHud(out var doc);
            yield return null;
            float tick = SimConfig.Default.TickSeconds;
            void Frame(int f)
            {
                int silver = 100 + (f * 7) % 400;
                HudView.BindGauges(refs, silver, 2f, 20 + f % 9, 30 - f % 7, f / 60, 1f, false);
                for (int i = 0; i < refs.Cards.Count; i++)
                {
                    var c = refs.Cards[i];
                    if (c.IsSupport) HudView.BindSupportCard(c, silver, (f + i * 13) % 300, 300, false, false, tick);
                    else HudView.BindCard(c, silver, (f + i * 17) % 200, 200, true, false, tick, f % 4);
                }
            }
            for (int f = 0; f < 200; f++) Frame(f);   // warm-up: caches grow once
            long before = System.GC.GetAllocatedBytesForCurrentThread();
            for (int f = 200; f < 500; f++) Frame(f);
            long perFrame = (System.GC.GetAllocatedBytesForCurrentThread() - before) / 300;
            Assert.That(perFrame, Is.LessThan(64), $"binding allocates {perFrame} bytes a frame with silver, cooldowns and men changing every frame (the IMGUI HUD: 13,302)");
        }
    }
}
