// Phase: B6 (2026-10-15) — the sidecar .json HudCapture writes next to a HUD still used to depend on Unity's
// unsorted FindObjectsByType order and an element's own display only, so a name shared by two documents (tabs,
// title, card) could pick the wrong one run to run with no sign anything moved.
using System.Collections.Generic;
using NUnit.Framework;
using TW.Editor;
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.Tests
{
    public class HudCaptureProbeTests
    {
        readonly List<GameObject> spawned = new List<GameObject>();

        GameObject Spawn(string name)
        {
            var go = new GameObject(name);
            spawned.Add(go);
            return go;
        }

        [TearDown]
        public void TearDown()
        {
            foreach (var go in spawned) Object.DestroyImmediate(go);
            spawned.Clear();
        }

        // [found-make-the-hudcapture-sidecar-rects-the-same-r-20261015-0]
        [Test]
        public void DocumentsVisitedInStableOrder()
        {
            var shell = Spawn("Shell").AddComponent<UIDocument>(); shell.sortingOrder = 0;
            var hud = Spawn("Hud").AddComponent<UIDocument>(); hud.sortingOrder = 10;
            var debug = Spawn("Debug").AddComponent<UIDocument>(); debug.sortingOrder = 0;

            var forward = HudCapture.InDrawOrder(new List<UIDocument> { shell, hud, debug });
            var reversed = HudCapture.InDrawOrder(new List<UIDocument> { debug, hud, shell });

            Assert.AreEqual(new[] { "Hud", "Debug", "Shell" }, new[] { forward[0].name, forward[1].name, forward[2].name },
                "[found-make-the-hudcapture-sidecar-rects-the-same-r-20261015-0] documents not visited in stable draw order");
            Assert.AreEqual(new[] { "Hud", "Debug", "Shell" }, new[] { reversed[0].name, reversed[1].name, reversed[2].name },
                "[found-make-the-hudcapture-sidecar-rects-the-same-r-20261015-0] draw order depends on input order");
        }

        // [found-make-the-hudcapture-sidecar-rects-the-same-r-20261015-0]
        [Test]
        public void HiddenAncestorSkipped()
        {
            var root = Spawn("root");
            var rootEl = new VisualElement();
            var screen = new VisualElement { style = { display = DisplayStyle.None } };
            var card = new VisualElement();
            screen.Add(card);
            rootEl.Add(screen);

            var panel = new VisualElement();
            var tabs = new VisualElement();
            panel.Add(tabs);
            rootEl.Add(panel);

            Assert.IsTrue(HudCapture.InHiddenBranch(card),
                "[found-make-the-hudcapture-sidecar-rects-the-same-r-20261015-0] element under display:none ancestor not detected as hidden");
            Assert.IsFalse(HudCapture.InHiddenBranch(tabs),
                "[found-make-the-hudcapture-sidecar-rects-the-same-r-20261015-0] visible element wrongly reported hidden");
        }

        // [found-make-the-hudcapture-sidecar-rects-the-same-r-20261015-0]
        [Test]
        public void RectJsonKeysInProbeOrder()
        {
            var rects = new Dictionary<string, Rect>();
            rects["title"] = new Rect(5, 6, 7, 8);
            rects["card"] = new Rect(1, 2, 3, 4);

            string json = HudCapture.RectsJson(1920, 1080, rects);

            Assert.AreEqual("{\"w\":1920,\"h\":1080,\"rects\":{\"card\":[1,2,3,4],\"title\":[5,6,7,8]}}", json,
                "[found-make-the-hudcapture-sidecar-rects-the-same-r-20261015-0] JSON keys not in Probes order");
        }
    }
}
