// Phase: B6 (implemented) — the one tooltip plate: a caps heading window, a body line and a cost line, shown a beat
// after the pointer settles on a card or an order button, at a fixed place above the bar (Dust Front's tooltips are
// plates, not cursor followers). UI Toolkit's runtime panel has no tooltip renderer of its own, so this is it.
using System;
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.UI
{
    public sealed class HudTooltip
    {
        readonly VisualElement plate;
        readonly Label title, body, cost;
        readonly float delay;
        VisualElement hovered;
        Func<string> titleOf, bodyOf, costOf;
        float since;
        bool visible;

        public HudTooltip(VisualElement plate, Label title, Label body, Label cost, float delaySeconds = HudLayout.TooltipDelaySeconds)
        {
            this.plate = plate; this.title = title; this.body = body; this.cost = cost; delay = delaySeconds;
        }

        /// <summary>Fixed text.</summary>
        public void Attach(VisualElement e, string heading, string text, string costLine = null) =>
            Attach(e, () => heading, () => text, costLine == null ? null : () => costLine);

        /// <summary>Text read when the tooltip opens, for buttons whose meaning flips with state (lock, hold fire).</summary>
        public void Attach(VisualElement e, Func<string> heading, Func<string> text, Func<string> costLine = null)
        {
            if (e == null) return;
            e.RegisterCallback<PointerEnterEvent>(_ => { hovered = e; titleOf = heading; bodyOf = text; costOf = costLine; since = Time.unscaledTime; });
            e.RegisterCallback<PointerLeaveEvent>(_ => { if (hovered == e) Clear(); });
        }

        public void Clear()
        {
            hovered = null; titleOf = bodyOf = costOf = null;
            if (visible) { plate.EnableInClassList("is-visible", false); visible = false; }
        }

        /// <summary>Once per frame on unscaled time: a tooltip must open during a tactical pause.</summary>
        public void Update()
        {
            if (hovered == null || visible) return;
            if (Time.unscaledTime - since < delay) return;
            if (hovered.panel == null || !hovered.enabledInHierarchy && hovered.resolvedStyle.display == DisplayStyle.None) { Clear(); return; }
            title.text = titleOf?.Invoke() ?? "";
            body.text = bodyOf?.Invoke() ?? "";
            string c = costOf?.Invoke();
            cost.text = c ?? "";
            cost.style.display = string.IsNullOrEmpty(c) ? DisplayStyle.None : DisplayStyle.Flex;
            plate.EnableInClassList("is-visible", true);
            visible = true;
        }
    }
}
