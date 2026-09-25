// Phase: B6 (implemented) — the small card beside the cursor when it rests on a unit (owner, 2026-09-24): the face,
// in the mood its health puts it in; the name; what it is doing now (UnitStatus, coloured by how bad it is); a health
// bar with the numbers. Over a knot of men it is the knot's card instead: a count badge on the face of the commonest
// type, the types, the two commonest states with counts, the mean health, and a hint that a click takes the lot.
// The same card shows a trench's garrison when the cursor rests on the trench's men badge (GarrisonStats).
// Fades and slides in (USS transition on .hud-hover--shown) after a short rest, so panning over the field does not
// strobe it; once up it follows the cursor from unit to unit at once, and fades out when the cursor leaves the men.
// Never takes the mouse (picking Ignore
// everywhere, or the HUD's click mask would see it under the cursor and hide it). Texts are set only when they change.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim;

namespace TW.UI
{
    public sealed class HoverCard
    {
        public const float ShowAfterSeconds = 0.12f, OffsetX = 18f, OffsetY = 16f, EdgePx = 8f, FallbackW = 230f, FallbackH = 90f;

        readonly VisualElement card, frame, portrait, fill;
        readonly Label count, name, state, hp, hint;
        readonly int[] stateCounts = new int[UnitStatus.StateCount];
        readonly Dictionary<byte, int> typeCounts = new Dictionary<byte, int>();
        string[] garrisonTitles = new string[0];
        int catTitleKey; string catTitle;
        static readonly System.Func<int, bool, string> CategoryHint = (n, ours) => "CLICK: SELECT   SHIFT: ADD   TWICE: GO THERE";
        static readonly System.Func<int, bool, string> KnotHint = (n, ours) => ours ? "CLICK: ALL " + n + "   ALT: PICK ONE" : "CLICK: INSPECT ALL " + n + "   ALT: ONE";
        static readonly System.Func<int, bool, string> PinnedHint = (n, ours) => "PINNED MEN WILL NOT GO OVER THE TOP";
        static readonly System.Func<int, bool, string> ReadyHint = (n, ours) => "READY TO GO OVER THE TOP";

        long key; int keyCount = -1; float restingSince; bool shown;
        // what is drawn now, so a text is rebuilt only when its numbers change
        int shownCount = -1, shownHpCur = -1, shownHpMax = -1, shownPct = -1, shownTypes = -1;
        int shownStateHash = -1, shownHint = -2;
        byte shownArch = 255; Mood shownMood = (Mood)255; string shownState; bool shownEnemy;

        public HoverCard(VisualElement root)
        {
            card = root?.Q("hover-card");
            if (card == null) return;
            card.pickingMode = PickingMode.Ignore;
            card.Clear();
            frame = Make("hud-hover__frame", card);
            portrait = Make("hud-hover__portrait", frame);
            count = MakeLabel("hud-hover__count", frame);
            var info = Make("hud-hover__info", card);
            name = MakeLabel("hud-hover__name", info);
            state = MakeLabel("hud-hover__state", info);
            var track = Make("hud-hover__hp-track", info);
            fill = Make("hud-hover__hp-fill", track);
            hp = MakeLabel("hud-hover__hp", info);
            hint = MakeLabel("hud-hover__hint", info);
            card.style.display = DisplayStyle.None;
        }

        static VisualElement Make(string cls, VisualElement parent)
        {
            var e = new VisualElement { pickingMode = PickingMode.Ignore }; e.AddToClassList(cls); parent.Add(e); return e;
        }

        static Label MakeLabel(string cls, VisualElement parent)
        {
            var l = new Label { pickingMode = PickingMode.Ignore }; l.AddToClassList(cls); parent.Add(l); return l;
        }

        public void Hide()
        {
            keyCount = -1;
            if (card == null || !shown) return;
            shown = false;
            card.RemoveFromClassList("hud-hover--shown");   // fades out where it stands (opacity transition)
        }

        /// <summary>Show the card for this clump (indices into units, the nearest first) beside the cursor (HUD px).</summary>
        /// <param name="pick">Under Alt, which man of the knot this is (0-based) and knotSize how many it has: the badge reads 2/7.</param>
        public void Show(SimWorld w, List<ScreenUnit> units, List<int> clump, Vector2 cursor, Vector2 hudSize, int pick = -1, int knotSize = 0)
        {
            if (card == null || w == null || clump.Count == 0) { Hide(); return; }
            var lead = units[clump[0]];
            if (!Rested(((long)lead.Slot << 16) | lead.Gen, clump.Count)) return;
            if (clump.Count == 1) ShowOne(w, lead, knotSize > 1 ? (pick + 1) * 1000 + knotSize : 1); else ShowMany(w, units, clump);
            Reveal(!lead.Ours, cursor, hudSize);
        }

        /// <summary>Show trench t's garrison (the cursor is on its men badge).</summary>
        public void ShowGarrison(GarrisonStats g, int t, Vector2 cursor, Vector2 hudSize)
        {
            if (card == null || g == null || t < 0 || t >= g.Trenches || g.Men(t) == 0) { Hide(); return; }
            if (!Rested(-1 - t, g.Men(t))) return;
            g.CopyStates(t, stateCounts);
            bool pinned = g.CountOf(t, UnitState.Pinned) > 0;
            if (garrisonTitles.Length < g.Trenches) garrisonTitles = new string[g.Trenches];
            string title = garrisonTitles[t] ??= "TRENCH " + (t + 1) + "  ·  GARRISON";
            Summarise(g.Commonest(t), g.Types(t), g.Men(t), g.MeanHp(t), true, -1000 - t, title,
                      pinned ? -20 : -21, pinned ? PinnedHint : ReadyHint);
            Reveal(false, cursor, hudSize);
        }

        /// <summary>One troop category of trench t (the cursor is on its chip in the trench's order cluster).</summary>
        public void ShowCategory(SimWorld w, int t, int archetype, Vector2 cursor, Vector2 hudSize)
        {
            if (card == null || w == null || t < 0 || archetype < 0) { Hide(); return; }
            System.Array.Clear(stateCounts, 0, stateCounts.Length);
            int n = 0; float sum = 0f;
            for (int i = 0; i < w.HighWater; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0 || (w.Team[i] & 1) != 0) continue;
                if (w.TrenchId[i] != t || w.Archetype[i] != archetype) continue;
                n++; sum += Mathf.Clamp01(w.Hp[i] / Mathf.Max(1f, w.MaxHp[i]));
                stateCounts[(int)UnitStatus.Of(w, i)]++;
            }
            if (n == 0) { Hide(); return; }
            if (!Rested(-100000 - t * 256 - archetype, n)) return;
            int titleKey = -3000 - t * 256 - archetype;
            if (titleKey != catTitleKey) { catTitleKey = titleKey; catTitle = HudText.Name((byte)archetype).ToUpperInvariant() + "  ·  TRENCH " + (t + 1); }
            Summarise((byte)archetype, 1, n, sum / n, true, titleKey, catTitle, -30, CategoryHint);
            Reveal(false, cursor, hudSize);
        }

        /// <summary>
        /// The first card waits for the cursor to rest; once one is up, the next unit's card replaces it at once.
        /// k identifies what is under the cursor (a unit's slot and generation, or a trench), n how many.
        /// </summary>
        bool Rested(long k, int n)
        {
            if (keyCount < 0) restingSince = Time.unscaledTime;   // the cursor has just come onto something
            key = k; keyCount = n;
            return shown || Time.unscaledTime - restingSince >= ShowAfterSeconds;
        }

        void Reveal(bool enemy, Vector2 cursor, Vector2 hudSize)
        {
            if (shownEnemy != enemy) { shownEnemy = enemy; card.EnableInClassList("hud-hover--enemy", enemy); }
            Place(cursor, hudSize);
            if (!shown)
            {
                shown = true;
                // laid out (display) one frame before it is shown, so the opacity and slide transition from the hidden state
                if (card.style.display != DisplayStyle.Flex) card.style.display = DisplayStyle.Flex;
                else card.AddToClassList("hud-hover--shown");
            }
            else if (!card.ClassListContains("hud-hover--shown")) card.AddToClassList("hud-hover--shown");
        }

        void ShowOne(SimWorld w, ScreenUnit u, int countKey)
        {
            int slot = u.Slot;
            float cur = w.Hp[slot], max = Mathf.Max(1f, w.MaxHp[slot]), k = Mathf.Clamp01(cur / max);
            SetFace(u.Archetype, k);
            SetCount(countKey);
            SetTypes(1, u.Archetype, u.Ours);
            var s = UnitStatus.Of(w, slot);
            shownStateHash = -1;
            SetState(UnitStatus.Word(s), UnitStatus.Tone(s));
            int c = Mathf.CeilToInt(cur), m = Mathf.CeilToInt(max);
            if (c != shownHpCur || m != shownHpMax || shownPct != -2)
            {
                shownHpCur = c; shownHpMax = m; shownPct = -2;
                hp.text = "HP " + c + " / " + m;
                fill.style.width = Length.Percent(k * 100f);
            }
            int hintKey = countKey >= 1000 ? -3 : -1;
            if (hintKey != shownHint) { shownHint = hintKey; SetHint(hintKey == -3 ? "WHEEL: NEXT   ALT+CLICK: SELECT" : null); }
        }

        void ShowMany(SimWorld w, List<ScreenUnit> units, List<int> clump)
        {
            System.Array.Clear(stateCounts, 0, stateCounts.Length);
            typeCounts.Clear();
            float sum = 0f; byte common = units[clump[0]].Archetype; int commonN = 0;
            foreach (int i in clump)
            {
                var u = units[i];
                sum += Mathf.Clamp01(w.Hp[u.Slot] / Mathf.Max(1f, w.MaxHp[u.Slot]));
                stateCounts[(int)UnitStatus.Of(w, u.Slot)]++;
                typeCounts.TryGetValue(u.Archetype, out int n); typeCounts[u.Archetype] = ++n;
                if (n > commonN) { commonN = n; common = u.Archetype; }
            }
            bool ours = units[clump[0]].Ours;
            Summarise(common, typeCounts.Count, clump.Count, sum / clump.Count, ours, 0, null, clump.Count * 2 + (ours ? 0 : 1), KnotHint);
        }

        /// <summary>
        /// A group's card from its tallies (stateCounts already filled). titleKey/title override the name line (0/null:
        /// the type's name); hintKey identifies the hint so hintFor(n, ours) runs only when it changes.
        /// </summary>
        void Summarise(byte common, int typeCount, int n, float mean, bool ours, int titleKey, string title, int hintKey, System.Func<int, bool, string> hintFor)
        {
            SetFace(common, mean);
            SetCount(n);
            if (title != null) SetTitle(titleKey, title); else SetTypes(typeCount, common, ours);
            int stateHash = 17;
            for (int s = 0; s < stateCounts.Length; s++) stateHash = stateHash * 31 + stateCounts[s];
            if (stateHash != shownStateHash)
            {
                shownStateHash = stateHash;
                string summary = UnitStatus.Summary(stateCounts, out var tone);
                SetState(summary, tone);
            }
            int pct = Mathf.RoundToInt(mean * 100f);
            if (pct != shownPct)
            {
                shownPct = pct; shownHpCur = shownHpMax = -1;
                hp.text = "AVG HP " + pct + "%";
                fill.style.width = Length.Percent(pct);
            }
            if (hintKey != shownHint) { shownHint = hintKey; SetHint(hintFor(n, ours)); }   // built only when it changes
        }

        void SetFace(byte archetype, float health)
        {
            var mood = health > 0.6f ? Mood.Neutral : health > 0.3f ? Mood.Wounded : Mood.Critical;
            if (archetype == shownArch && mood == shownMood) return;
            shownArch = archetype; shownMood = mood;
            string n = UnitArt.NameOf(archetype);
            var tex = UnitArt.HasFace(n) ? UnitArt.State(n, mood) : UnitArt.Full(n);
            portrait.style.backgroundImage = tex != null ? new StyleBackground(tex) : new StyleBackground(StyleKeyword.None);
        }

        /// <summary>n: 1 none, 2..999 "×n" for a knot, pick*1000+size "pick/size" for one man of a knot under Alt.</summary>
        void SetCount(int n)
        {
            if (n == shownCount) return;
            shownCount = n;
            count.text = n >= 1000 ? (n / 1000) + "/" + (n % 1000) : n > 1 ? "×" + n : "";
            count.style.display = n != 1 ? DisplayStyle.Flex : DisplayStyle.None;
            shownTypes = -1;   // the name line depends on the count too
        }

        void SetTitle(int k, string text)
        {
            if (k == shownTypes) return;
            shownTypes = k; name.text = text;
        }

        void SetTypes(int types, byte archetype, bool ours)
        {
            int k = types * 1000 + archetype * 2 + (ours ? 0 : 1);
            if (k == shownTypes) return;
            shownTypes = k;
            string what = types == 1 ? HudText.Name(archetype).ToUpperInvariant() : "MIXED  ·  " + types + " TYPES";
            name.text = ours ? what : what + "  ·  ENEMY";
        }

        void SetState(string text, StateTone tone)
        {
            if (text == shownState) return;
            shownState = text;
            state.text = text;
            state.EnableInClassList("hud-hover__state--warn", tone == StateTone.Warn);
            state.EnableInClassList("hud-hover__state--alarm", tone == StateTone.Alarm);
        }

        void SetHint(string text)
        {
            bool on = !string.IsNullOrEmpty(text);
            hint.style.display = on ? DisplayStyle.Flex : DisplayStyle.None;
            if (on && hint.text != text) hint.text = text;
        }

        /// <summary>Below-right of the cursor; flipped to the other side of it where the card would leave the screen.</summary>
        void Place(Vector2 cursor, Vector2 hudSize)
        {
            float w = card.resolvedStyle.width, h = card.resolvedStyle.height;
            if (float.IsNaN(w) || w <= 0f) w = FallbackW;
            if (float.IsNaN(h) || h <= 0f) h = FallbackH;
            float x = cursor.x + OffsetX, y = cursor.y + OffsetY;
            if (x + w > hudSize.x - EdgePx) x = cursor.x - OffsetX - w;
            if (y + h > hudSize.y - EdgePx) y = cursor.y - OffsetY - h;
            card.style.left = Mathf.Max(EdgePx, x); card.style.top = Mathf.Max(EdgePx, y);
        }
    }
}
