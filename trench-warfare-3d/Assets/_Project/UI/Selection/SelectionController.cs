// Phase: B6 (implemented) — selecting units on the field, Dust Front's way (owner, 2026-09-23; research in the session
// log): left click picks one, a left drag draws a thin crimson box and takes our units inside it on release, a double
// click takes every unit of that type on the screen, Shift adds or toggles, Ctrl removes, a click on empty ground
// clears. Where men stand in a knot on the screen (UnitPicker.Clump) a click takes the whole knot; holding Alt narrows
// it to one man (the nearest, and the wheel steps through the knot instead of zooming) for Alt+click. Resting the
// cursor on a unit or a knot shows the hover card (HoverCard) and the select cursor (SelectCursor). Ctrl+digit
// stores the selection as a group, Shift+digit recalls it and a second Shift+digit within 0.4 s
// centres the camera on it (the owner kept 1-8 for deploying). Tab steps the camera through the selection. An enemy can
// be clicked to inspect it but never boxed or grouped. Selection gives no orders: the trenches keep the orders.
// Two more places show the same card and counts, where orders are given: a trench's men badge (its garrison, and who
// is pinned and will not go over the top) and a strike being aimed (AimReadout: the enemy under it, ours in reach).
// Ignores the mouse over HUD chrome, while a support ability is being aimed, and when the field does not own the input.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Match;

namespace TW.UI
{
    public sealed class SelectionController : System.IDisposable, TrenchOrderCluster.ISelection
    {
        public const float DragPx = 6f, DoubleClickSeconds = 0.35f, GroupDoubleTapSeconds = 0.4f;

        readonly SimHost host;
        readonly TacticalCamera cam;
        readonly System.Func<TestPanel> panel;
        readonly AimReadout aimReadout;
        /// <summary>The trench order buttons (their men badges answer the cursor with the garrison card); set by the HUD.</summary>
        public TrenchOrderCluster Trenches;
        int scopeFrame = -1, scopeVersion = -1, scopeTrench = -1, scopeMask;
        public readonly GarrisonStats Garrison;
        readonly VisualElement marquee;
        readonly HoverCard hoverCard;
        public readonly SelectionModel Model = new SelectionModel();
        public readonly UnitPicker Picker = new UnitPicker();
        readonly SelectionMarkers markers;
        readonly DeathMarks deathMarks;
        /// <summary>The skulls where men die (DeathMarks).</summary>
        public DeathMarks Deaths => deathMarks;
        readonly List<int> hits = new List<int>();
        readonly List<int> hoverClump = new List<int>();   // what the card and the hover brackets show (one man under Alt)
        readonly List<int> fullClump = new List<int>();    // the whole knot under the cursor
        readonly SelectCursor cursor = new SelectCursor();
        UnitHandle altPick; int altIndex; bool altHeld;
        readonly List<ScreenUnit> selectedNow = new List<ScreenUnit>();
        readonly Dictionary<UnitHandle, int> index = new Dictionary<UnitHandle, int>();

        bool pressing, dragging; Vector2 pressAt;
        bool armedLastTick;   // TestPanel fires and disarms on the press, before we run: that press is the strike's, not ours
        readonly System.Func<UnitHandle, bool> alive;   // cached: Prune every frame without a new delegate
        float lastClickTime = -10f; UnitHandle lastClicked;
        int lastGroupKey = -1; float lastGroupTime = -10f;
        int tabCursor;
        public ScreenUnit? Hovered { get; private set; }
        /// <summary>For captures: where the cursor is taken to be (screen px) instead of the mouse; null = the mouse.</summary>
        public Vector2? CursorOverride;
        /// <summary>This frame's selected units as the screen sees them (the panel reads positions and types here).</summary>
        public IReadOnlyList<ScreenUnit> SelectedNow => selectedNow;

        public SelectionController(SimHost host, TacticalCamera cam, VisualElement root, System.Func<TestPanel> panel, GarrisonStats garrison = null)
        {
            this.host = host; this.cam = cam; this.panel = panel; Garrison = garrison ?? new GarrisonStats();
            alive = Alive;
            aimReadout = new AimReadout(root);
            markers = new SelectionMarkers(root, ToHud);
            deathMarks = new DeathMarks(root, host, ToHud);
            marquee = root?.Q("marquee"); hoverCard = new HoverCard(root);
            if (marquee != null) marquee.style.display = DisplayStyle.None;
            HudBridge.WheelClaimed = WantsWheel;
        }

        public void Dispose()
        {
            if (HudBridge.WheelClaimed == (System.Func<bool>)WantsWheel) HudBridge.WheelClaimed = null;
            markers.Dispose(); cursor.Dispose(); deathMarks?.Dispose();
        }

        /// <summary>The camera asks before it zooms: Alt over a knot steps through it instead (read a frame late, harmless).</summary>
        bool WantsWheel() => altHeld && fullClump.Count > 1;

        SimWorld World => host != null && host.Local != null ? host.Local.World : null;

        bool Alive(UnitHandle h)
        {
            var w = World;
            return w != null && w.IsAlive(h.Slot) && w.Generation[h.Slot] == h.Gen;
        }

        /// <summary>Once per frame from HudController: input, the marquee and hover, then the markers on the field.</summary>
        public void Tick(bool interactive)
        {
            var w = World; var unity = Camera.main;
            if (w == null || unity == null || host.Presenter == null) return;
            Model.Prune(alive);
            Picker.Build(w, host.Presenter, host.Local.Map, unity, cam != null ? cam.CurrentZoom : 60f);
            index.Clear();
            for (int i = 0; i < Picker.Units.Count; i++) index[Picker.Units[i].Handle] = i;

            var mouse = Mouse.current; var kb = Keyboard.current;
            var p = panel?.Invoke();
            var armed = p != null ? p.Armed : OffMapAbilityId.None;
            bool fieldOwnsInput = interactive && InputFocus.Gameplay && armed == OffMapAbilityId.None;
            bool strikeClick = armedLastTick; armedLastTick = armed != OffMapAbilityId.None;
            if (mouse != null && kb != null && fieldOwnsInput) HandleMouse(mouse, kb, strikeClick);
            else { CancelDrag(); ClearHover(); }
            // aiming a strike: who is under it, beside the reticle and on the field
            if (interactive && armed != OffMapAbilityId.None && mouse != null && p.TryGroundPoint(out var aim))
            {
                aimReadout.Show(Picker.Units, armed, aim, ToHud(mouse.position.ReadValue()));
            }
            else aimReadout.Hide();
            if (kb != null && fieldOwnsInput) HandleKeys(kb);

            selectedNow.Clear();
            var items = Model.Items;
            for (int i = 0; i < items.Count; i++) if (index.TryGetValue(items[i], out int k)) selectedNow.Add(Picker.Units[k]);
            // the marks on the field, back to front in importance: hovered (faint, with their bar), selected, a strike's targets
            markers.Begin(unity);
            if (interactive)
            {
                foreach (int i in hoverClump)
                    if (!Model.Contains(Picker.Units[i].Handle)) markers.Add(Picker.Units[i], SelectionMarkers.Kind.Hover, HpFraction(Picker.Units[i]));
                foreach (var u in selectedNow) markers.Add(u, u.Ours ? SelectionMarkers.Kind.Ours : SelectionMarkers.Kind.Theirs, HpFraction(u));
                PreviewTrench(w);
                foreach (var u in aimReadout.EnemyIn) markers.Add(u, SelectionMarkers.Kind.Theirs, -1f);
                foreach (var u in aimReadout.OursIn) markers.Add(u, SelectionMarkers.Kind.Danger, -1f);
            }
            markers.End();
            deathMarks?.Tick(unity, interactive);
        }

        /// <summary>
        /// Hovering a trench's over the top: amber brackets under the men it would send and red under the pinned who
        /// would stay; hovering a category chip: faint brackets (and bars) under that category, so you see where they are.
        /// </summary>
        void PreviewTrench(SimWorld w)
        {
            if (Trenches == null) return;
            int adv = Trenches.HoveredAdvanceTrench, cat = Trenches.HoveredCatTrench, arch = Trenches.HoveredCatArchetype;
            if (adv < 0 && cat < 0) return;
            int mask = adv >= 0 ? AdvanceMask(adv) : 0;
            foreach (var u in Picker.Units)
            {
                if (!u.Ours || u.Vehicle) continue;
                int t = w.TrenchId[u.Slot];
                if (adv >= 0 && t == adv && (mask == 0 || (mask & (1 << u.Archetype)) != 0))
                    markers.Add(u, w.Suppression[u.Slot] >= StanceRules.PinnedSuppression ? SelectionMarkers.Kind.Danger : SelectionMarkers.Kind.Go, -1f);
                else if (cat >= 0 && t == cat && u.Archetype == arch && !Model.Contains(u.Handle))
                    markers.Add(u, SelectionMarkers.Kind.Hover, HpFraction(u));
            }
        }

        float HpFraction(ScreenUnit u)
        {
            var w = World;
            return w == null ? 0f : w.Hp[u.Slot] / Mathf.Max(1f, w.MaxHp[u.Slot]);
        }

        // ---- the mouse -------------------------------------------------------------------------------------------
        void HandleMouse(Mouse mouse, Keyboard kb, bool strikeClick)
        {
            Vector2 at = CursorOverride ?? mouse.position.ReadValue();
            bool overUi = HudBridge.IsPointerOverUi(at);
            bool shift = kb.leftShiftKey.isPressed || kb.rightShiftKey.isPressed;
            bool ctrl = kb.leftCtrlKey.isPressed || kb.rightCtrlKey.isPressed;
            bool alt = kb.leftAltKey.isPressed || kb.rightAltKey.isPressed;

            if (mouse.leftButton.wasPressedThisFrame && !overUi && !strikeClick) { pressing = true; dragging = false; pressAt = at; }
            if (pressing && !dragging && (at - pressAt).sqrMagnitude > DragPx * DragPx) dragging = true;
            if (pressing && mouse.leftButton.wasReleasedThisFrame)
            {
                if (dragging) BoxSelect(pressAt, at, shift, ctrl); else ClickSelect(at, shift, ctrl, alt);
                CancelDrag();
            }
            else if (pressing && !mouse.leftButton.isPressed) CancelDrag();   // released somewhere we did not see
            ShowMarquee(dragging, pressAt, at);

            // hover: the unit or knot under the cursor and its card beside it (not while boxing, not over the HUD)
            altHeld = alt;
            // the cursor on a trench's troop-category chip: that category's card
            if (overUi && !dragging && Trenches != null && Trenches.HoveredCatTrench >= 0 && hoverCard != null)
            {
                hoverClump.Clear(); fullClump.Clear(); Hovered = null; cursor.Set(SelectCursor.Kind.None);
                hoverCard.ShowCategory(World, Trenches.HoveredCatTrench, Trenches.HoveredCatArchetype, ToHud(at), HudSize());
                return;
            }
            // the cursor on a trench's men badge: that garrison's card
            if (overUi && !dragging && Trenches != null && Trenches.HoveredTrench >= 0 && hoverCard != null)
            {
                hoverClump.Clear(); fullClump.Clear(); Hovered = null; cursor.Set(SelectCursor.Kind.None);
                Garrison.Refresh(World, host.Local.Fields.Trenches.Length);
                hoverCard.ShowGarrison(Garrison, Trenches.HoveredTrench, ToHud(at), HudSize());
                return;
            }
            if (dragging || overUi) { ClearHover(); return; }
            int hit = UnitPicker.Clump(Picker.Units, at, fullClump);
            hoverClump.Clear();
            if (hit >= 0 && alt && fullClump.Count > 1)
            {
                // Alt: one man of the knot, the nearest at first; the wheel steps to the next (the camera leaves it alone)
                int k = 0;
                for (int i = 0; i < fullClump.Count; i++) if (Picker.Units[fullClump[i]].Handle.Equals(altPick)) { k = i; break; }
                float wheel = mouse.scroll.ReadValue().y;
                if (wheel > 0.01f) k = (k + 1) % fullClump.Count; else if (wheel < -0.01f) k = (k + fullClump.Count - 1) % fullClump.Count;
                altPick = Picker.Units[fullClump[k]].Handle;
                altIndex = k;
                hoverClump.Add(fullClump[k]);
            }
            else
            {
                hoverClump.AddRange(fullClump);
                altIndex = 0;
                if (hit >= 0) altPick = Picker.Units[hit].Handle;
            }
            Hovered = hoverClump.Count > 0 ? Picker.Units[hoverClump[0]] : (ScreenUnit?)null;
            cursor.Set(Hovered.HasValue ? (Hovered.Value.Ours ? SelectCursor.Kind.Ours : SelectCursor.Kind.Theirs) : SelectCursor.Kind.None);
            if (Hovered.HasValue && hoverCard != null)
                hoverCard.Show(World, Picker.Units, hoverClump, ToHud(at), HudSize(), altIndex, alt ? fullClump.Count : 0);
            else hoverCard?.Hide();
        }

        /// <summary>The HUD is going away (F9 to the legacy HUD): no hover, no drag, no painted cursor, no wheel claim.</summary>
        public void Release() { CancelDrag(); ClearHover(); altHeld = false; aimReadout.Hide(); }

        void ClearHover() { hoverClump.Clear(); fullClump.Clear(); Hovered = null; hoverCard?.Hide(); cursor.Set(SelectCursor.Kind.None); }

        void ClickSelect(Vector2 at, bool shift, bool ctrl, bool alt)
        {
            int hit = UnitPicker.Clump(Picker.Units, at, hits);
            if (hit < 0) { if (!shift && !ctrl) Model.Clear(); return; }
            if (alt && hits.Count > 1)   // Alt: the man the wheel has stepped to (the nearest if it has not moved)
                for (int i = 0; i < hits.Count; i++) if (Picker.Units[hits[i]].Handle.Equals(altPick)) { hit = hits[i]; break; }
            var u = Picker.Units[hit];
            bool doubled = Time.unscaledTime - lastClickTime <= DoubleClickSeconds && lastClicked.Equals(u.Handle);
            lastClickTime = Time.unscaledTime; lastClicked = u.Handle;
            if (doubled)
            {
                UnitPicker.SameTypeOnScreen(Picker.Units, u.Archetype, u.Ours, Screen.width, Screen.height, hits);
                if (!u.Ours) Apply(hits, false, false); else Apply(hits, shift, ctrl);   // enemies are inspected, never mixed with ours
                return;
            }
            if (!alt && hits.Count > 1)   // a knot: the lot (enemies only ever alone, inspected, never mixed with ours)
            {
                if (!u.Ours) { if (!ctrl) Apply(hits, false, false); return; }
                Apply(hits, shift, ctrl);
                return;
            }
            if (!u.Ours)   // an enemy is inspected on its own, never mixed into our selection
            {
                if (!ctrl) Model.Set(new[] { u.Handle });
                return;
            }
            if (ctrl) Model.Remove(u.Handle);
            else if (shift) { DropEnemies(); Model.Toggle(u.Handle); }
            else Model.Set(new[] { u.Handle });
        }

        void BoxSelect(Vector2 a, Vector2 b, bool shift, bool ctrl)
        {
            UnitPicker.InRect(Picker.Units, a, b, true, hits);
            Apply(hits, shift, ctrl);
        }

        void Apply(List<int> picked, bool shift, bool ctrl)
        {
            if (ctrl) { foreach (int i in picked) Model.Remove(Picker.Units[i].Handle); return; }
            if (shift) { DropEnemies(); foreach (int i in picked) Model.Add(Picker.Units[i].Handle); return; }
            var set = new List<UnitHandle>(picked.Count);
            foreach (int i in picked) set.Add(Picker.Units[i].Handle);
            Model.Set(set);
        }

        void DropEnemies()
        {
            var w = World;
            for (int i = Model.Count - 1; i >= 0; i--)
            {
                var h = Model.Items[i];
                if (w != null && (w.Team[h.Slot] & 1) != 0) Model.Remove(h);
            }
        }

        void CancelDrag() { pressing = false; dragging = false; ShowMarquee(false, default, default); }

        void ShowMarquee(bool on, Vector2 a, Vector2 b)
        {
            if (marquee == null) return;
            marquee.style.display = on ? DisplayStyle.Flex : DisplayStyle.None;
            if (!on || marquee.panel == null) return;
            Vector2 pa = ToHud(a), pb = ToHud(b);
            marquee.style.left = Mathf.Min(pa.x, pb.x); marquee.style.top = Mathf.Min(pa.y, pb.y);
            marquee.style.width = Mathf.Abs(pa.x - pb.x); marquee.style.height = Mathf.Abs(pa.y - pb.y);
        }

        /// <summary>Screen px (bottom-left origin) to the HUD's own layout space (top-left origin, HudLayout.HudScale).</summary>
        Vector2 ToHud(Vector2 screen)
        {
            var panel = marquee?.panel;
            if (panel == null) return screen;
            return RuntimePanelUtils.ScreenToPanel(panel, new Vector2(screen.x, Screen.height - screen.y)) / HudLayout.HudScale;
        }

        /// <summary>The HUD's own layout size (what ToHud positions are in), for keeping the card on the screen.</summary>
        Vector2 HudSize()
        {
            var r = marquee?.parent;
            if (r == null || float.IsNaN(r.resolvedStyle.width)) return new Vector2(Screen.width, Screen.height) / HudLayout.HudScale;
            return new Vector2(r.resolvedStyle.width, r.resolvedStyle.height);
        }

        // ---- the keys --------------------------------------------------------------------------------------------
        static readonly Key[] Digits = { Key.Digit1, Key.Digit2, Key.Digit3, Key.Digit4, Key.Digit5, Key.Digit6, Key.Digit7, Key.Digit8, Key.Digit9, Key.Digit0 };

        /// <summary>True while Ctrl or Shift is held: the digits are group keys then, not deploy keys (HudHotkeys asks).</summary>
        public static bool GroupModifierHeld()
        {
            var kb = Keyboard.current;
            return kb != null && (kb.leftCtrlKey.isPressed || kb.rightCtrlKey.isPressed || kb.leftShiftKey.isPressed || kb.rightShiftKey.isPressed);
        }

        void HandleKeys(Keyboard kb)
        {
            bool shift = kb.leftShiftKey.isPressed || kb.rightShiftKey.isPressed;
            bool ctrl = kb.leftCtrlKey.isPressed || kb.rightCtrlKey.isPressed;
            for (int g = 0; g < Digits.Length; g++)
            {
                if (!kb[Digits[g]].wasPressedThisFrame) continue;
                if (ctrl) { DropEnemies(); Model.Assign(g); }
                else if (shift) RecallGroup(g);
            }
            if (kb.tabKey.wasPressedThisFrame && selectedNow.Count > 0)
            {
                tabCursor = (tabCursor + 1) % selectedNow.Count;
                Focus(selectedNow[tabCursor].World);
            }
        }

        public void RecallGroup(int g)
        {
            bool again = lastGroupKey == g && Time.unscaledTime - lastGroupTime <= GroupDoubleTapSeconds;
            lastGroupKey = g; lastGroupTime = Time.unscaledTime;
            if (!Model.Recall(g)) return;
            if (again) FocusSelection();
        }

        /// <summary>Put the camera over the middle of the selection.</summary>
        public void FocusSelection()
        {
            var w = World;
            if (w == null || Model.Count == 0 || host.Presenter == null) return;
            Vector3 sum = Vector3.zero; int n = 0;
            foreach (var h in Model.Items) { var d = host.Presenter.Drawn(h.Slot); sum += new Vector3(d.x, 0f, d.z); n++; }
            Focus(sum / n);
        }

        void Focus(Vector3 world) { if (cam != null) cam.Focus = new Vector2(world.x, world.z); }

        // ---- a trench's troop categories (TrenchOrderCluster.ISelection) --------------------------------------------
        /// <summary>The trench the selection is scoped to (every selected man garrisons it) and its category mask.</summary>
        public bool ScopedTrench(out int trench, out int mask)
        {
            if (scopeFrame != Time.frameCount || scopeVersion != Model.Version)
            {
                scopeFrame = Time.frameCount; scopeVersion = Model.Version;
                if (!TrenchScope.Of(World, Model.Items, out scopeTrench, out scopeMask)) { scopeTrench = -1; scopeMask = 0; }
            }
            trench = scopeTrench; mask = scopeMask;
            return scopeTrench >= 0;
        }

        public int AdvanceMask(int trench)
        {
            if (!ScopedTrench(out int t, out int mask) || t != trench) return 0;
            return TrenchScope.Effective(mask, TrenchScope.Present(Garrison, trench));
        }

        public bool CategorySelected(int trench, int archetype) =>
            ScopedTrench(out int t, out int mask) && t == trench && (mask & (1 << archetype)) != 0;

        /// <summary>A chip: select that category of the trench (Shift: add it, or drop it if it is in); a double click goes there.</summary>
        public void SelectCategory(int trench, int archetype, bool toggle, bool focus)
        {
            var w = World; if (w == null) return;
            var men = new List<UnitHandle>();
            for (int i = 0; i < w.HighWater; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0 || (w.Team[i] & 1) != 0) continue;
                if (w.TrenchId[i] != trench || w.Archetype[i] != archetype) continue;
                men.Add(new UnitHandle(i, w.Generation[i]));
            }
            if (men.Count == 0) return;
            if (toggle)
            {
                DropEnemies();
                bool allIn = true;
                foreach (var h in men) if (!Model.Contains(h)) { allIn = false; break; }
                if (allIn) foreach (var h in men) Model.Remove(h); else foreach (var h in men) Model.Add(h);
            }
            else Model.Set(men);
            scopeFrame = -1;
            if (focus) FocusSelection();
        }

        /// <summary>The panel's tiles: select just this unit (or drop it with Shift).</summary>
        public void SelectOnly(UnitHandle h, bool drop) { if (drop) Model.Remove(h); else Model.Set(new[] { h }); }

        /// <summary>The panel's type tiles: keep only that archetype (or drop it with Shift).</summary>
        public void KeepType(byte archetype, bool drop)
        {
            var w = World; if (w == null) return;
            var keep = new List<UnitHandle>();
            foreach (var h in Model.Items) if ((w.Archetype[h.Slot] == archetype) != drop) keep.Add(h);
            Model.Set(keep);
        }
    }
}
