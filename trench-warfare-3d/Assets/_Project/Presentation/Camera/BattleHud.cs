// Phase: B1 (implemented; greybox stand-in for the U1 HUD, laid out like the 2D game)
// Silver top-left, a deploy bar along the bottom (five roster slots, two support abilities, speed), and for every
// trench the player owns a cluster of buttons anchored to the trench on screen: lock on the left, fall back,
// hold fire and >> on the right. IMGUI only. Support buttons arm TestPanel's click-to-target.
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed class BattleHud : MonoBehaviour
    {
        public SimHost Host;
        public TestPanel Panel;
        public const float BarHeight = 92f;

        static readonly string[] SlotNames = { "Rifle", "Assault", "MG", "Sniper", "Tank" };
        GUIStyle slot, slotCost, silver, order, word, hint;
        Texture2D barTex, silverTex, orderTex;

        void Start()
        {
            if (Panel == null) Panel = GetComponent<TestPanel>();
            barTex = Solid(new Color(0.30f, 0.29f, 0.27f, 0.96f));
            silverTex = Solid(new Color(0.16f, 0.50f, 0.16f, 0.95f));
            orderTex = Solid(new Color(0.24f, 0.25f, 0.34f, 0.95f));
        }

        static Texture2D Solid(Color c)
        {
            var t = new Texture2D(1, 1) { hideFlags = HideFlags.HideAndDontSave };
            t.SetPixel(0, 0, c); t.Apply();
            return t;
        }

        void EnsureStyles()
        {
            if (slot != null) return;
            slot = new GUIStyle(GUI.skin.button) { fontSize = 13, fontStyle = FontStyle.Bold, alignment = TextAnchor.UpperCenter, padding = new RectOffset(4, 4, 10, 4) };
            slotCost = new GUIStyle(GUI.skin.label) { fontSize = 13, alignment = TextAnchor.LowerCenter };
            slotCost.normal.textColor = new Color(1f, 0.92f, 0.6f);
            silver = new GUIStyle(GUI.skin.label) { fontSize = 22, fontStyle = FontStyle.Bold, alignment = TextAnchor.MiddleCenter };
            silver.normal.textColor = Color.white;
            order = new GUIStyle(GUI.skin.button) { fontSize = 22, fontStyle = FontStyle.Bold };
            order.normal.textColor = new Color(1f, 0.85f, 0.3f);
            order.hover.textColor = Color.white;
            hint = new GUIStyle(GUI.skin.label) { fontSize = 11, alignment = TextAnchor.MiddleCenter };
            word = new GUIStyle(order) { fontSize = 13 };
        }

        void OnGUI()
        {
            if (Host == null || Host.Local == null) return;
            EnsureStyles();
            var w = Host.Local.World;
            bool over = w.WinnerTeam >= 0;

            // ---- silver --------------------------------------------------------------------------------------
            bool panelOpen = Panel != null && Panel.Visible;
            float left = panelOpen ? 330f : 12f;
            GUI.DrawTexture(new Rect(left, 12f, 120f, 40f), silverTex);
            GUI.Label(new Rect(left, 12f, 120f, 40f), $"{w.Silver[0]}  ◎", silver);

            // ---- bottom bar ----------------------------------------------------------------------------------
            GUI.DrawTexture(new Rect(0f, Screen.height - BarHeight, Screen.width, BarHeight), barTex);
            float x = left + 8f, y = Screen.height - BarHeight + 8f, size = BarHeight - 16f;
            const float speedBlock = 5f * 44f + 24f;
            float slotW = Mathf.Clamp((Screen.width - x - speedBlock - 18f - 7f * 8f) / 7.3f, 56f, size + 40f);
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                var e = w.Roster[s];
                int cd = w.SlotCooldown[s];
                bool can = !over && w.SlotUnlocked[s] != 0 && cd == 0 && w.Silver[0] >= e.Cost;
                GUI.enabled = can;
                var r = new Rect(x, y, slotW, size);
                if (GUI.Button(r, $"{s + 1}\n{SlotNames[s]}", slot)) Host.Issue(SimCommand.Deploy(w.Tick, 0, s));
                GUI.enabled = true;
                GUI.Label(new Rect(r.x, r.y, r.width, r.height - 4f), cd > 0 ? $"{cd * w.Config.TickSeconds:0}s" : $"{e.Cost}", slotCost);
                x += r.width + 8f;
            }
            x += 18f;
            SupportSlot(ref x, y, slotW * 1.15f, size, "Barrage", OffMapAbilityId.HeBarrage, over);
            SupportSlot(ref x, y, slotW * 1.15f, size, "Gas", OffMapAbilityId.ChlorineGas, over);

            // speed, right side
            float rx = Screen.width - 12f;
            string[] labels = { "II", "1x", "2x", "4x", "8x" };
            float[] scales = { 0f, 1f, 2f, 4f, 8f };
            for (int k = labels.Length - 1; k >= 0; k--)
            {
                rx -= 44f;
                GUI.enabled = !Mathf.Approximately(Host.TimeScale, scales[k]);
                if (GUI.Button(new Rect(rx, y + 18f, 40f, 40f), labels[k])) Host.TimeScale = scales[k];
                GUI.enabled = true;
            }
            if (Panel != null && Panel.Armed != OffMapAbilityId.None)
                GUI.Label(new Rect(0f, Screen.height - BarHeight - 26f, Screen.width, 22f), "Click the map to fire.  Esc or right click cancels.", hint);

            // ---- orders anchored to the player's trenches ------------------------------------------------------
            var cam = Camera.main;
            var tc = cam != null ? cam.GetComponent<TacticalCamera>() : null;
            if (cam == null || over) return;
            var fields = Host.Local.Fields;
            var map = Host.Local.Map;
            float focusX = tc != null ? tc.Focus.x : map.SizeMeters.x * 0.5f;
            for (int t = 0; t < fields.Trenches.Length; t++)
            {
                var ts = fields.Trenches[t];
                if (ts.OwnerTeam != 0) continue;
                var def = map.Trenches[t];
                if (def.CellCount == 0) continue;
                float tz = map.NavCellCenter(map.TrenchCells[def.CellStart]).z + 1f;
                // anchor on the trench below the middle of the view (screen-down is +X), so it follows pan and zoom
                Vector3 sp = cam.WorldToScreenPoint(new Vector3(focusX + 9f, 0f, tz));
                if (sp.z <= 0f || sp.x < left + 70f || sp.x > Screen.width - 150f) continue;
                float by = Mathf.Clamp(Screen.height - sp.y, 70f, Screen.height - BarHeight - 66f);
                const float b = 54f;
                GUI.DrawTexture(new Rect(sp.x - 46f - b, by - 3f, b + 6f, b + 6f), orderTex);
                if (GUI.Button(new Rect(sp.x - 43f - b, by, b, b), ts.Locked != 0 ? "LOCKED" : "open", word)) Order(CommandType.TrenchLock, t, ts.Locked != 0 ? 0 : 1);
                float ox = sp.x + 40f;
                GUI.DrawTexture(new Rect(ox - 3f, by - 3f, b * 3f + 18f, b + 6f), orderTex);
                if (GUI.Button(new Rect(ox, by, b, b), "↩", order)) Order(CommandType.TrenchFallback, t, 0);
                if (GUI.Button(new Rect(ox + b + 6f, by, b, b), ts.HoldFire != 0 ? "HOLD" : "fire", word)) Order(CommandType.TrenchHoldFire, t, ts.HoldFire != 0 ? 0 : 1);
                GUI.enabled = ts.GarrisonCount > 0;
                if (GUI.Button(new Rect(ox + (b + 6f) * 2f, by, b, b), "»", order)) Order(CommandType.TrenchAdvance, t, 0);
                GUI.enabled = true;
                GUI.Label(new Rect(sp.x - 60f, by + b + 4f, 120f, 18f), $"{ts.GarrisonCount} men", hint);
            }
        }

        void SupportSlot(ref float x, float y, float width, float size, string label, OffMapAbilityId id, bool over)
        {
            var w = Host.Local.World;
            var abilities = Host.Local.Abilities;
            if (abilities == null || Panel == null || !OffMapAbilitySystem.TryGetStats((int)id, out var stats)) return;
            int cd = abilities.CooldownOf(0, id);
            bool armed = Panel.Armed == id;
            GUI.enabled = armed || (!over && cd == 0 && w.Silver[0] >= stats.Cost);
            var r = new Rect(x, y, width, size);
            if (GUI.Button(r, armed ? "AIM" : label, slot)) Panel.Arm(armed ? OffMapAbilityId.None : id);
            GUI.enabled = true;
            GUI.Label(new Rect(r.x, r.y, r.width, r.height - 4f), cd > 0 ? $"{cd * w.Config.TickSeconds:0}s" : $"{stats.Cost}", slotCost);
            x += r.width + 8f;
        }

        void Order(CommandType type, int trench, int b)
            => Host.Issue(new SimCommand { Tick = Host.Local.World.Tick, Player = 0, Type = type, A = trench, B = b });
    }
}
