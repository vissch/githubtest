// Phase: B1 (implemented; the M1.5 fun-gate interface, replaced by the real HUD in U1)
// Mouse-driven IMGUI panel so the greybox can be played without knowing the hotkeys: deploy buttons with cost and
// cooldown, one block per trench with >> ↩ lock hold-fire, the enemy's orders (to stage an assault against you),
// time control, camera presets and restart. Everything goes through SimHost.Issue / IssuePeer so the lockstep path
// is exercised exactly as it will be with a real opponent.
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed class TestPanel : MonoBehaviour
    {
        public SimHost Host;
        public TacticalCamera Cam;
        public bool Visible = true;

        static readonly string[] SlotNames = { "Rifleman", "Assault", "MG team", "Sniper", "Mark IV" };
        const float Width = 300f;
        GUIStyle box, header, small;
        Vector2 scroll;

        void Start()
        {
            if (Cam == null) Cam = GetComponent<TacticalCamera>();
        }

        void EnsureStyles()
        {
            if (box != null) return;
            box = new GUIStyle(GUI.skin.box) { alignment = TextAnchor.UpperLeft, padding = new RectOffset(10, 10, 8, 8) };
            header = new GUIStyle(GUI.skin.label) { fontStyle = FontStyle.Bold, fontSize = 13 };
            small = new GUIStyle(GUI.skin.label) { fontSize = 11 };
        }

        void OnGUI()
        {
            if (Host == null || Host.Local == null) return;
            EnsureStyles();
            if (!Visible)
            {
                if (GUI.Button(new Rect(Screen.width - 110, 10, 100, 24), "Show panel")) Visible = true;
                return;
            }
            var w = Host.Local.World;
            var fields = Host.Local.Fields;
            var area = new Rect(Screen.width - Width - 10, 10, Width, Screen.height - 20);
            GUILayout.BeginArea(area, box);
            scroll = GUILayout.BeginScrollView(scroll);

            // ---- match ------------------------------------------------------------------------------------------
            GUILayout.BeginHorizontal();
            GUILayout.Label("Match", header);
            GUILayout.FlexibleSpace();
            if (GUILayout.Button("Hide", GUILayout.Width(50))) Visible = false;
            GUILayout.EndHorizontal();
            GUILayout.Label($"tick {w.Tick}   {w.Tick * w.Config.TickSeconds:0}s   alive {w.AliveCount}   {(Host.Desync ? "DESYNC" : "in sync")}", small);
            GUILayout.Label($"You: {w.Silver[0]} silver   Enemy: {w.Silver[1]} silver", small);
            if (w.WinnerTeam >= 0) GUILayout.Label(w.WinnerTeam == 0 ? "YOU WIN" : "YOU LOSE", header);
            GUILayout.BeginHorizontal();
            GUILayout.Label("Speed", GUILayout.Width(45));
            SpeedButton("||", 0f); SpeedButton("1x", 1f); SpeedButton("2x", 2f); SpeedButton("4x", 4f); SpeedButton("8x", 8f);
            GUILayout.EndHorizontal();
            if (GUILayout.Button("Restart match")) Host.Restart();

            // ---- deploy -----------------------------------------------------------------------------------------
            GUILayout.Space(8);
            GUILayout.Label("Deploy (walks to your front trench)", header);
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                var e = w.Roster[s];
                int cd = w.SlotCooldown[s];
                bool can = w.SlotUnlocked[s] != 0 && cd == 0 && w.Silver[0] >= e.Cost && w.WinnerTeam < 0;
                GUI.enabled = can;
                string label = $"{s + 1}. {SlotNames[s]}   {e.Cost}s" + (cd > 0 ? $"   ({cd * w.Config.TickSeconds:0}s)" : "");
                if (GUILayout.Button(label)) Host.Issue(SimCommand.Deploy(w.Tick, 0, s));
                GUI.enabled = true;
            }

            // ---- trenches ---------------------------------------------------------------------------------------
            GUILayout.Space(8);
            GUILayout.Label("Trenches", header);
            int open0 = 0, open1 = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0 || w.TrenchId[i] >= 0) continue;
                if (w.Team[i] == 0) open0++; else open1++;
            }
            GUILayout.Label($"in the open: you {open0}, enemy {open1}", small);
            for (int t = 0; t < fields.Trenches.Length; t++)
            {
                var ts = fields.Trenches[t];
                bool mine = ts.OwnerTeam == 0;
                float z = Host.Local.Map.Trenches[t].CellStart < Host.Local.Map.TrenchCells.Length
                    ? Host.Local.Map.NavCellCenter(Host.Local.Map.TrenchCells[Host.Local.Map.Trenches[t].CellStart]).z : 0f;
                GUILayout.BeginVertical(GUI.skin.box);
                GUILayout.Label($"Trench {t}  ({(mine ? "yours" : "enemy")}, Z {z:0} m)   garrison {ts.GarrisonCount}" +
                                (ts.Locked != 0 ? "   LOCKED" : "") + (ts.HoldFire != 0 ? "   HOLD FIRE" : ""), small);
                byte player = mine ? (byte)0 : (byte)1;
                GUILayout.BeginHorizontal();
                GUI.enabled = ts.GarrisonCount > 0;
                if (GUILayout.Button(">> Advance")) Order(player, CommandType.TrenchAdvance, t, 0);
                GUI.enabled = true;
                if (GUILayout.Button("↩ Fall back")) Order(player, CommandType.TrenchFallback, t, 0);
                GUILayout.EndHorizontal();
                GUILayout.BeginHorizontal();
                if (GUILayout.Button(ts.Locked != 0 ? "Unlock" : "Lock")) Order(player, CommandType.TrenchLock, t, ts.Locked != 0 ? 0 : 1);
                if (GUILayout.Button(ts.HoldFire != 0 ? "Free fire" : "Hold fire")) Order(player, CommandType.TrenchHoldFire, t, ts.HoldFire != 0 ? 0 : 1);
                if (GUILayout.Button("Look", GUILayout.Width(50)) && Cam != null) Cam.Frame(new Vector2(Host.Local.Map.SizeMeters.x * 0.5f, z), 60f);
                GUILayout.EndHorizontal();
                GUILayout.EndVertical();
            }

            // ---- enemy ------------------------------------------------------------------------------------------
            GUILayout.Space(8);
            GUILayout.Label("Enemy (scripted peer)", header);
            Host.ScriptedPeer = GUILayout.Toggle(Host.ScriptedPeer, " auto-deploy a unit every " + (Host.PeerDeployEveryTicks * w.Config.TickSeconds).ToString("0") + " s");
            GUILayout.BeginHorizontal();
            if (GUILayout.Button("Enemy deploy 5")) for (int k = 0; k < 5; k++) Host.IssuePeer(SimCommand.Deploy(Host.Peer.World.Tick, 1, 0));
            GUILayout.EndHorizontal();
            GUILayout.Label("Use the enemy trench's >> above to send them at you.", small);

            // ---- camera -----------------------------------------------------------------------------------------
            GUILayout.Space(8);
            GUILayout.Label("Camera", header);
            if (Cam != null)
            {
                var size = Host.Local.Map.SizeMeters;
                float mid = size.x * 0.5f;
                short f0 = fields.FrontTrench(0), f1 = fields.FrontTrench(1);
                float z0 = f0 >= 0 ? TrenchZ(f0) : 120f, z1 = f1 >= 0 ? TrenchZ(f1) : size.y - 120f;
                GUILayout.BeginHorizontal();
                if (GUILayout.Button("My trench")) Cam.Frame(new Vector2(mid, z0), 60f);
                if (GUILayout.Button("No man's land")) Cam.Frame(new Vector2(mid, (z0 + z1) * 0.5f), 220f);
                if (GUILayout.Button("Enemy trench")) Cam.Frame(new Vector2(mid, z1), 60f);
                GUILayout.EndHorizontal();
                GUILayout.BeginHorizontal();
                if (GUILayout.Button("Overview")) Cam.Frame(new Vector2(mid, size.y * 0.5f), Cam.ZoomMax);
                if (GUILayout.Button("Follow my units")) FollowUnits();
                GUILayout.EndHorizontal();
                GUILayout.Label("WASD / edge: pan   wheel: zoom   Q/E: rotate   F1 flow field   F2 stats", small);
            }

            GUILayout.EndScrollView();
            GUILayout.EndArea();
        }

        float TrenchZ(short trench)
        {
            var map = Host.Local.Map;
            var def = map.Trenches[trench];
            return def.CellCount > 0 ? map.NavCellCenter(map.TrenchCells[def.CellStart]).z : 0f;
        }

        void SpeedButton(string label, float scale)
        {
            bool active = Mathf.Approximately(Host.TimeScale, scale);
            GUI.enabled = !active;
            if (GUILayout.Button(label, GUILayout.Width(36))) Host.TimeScale = scale;
            GUI.enabled = true;
        }

        void Order(byte player, CommandType type, int trench, int b)
        {
            var c = new SimCommand { Type = type, A = trench, B = b, Player = player };
            if (player == 0) { c.Tick = Host.Local.World.Tick; Host.Issue(c); }
            else { c.Tick = Host.Peer.World.Tick; Host.IssuePeer(c); }
        }

        void FollowUnits()
        {
            var w = Host.Local.World;
            Vector3 sum = Vector3.zero; int n = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0 || w.Team[i] != 0) continue;
                sum += (Vector3)w.Position[i]; n++;
            }
            if (n == 0) return;
            sum /= n;
            Cam.Frame(new Vector2(sum.x, sum.z), 70f);
        }
    }
}
