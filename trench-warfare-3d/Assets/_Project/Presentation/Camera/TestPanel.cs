// Phase: B1 (implemented; the M1.5 fun-gate interface, replaced by the real HUD in U1)
// Mouse-driven IMGUI panel so the greybox can be played without knowing the hotkeys: deploy buttons with cost and
// cooldown, one block per trench with >> ↩ lock hold-fire, the enemy's orders (to stage an assault against you),
// time control, camera presets and restart. Everything goes through SimHost.Issue / IssuePeer so the lockstep path
// is exercised exactly as it will be with a real opponent.
using UnityEngine;
using UnityEngine.InputSystem;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed class TestPanel : MonoBehaviour
    {
        public SimHost Host;
        public TacticalCamera Cam;
        public bool Visible = false;   // the BattleHud is the playing interface; this is the debug drawer

        static readonly string[] SlotNames = { "Rifleman", "Assault", "MG team", "Sniper", "Mark IV" };
        const float Width = 300f;
        GUIStyle box, header, small;
        Vector2 scroll;
        OffMapAbilityId armed = OffMapAbilityId.None;   // waiting for a click on the map

        /// <summary>The ability waiting for a target click, or None. CombatFx draws the aiming circle from it.</summary>
        public OffMapAbilityId Armed => armed;
        public void Arm(OffMapAbilityId id) => armed = id;

        /// <summary>Where the mouse points on the ground plane, if it is over the map and not over this panel.</summary>
        public bool TryGroundPoint(out Vector3 point)
        {
            point = default;
            var mouse = Mouse.current;
            var cam = Camera.main;
            if (mouse == null || cam == null) return false;
            Vector2 m = mouse.position.ReadValue();
            if (m.x < 0f || m.y < 0f || m.x > Screen.width || m.y > Screen.height) return false;
            if (Visible && m.x < Width + 20f) return false;
            if (m.y < BattleHud.BarHeight + 4f) return false;   // the deploy bar (mouse Y counts from the bottom)
            var ray = cam.ScreenPointToRay(m);
            if (Mathf.Abs(ray.direction.y) < 1e-4f) return false;
            float t = (1f - ray.origin.y) / ray.direction.y;   // the greybox ground lies between 0 and 2 m
            if (t <= 0f) return false;
            point = ray.origin + ray.direction * t;
            var size = Host.Local.Map.SizeMeters;
            return point.x >= 0f && point.z >= 0f && point.x <= size.x && point.z <= size.y;
        }

        void Update()
        {
            if (armed == OffMapAbilityId.None || Host == null || Host.Local == null) return;
            var mouse = Mouse.current;
            var kb = Keyboard.current;
            if ((kb != null && kb.escapeKey.wasPressedThisFrame) || (mouse != null && mouse.rightButton.wasPressedThisFrame)) { armed = OffMapAbilityId.None; return; }
            if (mouse != null && mouse.leftButton.wasPressedThisFrame && TryGroundPoint(out var p))
            {
                Host.Issue(new SimCommand { Tick = Host.Local.World.Tick, Player = 0, Type = CommandType.SupportFire, A = (int)armed, Pos = new Unity.Mathematics.float3(p.x, 0f, p.z) });
                armed = OffMapAbilityId.None;
            }
        }

        void SupportButton(string name, OffMapAbilityId id)
        {
            var w = Host.Local.World;
            var abilities = Host.Local.Abilities;
            if (abilities == null || !OffMapAbilitySystem.TryGetStats((int)id, out var stats)) return;
            int cd = abilities.CooldownOf(0, id);
            bool can = cd == 0 && w.Silver[0] >= stats.Cost && w.WinnerTeam < 0;
            GUI.enabled = can || armed == id;
            string label = armed == id ? $"{name}: click the map  (Esc cancels)" : $"{name}   {stats.Cost}s" + (cd > 0 ? $"   ({cd * w.Config.TickSeconds:0}s)" : "");
            if (GUILayout.Button(label)) armed = armed == id ? OffMapAbilityId.None : id;
            GUI.enabled = true;
        }


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
                if (GUI.Button(new Rect(Screen.width - 100, 10, 90, 24), "Debug panel")) Visible = true;
                return;
            }
            var w = Host.Local.World;
            var fields = Host.Local.Fields;
            var area = new Rect(10, 10, Width, Screen.height - 20);   // left = behind your own lines; the enemy side stays clear
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
            var fire = Host.Local.Fire;
            if (fire != null) GUILayout.Label($"Kills  you {fire.Kills[0]}  enemy {fire.Kills[1]}     shots {fire.Shots[0]} / {fire.Shots[1]}", small);
            if (w.WinnerTeam >= 0) GUILayout.Label(w.WinnerTeam == 0 ? "YOU WIN" : "YOU LOSE", header);
            GUILayout.BeginHorizontal();
            GUILayout.Label("Speed", GUILayout.Width(45));
            SpeedButton("||", 0f); SpeedButton("1x", 1f); SpeedButton("2x", 2f); SpeedButton("4x", 4f); SpeedButton("8x", 8f);
            GUILayout.EndHorizontal();
            if (GUILayout.Button("Restart match")) Host.Restart();
            GUILayout.Label($"Bombardment: {Host.BombardmentNow:0} shells a minute (restarts the match)", small);
            GUILayout.BeginHorizontal();
            if (GUILayout.Button("Quiet")) { SimHost.BombardmentOverride = 0f; Host.Restart(); }
            if (GUILayout.Button("Light")) { SimHost.BombardmentOverride = 8f; Host.Restart(); }
            if (GUILayout.Button("Heavy")) { SimHost.BombardmentOverride = 25f; Host.Restart(); }
            if (GUILayout.Button("Drumfire")) { SimHost.BombardmentOverride = 70f; Host.Restart(); }
            GUILayout.EndHorizontal();

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

            // ---- support ----------------------------------------------------------------------------------------
            GUILayout.Space(8);
            GUILayout.Label("Support (click the button, then the map)", header);
            SupportButton("HE barrage", OffMapAbilityId.HeBarrage);
            SupportButton("Chlorine gas", OffMapAbilityId.ChlorineGas);
            GUILayout.Label("Barrage: 12 shells in 25 m after 4 s, craters give cover. Gas drifts left with the wind, pools in trenches, drives the garrison out.", small);

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
                if (GUILayout.Button("Look", GUILayout.Width(50)) && Cam != null) Cam.Frame(new Vector2(Host.Local.Map.SizeMeters.x * 0.5f, z), 30f);
                GUILayout.EndHorizontal();
                GUILayout.EndVertical();
            }

            // ---- objectives -------------------------------------------------------------------------------------
            var sectors = Host.Local.Sectors;
            if (sectors != null && sectors.States.IsCreated)
            {
                GUILayout.Space(8);
                GUILayout.Label("Objectives (3 men, no defenders, 10 s)", header);
                for (int o = 0; o < sectors.States.Length; o++)
                {
                    var def = Host.Local.Map.Objectives[o];
                    var st = sectors.States[o];
                    string side = def.SideTeam == 0 ? "your" : "enemy";
                    string owner = st.Owner == 0 ? "YOU" : "ENEMY";
                    string progress = st.CaptureProgressTicks > 0 ? $"   capturing {100 * st.CaptureProgressTicks / Mathf.Max(1, def.CaptureTicks)}%" : "";
                    GUILayout.Label($"{side} {def.Kind}: held by {owner}{progress}", small);
                }
            }

            // ---- enemy ------------------------------------------------------------------------------------------
            GUILayout.Space(8);
            GUILayout.Label("Enemy (scripted peer)", header);
            Host.ScriptedPeer = GUILayout.Toggle(Host.ScriptedPeer, " auto-deploy a unit every " + (Host.PeerDeployEveryTicks * w.Config.TickSeconds).ToString("0") + " s");
            GUILayout.BeginHorizontal();
            if (GUILayout.Button("Enemy deploy 5")) for (int k = 0; k < 5; k++) Host.IssuePeer(SimCommand.Deploy(Host.Peer.World.Tick, 1, 0));
            GUILayout.EndHorizontal();
            Host.PeerAttacks = GUILayout.Toggle(Host.PeerAttacks, $" attacks on its own with {Host.PeerAttackGarrison}+ men");
            GUILayout.Label("Or use the enemy trench's >> above to send them at you.", small);

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
                if (GUILayout.Button("My trench")) Cam.Frame(new Vector2(mid, z0 + 6f), 30f);   // trench just right of this panel, no man's land beyond
                if (GUILayout.Button("No man's land")) Cam.Frame(new Vector2(mid, (z0 + z1) * 0.5f), 220f);
                if (GUILayout.Button("Enemy trench")) Cam.Frame(new Vector2(mid, z1), 30f);
                GUILayout.EndHorizontal();
                GUILayout.BeginHorizontal();
                if (GUILayout.Button("Overview")) Cam.Frame(new Vector2(mid, size.y * 0.5f), Cam.ZoomMax);
                if (GUILayout.Button("Follow my units")) FollowUnits();
                GUILayout.EndHorizontal();
                GUILayout.Label("You are on the LEFT, the enemy on the RIGHT.", small);
                GUILayout.Label("WASD / edge: pan (speeds up)   wheel: zoom   Z: super zoom", small);
                GUILayout.Label("right drag: turn and tilt   middle drag: pan   Home: reset view", small);
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
            Cam.Frame(new Vector2(sum.x, sum.z), 35f);
        }
    }
}
