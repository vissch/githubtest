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

        static readonly string[] SlotNames = { "Rifleman", "Assault", "MG team", "Sniper", "Tank (Maw)" };
        const float Width = 300f;
        GUIStyle box, header, small;
        Vector2 scroll;
        readonly AbilityAim aim = new AbilityAim();     // the ability waiting for its target: a click, or a press-drag-release (docs/21 phase 5)
        // A5c has no flamethrower in the roster yet (RosterEntry's archetypes stop at the sniper) and no BurningSystem,
        // so the fire has no sim to come from. These put it on the field by hand: the show side is finished and this is
        // how it is looked at, and when the sim grows the unit the same calls move behind a Shot and a Death event.
        enum FlameTool { None, Burst, Alight, CookOff, BigFire }
        FlameTool flameTool = FlameTool.None;

        /// <summary>The ability waiting for its target, or None. CombatFx draws the aim from Aim.Shape; SelectionController counts under it.</summary>
        public OffMapAbilityId Armed => aim.Armed;
        public AbilityAim Aim => aim;
        public void Arm(OffMapAbilityId id) { if (id == OffMapAbilityId.None) aim.Cancel(); else aim.Arm(id); }

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
            if (HudBridge.UseToolkitHud)
            {
                if (HudBridge.IsPointerOverUi(m)) return false;   // the Toolkit HUD answers for its own chrome (panel.Pick)
            }
            else
            {
                if (m.y < BattleHud.BarHeight + 4f) return false;   // the deploy bar (mouse Y counts from the bottom)
                if (BattleHud.MinimapRect.Contains(new Vector2(m.x, Screen.height - m.y))) return false;   // a click on the minimap moves the view
            }
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
            FlameClick();
            if (aim.Armed == OffMapAbilityId.None || Host == null || Host.Local == null) return;
            if (!InputFocus.Gameplay) return;   // a shell screen has the input
            var mouse = Mouse.current;
            var kb = Keyboard.current;
            if ((kb != null && kb.escapeKey.wasPressedThisFrame) || (mouse != null && mouse.rightButton.wasPressedThisFrame))
            {
                if (kb != null && kb.escapeKey.wasPressedThisFrame) InputFocus.ConsumeEscape();   // this Esc cancelled the aim; it does not also open the menu
                aim.Cancel(); return;
            }
            // a point ability fires on the click; a line ability (a corridor) is press-drag-release, Tab cycles its
            // patterns and Shift snaps the heading (AbilityAim)
            if (kb != null && kb.tabKey.wasPressedThisFrame) aim.CyclePattern();
            aim.Snap = kb != null && (kb.leftShiftKey.isPressed || kb.rightShiftKey.isPressed);
            uint tick = Host.Local.World.Tick;
            if (mouse != null && mouse.leftButton.wasPressedThisFrame && TryGroundPoint(out var p))
            {
                if (aim.Press(p, tick, 0, out var fired)) Host.Issue(fired);
            }
            else if (aim.Dragging && mouse != null)
            {
                if (TryGroundPoint(out var q)) aim.Drag(q);
                if (mouse.leftButton.wasReleasedThisFrame && aim.Release(aim.Current, tick, 0, out var line)) Host.Issue(line);
            }
        }

        /// <summary>The living man of a team nearest a point on the ground, or -1. Vehicles do not carry flamethrowers.</summary>
        int NearestMan(Vector3 p, int team)
        {
            var w = Host.Local.World;
            int best = -1; float near = float.MaxValue;
            for (int i = 0; i < w.HighWater; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0) continue;
                if (team >= 0 && w.Team[i] != team) continue;
                var q = w.Position[i];
                float d = (q.x - p.x) * (q.x - p.x) + (q.z - p.z) * (q.z - p.z);
                if (d < near) { near = d; best = i; }
            }
            return best;
        }

        /// <summary>An armed fire tool waiting for its click on the map.</summary>
        void FlameClick()
        {
            if (flameTool == FlameTool.None || Host == null || Host.Local == null || !InputFocus.Gameplay) return;
            var kb = Keyboard.current;
            var mouse = Mouse.current;
            if ((kb != null && kb.escapeKey.wasPressedThisFrame) || (mouse != null && mouse.rightButton.wasPressedThisFrame))
            {
                if (kb != null && kb.escapeKey.wasPressedThisFrame) InputFocus.ConsumeEscape();
                flameTool = FlameTool.None; return;
            }
            if (mouse == null || !mouse.leftButton.wasPressedThisFrame || !TryGroundPoint(out var p)) return;
            var fire = Flamethrower.Active;
            if (fire == null) return;
            p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z);
            switch (flameTool)
            {
                case FlameTool.Burst:
                {
                    // the man of yours nearest the click turns his nozzle on it, held on for a few bursts so the
                    // stream can actually be watched rather than glimpsed
                    int slot = NearestMan(p, 0);
                    if (slot >= 0) fire.BurstFrom(slot, p + Vector3.up * 0.6f, 2.4f);
                    break;
                }
                case FlameTool.Alight:
                {
                    int slot = NearestMan(p, -1);
                    if (slot >= 0) fire.Ignite(slot, 7f);
                    break;
                }
                case FlameTool.CookOff:
                    fire.TankCookOff(p + Vector3.up * 1.1f);
                    break;
                case FlameTool.BigFire:
                    fire.Alight(p, 3.5f, 30f);
                    break;
            }
            if (kb == null || !kb.leftShiftKey.isPressed) flameTool = FlameTool.None;   // shift keeps the tool for a second click
        }

        void FlameButton(string name, FlameTool tool)
        {
            GUI.enabled = Flamethrower.Active != null;
            if (GUILayout.Button(flameTool == tool ? $"{name}: click the map  (shift: keep, Esc cancels)" : name))
                flameTool = flameTool == tool ? FlameTool.None : tool;
            GUI.enabled = true;
        }

        void SupportButton(string name, OffMapAbilityId id)
        {
            var w = Host.Local.World;
            var abilities = Host.Local.Abilities;
            if (abilities == null || !OffMapAbilitySystem.TryGetStats((int)id, out var stats)) return;
            int cd = abilities.CooldownOf(0, id);
            bool can = cd == 0 && w.Silver[0] >= stats.Cost && w.WinnerTeam < 0;
            bool armedNow = aim.Armed == id;
            GUI.enabled = can || armedNow;
            string label = armedNow ? (aim.IsLine ? $"{name}: press, drag, release  (Tab pattern, Esc cancels)" : $"{name}: click the map  (Esc cancels)") : $"{name}   {stats.Cost}s" + (cd > 0 ? $"   ({cd * w.Config.TickSeconds:0}s)" : "");
            if (GUILayout.Button(label)) Arm(armedNow ? OffMapAbilityId.None : id);
            GUI.enabled = true;
        }


        /// <summary>The aim, for the effects (SceneHooks.AimPreview): the shape under the cursor while an ability is armed.</summary>
        bool TryAimShape(out AimShape shape)
        {
            shape = default;
            return aim.Armed != OffMapAbilityId.None && TryGroundPoint(out var cursor) && aim.Shape(cursor, 0, out shape);
        }
        void OnEnable() { SceneHooks.AimPreview = TryAimShape; }
        void OnDisable() { if (SceneHooks.AimPreview == (TryAimShape)TryAimShape) SceneHooks.AimPreview = null; }

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
            if (InputFocus.Modal) return;   // a shell screen is up
            EnsureStyles();
            if (!Visible)
            {
                if (GUI.Button(new Rect(Screen.width - 104, 8, 90, 24), "Debug panel")) Visible = true;
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
            GUILayout.Label($"tick {w.Tick}   {w.Tick * w.Config.TickSeconds:0}s   alive {w.AliveCount}   {(Host.Peer == null ? "one world" : Host.Desync ? "DESYNC" : "in sync")}", small);
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

            // ---- fire (A5c show side, no sim behind it yet) ------------------------------------------------------
            GUILayout.Space(8);
            GUILayout.Label("Fire (click the button, then the map)", header);
            FlameButton("Flame burst", FlameTool.Burst);
            FlameButton("Set a man alight", FlameTool.Alight);
            FlameButton("Fuel tank cook-off", FlameTool.CookOff);
            FlameButton("Big fire", FlameTool.BigFire);
            var ft = Flamethrower.Active;
            GUILayout.Label(ft == null
                ? "No CombatFx running: nothing to set on fire."
                : $"Burst: your nearest man plays his stream on the point ({Flamethrower.Reach:0} m). Pools of fuel keep burning after it. {ft.Jets} jet(s), {ft.Fires} fire(s) alight.", small);

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
            if (GUILayout.Button("Enemy deploy 5")) for (int k = 0; k < 5; k++) Host.IssuePeer(SimCommand.Deploy(Host.EnemyView.World.Tick, 1, 0));
            if (GUILayout.Button("Enemy tank (Tusk)")) Host.IssuePeer(SimCommand.Deploy(Host.EnemyView.World.Tick, 1, 4));
            GUILayout.EndHorizontal();
            Host.PeerDeploysTanks = GUILayout.Toggle(Host.PeerDeploysTanks, " sends a tank whenever it can afford one");
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
                if (GUILayout.Button("Next tank")) NextTank();
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
            else { c.Tick = Host.EnemyView.World.Tick; Host.IssuePeer(c); }
        }

        int lastTank = -1;

        /// <summary>Frame the next tank on the field (either side), close enough to see its tracks run.</summary>
        void NextTank()
        {
            var w = Host.Local.World;
            for (int k = 1; k <= w.HighWater; k++)
            {
                int i = (lastTank + k) % Mathf.Max(1, w.HighWater);
                if ((w.Flags[i] & ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) != ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) continue;
                lastTank = i;
                Cam.Frame(new Vector2(w.Position[i].x, w.Position[i].z), 24f);
                return;
            }
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
