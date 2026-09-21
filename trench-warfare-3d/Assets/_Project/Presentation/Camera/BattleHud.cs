// Phase: B1 (implemented; greybox stand-in for the U1 HUD, styled after the 2D game)
// Green silver box top-left; along the bottom a wooden frame with five stone roster slots (padlock when locked), a
// divider and two support slots, the income on the left and pause/speed on the right; and for every trench the player
// owns, slate buttons with gold icons anchored to the trench on screen: fall back and lock on the left, hold fire and
// >> on the right. IMGUI only, every texture is generated here. Support buttons arm TestPanel's click-to-target.
// Top right: a minimap (enemy on the right, as on screen) with the ground, both sides' men as dots and the camera's
// position; click or drag on it to move the view. Next to the silver: how many men each side has on the field.
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
        public const float BarHeight = 100f;
        /// <summary>Where the minimap is on screen (GUI coordinates), so map clicks under it are not taken for targets.</summary>
        public static Rect MinimapRect;
        const float MapScale = 1.6f;   // minimap pixels per nav cell
        Texture2D mapGround, mapDots, whiteTex;
        Color32[] dotPixels;
        float nextGround, nextDots;
        int myMen, theirMen;

        static readonly string[] SlotNames = { "Rifle", "Assault", "MG", "Sniper", "Tank" };
        static readonly string[] SlotTips =
        {
            "Rifleman: cheap line infantry", "Assault: fast, short range", "MG team: holds a trench, suppresses",
            "Sniper: long range, slow fire", "Tank: immune to small arms, grenades within 8 m hurt it",
        };
        static readonly Color Gold = new Color(0.87f, 0.69f, 0.22f);
        static readonly Color Pale = new Color(0.86f, 0.84f, 0.76f);
        static readonly Color Dark = new Color(0.17f, 0.17f, 0.16f);

        GUIStyle stone, slate, wood, green, number, cost, unitName, silver, hint, tip, speed;
        Texture2D sideTex, stripTex, coin, lockClosed, lockOpen, lockGrey, advance, fallback, fire, fireHeld, pause, barrage, gas;
        Texture2D[] unitIcons;
        readonly System.Collections.Generic.List<Vector3> anchors = new System.Collections.Generic.List<Vector3>(8);   // x, y = screen, z = trench index

        // ---- generated art -------------------------------------------------------------------------------------
        static Texture2D Icon(Color main, Color shade, params string[] rows)
        {
            int h = rows.Length, w = rows[0].Length;
            var t = new Texture2D(w, h, TextureFormat.RGBA32, false) { filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
            for (int y = 0; y < h; y++)
            {
                string row = rows[h - 1 - y];
                for (int x = 0; x < w; x++)
                {
                    char c = x < row.Length ? row[x] : '.';
                    t.SetPixel(x, y, c == '#' ? main : c == 'o' ? shade : Color.clear);
                }
            }
            t.Apply();
            return t;
        }

        static string[] Mirror(string[] rows)
        {
            var m = new string[rows.Length];
            for (int i = 0; i < rows.Length; i++) { var a = rows[i].ToCharArray(); System.Array.Reverse(a); m[i] = new string(a); }
            return m;
        }

        /// <summary>A 16 px nine-slice panel: fill, a border and cut corners.</summary>
        static Texture2D Frame(Color fill, Color border, int thick)
        {
            const int n = 16;
            var t = new Texture2D(n, n, TextureFormat.RGBA32, false) { filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
            for (int y = 0; y < n; y++)
                for (int x = 0; x < n; x++)
                {
                    int ex = Mathf.Min(x, n - 1 - x), ey = Mathf.Min(y, n - 1 - y);
                    Color c = ex + ey < 2 ? Color.clear : (ex < thick || ey < thick || ex + ey < thick + 2) ? border : fill;
                    t.SetPixel(x, y, c);
                }
            t.Apply();
            return t;
        }

        static Texture2D Solid(Color c)
        {
            var t = new Texture2D(1, 1) { hideFlags = HideFlags.HideAndDontSave };
            t.SetPixel(0, 0, c); t.Apply();
            return t;
        }

        static GUIStyle Button(Color fill, Color border, int thick)
        {
            var s = new GUIStyle { border = new RectOffset(5, 5, 5, 5), alignment = TextAnchor.MiddleCenter, fontStyle = FontStyle.Bold, fontSize = 14 };
            s.normal.background = Frame(fill, border, thick);
            s.hover.background = Frame(Color.Lerp(fill, Color.white, 0.18f), border, thick);
            s.active.background = Frame(Color.Lerp(fill, Color.black, 0.25f), border, thick);
            s.normal.textColor = s.hover.textColor = s.active.textColor = Gold;
            return s;
        }

        void Start()
        {
            if (Panel == null) Panel = GetComponent<TestPanel>();
            sideTex = Solid(new Color(0.42f, 0.42f, 0.39f));
            stripTex = Solid(new Color(0.36f, 0.23f, 0.16f));

            string[] chevrons =
            {
                "#....#.....", "##...##....", ".##...##...", "..##...##..", "...##...##.",
                "..##...##..", ".##...##...", "##...##....", "#....#.....",
            };
            string[] cross =
            {
                "....###....", "..##.#.##..", ".#...#...#.", ".#...#...#.", "#....#....#", "###########",
                "#....#....#", ".#...#...#.", ".#...#...#.", "..##.#.##..", "....###....",
            };
            string[] closed =
            {
                "..######..", ".##....##.", ".##....##.", ".##....##.", "##########",
                "##########", "####oo####", "####oo####", "##########", "##########",
            };
            string[] open =
            {
                "..######..", ".##....##.", ".##.......", ".##.......", "##########",
                "##########", "####oo####", "####oo####", "##########", "##########",
            };
            advance = Icon(Gold, Dark, chevrons);
            fallback = Icon(Gold, Dark, Mirror(chevrons));
            fire = Icon(Gold, Dark, cross);
            fireHeld = Icon(new Color(0.55f, 0.50f, 0.40f), Dark, cross);
            for (int i = 0; i < 11; i++)   // struck through
            {
                fireHeld.SetPixel(i, i, new Color(0.85f, 0.2f, 0.15f));
                if (i + 1 < 11) fireHeld.SetPixel(i + 1, i, new Color(0.85f, 0.2f, 0.15f));
            }
            fireHeld.Apply();
            lockClosed = Icon(Gold, Dark, closed);
            lockOpen = Icon(Gold, Dark, open);
            lockGrey = Icon(new Color(0.27f, 0.27f, 0.26f), new Color(0.45f, 0.45f, 0.42f), closed);
            pause = Icon(Gold, Dark, "##..##", "##..##", "##..##", "##..##", "##..##", "##..##", "##..##");
            coin = Icon(new Color(0.82f, 0.83f, 0.85f), new Color(0.55f, 0.56f, 0.60f), ".####.", "######", "##oo##", "##oo##", "######", ".####.");
            barrage = Icon(Pale, new Color(0.9f, 0.45f, 0.15f),
                "......##....", ".....####...", ".....####...", ".....####...", ".....####...", "....######..",
                "....#.##.#..", "............", "..o..oo..o..", ".ooo.oo.ooo.", "oooooooooooo");
            gas = Icon(new Color(0.72f, 0.80f, 0.35f), new Color(0.50f, 0.60f, 0.22f),
                "....###.....", "..#######...", ".####oo###..", "####oooo###.", "###oooooo###", "############",
                ".##########.", "..###..###..", "............", "...#....#...", "....#..#....");
            unitIcons = new[]
            {
                Icon(Pale, Dark, "..............", "...........##.", "############..", "#####.##......", "###...........", "##............"),
                Icon(Pale, Dark, ".......###", "......####", ".....####.", "....###...", "...##.....", "..##......", ".##.......", "##........"),
                Icon(Pale, Dark, "..............", "#############.", "..#####..o....", "...###...o....", "..#...#.......", ".#.....#......"),
                Icon(Pale, Dark, cross),
                Icon(Pale, Dark, "...########...", "..##########..", ".############.", "##############", "#o#o#o#o#o#o##", ".############."),
            };
        }

        void EnsureStyles()
        {
            if (stone != null) return;
            stone = Button(new Color(0.48f, 0.48f, 0.45f), new Color(0.27f, 0.27f, 0.25f), 2);
            slate = Button(new Color(0.29f, 0.29f, 0.40f), new Color(0.10f, 0.10f, 0.15f), 2);
            speed = new GUIStyle(stone) { fontSize = 13 };
            wood = new GUIStyle { border = new RectOffset(6, 6, 6, 6) };
            wood.normal.background = Frame(new Color(0.37f, 0.37f, 0.34f), new Color(0.36f, 0.23f, 0.16f), 4);
            green = new GUIStyle { border = new RectOffset(5, 5, 5, 5) };
            green.normal.background = Frame(new Color(0.18f, 0.55f, 0.17f), new Color(0.08f, 0.27f, 0.08f), 2);
            number = new GUIStyle(GUI.skin.label) { fontSize = 10, alignment = TextAnchor.UpperLeft };
            number.normal.textColor = new Color(0.15f, 0.15f, 0.14f);
            unitName = new GUIStyle(GUI.skin.label) { fontSize = 11, fontStyle = FontStyle.Bold, alignment = TextAnchor.LowerLeft };
            unitName.normal.textColor = Dark;
            cost = new GUIStyle(GUI.skin.label) { fontSize = 13, fontStyle = FontStyle.Bold, alignment = TextAnchor.LowerRight };
            cost.normal.textColor = Color.white;
            silver = new GUIStyle(GUI.skin.label) { fontSize = 22, fontStyle = FontStyle.Bold, alignment = TextAnchor.MiddleRight };
            silver.normal.textColor = Color.white;
            hint = new GUIStyle(GUI.skin.label) { fontSize = 11, fontStyle = FontStyle.Bold, alignment = TextAnchor.MiddleCenter };
            hint.normal.textColor = Color.white;
            tip = new GUIStyle(GUI.skin.box) { fontSize = 12, alignment = TextAnchor.MiddleCenter };
            tip.normal.textColor = Color.white;
        }

        static void DrawIcon(Rect r, Texture2D icon, float inset, bool enabled = true)
        {
            var old = GUI.color;
            if (!enabled) GUI.color = new Color(1f, 1f, 1f, 0.35f);
            GUI.DrawTexture(new Rect(r.x + inset, r.y + inset, r.width - 2f * inset, r.height - 2f * inset), icon, ScaleMode.ScaleToFit);
            GUI.color = old;
        }

        void OnGUI()
        {
            if (Host == null || Host.Local == null || stripTex == null) return;
            EnsureStyles();
            var w = Host.Local.World;
            bool over = w.WinnerTeam >= 0;

            // ---- silver --------------------------------------------------------------------------------------
            bool panelOpen = Panel != null && Panel.Visible;
            float left = panelOpen ? 330f : 12f;
            var sr = new Rect(left, 12f, 132f, 44f);
            GUI.Box(sr, GUIContent.none, green);
            GUI.Label(new Rect(sr.x, sr.y, sr.width - 42f, sr.height), $"{w.Silver[0]}", silver);
            GUI.DrawTexture(new Rect(sr.xMax - 36f, sr.y + 10f, 24f, 24f), coin);

            GUI.Box(new Rect(sr.xMax + 8f, 12f, 176f, 44f), GUIContent.none, stone);
            GUI.Label(new Rect(sr.xMax + 8f, 12f, 176f, 44f), $"Men {myMen}   Enemy {theirMen}", hint);
            Minimap(w, over);

            // ---- bottom bar: income | wooden frame with roster and support | pause and speed -----------------
            const float sideH = BarHeight - 14f;
            bool narrow = Screen.width - (panelOpen ? 320f : 0f) < 1000f;
            float rightW = narrow ? 104f : 236f, leftW = narrow ? 8f : 118f;
            float barLeft = panelOpen ? 320f : 0f;
            GUI.DrawTexture(new Rect(barLeft, Screen.height - sideH, Screen.width - barLeft, sideH), sideTex);
            GUI.DrawTexture(new Rect(barLeft, Screen.height - sideH, Screen.width - barLeft, 4f), stripTex);
            if (!narrow)
            {
                GUI.Label(new Rect(barLeft + 8f, Screen.height - sideH, leftW - 40f, sideH), $"+{Host.SilverPerSecond:0.#}/s", silver);
                GUI.DrawTexture(new Rect(barLeft + leftW - 28f, Screen.height - sideH * 0.5f - 10f, 20f, 20f), coin);
            }

            float room = Screen.width - barLeft - leftW - rightW;
            float size = Mathf.Clamp((room - 8f * 8f - 22f - 16f) / 7f, 46f, BarHeight - 22f);
            float frameW = 7f * size + 8f * 8f + 22f;
            float fx = barLeft + leftW + Mathf.Max(0f, (room - frameW) * 0.5f);
            GUI.Box(new Rect(fx, Screen.height - BarHeight, frameW, BarHeight + 6f), GUIContent.none, wood);
            float x = fx + 12f, y = Screen.height - BarHeight + (BarHeight - size) * 0.5f + 2f;
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                var e = w.Roster[s];
                int cd = w.SlotCooldown[s];
                bool unlocked = w.SlotUnlocked[s] != 0;
                bool can = !over && unlocked && cd == 0 && w.Silver[0] >= e.Cost;
                var r = new Rect(x, y, size, size);
                GUI.enabled = can;
                if (GUI.Button(r, new GUIContent("", unlocked ? $"{SlotTips[s]}  ({e.Cost} silver, key {s + 1})" : "Locked"), stone))
                    Host.Issue(SimCommand.Deploy(w.Tick, 0, s));
                GUI.enabled = true;
                if (!unlocked) DrawIcon(r, lockGrey, size * 0.26f);
                else
                {
                    DrawIcon(new Rect(r.x, r.y - 4f, r.width, r.height), unitIcons[s], size * 0.2f, can);
                    GUI.Label(new Rect(r.x + 5f, r.y + 2f, 20f, 14f), $"{s + 1}", number);
                    if (size >= 70f) GUI.Label(new Rect(r.x + 5f, r.y, r.width - 10f, r.height - 3f), SlotNames[s], unitName);
                    GUI.Label(new Rect(r.x, r.y, r.width - 5f, r.height - 3f), cd > 0 ? $"{cd * w.Config.TickSeconds:0}s" : $"{e.Cost}", cost);
                }
                x += size + 8f;
            }
            GUI.DrawTexture(new Rect(x + 3f, Screen.height - BarHeight + 4f, 6f, BarHeight), stripTex);
            x += 20f;
            SupportSlot(ref x, y, size, barrage, "HE barrage: 12 shells in 25 m after 4 s; craters give cover", OffMapAbilityId.HeBarrage, over);
            SupportSlot(ref x, y, size, gas, "Chlorine gas: drifts with the wind, pools in trenches, drives the garrison out", OffMapAbilityId.ChlorineGas, over);

            // pause and speed, right side
            float rx = Screen.width - 10f, ry = Screen.height - sideH * 0.5f - 18f;
            string[] labels = { "", "1x", "2x", "4x", "8x" };
            float[] scales = { 0f, 1f, 2f, 4f, 8f };
            if (narrow)
            {
                int cur = System.Array.FindIndex(scales, v => Mathf.Approximately(v, Host.TimeScale));
                var rc = new Rect(rx - 44f, ry, 40f, 40f);
                if (GUI.Button(rc, new GUIContent(cur > 0 ? labels[cur] : "1x", "Speed: click to cycle 1x, 2x, 4x, 8x"), speed)) Host.TimeScale = scales[cur <= 0 || cur == 4 ? 1 : cur + 1];
                rx -= 44f;
            }
            for (int k = narrow ? 0 : labels.Length - 1; k >= 0; k--)
            {
                rx -= 44f;
                bool active = Mathf.Approximately(Host.TimeScale, scales[k]);
                var r = new Rect(rx, ry, 40f, 40f);
                if (GUI.Button(r, new GUIContent(labels[k], k == 0 ? "Pause" : $"Speed {labels[k]}"), active ? slate : speed)) Host.TimeScale = active && k == 0 ? 1f : scales[k];
                if (k == 0) DrawIcon(r, pause, 11f);
            }

            // ---- orders anchored to the player's trenches ------------------------------------------------------
            var cam = Camera.main;
            var tc = cam != null ? cam.GetComponent<TacticalCamera>() : null;
            if (cam != null && !over)
            {
                var fields = Host.Local.Fields;
                var map = Host.Local.Map;
                float focusX = tc != null ? tc.Focus.x : map.SizeMeters.x * 0.5f;
                const float b = 62f, gap = 6f, off = 40f;
                // zoomed out, neighbouring trenches' buttons would overlap: the trench nearest the enemy keeps its buttons
                anchors.Clear();
                for (int t = fields.Trenches.Length - 1; t >= 0; t--)
                {
                    if (fields.Trenches[t].OwnerTeam != 0 || map.Trenches[t].CellCount == 0) continue;
                    float z = map.NavCellCenter(map.TrenchCells[map.Trenches[t].CellStart]).z + 1f;
                    Vector3 s = cam.WorldToScreenPoint(new Vector3(focusX + 9f, 0f, z));
                    if (s.z > 0f) anchors.Add(new Vector3(s.x, s.y, t));
                }
                anchors.Sort((p, q) => q.x.CompareTo(p.x));   // the enemy is on screen-right
                for (int k = anchors.Count - 1; k >= 0; k--)
                    for (int j = 0; j < k; j++)
                        if (Mathf.Abs(anchors[j].x - anchors[k].x) < 2f * (off + 2f * b + gap)) { anchors.RemoveAt(k); break; }
                for (int t = 0; t < fields.Trenches.Length; t++)
                {
                    var ts = fields.Trenches[t];
                    if (ts.OwnerTeam != 0) continue;
                    if (!anchors.Exists(p => (int)p.z == t)) continue;
                    var def = map.Trenches[t];
                    if (def.CellCount == 0) continue;
                    float tz = map.NavCellCenter(map.TrenchCells[def.CellStart]).z + 1f;
                    // anchor on the trench below the middle of the view (screen-down is +X), so it follows pan and zoom
                    Vector3 sp = cam.WorldToScreenPoint(new Vector3(focusX + 9f, 0f, tz));
                    if (sp.z <= 0f || sp.x < left + off + 2f * b || sp.x > Screen.width - off - 2f * b - gap) continue;
                    float by = Mathf.Clamp(Screen.height - sp.y, 70f, Screen.height - BarHeight - b - 26f);
                    bool locked = ts.Locked != 0, held = ts.HoldFire != 0, manned = ts.GarrisonCount > 0;

                    var rLock = new Rect(sp.x - off - b, by, b, b);
                    if (GUI.Button(rLock, new GUIContent("", locked ? "Locked: reinforcements pass through to the next trench. Click to open" : "Open: reinforcements stop here. Click to lock"), slate))
                        Order(CommandType.TrenchLock, t, locked ? 0 : 1);
                    DrawIcon(rLock, locked ? lockClosed : lockOpen, 13f);

                    var rBack = new Rect(rLock.x - gap - b, by, b, b);
                    GUI.enabled = manned;
                    if (GUI.Button(rBack, new GUIContent("", "Fall back to the trench behind"), slate)) Order(CommandType.TrenchFallback, t, 0);
                    GUI.enabled = true;
                    DrawIcon(rBack, fallback, 13f, manned);

                    var rFire = new Rect(sp.x + off, by, b, b);
                    if (GUI.Button(rFire, new GUIContent("", held ? "Holding fire. Click to fire at will" : "Firing at will. Click to hold fire"), slate))
                        Order(CommandType.TrenchHoldFire, t, held ? 0 : 1);
                    DrawIcon(rFire, held ? fireHeld : fire, 13f);

                    var rGo = new Rect(rFire.xMax + gap, by, b, b);
                    GUI.enabled = manned;
                    if (GUI.Button(rGo, new GUIContent("", "Over the top: the garrison advances on the next trench"), slate)) Order(CommandType.TrenchAdvance, t, 0);
                    GUI.enabled = true;
                    DrawIcon(rGo, advance, 13f, manned);

                    GUI.Label(new Rect(sp.x - 60f, by + b + 2f, 120f, 18f), $"{ts.GarrisonCount} men", hint);
                }
            }

            // ---- hint line above the bar ---------------------------------------------------------------------
            string line = Panel != null && Panel.Armed != OffMapAbilityId.None ? "Click the map to fire.  Esc or right click cancels." : GUI.tooltip;
            if (!string.IsNullOrEmpty(line))
            {
                float tw = Mathf.Min(Screen.width - 40f, tip.CalcSize(new GUIContent(line)).x + 24f);
                GUI.Label(new Rect((Screen.width - tw) * 0.5f, Screen.height - BarHeight - 30f, tw, 24f), line, tip);
            }
        }

        // ---- minimap ---------------------------------------------------------------------------------------------
        // Map pixel (u, v): u runs along world Z (you left, enemy right), v along world X, top = X 0, like the main view.
        void Minimap(SimWorld w, bool over)
        {
            var map = Host.Local.Map;
            int tw = map.NavLength, th = map.NavWidth;
            if (mapGround == null)
            {
                mapGround = new Texture2D(tw, th, TextureFormat.RGBA32, false) { filterMode = FilterMode.Bilinear, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
                mapDots = new Texture2D(tw, th, TextureFormat.RGBA32, false) { filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
                dotPixels = new Color32[tw * th];
                whiteTex = Solid(Color.white);
            }
            float now = Time.unscaledTime;
            if (now >= nextGround)   // craters, cut wire and floods change it slowly
            {
                nextGround = now + 3f;
                var px = new Color32[tw * th];
                for (int x = 0; x < th; x++)
                for (int z = 0; z < tw; z++)
                {
                    var layer = (TW.Sim.Terrain.NavLayer)map.NavLayers[map.NavIndex(x, z)];
                    Color32 c = new Color32(92, 76, 54, 255);
                    if ((layer & TW.Sim.Terrain.NavLayer.Mud) != 0) c = new Color32(70, 57, 40, 255);
                    if ((layer & TW.Sim.Terrain.NavLayer.Crater) != 0) c = new Color32(58, 49, 38, 255);
                    if ((layer & TW.Sim.Terrain.NavLayer.Wire) != 0) c = new Color32(120, 118, 116, 255);
                    if ((layer & TW.Sim.Terrain.NavLayer.Blocked) != 0) c = map.WaterDepthAtCell(x, z) > 0.5f ? new Color32(52, 70, 74, 255) : new Color32(40, 52, 34, 255);
                    if ((layer & TW.Sim.Terrain.NavLayer.Trench) != 0) c = (layer & TW.Sim.Terrain.NavLayer.Link) != 0 ? new Color32(200, 170, 110, 255) : new Color32(30, 22, 16, 255);
                    px[(th - 1 - x) * tw + z] = c;   // texture rows run bottom-up
                }
                mapGround.SetPixels32(px); mapGround.Apply(false, false);
            }
            if (now >= nextDots)
            {
                nextDots = now + 0.15f;
                System.Array.Clear(dotPixels, 0, dotPixels.Length);
                myMen = 0; theirMen = 0;
                var mine = new Color32(255, 214, 92, 255); var theirs = new Color32(235, 70, 60, 255);
                for (int i = 0; i < w.HighWater; i++)
                {
                    if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                    bool me = w.Team[i] == 0;
                    if (me) myMen++; else theirMen++;
                    var c = map.NavCellOf(w.Position[i]);
                    int u = c.y, v = th - 1 - c.x;
                    for (int dv = 0; dv < 2; dv++) for (int du = 0; du < 2; du++)
                    {
                        int uu = Mathf.Min(tw - 1, u + du), vv = Mathf.Max(0, v - dv);
                        dotPixels[vv * tw + uu] = me ? mine : theirs;
                    }
                }
                mapDots.SetPixels32(dotPixels); mapDots.Apply(false, false);
            }

            float mw = tw * MapScale, mh = th * MapScale;
            var r = new Rect(Screen.width - mw - 14f, 44f, mw, mh);
            MinimapRect = new Rect(r.x - 5f, r.y - 5f, r.width + 10f, r.height + 10f);
            GUI.Box(MinimapRect, GUIContent.none, wood);
            GUI.DrawTexture(r, mapGround);
            GUI.DrawTexture(r, mapDots);

            var cam = Camera.main;
            var tc = cam != null ? cam.GetComponent<TacticalCamera>() : null;
            if (tc == null) return;
            // the view: a frame around the camera's focus, as wide as the ground it shows
            float viewZ = tc.Zoom * cam.aspect * 0.55f, viewX = tc.Zoom * 0.55f;
            float cx = r.x + tc.Focus.y / MapData_NavCell * MapScale, cy = r.y + tc.Focus.x / MapData_NavCell * MapScale;
            float fw = Mathf.Clamp(viewZ * 2f / MapData_NavCell * MapScale, 8f, mw), fh = Mathf.Clamp(viewX * 2f / MapData_NavCell * MapScale, 6f, mh);
            var f = new Rect(Mathf.Clamp(cx - fw * 0.5f, r.x, r.xMax - fw), Mathf.Clamp(cy - fh * 0.5f, r.y, r.yMax - fh), fw, fh);
            GUI.DrawTexture(new Rect(f.x, f.y, f.width, 2f), whiteTex); GUI.DrawTexture(new Rect(f.x, f.yMax - 2f, f.width, 2f), whiteTex);
            GUI.DrawTexture(new Rect(f.x, f.y, 2f, f.height), whiteTex); GUI.DrawTexture(new Rect(f.xMax - 2f, f.y, 2f, f.height), whiteTex);

            var e = Event.current;
            if ((e.type == EventType.MouseDown || e.type == EventType.MouseDrag) && e.button == 0 && r.Contains(e.mousePosition))
            {
                tc.Focus = new Vector2((e.mousePosition.y - r.y) / MapScale * MapData_NavCell, (e.mousePosition.x - r.x) / MapScale * MapData_NavCell);
                e.Use();
            }
        }

        const float MapData_NavCell = TW.Sim.Terrain.MapData.NavCellSize;

        void SupportSlot(ref float x, float y, float size, Texture2D icon, string tooltip, OffMapAbilityId id, bool over)
        {
            var w = Host.Local.World;
            var abilities = Host.Local.Abilities;
            if (abilities == null || Panel == null || !OffMapAbilitySystem.TryGetStats((int)id, out var stats)) return;
            int cd = abilities.CooldownOf(0, id);
            bool armed = Panel.Armed == id;
            bool can = armed || (!over && cd == 0 && w.Silver[0] >= stats.Cost);
            var r = new Rect(x, y, size, size);
            GUI.enabled = can;
            if (GUI.Button(r, new GUIContent("", $"{tooltip}  ({stats.Cost} silver)"), armed ? slate : stone)) Panel.Arm(armed ? OffMapAbilityId.None : id);
            GUI.enabled = true;
            DrawIcon(new Rect(r.x, r.y - 4f, r.width, r.height), icon, size * 0.2f, can);
            GUI.Label(new Rect(r.x + 5f, r.y, r.width - 10f, r.height - 3f), armed ? "AIM" : "", unitName);
            GUI.Label(new Rect(r.x, r.y, r.width - 5f, r.height - 3f), cd > 0 ? $"{cd * w.Config.TickSeconds:0}s" : $"{stats.Cost}", cost);
            x += size + 8f;
        }

        void Order(CommandType type, int trench, int b)
            => Host.Issue(new SimCommand { Tick = Host.Local.World.Tick, Player = 0, Type = type, A = trench, B = b });
    }
}
