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
        /// <summary>
        /// The bottom bar's spacing, in one place. The frame's width is computed from these and the buttons are laid
        /// out with these, so the two cannot drift: they had, by two pixels, and the roster growing from five slots
        /// to seven turned a two-pixel overhang into a frame sized for seven cells with nine drawn into it.
        /// </summary>
        const float Gap = 8f, Inset = 12f, Divider = 20f;
        const int SupportSlots = UnitLook.SupportCards;   // barrage, gas, paratroopers

        /// <summary>
        /// The cell size and wooden frame width for a given amount of horizontal room. Pure, static and public
        /// purely so the gate can reach it: OnGUI is not executed by any test, so before this existed the whole of
        /// this file could be wrong in any way at all and still pass green. It was — the frame was two pixels
        /// narrower than its own contents at every resolution, for longer than the roster has had seven slots.
        ///
        /// The frame is the sum of what the drawing loop consumes, never an independent guess at it: the left inset,
        /// every cell at a (size + Gap) pitch, the divider before the support pair, and the matching right inset.
        /// The floor of 16 is a guard against a degenerate or inverted rect, NOT a comfort floor — a floor the room
        /// cannot afford does not make the buttons bigger, it pushes the right-hand end of the bar off the screen,
        /// and a button you cannot reach is worse than a small one. The player is a non-resizable
        /// native-resolution fullscreen one (ProjectSettings.asset), and even 800x600 with the test panel open
        /// leaves 24 px a cell at ten cells, so 16 is unreachable in anything that ships.
        /// </summary>
        public static void BarMetrics(float room, out float size, out float frameW)
        {
            int cells = RosterEntry.SlotCount + SupportSlots;
            float fixedW = Gap * cells + Inset * 2f + Divider;
            size = Mathf.Clamp((room - fixedW) / cells, 16f, BarHeight - 22f);
            frameW = cells * size + fixedW;
        }

        /// <summary>
        /// What the drawing loop actually advances through for a given cell size — the figure BarMetrics has to
        /// cover. This duplicates the loop's arithmetic, which is the very thing that drifted, so it is NOT the
        /// safeguard: the safeguard is the assertion at the end of the bar, which measures the real loop. This
        /// exists so a test can state the invariant without running OnGUI.
        /// </summary>
        public static float BarConsumed(float size) =>
            Inset + (RosterEntry.SlotCount + SupportSlots) * (size + Gap) + Divider + Inset;

        /// <summary>
        /// How many unit icons the bar has: one per ARCHETYPE, ids 0..18. They used to be indexed by roster slot and
        /// clamped to the last one, which sounds safe and is worse: the Banner and the Pavise drew the same picture and
        /// the bar looked perfectly correct while telling the player two different machines were the same one. Now that
        /// the factions field different slots, a slot index would draw Iron's officer for Brass's sniper. A test holds
        /// an icon for every archetype either faction's roster hands out.
        /// </summary>
        public const int UnitIcons = UnitLook.PortraitCount;

#if UNITY_EDITOR
        /// <summary>Once per session, not once per frame: OnGUI runs twice a frame and this is an error, not a log.</summary>
        static bool barWarned;
#endif
        /// <summary>Where the minimap is on screen (GUI coordinates), so map clicks under it are not taken for targets.</summary>
        public static Rect MinimapRect;
        const float MapScale = 3.2f;   // minimap pixels per nav cell
        Texture2D mapGround, mapDots, whiteTex;
        Color32[] dotPixels;
        float nextGround, nextDots;
        int myMen, theirMen;

        // The infantry slots are the same on both sides; the vehicle slots are not — player 0 fields the Maw,
        // Pincer and Pavise and player 1 the Tusk, Kettle and Censer. So a vehicle is named by WHAT IT IS and not
        // by which slot it sits in, and the bar cannot fall out of step with the roster the way it just did: these
        // arrays were five long while RosterEntry.SlotCount had already gone to seven, which is an index out of
        // range on the first frame the bar is drawn.
        // The words are UnitLook's, which the Toolkit HUD reads too: while each HUD kept its own copy the two said
        // different things about the same man, and the copies drifted the moment the roster grew. Only the pictures
        // below are this file's own.
        static string NameOf(int slot, in RosterEntry e) => UnitLook.Name(e.Archetype);

        static string TipOf(int slot, in RosterEntry e) => UnitLook.Tip(e.Archetype);

        public static string VehicleName(byte archetype) => UnitLook.VehicleName(archetype);

        /// <summary>UnitLook holds the words and the sim constants they quote; HudTextTests checks them there.</summary>
        public static string VehicleTip(byte archetype) => UnitLook.VehicleTip(archetype);

        /// <summary>The two support buttons' text. Constants rather than literals at the call site so that the
        /// numbers in them can be checked against OffMapAbilitySystem's stats by a test: 12 shells, 25 m and a
        /// 4 s delay are WarmupTicks 80 at TickRate 20, and all three go stale silently if the ability is retuned.</summary>
        public const string BarrageTip = UnitLook.BarrageTip;
        public const string GasTip = UnitLook.GasTip;
        public const string DropTip = UnitLook.DropTip;
        static readonly Color Gold = new Color(0.88f, 0.79f, 0.58f);
        static readonly Color Pale = new Color(0.86f, 0.84f, 0.76f);
        static readonly Color Dark = new Color(0.17f, 0.17f, 0.16f);

        GUIStyle stone, slate, wood, green, number, cost, unitName, silver, hint, tip, speed;
        Texture2D sideTex, stripTex, coin, lockClosed, lockOpen, lockGrey, advance, fallback, fire, fireHeld, pause, barrage, gas, drop;
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
            drop = Icon(Pale, new Color(0.55f, 0.62f, 0.40f),
                "..#######...", ".#########..", "###########.", "..#..###..#.", "...#.###.#..", "....#####...",
                "......#.....", ".....###....", "....#####...");
            // one picture per ARCHETYPE: these eight were drawn for player 0's old eight, in that order
            unitIcons = new Texture2D[UnitIcons];
            var old8 = new[]
            {
                Icon(Pale, Dark, "..............", "...........##.", "############..", "#####.##......", "###...........", "##............"),
                Icon(Pale, Dark, ".......###", "......####", ".....####.", "....###...", "...##.....", "..##......", ".##.......", "##........"),
                Icon(Pale, Dark, "..............", "#############.", "..#####..o....", "...###...o....", "..#...#.......", ".#.....#......"),
                Icon(Pale, Dark, cross),
                Icon(Pale, Dark, "...########...", "..##########..", ".############.", "##############", "#o#o#o#o#o#o##", ".############."),
                // The bar deploys for player 0, so the walker slots are the Pincer, the Pavise and the Banner. A
                // walker has to read as legs at 46 px or it is just another tank: claws and six legs for the first,
                // a long gun and a shield for the second, a standard over the third.
                Icon(Pale, Dark, "##..........##", ".##........##.", "...#o####o#...", "..############", "..############", "#..#..#..#..#."),
                Icon(Pale, Dark, "###...........", "###..#########", "###o####......", "###...........", ".############.", "..#..#..#..#.."),
                Icon(Pale, Dark, "....######....", "....##........", "....##........", "##############", "..####o#####..", "..#..#..#..#.."),
            };
            byte[] were = { InfantryArchetype.Rifle, InfantryArchetype.Assault, InfantryArchetype.Machinegunner,
                            InfantryArchetype.Sniper, VehicleArchetype.Maw, VehicleArchetype.Pincer,
                            VehicleArchetype.Pavise, VehicleArchetype.Banner };
            for (int i = 0; i < were.Length; i++) unitIcons[were[i]] = old8[i];
            // the other faction's machines borrow the nearest silhouette until they are drawn their own: a tank reads
            // as a tank at 46 px, and a walker as legs
            unitIcons[VehicleArchetype.Tusk] = old8[4];
            unitIcons[VehicleArchetype.Kettle] = old8[5];
            unitIcons[VehicleArchetype.Censer] = old8[5];
            unitIcons[VehicleArchetype.Redoubt] = old8[6];
            // and the units of 2026-09-25, each one silhouette that has to read at 46 px: a raised arm, a plate held
            // out, a cross, a spanner, a canopy, a pair of tanks on a back, and a squat hull behind a ram
            unitIcons[InfantryArchetype.Officer] = Icon(Pale, Dark, "......##......", ".....####.....", "......##......", "...#..##..#...", "..##..##..##..", "......##......");
            unitIcons[InfantryArchetype.Shield] = Icon(Pale, Dark, "..####....##..", ".######...##..", ".######...##..", ".######...##..", "..####....##..", "...##.....##..");
            unitIcons[InfantryArchetype.Medic] = Icon(Pale, new Color(0.85f, 0.25f, 0.22f), "......##......", "......##......", "..##########..", "..##########..", "......##......", "......##......");
            unitIcons[InfantryArchetype.Repair] = Icon(Pale, Dark, "..##..........", ".####.........", "..####........", "...####.......", "....####......", ".....###......");
            unitIcons[InfantryArchetype.Para] = Icon(Pale, Dark, "..##########..", ".############.", "..#...##...#..", "...#..##..#...", "....#.##.#....", "......##......");
            unitIcons[InfantryArchetype.Jetpack] = Icon(Pale, Dark, "....##..##....", "....##..##....", "....######....", "......##......", ".....#..#.....", "....#....#....");
            unitIcons[VehicleArchetype.Breaker] = Icon(Pale, Dark, "..............", "..##########..", ".############.", "###o######o###", "##############", "..#..#..#..#..");
#if UNITY_EDITOR
            // The roster grew twice while the array did not, and the draw clamped the index, so the overflow was
            // silent: two machines shared one picture and the bar looked right while lying. A missing archetype says
            // so instead, once, the first time the icons are built.
            for (byte a = 0; a < UnitIcons; a++)
                if (unitIcons[a] == null && (InfantryArchetype.IsInfantry(a) || RosterEntry.ForArchetype(a).IsVehicle))
                    Debug.LogError($"BattleHud: archetype {a} ({UnitLook.Name(a)}) has no icon");
#endif
        }

        /// <summary>The picture for an archetype, or the rifleman's if an id has slipped in without one.</summary>
        Texture2D IconFor(byte archetype) =>
            archetype < unitIcons.Length && unitIcons[archetype] != null ? unitIcons[archetype] : unitIcons[InfantryArchetype.Rifle];

        void EnsureStyles()
        {
            if (stone != null) return;
            stone = Button(new Color(0.53f, 0.50f, 0.43f), new Color(0.19f, 0.19f, 0.18f), 2);
            slate = Button(new Color(0.28f, 0.31f, 0.31f), new Color(0.12f, 0.14f, 0.14f), 2);
            speed = new GUIStyle(stone) { fontSize = 13 };
            wood = new GUIStyle { border = new RectOffset(6, 6, 6, 6) };
            wood.normal.background = Frame(new Color(0.30f, 0.31f, 0.29f), new Color(0.21f, 0.19f, 0.16f), 4);
            green = new GUIStyle { border = new RectOffset(5, 5, 5, 5) };
            green.normal.background = Frame(new Color(0.34f, 0.39f, 0.31f), new Color(0.16f, 0.19f, 0.15f), 2);
            number = new GUIStyle(GUI.skin.label) { fontSize = 10, alignment = TextAnchor.UpperLeft };
            number.normal.textColor = new Color(0.15f, 0.15f, 0.14f);
            unitName = new GUIStyle(GUI.skin.label) { fontSize = 11, fontStyle = FontStyle.Bold, alignment = TextAnchor.LowerLeft };
            unitName.normal.textColor = new Color(0.10f, 0.11f, 0.10f);
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
            if (InputFocus.Modal) return;   // a shell screen is up
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
            // Sized by BarMetrics from the same constants this loop advances by, so the frame cannot drift from its
            // contents the way it had — and checked against what the loop actually drew, at the end of the bar.
            BarMetrics(room, out float size, out float frameW);
            float fx = barLeft + leftW + Mathf.Max(0f, (room - frameW) * 0.5f);
            GUI.Box(new Rect(fx, Screen.height - BarHeight, frameW, BarHeight + 6f), GUIContent.none, wood);
            float x = fx + Inset, y = Screen.height - BarHeight + (BarHeight - size) * 0.5f + 2f;
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                var e = w.Roster[s];
                int cd = w.SlotCooldown[s];
                bool unlocked = w.SlotUnlocked[s] != 0;
                bool can = !over && unlocked && cd == 0 && w.Silver[0] >= e.Cost;
                var r = new Rect(x, y, size, size);
                GUI.enabled = can;
                if (GUI.Button(r, new GUIContent("", unlocked ? $"{TipOf(s, e)}  ({e.Cost} silver, key {UnitLook.Hotkey(s)})" : "Locked"), stone))
                    Host.Issue(SimCommand.Deploy(w.Tick, 0, s));
                GUI.enabled = true;
                if (!unlocked) DrawIcon(r, lockGrey, size * 0.26f);
                else
                {
                    DrawIcon(new Rect(r.x, r.y - 4f, r.width, r.height), IconFor(e.Archetype), size * 0.2f, can);
                    GUI.Label(new Rect(r.x + 5f, r.y + 2f, 20f, 14f), UnitLook.Hotkey(s), number);
                    if (size >= 70f) GUI.Label(new Rect(r.x + 5f, r.y, r.width - 10f, r.height - 3f), NameOf(s, e), unitName);
                    GUI.Label(new Rect(r.x, r.y, r.width - 5f, r.height - 3f), cd > 0 ? $"{cd * w.Config.TickSeconds:0}s" : $"{e.Cost}", cost);
                }
                x += size + Gap;
            }
            GUI.DrawTexture(new Rect(x + 3f, Screen.height - BarHeight + 4f, 6f, BarHeight), stripTex);
            x += Divider;
            SupportSlot(ref x, y, size, barrage, BarrageTip, OffMapAbilityId.HeBarrage, over);
            SupportSlot(ref x, y, size, gas, GasTip, OffMapAbilityId.ChlorineGas, over);
            SupportSlot(ref x, y, size, drop, DropTip, OffMapAbilityId.ParaDrop, over);
#if UNITY_EDITOR
            // x has now been advanced by the real loop, so this compares the frame against what was actually drawn
            // rather than against a second copy of the arithmetic. A copy is what drifted last time. If anyone adds
            // a cell, a divider or an inset and does not tell BarMetrics, this says so the first time the bar is
            // drawn, which is the earliest anything in this file has ever been checked: no test executes OnGUI.
            if (x + Inset > fx + frameW + 0.5f && !barWarned)
            {
                barWarned = true;
                Debug.LogError($"BattleHud: the bar drew {x + Inset - fx:0.#} px into a {frameW:0.#} px frame. " +
                               "BarMetrics no longer matches the loop below it — see BarConsumed.");
            }
#endif

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
            x += size + Gap;
        }

        void Order(CommandType type, int trench, int b)
            => Host.Issue(new SimCommand { Tick = Host.Local.World.Tick, Player = 0, Type = type, A = trench, B = b });
    }
}
