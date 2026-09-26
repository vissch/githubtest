// Phase: C4 (implemented) — the drawn part of the fight. Hand-painted flipbooks (Resources/VFX, from the SrRubfish and
// Hun0FX packs the team owns) played on camera-facing cards, one instanced draw a book, no GameObjects. CombatFx decides
// what happens where; this only keeps the cards alive, moves and grows them, and packs each into the matrix TW/Flipbook
// reads. A card lives Life seconds and plays its book once over that time.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed class FlipbookFx
    {
        public enum Book : byte
        {
            Burst,      // a shell burst: the flash, the boiling cloud, the ring it leaves
            Column,     // earth thrown straight up and falling back
            Splash,     // the same column in water, white
            Wings,      // the low burst that runs out along the ground either side
            Spurt,      // the dust a round kicks up where it strikes
            Puff,       // a small round cloud: cloth and dust where a man is hit
            Smoke,      // the same cloud dark: what a burst leaves hanging
            Gas,        // the same cloud yellow-green: chlorine, drawn per field cell by CombatFx
            Muzzle,     // the flare at the muzzle, along the shot (additive)
            Star,       // the spike of a strike (additive)
            Flash,      // the burst's own light (additive)
            Count
        }

        [System.Flags] public enum Kind : byte { None = 0, Upright = 1, Anchored = 2, Mirror = 4, HoldLast = 8 }

        struct Card
        {
            public Vector3 Pos, Vel;
            public float Born, Life, Width, Height, Grow, Roll, Alpha, Glow, Pop;
            public Book Book; public Kind Kind;
        }

        // one book: its texture in Resources/VFX, grid, whether it adds light or is a cloud the moon lights, and which of
        // the drawing's values are its shade and its light (Low, High: measured from the pixels, so each book uses both bands)
        struct Sheet { public string Name; public int Cols, Rows, Frames; public bool Additive, MaskOnly, Erode; public Color Tint; public float Low, High, Play, Lit, RampIn, Mood; public bool Deep; }   // Play: the part of the book used (0 = all); Lit: 0 = own values (default: additive 0, else 1); RampIn: seconds to fade in; Mood: how much the mood tints the shade (0 = default 1); Deep: a cloud as deep as it is wide (fx.smokeSoft)
        static readonly Sheet[] Sheets =
        {
            new Sheet { Name = "Burst",  Cols = 4, Rows = 4, Frames = 16, Erode = true, Tint = new Color(0.50f, 0.53f, 0.58f), Low = 0.12f, High = 0.62f, Play = 0.7f, Mood = 0.45f, Deep = true },   // a cloud born of fire: the full night tint turned it saturated blue
            new Sheet { Name = "Column", Cols = 4, Rows = 4, Frames = 16, Tint = new Color(0.40f, 0.33f, 0.26f), Low = 0.30f, High = 0.95f },
            new Sheet { Name = "Column", Cols = 4, Rows = 4, Frames = 16, Tint = new Color(0.60f, 0.66f, 0.76f), Low = 0.28f, High = 0.85f },
            new Sheet { Name = "Wings",  Cols = 4, Rows = 4, Frames = 16, Erode = true, Play = 0.75f, Tint = new Color(0.74f, 0.65f, 0.52f), Low = 0.15f, High = 0.42f },
            new Sheet { Name = "Spurt",  Cols = 2, Rows = 5, Frames = 10, Tint = new Color(0.86f, 0.78f, 0.64f), Low = 0.50f, High = 0.80f },
            new Sheet { Name = "Puff",   Cols = 3, Rows = 3, Frames = 9,  Tint = new Color(0.86f, 0.80f, 0.66f), Low = 0.20f, High = 0.50f },
            new Sheet { Name = "Puff",   Cols = 3, Rows = 3, Frames = 9,  Erode = true, Tint = new Color(0.50f, 0.47f, 0.43f), Low = 0.10f, High = 0.60f, Play = 0.6f, Lit = 0.6f, RampIn = 0.3f, Mood = 0.45f, Deep = true },   // smoke: warm grey, and only half the moon's blue in its shade (it read as blue cotton at night)
            new Sheet { Name = "Puff",   Cols = 3, Rows = 3, Frames = 9,  Tint = new Color(0.74f, 0.80f, 0.34f), Low = 0.15f, High = 0.60f, Lit = 0.75f },
            new Sheet { Name = "Muzzle", Cols = 3, Rows = 4, Frames = 12, Additive = true, Tint = new Color(1.0f, 0.78f, 0.42f), Low = 0f, High = 1f },
            new Sheet { Name = "Star",   Cols = 1, Rows = 1, Frames = 1,  Additive = true, MaskOnly = true, Tint = new Color(1.0f, 0.88f, 0.62f), Low = 0f, High = 1f },
            new Sheet { Name = "Flash",  Cols = 1, Rows = 1, Frames = 1,  Additive = true, Tint = new Color(1.0f, 0.80f, 0.50f), Low = 0f, High = 1f },
        };

        public const int MaxCards = 1536;

        // AOSA C52 (juice J01): the smoke of a barrage hid the men it fell among. At the standard view a shell leaves 7
        // puffs 9-13 m wide that grow to 30-43 m and hold near-opaque for 4-6 s, and a barrage is 12 shells, so the trench
        // it fell on was under dozens of stacked cards. Two knobs, read once where the smoke is made, bring the men back
        // through it at the standard view (both go back to the old look as the lens goes in among the men):
        //   fx.smokeSoft   a burst's cloud and its smoke are as deep as they are wide: a card fades out in front of any
        //                  surface (the ground, a man) over this fraction of its own width, so the cloud's base, where the
        //                  men are, is thin and the smoke aloft stays dark. 0 = the old look (only the 0.8 m soft edge).
        //   fx.smokeAlpha  multiplies the opacity a shell's smoke puff is born with. 1 = the old look.
        // Both at their old values (fx.smokeSoft=0,fx.smokeAlpha=1) draw the image before C52 bit for bit.
        public const string SoftKnob = "fx.smokeSoft", AlphaKnob = "fx.smokeAlpha";
        public const float DefaultSoft = 0.3f, DefaultAlpha = 0.85f;   // blind critic, cycle 5 (runs 5/w1): the one balance of six where the barrage stays heavy and the trench countable
        public const float OldSoft = 0f, OldAlpha = 1f;

        /// <summary>fx.smokeSoft, never below 0 (0 = the old look).</summary>
        public static float ReadSoft() { float v = Knobs.Get(SoftKnob, DefaultSoft); return v > 0f ? v : 0f; }

        /// <summary>fx.smokeAlpha, in [0, 1] (1 = the old look).</summary>
        public static float ReadAlpha() => Mathf.Clamp01(Knobs.Get(AlphaKnob, DefaultAlpha));

        /// <summary>The opacity a shell's smoke puff is born with: the recipe's own, times the knob at the standard view
        /// (closeUp 0), and the recipe's own among the men (closeUp 1). With the knob at 1 it is the recipe's exactly.</summary>
        public static float SmokeOpacity(float recipe, float knob, float closeUp) => recipe * Mathf.Lerp(knob, 1f, closeUp);

        // AOSA C59 (juice J01 at night): under the moon the burst's cloud and the smoke it leaves read as pale periwinkle
        // cotton, lighter than the ground, and buried the trench lines (critic, runs 6/v6c-1: smoke 3, readability 2). The
        // blue is the moon itself: the shader lights a drawing's light band with _MainLightColor (Flipbook_URP.shader:123),
        // and the night key is (0.56, 0.70, 1.0), so the neutral Burst tint came out (0.33, 0.40, 0.58) on screen. Two
        // knobs, read once in CombatFx.Awake, and only on a moonlit field (SceneMood.Night and not a molten one: the lava
        // field is dark but lit from its floor, and keeps its own rose smoke):
        //   fx.smokeNight      the value (luma, 0-1) the Burst and Smoke books are drawn at, in a dark warm grey that
        //                      ignores the moon: every ink value goes to the shade band (_Levels), the shade is a plain
        //                      grey with no mood in it, and the drawing keeps 40% of its own values for form. The burst's
        //                      own flash still lights it (glow, TWBurstLight). 0 = the old moonlit look (nothing is set).
        //   fx.smokeNightSize  the width of a shell's Burst cloud and its smoke puffs at the standard view (back to 1 as the
        //                      lens goes in among the men). 1 = the old size.
        // Both at their old values (fx.smokeNight=0,fx.smokeNightSize=1) draw the image before C59 bit for bit.
        public const string NightKnob = "fx.smokeNight", NightSizeKnob = "fx.smokeNightSize";
        public const float DefaultNight = 0.15f, DefaultNightSize = 0.6f;   // critic: #2E2A28-#3A342F, darker than the ground; radius about -40%
        public const float OldNight = 0f, OldNightSize = 1f;
        public static readonly Color NightHue = new Color(1f, 0.89f, 0.78f);   // warm grey: #3A342F is (1, 0.90, 0.81), biased warm against the blue mist
        public const float NightShade = 0.5f, NightLit = 0.6f;   // shade grey and the share of it (the rest is the drawing's ink); NightLit stays >= 0.5, the shader's alpha-blended branch
        const float NightMidInk = 0.43f;   // the drawings' middle ink (Burst median 0.38, Puff 0.48, measured from the pixels at alpha > 0.3)

        /// <summary>fx.smokeNight, in [0, 1] (0 = the old moonlit look).</summary>
        public static float ReadNight() => Mathf.Clamp01(Knobs.Get(NightKnob, DefaultNight));

        /// <summary>fx.smokeNightSize, in [0.05, 4] (1 = the old size).</summary>
        public static float ReadNightSize() => Mathf.Clamp(Knobs.Get(NightSizeKnob, DefaultNightSize), 0.05f, 4f);

        /// <summary>A field the moon lights: dark, and not lit from a molten floor.</summary>
        public static bool MoonLit(bool night, bool molten) => night && !molten;

        /// <summary>The tint that draws a night cloud at this value (luma at the drawings' middle ink), before fog and grade.</summary>
        public static Color NightTint(float value)
        {
            float luma = 0.299f * NightHue.r + 0.587f * NightHue.g + 0.114f * NightHue.b;
            float k = value / (luma * Mathf.Lerp(NightMidInk, NightShade, NightLit));
            return new Color(NightHue.r * k, NightHue.g * k, NightHue.b * k, 1f);
        }

        /// <summary>The width factor of a shell's cloud and smoke: the knob at the standard view on a moonlit field, 1
        /// among the men and on any other field. With the knob at 1 it is 1 exactly.</summary>
        public static float NightScale(float knob, float closeUp, bool moonLit) => moonLit ? Mathf.Lerp(knob, 1f, closeUp) : 1f;

        /// <summary>AOSA C59: paint the Burst and Smoke books as dark warm grey that the moon does not light (see NightKnob).
        /// Called after the biome's tints (CombatFx.ApplyTints), and only on a moonlit field; value 0 sets nothing.</summary>
        public void NightSmoke(float value)
        {
            if (value <= 0f) return;
            var tint = NightTint(value);
            PaintNight(mats[(int)Book.Burst], tint);
            PaintNight(mats[(int)Book.Smoke], tint);
        }

        static void PaintNight(Material m, Color tint)
        {
            if (m == null) return;
            m.SetColor("_Tint", tint);
            m.SetVector("_Levels", new Vector4(2f, 3f, 0f, 0f));   // no ink reaches the light band, so _MainLightColor (the moon) is never used
            m.SetColor("_Shade", new Color(NightShade, NightShade, NightShade));
            m.SetFloat("_ShadeMood", 0f);   // and no night-blue shade tint either
            m.SetFloat("_Lit", NightLit);
        }

        // AOSA C57 (juice J01, the earth column at night): the column read as a see-through dark smear, a shadow (critic,
        // runs 7/d59: column 4). Three things, all from the code and the drawing, and the first measured on screen:
        //   the moon  the Column book is toon-lit like the Burst was before C59, so its brown (SceneTints.Column 0.40, 0.33,
        //             0.26) goes out as the night key's blue in the light band (0.56, 0.70, 1.0) and as near-black navy in the
        //             shade band (_Shade x the night shade tint 0.20, 0.29, 0.56). On screen it measured (40, 43, 48): blue over
        //             red from a red-over-blue tint, a little darker than the moonlit ground, which is how a shadow looks
        //             (runs 6/col3 against 6/v6c, the pixels the column's size changed, frames 0-15).
        //   the fade  the book is not Erode, so from 65% of its 1.8 s every column fades EVENLY, and the drawing's own alpha
        //             falls from frame 9 (median 0.93 -> 0.15): for its last 0.6 s each column is a uniformly half-clear card.
        //   the size  a barrage shell is r 8 (OffMapAbilities ShellRadius), so the column is drawn about 10 m wide and 20-26 m
        //             tall at the standard view (the drawing fills 62% x 79% of its 16.8 x 24.7 m card, then grows 35%).
        // Two knobs, read once in CombatFx.Awake, and only on a moonlit field (as C59: the cause is the moon; the day and
        // the lava field are untouched):
        //   fx.columnEarth      the value (luma, 0-1) the earth is drawn at, in #3B2A1E's dark brown that ignores the moon
        //                       (every ink value in the shade band, a plain grey shade, 40% of the drawing's own values kept
        //                       for its clods), and torn like the smoke (Erode: at full life the alpha edge is 2.2x harder,
        //                       and at the end it breaks up from its edges into the smoke instead of fading evenly). The
        //                       burst's own light (TWBurstLight) still lights its foot orange. 0 = the old look (nothing set).
        //   fx.columnEarthSize  the column's width and height at the standard view (back to 1 as the lens goes in among the
        //                       men). 1 = the old size.
        // Both at their old values (fx.columnEarth=0,fx.columnEarthSize=1) draw the image before C57 bit for bit.
        public const string EarthKnob = "fx.columnEarth", EarthSizeKnob = "fx.columnEarthSize";
        public const float DefaultEarth = 0.22f, DefaultEarthSize = 0.38f;   // critic: #3B2A1E; about 3-4 m wide and 8-10 m tall at T1 (r 8: about 4.4 x 8.2 m as it rises, 5.3 x 10 m at the end)
        public const float OldEarth = 0f, OldEarthSize = 1f;
        public static readonly Color EarthHue = new Color(1f, 0.712f, 0.508f);   // #3B2A1E is (59, 42, 30) = (1, 0.712, 0.508)
        const float EarthMidInk = 0.61f;   // the Column drawing's middle ink (median 0.58-0.63 over frames 0-14, pixels at alpha > 0.3)

        /// <summary>fx.columnEarth, in [0, 1] (0 = the old moonlit look).</summary>
        public static float ReadEarth() => Mathf.Clamp01(Knobs.Get(EarthKnob, DefaultEarth));

        /// <summary>fx.columnEarthSize, in [0.05, 4] (1 = the old size).</summary>
        public static float ReadEarthSize() => Mathf.Clamp(Knobs.Get(EarthSizeKnob, DefaultEarthSize), 0.05f, 4f);

        /// <summary>The tint that draws the night column at this value (luma at the drawing's middle ink), before fog and grade.</summary>
        public static Color EarthTint(float value)
        {
            float luma = 0.299f * EarthHue.r + 0.587f * EarthHue.g + 0.114f * EarthHue.b;
            float k = value / (luma * Mathf.Lerp(EarthMidInk, NightShade, NightLit));
            return new Color(EarthHue.r * k, EarthHue.g * k, EarthHue.b * k, 1f);
        }

        /// <summary>AOSA C57: paint the Column book as opaque dark-brown earth that the moon does not light, torn at its end
        /// (see EarthKnob). Called after the biome's tints (CombatFx.ApplyTints), and only on a moonlit field; value 0 sets
        /// nothing. The Splash book (a shell in water) and the Wings are not touched.</summary>
        public void NightEarth(float value)
        {
            if (value <= 0f) return;
            var m = mats[(int)Book.Column];
            if (m == null) return;
            PaintNight(m, EarthTint(value));
            m.SetFloat("_Erode", 1f);
        }
        readonly int maxCards;   // MaxCards, or the knob flipbook.maxCards (read in the constructor)
        readonly List<Card> cards = new List<Card>(512);
        readonly Material[] mats = new Material[(int)Book.Count];
        readonly float[] aspect = new float[(int)Book.Count];
        readonly Matrix4x4[] batch = new Matrix4x4[1023];
        Mesh quad;
        public bool Ready { get; private set; }
        public int Alive => cards.Count;

        public FlipbookFx()
        {
            maxCards = Mathf.Max(1, Knobs.Get("flipbook.maxCards", MaxCards));
            float soft = ReadSoft();   // C52: the deep clouds' softness (the shader takes it back to 0 as the lens goes in)
            var shader = Shader.Find("TW/Flipbook (URP)");
            if (shader == null) return;
            int found = 0;
            for (int k = 0; k < Sheets.Length; k++)
            {
                var s = Sheets[k];
                var tex = Resources.Load<Texture2D>("VFX/" + s.Name);
                if (tex == null) continue;
                found++;
                var m = new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave, mainTexture = tex };
                m.SetVector("_Grid", new Vector4(s.Cols, s.Rows, s.Frames, 0f));
                m.SetColor("_Tint", s.Tint);
                m.SetVector("_Levels", new Vector4(s.Low, s.High, 0f, 0f));
                m.SetColor("_Shade", new Color(0.70f, 0.71f, 0.74f));   // a cloud is lit through: its shade is paler than the ground's
                m.SetFloat("_Lit", s.Additive ? 0f : s.Lit > 0f ? s.Lit : 1f);
                m.SetFloat("_MaskOnly", s.MaskOnly ? 1f : 0f);
                m.SetFloat("_Erode", s.Erode ? 1f : 0f);
                m.SetFloat("_ShadeMood", s.Mood > 0f ? s.Mood : 1f);
                m.SetFloat("_Soft", s.Deep ? soft : 0f);
                m.SetFloat("_SrcBlend", (float)(s.Additive ? UnityEngine.Rendering.BlendMode.One : UnityEngine.Rendering.BlendMode.SrcAlpha));
                m.SetFloat("_DstBlend", (float)(s.Additive ? UnityEngine.Rendering.BlendMode.One : UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha));
                m.renderQueue = s.Additive ? 3020 : 3010;
                mats[k] = m;
                aspect[k] = ((float)tex.width / s.Cols) / ((float)tex.height / s.Rows);   // a cell's width over its height
            }
            Ready = found == Sheets.Length;
            quad = new Mesh { name = "Flipbook card", hideFlags = HideFlags.HideAndDontSave };
            quad.SetVertices(new List<Vector3> { new Vector3(-.5f, -.5f, 0f), new Vector3(-.5f, .5f, 0f), new Vector3(.5f, .5f, 0f), new Vector3(.5f, -.5f, 0f) });
            quad.SetUVs(0, new List<Vector2> { new Vector2(0f, 0f), new Vector2(0f, 1f), new Vector2(1f, 1f), new Vector2(1f, 0f) });
            quad.SetTriangles(new[] { 0, 1, 2, 0, 2, 3 }, 0);
            quad.bounds = new Bounds(Vector3.zero, Vector3.one * 100f);
        }

        public void Dispose()
        {
            foreach (var m in mats) if (m != null) Object.Destroy(m);
            if (quad != null) Object.Destroy(quad);
            cards.Clear(); Ready = false;
        }

        /// <summary>The colour a book is drawn in (the men's cloth for a hit, pale for water).</summary>
        public void Tint(Book book, Color color) { if (mats[(int)book] != null) mats[(int)book].SetColor("_Tint", color); }

        /// <summary>
        /// A card of a book at a place: width in metres (height follows the drawing unless given), how long it plays, how it
        /// moves and swells while it does, its roll about the view axis, how bright it starts (glow > 1 is self-lit: the
        /// first moments of a burst, or anything additive; it settles to 1 over the first sixth of the life), and pop: the
        /// fraction of its size it is born at, growing to full in the first fifth (0 = born full size). Every card fades
        /// out over its last third.
        /// </summary>
        public void Add(Book book, Vector3 at, float width, float life, Kind kind = Kind.None, Vector3 velocity = default, float grow = 0f, float roll = 0f, float alpha = 1f, float glow = 1f, float height = 0f, float pop = 0f, float delay = 0f)
        {
            if (!Ready) return;
            if (cards.Count >= maxCards) cards.RemoveAt(0);
            float h = height > 0f ? height : width / Mathf.Max(0.05f, aspect[(int)book]);
            cards.Add(new Card { Pos = at, Vel = velocity, Born = Time.time + delay, Life = Mathf.Max(0.02f, life), Width = width, Height = h, Grow = grow, Roll = roll, Alpha = alpha, Glow = glow, Pop = pop, Book = book, Kind = kind });
        }

        /// <summary>Move, age and draw every card. Clouds before the lights, so the flash is not hidden by its own smoke.</summary>
        public void Draw(float now, Bounds bounds)
        {
            if (!Ready) return;
            float dt = Time.deltaTime;
            for (int i = cards.Count - 1; i >= 0; i--)
            {
                var c = cards[i];
                if (now - c.Born > c.Life) { cards.RemoveAt(i); continue; }
                if (now < c.Born) continue;   // not born yet
                if (c.Vel.sqrMagnitude > 0f) { c.Pos += c.Vel * dt; c.Vel = Vector3.Lerp(c.Vel, Vector3.zero, dt * 0.6f); cards[i] = c; }   // the throw slows; the drift on a long card stays
            }
            for (int b = 0; b < (int)Book.Count; b++)
            {
                var mat = mats[b]; if (mat == null) continue;
                int frames = Sheets[b].Frames, n = 0;
                var rp = new RenderParams(mat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = false };
                for (int i = 0; i < cards.Count; i++)
                {
                    var c = cards[i]; if ((int)c.Book != b || now < c.Born) continue;
                    float k = Mathf.Clamp01((now - c.Born) / c.Life);
                    float swell = 1f + c.Grow * k;
                    if (c.Pop > 0f) { float u = 1f - Mathf.Clamp01(k / 0.2f); swell *= Mathf.Lerp(1f, c.Pop, u * u * u); }   // bursts out of a point, eased
                    float fade = 1f - Mathf.SmoothStep(0f, 1f, (k - 0.65f) / 0.35f);
                    if (Sheets[b].RampIn > 0f) fade *= Mathf.Clamp01((now - c.Born) / Sheets[b].RampIn);
                    float play = Sheets[b].Play > 0f ? Sheets[b].Play : 1f;
                    float frame = (c.Kind & Kind.HoldLast) != 0 ? Mathf.Min(k * frames, frames - 1f) : Mathf.Min(k * frames * play, frames - 1.001f);
                    float bright = 1f + (c.Glow - 1f) * (1f - Mathf.Clamp01(k / 0.15f));   // the fire is out in the first sixth
                    batch[n++] = Pack(c.Pos, c.Width * swell, c.Height * swell, frame, fade, bright, c.Roll, c.Kind, c.Alpha);
                    if (n == batch.Length) { FrameBudget.Draw(rp, quad, 0, batch, n); n = 0; }
                }
                if (n > 0) FrameBudget.Draw(rp, quad, 0, batch, n);
            }
        }

        /// <summary>Draw cards the caller keeps itself (a gas field, a persistent cloud): packed records, any count.</summary>
        public void DrawPacked(Book book, List<Matrix4x4> packed, Bounds bounds)
        {
            if (!Ready || packed.Count == 0) return;
            var mat = mats[(int)book]; if (mat == null) return;
            var rp = new RenderParams(mat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = false };
            for (int start = 0; start < packed.Count; start += batch.Length)
            {
                int n = Mathf.Min(batch.Length, packed.Count - start);
                packed.CopyTo(start, batch, 0, n);
                FrameBudget.Draw(rp, quad, 0, batch, n);
            }
        }

        /// <summary>The record TW/Flipbook reads out of the instance matrix (see the shader's header).</summary>
        public static Matrix4x4 Pack(Vector3 at, float width, float height, float frame, float fade, float bright, float roll, Kind kind, float opacity = 1f)
        {
            var m = Matrix4x4.identity;
            m.m03 = at.x; m.m13 = at.y; m.m23 = at.z;
            m.m00 = width; m.m11 = height; m.m22 = frame;
            m.m01 = fade; m.m10 = bright; m.m02 = roll;
            m.m12 = (kind & Kind.Upright) != 0 ? 1f : 0f;
            m.m20 = (kind & Kind.Anchored) != 0 ? 1f : 0f;
            m.m21 = (kind & Kind.Mirror) != 0 ? -opacity : opacity;
            return m;
        }

        /// <summary>A direction's angle on the screen, for a card drawn along it (the muzzle flare points along the shot).</summary>
        public static float ScreenRoll(Camera cam, Vector3 direction)
        {
            if (cam == null) return 0f;
            var t = cam.transform;
            return Mathf.Atan2(Vector3.Dot(direction, t.up), Vector3.Dot(direction, t.right));
        }
    }
}
