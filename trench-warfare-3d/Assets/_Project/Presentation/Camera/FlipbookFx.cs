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
            Fire,       // rolling fire: the flamethrower's stream, a pool of burning fuel, a man alight (additive)
            Pyre,       // the same fire standing up and licking: a big thing burning, what a pool settles into (additive)
            Fireball,   // fuel going up at once: the flamethrower's tank when he is killed (additive)
            // round 3: the rest of the pack. Tools/firebooks.py throws the pack's colour away, so a blue sheet is not a
            // blue effect - it is a different DRAWING, and these are the drawings the fire had been faking until now.
            Jet,        // the stream itself, drawn rooted at its left edge: it starts, holds and stops like a valve
            Blast,      // one huge soot-ringed fireball with a hot heart: the tank going up, in a single drawing
            Fan,        // the cone a stream throws where it lands on something
            Stand,      // a standing flame with a skirt and tongues, looping: a big thing burning, a man alight
            Pool,       // a puddle with a blob rising off it: burning fuel lying on the ground
            Core,       // a second, denser stream drawing: the opaque inside of the jet, and a different glyph to Jet
            Head,       // a CLOSED bolus: the jet's leading mass, where a curl's hole would be fatal
            Bloom,      // the jet's TERMINUS: a ground-rooted bloom, where the stream stops being a jet and goes up
            Count
        }

        [System.Flags] public enum Kind : byte { None = 0, Upright = 1, Anchored = 2, Mirror = 4, HoldLast = 8 }

        struct Card
        {
            public Vector3 Pos, Vel;
            public float Born, Life, Width, Height, Grow, Roll, Alpha, Glow, Pop, Start;
            public Book Book; public Kind Kind;
        }

        // one book: its texture in Resources/VFX, grid, whether it adds light or is a cloud the moon lights, and which of
        // the drawing's values are its shade and its light (Low, High: measured from the pixels, so each book uses both bands)
        struct Sheet { public string Name; public int Cols, Rows, Frames; public bool Additive, MaskOnly, Erode, Snap, Fire; public Color Tint; public float Low, High, Play, Lit, RampIn, Mood, Fps; public bool Cycle; public Vector4 Bands; public Vector2 Ink; public float Fill; public float Rise; }
        // Bands: a fire book's own cel cuts (soot|fringe|body|core, then edge softness), read off ITS ink histogram by
        // Tools/firebooks.py; left at zero the shader's default is used, which was measured on FireBall. Ink: where the
        // drawing sits inside its cell as bottom-up fractions, so a card standing on something can be sized and sunk to
        // put the DRAWING on the ground (see Flamethrower.Standing). Fill: how much of the cell's width it uses. All
        // three are per book because they are facts about a drawing, not tuning.
        // Snap: the book was drawn frame by frame and is played that way - each cel is CUT to, never dissolved into the
        // next (TW/Flipbook reads it off _Grid.w). Fps: play it at the rate it was drawn at rather than stretching the
        // Fire: drawn OVER the frame premultiplied rather than added to it (see TW/Flipbook's note). Fire is the one
        // thing here that is a drawing AND is bright, and additive can only do the bright half of that.
        // Cycle: the book is a loop of a thing that keeps happening (fire) rather than an event that happens once
        // (a burst). A cycling card WRAPS instead of holding its last frame, and may be entered part-way through, which
        // matters more than it sounds: the first frames of a fire book are the flame growing in, so a short card played
        // from frame 0 shows nothing but the weak start of it, over and over, and a fire made of short cards never
        // looks alight. Entered at a random frame, every card is a developed flame and no two are in step.
        // whole book over the card's life. The two go together: a drawing held for its frame and then cut away from is
        // what makes hand-drawn animation read as drawn, and a book stretched to fit a card plays at whatever rate the
        // card happened to want. A book with no Fps keeps the old behaviour and spends itself over the life exactly.   // Play: the part of the book used (0 = all); Lit: 0 = own values (default: additive 0, else 1); RampIn: seconds to fade in; Mood: how much the mood tints the shade (0 = default 1)
        static readonly Sheet[] Sheets =
        {
            new Sheet { Name = "Burst",  Cols = 4, Rows = 4, Frames = 16, Erode = true, Tint = new Color(0.50f, 0.53f, 0.58f), Low = 0.12f, High = 0.62f, Play = 0.7f, Mood = 0.45f },   // a cloud born of fire: the full night tint turned it saturated blue
            new Sheet { Name = "Column", Cols = 4, Rows = 4, Frames = 16, Tint = new Color(0.40f, 0.33f, 0.26f), Low = 0.30f, High = 0.95f },
            new Sheet { Name = "Column", Cols = 4, Rows = 4, Frames = 16, Tint = new Color(0.60f, 0.66f, 0.76f), Low = 0.28f, High = 0.85f },
            new Sheet { Name = "Wings",  Cols = 4, Rows = 4, Frames = 16, Erode = true, Play = 0.75f, Tint = new Color(0.74f, 0.65f, 0.52f), Low = 0.15f, High = 0.42f },
            new Sheet { Name = "Spurt",  Cols = 2, Rows = 5, Frames = 10, Tint = new Color(0.86f, 0.78f, 0.64f), Low = 0.50f, High = 0.80f },
            new Sheet { Name = "Puff",   Cols = 3, Rows = 3, Frames = 9,  Tint = new Color(0.86f, 0.80f, 0.66f), Low = 0.20f, High = 0.50f },
            new Sheet { Name = "Puff",   Cols = 3, Rows = 3, Frames = 9,  Erode = true, Tint = new Color(0.50f, 0.47f, 0.43f), Low = 0.10f, High = 0.60f, Play = 0.6f, Lit = 0.6f, RampIn = 0.3f, Mood = 0.45f },   // smoke: warm grey, and only half the moon's blue in its shade (it read as blue cotton at night)
            new Sheet { Name = "Puff",   Cols = 3, Rows = 3, Frames = 9,  Tint = new Color(0.74f, 0.80f, 0.34f), Low = 0.15f, High = 0.60f, Lit = 0.75f },
            new Sheet { Name = "Muzzle", Cols = 3, Rows = 4, Frames = 12, Additive = true, Tint = new Color(1.0f, 0.78f, 0.42f), Low = 0f, High = 1f },
            new Sheet { Name = "Star",   Cols = 1, Rows = 1, Frames = 1,  Additive = true, MaskOnly = true, Tint = new Color(1.0f, 0.88f, 0.62f), Low = 0f, High = 1f },
            new Sheet { Name = "Flash",  Cols = 1, Rows = 1, Frames = 1,  Additive = true, Tint = new Color(1.0f, 0.80f, 0.50f), Low = 0f, High = 1f },
            // fire (generated from the owner's flipbook pack by Tools/firebooks.py). Additive, so the dark smoke the
            // pack drew into them adds nothing and only the flame survives - the smoke off a fire is its own cards.
            // A deep orange tint taken past 1 by the caller's glow is what gives fire its colour ramp: the heart of a
            // tongue clips through yellow to white in the bloom, the fringe keeps the tint. Each plays only the living
            // part of its book (Play). The rolling fire tears into islands as it goes (Erode), the way fire leaves; the
            // standing one does not, because an eroding card is darkened 22% along its underside (a cloud lies in its own
            // shadow) and the base of a flame is the hottest part of it.
            new Sheet { Name = "FireBall",   Cycle = true, Cols = 8, Rows = 4, Frames = 32, Fire = true, Snap = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.16f, High = 0.86f, Play = 0.62f, Fps = 12f, Bands = new Vector4(0.00f, 0.21f, 0.88f, 0.75f), Ink = new Vector2(0.23f, 0.72f), Fill = 0.90f },
            new Sheet { Name = "FireColumn", Cycle = true, Cols = 8, Rows = 4, Frames = 30, Fire = true, Snap = true, Tint = new Color(1.3f, 0.64f, 0.19f), Low = 0.18f, High = 0.88f, Play = 0.80f, Fps = 12f, Rise = 0.70f, Bands = new Vector4(0.00f, 0.05f, 0.42f, 0.75f), Ink = new Vector2(0.29f, 0.71f), Fill = 0.90f },
            new Sheet { Name = "FireBurst",  Cols = 8, Rows = 4, Frames = 32, Fire = true, Snap = true, Tint = new Color(1.4f, 0.68f, 0.24f), Low = 0.14f, High = 0.84f, Fps = 12f, Rise = 0.55f, Bands = new Vector4(0.00f, 0.04f, 0.31f, 0.75f), Ink = new Vector2(0.28f, 0.78f), Fill = 0.90f },
            // FireStand's window is wide open, and the reason is worth keeping. It was narrowed to 0.62 to make the
            // book brighter and it came out DARKER - twenty luminance darker, the dimmest fire in the build. The
            // window was innocent: narrowing it pushes the measured band cuts up with it, and at 0.62 the core cut
            // computed to 1.00 exactly. Saturated. Nothing in the drawing could ever reach the core band, so the
            // hottest thing in a standing flame was its mid-tone. Whenever Levels move, the Bands MUST be re-measured
            // against the new window, and a core cut that lands at 1.00 means the band has been switched off.
            // Round 3 (measured 2026-09-25 by Tools/firebooks.py). Every fire book is cut to the SAME value
            // hierarchy - about 15% soot, 16% red rim, 46% orange body, 23% pale heart - and each book's thresholds are
            // the percentiles of its own ink that produce it.
            //
            // That split is deliberately LIGHT, and it was arrived at by getting it wrong in both directions. Matching
            // FireBall's old shares gave 26% soot and 8% core, which is a textbook cel hierarchy on paper and came out
            // maroon in the game: this is a night palette under volumetric fog, the cards are small at tactical zoom,
            // and the two dark bands simply ate the read - a jet across the field looked like spilled blood on churned
            // mud. A heart of 8% also disappears below about 300 px of screen height, which is every card here except a
            // fire the camera is standing next to. Earlier still, picking the numbers by eye put a third of a sheet in
            // the top band and the fire rendered as one cream slab. Fog and darkness are a tax on contrast, so the
            // drawing has to be cut brighter than a sheet of paper would want.
            //
            // FireJet is the one exception, at 12% core rather than 23%. Share is not area: the stream is ONE card as
            // long as the reach, so a share that is a highlight on a two-metre flame is a cream slab eleven metres
            // across. A book's core share has to be read against the size the card is actually drawn at.
            //
            // FireColumn and FireBurst cannot reach 15% soot: 36% and 41% of their drawn pixels are PURE BLACK, because
            // they are not flame drawings - they are a burst of smoke with a crescent of flame at the foot. Their soot
            // cut is therefore 0, which means "only true black is soot", and it is the closest they get. Left on the
            // shared default they sat at 64% and 70% soot and Book.Pyre rendered as a near-black slab standing through
            // the middle of every pyre and cook-off.
            new Sheet { Name = "FireJet",   Cols = 8, Rows = 4, Frames = 29, Fire = true, Snap = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.11f, High = 0.80f, Fps = 12f, Bands = new Vector4(0.07f, 0.23f, 0.89f, 0.75f), Ink = new Vector2(0.25f, 0.77f), Fill = 0.90f },
            new Sheet { Name = "FireBlast", Cols = 8, Rows = 4, Frames = 32, Fire = true, Snap = true, Tint = new Color(1.40f, 0.66f, 0.20f), Low = 0.11f, High = 0.75f, Fps = 12f, Rise = 0.45f, Bands = new Vector4(0.08f, 0.30f, 0.96f, 0.75f), Ink = new Vector2(0.23f, 0.77f), Fill = 0.90f },
            new Sheet { Name = "FireFan",   Cols = 8, Rows = 4, Frames = 16, Fire = true, Snap = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.09f, High = 0.75f, Fps = 12f, Bands = new Vector4(0.02f, 0.08f, 0.64f, 0.75f), Ink = new Vector2(0.00f, 1.00f), Fill = 0.98f },
            new Sheet { Name = "FireStand", Cycle = true, Cols = 8, Rows = 4, Frames = 16, Fire = true, Snap = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.13f, High = 1.00f, Fps = 12f, Rise = 0.62f, Bands = new Vector4(0.05f, 0.13f, 0.73f, 0.75f), Ink = new Vector2(0.11f, 0.90f), Fill = 0.83f },
            new Sheet { Name = "FirePool",  Cycle = true, Cols = 8, Rows = 4, Frames = 16, Fire = true, Snap = true, Tint = new Color(1.30f, 0.58f, 0.16f), Low = 0.00f, High = 0.53f, Fps = 12f, Rise = 0.80f, Play = 0.75f, Bands = new Vector4(0.42f, 0.51f, 0.80f, 0.75f), Ink = new Vector2(0.06f, 0.94f), Fill = 1.00f },
            new Sheet { Name = "FireHead",  Cols = 8, Rows = 4, Frames = 19, Fire = true, Snap = true, Tint = new Color(1.44f, 0.66f, 0.18f), Low = 0.08f, High = 0.79f, Fps = 12f, Bands = new Vector4(0.03f, 0.05f, 0.29f, 0.75f), Ink = new Vector2(0.23f, 0.80f), Fill = 0.89f },
            new Sheet { Name = "FireBloom", Cols = 8, Rows = 4, Frames = 21, Fire = true, Snap = true, Tint = new Color(1.38f, 0.64f, 0.18f), Low = 0.12f, High = 0.74f, Fps = 12f, Bands = new Vector4(0.01f, 0.02f, 0.39f, 0.75f), Ink = new Vector2(0.05f, 0.98f), Fill = 0.84f },
            new Sheet { Name = "FireCore",  Cols = 8, Rows = 4, Frames = 22, Fire = true, Snap = true, Tint = new Color(1.42f, 0.64f, 0.16f), Low = 0.11f, High = 0.76f, Fps = 12f, Bands = new Vector4(0.01f, 0.05f, 0.51f, 0.75f), Ink = new Vector2(0.23f, 0.76f), Fill = 0.90f },
        };

        /// <summary>Where a book's drawing sits in its cell: y as bottom-up fractions, and its width as a fraction of the cell.</summary>
        public static void Geometry(Book b, out float inkLow, out float inkHigh, out float fill)
        {
            var s = Sheets[(int)b];
            // a book with no measurement carries FireBall's, which is what the old shared constants were
            inkLow = s.Ink.y > 0f ? s.Ink.x : 0.23f;
            inkHigh = s.Ink.y > 0f ? s.Ink.y : 0.72f;
            fill = s.Fill > 0f ? s.Fill : 0.90f;
        }

        public const int MaxCards = 1536;
        readonly List<Card> cards = new List<Card>(512);
        readonly Material[] mats = new Material[(int)Book.Count];
        readonly float[] aspect = new float[(int)Book.Count];
        readonly Matrix4x4[] batch = new Matrix4x4[1023];
        Mesh quad;
        public bool Ready { get; private set; }
        public int Alive => cards.Count;

        public FlipbookFx()
        {
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
                m.SetVector("_Grid", new Vector4(s.Cols, s.Rows, s.Frames, s.Snap ? 1f : 0f));
                m.SetColor("_Tint", s.Tint);
                m.SetVector("_Levels", new Vector4(s.Low, s.High, 0f, 0f));
                m.SetColor("_Shade", new Color(0.70f, 0.71f, 0.74f));   // a cloud is lit through: its shade is paler than the ground's
                m.SetFloat("_Lit", s.Additive || s.Fire ? 0f : s.Lit > 0f ? s.Lit : 1f);   // fire is its own light, like the additive books
                m.SetFloat("_MaskOnly", s.MaskOnly ? 1f : 0f);
                m.SetFloat("_Fire", s.Fire ? 1f : 0f);
                if (s.Bands.sqrMagnitude > 0f) m.SetVector("_Bands", s.Bands);
                m.SetFloat("_Rise", s.Rise);   // how far the top of the card is rotated toward umber; standing flames only   // this book's own cel cuts, else the shader's (FireBall's)
                m.SetFloat("_Erode", s.Erode ? 1f : 0f);
                m.SetFloat("_ShadeMood", s.Mood > 0f ? s.Mood : 1f);
                // fire is premultiplied over (One, OneMinusSrcAlpha), additive books add, everything else is straight alpha
                m.SetFloat("_SrcBlend", (float)(s.Additive || s.Fire ? UnityEngine.Rendering.BlendMode.One : UnityEngine.Rendering.BlendMode.SrcAlpha));
                m.SetFloat("_DstBlend", (float)(s.Additive ? UnityEngine.Rendering.BlendMode.One : UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha));
                m.renderQueue = s.Additive ? 3020 : s.Fire ? 3015 : 3010;
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
        public void Add(Book book, Vector3 at, float width, float life, Kind kind = Kind.None, Vector3 velocity = default, float grow = 0f, float roll = 0f, float alpha = 1f, float glow = 1f, float height = 0f, float pop = 0f, float delay = 0f, float startFrame = 0f)
        {
            if (!Ready) return;
            if (cards.Count >= MaxCards) cards.RemoveAt(0);
            float h = height > 0f ? height : width / Mathf.Max(0.05f, aspect[(int)book]);
            cards.Add(new Card { Pos = at, Vel = velocity, Born = Time.time + delay, Life = Mathf.Max(0.02f, life), Width = width, Height = h, Grow = grow, Roll = roll, Alpha = alpha, Glow = glow, Pop = pop, Start = startFrame, Book = book, Kind = kind });
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
                    float fps = Sheets[b].Fps;
                    float run = (fps > 0f ? (now - c.Born) * fps : k * frames * play) + c.Start;   // at the rate it was drawn, or spread over the life
                    float span = frames * play - 1.001f;
                    float frame = (c.Kind & Kind.HoldLast) != 0 ? Mathf.Min(k * frames, frames - 1f)
                                : Sheets[b].Cycle ? Mathf.Repeat(run, span) : Mathf.Min(run, span);
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
