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
        struct Sheet { public string Name; public int Cols, Rows, Frames; public bool Additive, MaskOnly, Erode; public Color Tint; public float Low, High, Play, Lit, RampIn; }   // Play: the part of the book used (0 = all); Lit: 0 = own values (default: additive 0, else 1); RampIn: seconds to fade in
        static readonly Sheet[] Sheets =
        {
            new Sheet { Name = "Burst",  Cols = 4, Rows = 4, Frames = 16, Erode = true, Tint = new Color(0.50f, 0.53f, 0.58f), Low = 0.12f, High = 0.62f, Play = 0.7f },
            new Sheet { Name = "Column", Cols = 4, Rows = 4, Frames = 16, Tint = new Color(0.40f, 0.33f, 0.26f), Low = 0.30f, High = 0.95f },
            new Sheet { Name = "Column", Cols = 4, Rows = 4, Frames = 16, Tint = new Color(0.60f, 0.66f, 0.76f), Low = 0.28f, High = 0.85f },
            new Sheet { Name = "Wings",  Cols = 4, Rows = 4, Frames = 16, Erode = true, Play = 0.75f, Tint = new Color(0.74f, 0.65f, 0.52f), Low = 0.15f, High = 0.42f },
            new Sheet { Name = "Spurt",  Cols = 2, Rows = 5, Frames = 10, Tint = new Color(0.86f, 0.78f, 0.64f), Low = 0.50f, High = 0.80f },
            new Sheet { Name = "Puff",   Cols = 3, Rows = 3, Frames = 9,  Tint = new Color(0.86f, 0.80f, 0.66f), Low = 0.20f, High = 0.50f },
            new Sheet { Name = "Puff",   Cols = 3, Rows = 3, Frames = 9,  Erode = true, Tint = new Color(0.46f, 0.47f, 0.50f), Low = 0.10f, High = 0.60f, Play = 0.6f, Lit = 0.6f, RampIn = 0.3f },
            new Sheet { Name = "Muzzle", Cols = 3, Rows = 4, Frames = 12, Additive = true, Tint = new Color(1.0f, 0.78f, 0.42f), Low = 0f, High = 1f },
            new Sheet { Name = "Star",   Cols = 1, Rows = 1, Frames = 1,  Additive = true, MaskOnly = true, Tint = new Color(1.0f, 0.88f, 0.62f), Low = 0f, High = 1f },
            new Sheet { Name = "Flash",  Cols = 1, Rows = 1, Frames = 1,  Additive = true, Tint = new Color(1.0f, 0.80f, 0.50f), Low = 0f, High = 1f },
        };

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
                m.SetVector("_Grid", new Vector4(s.Cols, s.Rows, s.Frames, 0f));
                m.SetColor("_Tint", s.Tint);
                m.SetVector("_Levels", new Vector4(s.Low, s.High, 0f, 0f));
                m.SetColor("_Shade", new Color(0.70f, 0.71f, 0.74f));   // a cloud is lit through: its shade is paler than the ground's
                m.SetFloat("_Lit", s.Additive ? 0f : s.Lit > 0f ? s.Lit : 1f);
                m.SetFloat("_MaskOnly", s.MaskOnly ? 1f : 0f);
                m.SetFloat("_Erode", s.Erode ? 1f : 0f);
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
            if (cards.Count >= MaxCards) cards.RemoveAt(0);
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
                    if (n == batch.Length) { Graphics.RenderMeshInstanced(rp, quad, 0, batch, n); n = 0; }
                }
                if (n > 0) Graphics.RenderMeshInstanced(rp, quad, 0, batch, n);
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
