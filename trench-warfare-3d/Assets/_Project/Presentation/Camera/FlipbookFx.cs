// Phase: C4 (implemented) — the drawn part of the fight. Hand-painted flipbooks (Resources/VFX, from the SrRubfish and
// Hun0FX packs the team owns) played on camera-facing cards, one instanced draw a book, no GameObjects. CombatFx decides
// what happens where; this only keeps the cards alive, moves and grows them, and packs each into the matrix TW/Flipbook
// reads. A card lives Life seconds and plays its book once over that time.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class FlipbookFx
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
            // VFX pass wave 1 (2026-09-28): pack sheets and new desktop sheets, rows below in this order (FlipbookOrdinalTests)
            GasBank,    // a chlorine cloud lying on the field
            GasVent,    // gas let out of a canister or the Censer
            SmokeBank,  // a smoke screen's bank
            MortarBurst,// a lobbed round's burst: no lean
            ShellFall,  // the falling shell, before it lands
            LeanBurst,  // prototype of the directional burst (ShellLean replaces it)
            FireCookOff,// a hull cooking off: the fireball
            FireTall,   // a tree or post burning, looping
            GunBlast,   // a big gun's muzzle blast, rooted at its left edge
            Smoulder,   // a crater smouldering, looping
            GroundRing, // the shock ring a big burst throws along the ground, seen from above
            DustPuff,   // dust where a round or a man lands
            ShellPlume, // the smoke a shell burst leaves standing
            // VFX pass wave 2 (2026-09-28)
            FireLance,  // the beam: a pillar of fire pouring down, steady (loops)
            MendSparks, // a welding torch's sparks: an engineer mending (loops)
            ShellLean,  // earth thrown one way: the burst of a shell that came in flying
            WreckSmoke, // black smoke standing over a burning wreck (loops)
            // blood on hits (owner, 2026-09-28), rooted at the left edge: the spray goes along the round
            BloodSpurt, // a rifle hit
            BloodSnipe, // a heavy hit: a sniper's, a machine gun's burst at close range
            // each class its own muzzle (owner, 2026-09-29; tw3d-board tools/muzzlebooks.py), fire books rooted at the left edge
            MuzzleBrake,  // the sniper's brake: a forward jet and two side jets square to the barrel
            MuzzleStream, // the machine gun: a long tongue, a star at the root
            MuzzlePop,    // the pistol and the machine pistol: a round spiky pop
            MuzzleBurst,  // the SMG: a fat cone opening into petals
            MuzzleCarbine,// the officer's carbine: the cone with three petals, shorter
            MuzzleRifle,  // the rifle by day up close: a plain medium cone, its soot ring the edge on snow
            Count
        }

        [System.Flags] public enum Kind : byte { None = 0, Upright = 1, Anchored = 2, Mirror = 4, HoldLast = 8, Flat = 16 }   // Flat: lying on the ground, facing up (wins over Upright)

        struct Card
        {
            public Vector3 Pos, Vel;
            public float Born, Life, Width, Height, Grow, Roll, Alpha, Glow, Pop, Start;
            public float Cut;         // AOSA C109 (fx.columnPlay): the part of its life (and so of its book) it plays before it is gone (0 = all)
            public float Soil, Cap;   // AOSA C103 (fx.columnSoil): how far this column plays the soil heave (SoilShape; 0 = the book's own timing), and its height cap over men (SoilCap)
            public Book Book; public Kind Kind;
        }

        // one book: its texture in Resources/VFX, grid, whether it adds light or is a cloud the moon lights, and which of
        // the drawing's values are its shade and its light (Low, High: measured from the pixels, so each book uses both bands)
        struct Sheet { public string Name; public int Cols, Rows, Frames; public bool Additive, MaskOnly, Erode, Snap, Fire; public Color Tint; public float Low, High, Play, Lit, RampIn, Mood, Fps; public bool Cycle; public Vector4 Bands; public Vector2 Ink; public float Fill; public float Rise; public bool Deep; public bool Ground; }
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
        // Deep: a cloud as deep as it is wide (fx.smokeSoft)
        static readonly Sheet[] Sheets =
        {
            new Sheet { Name = "Burst",  Cols = 4, Rows = 4, Frames = 16, Erode = true, Tint = new Color(0.58f, 0.53f, 0.48f), Low = 0.12f, High = 0.62f, Play = 0.7f, Mood = 0.45f, Deep = true },   // a cloud born of fire: the full night tint turned it saturated blue; a warm grey, not brown (bench s1: 0.55/0.50/0.45 read as mud)
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
            // fire (generated from the owner's flipbook pack by Tools/firebooks.py). Additive, so the dark smoke the
            // pack drew into them adds nothing and only the flame survives - the smoke off a fire is its own cards.
            // A deep orange tint taken past 1 by the caller's glow is what gives fire its colour ramp: the heart of a
            // tongue clips through yellow to white in the bloom, the fringe keeps the tint. Each plays only the living
            // part of its book (Play). The rolling fire tears into islands as it goes (Erode), the way fire leaves; the
            // standing one does not, because an eroding card is darkened 22% along its underside (a cloud lies in its own
            // shadow) and the base of a flame is the hottest part of it.
            new Sheet { Name = "FireBall",   Cycle = true, Cols = 8, Rows = 4, Frames = 32, Fire = true, Snap = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.16f, High = 0.86f, Play = 0.62f, Fps = 12f, Bands = new Vector4(0.00f, 0.21f, 0.88f, 0.75f), Ink = new Vector2(0.23f, 0.72f), Fill = 0.90f },
            new Sheet { Name = "FireColumn", Cycle = true, Cols = 8, Rows = 4, Frames = 30, Fire = true, Snap = true, Tint = new Color(1.3f, 0.64f, 0.19f), Low = 0.18f, High = 0.88f, Play = 0.80f, Fps = 12f, Rise = 0.70f, Bands = new Vector4(0.00f, 0.05f, 0.42f, 0.75f), Ink = new Vector2(0.29f, 0.71f), Fill = 0.90f },
            // Book.Fireball is the shell's fireball (CombatFx.ShellFire.cs): cut so the 44 % of its ink that is pure black stays the
            // soot drawn into it (the reference's dark flecks in the fire) and the heart cut at 0.90, about a tenth (at 0.80 a
            // quarter of it was white: critique c2); and
            // its soot in the burst cloud's own brown (below), so the smoke drawn into the fire runs into the cloud behind it and
            // the flame reads as coming out from inside the cloud. (Bench c1 drew it under the cloud instead: the cloud hid all of it.)
            new Sheet { Name = "FireBurst",  Cols = 8, Rows = 4, Frames = 32, Fire = true, Snap = true, Tint = new Color(1.4f, 0.68f, 0.24f), Low = 0.14f, High = 0.84f, Fps = 12f, Rise = 0.55f, Bands = new Vector4(0.004f, 0.05f, 0.90f, 0.75f), Ink = new Vector2(0.28f, 0.78f), Fill = 0.90f },
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
            new Sheet { Name = "FireCore",  Cols = 8, Rows = 4, Frames = 22, Fire = true, Snap = true, Tint = new Color(1.42f, 0.64f, 0.16f), Low = 0.11f, High = 0.76f, Fps = 12f, Bands = new Vector4(0.01f, 0.05f, 0.51f, 0.75f), Ink = new Vector2(0.23f, 0.76f), Fill = 0.90f },
            new Sheet { Name = "FireHead",  Cols = 8, Rows = 4, Frames = 19, Fire = true, Snap = true, Tint = new Color(1.44f, 0.66f, 0.18f), Low = 0.08f, High = 0.79f, Fps = 12f, Bands = new Vector4(0.03f, 0.05f, 0.29f, 0.75f), Ink = new Vector2(0.23f, 0.80f), Fill = 0.89f },
            new Sheet { Name = "FireBloom", Cols = 8, Rows = 4, Frames = 21, Fire = true, Snap = true, Tint = new Color(1.38f, 0.64f, 0.18f), Low = 0.12f, High = 0.74f, Fps = 12f, Bands = new Vector4(0.01f, 0.02f, 0.39f, 0.75f), Ink = new Vector2(0.05f, 0.98f), Fill = 0.84f },
            // VFX pass wave 1 (2026-09-28): Low/High, Ink, Fill and the fire Bands from the Tools/firebooks.py printout.
            // Tints are starting points: CombatFx.ApplyTints sets the biome's (catalogue IN-2). Smoke-like books erode like Smoke.
            new Sheet { Name = "GasBank", Cols = 8, Rows = 4, Frames = 30, Snap = true, Fps = 12f, Erode = true, Deep = true, Mood = 0.45f, Tint = new Color(0.74f, 0.80f, 0.34f), Low = 0.17f, High = 0.73f, Ink = new Vector2(0.00f, 0.35f), Fill = 0.94f },
            new Sheet { Name = "GasVent", Cols = 8, Rows = 4, Frames = 32, Snap = true, Fps = 12f, Mood = 0.45f, Tint = new Color(0.74f, 0.80f, 0.34f), Low = 0.17f, High = 0.66f, Ink = new Vector2(0.00f, 0.35f), Fill = 0.94f },
            new Sheet { Name = "SmokeBank", Cols = 8, Rows = 4, Frames = 22, Snap = true, Fps = 12f, Erode = true, Deep = true, Mood = 0.45f, Tint = new Color(0.50f, 0.47f, 0.43f), Low = 0.17f, High = 0.31f, Ink = new Vector2(0.03f, 0.87f), Fill = 0.94f },
            new Sheet { Name = "MortarBurst", Cols = 8, Rows = 4, Frames = 21, Snap = true, Fps = 12f, Mood = 0.45f, Tint = new Color(0.40f, 0.33f, 0.26f), Low = 0.09f, High = 0.77f, Ink = new Vector2(0.05f, 0.65f), Fill = 0.54f },
            new Sheet { Name = "ShellFall", Cols = 8, Rows = 4, Frames = 8, Snap = true, Fps = 12f, Mood = 0.45f, Tint = new Color(0.40f, 0.33f, 0.26f), Low = 0.08f, High = 0.74f, Ink = new Vector2(0.07f, 0.95f), Fill = 0.25f },
            new Sheet { Name = "LeanBurst", Cols = 8, Rows = 4, Frames = 28, Snap = true, Fps = 12f, Mood = 0.45f, Tint = new Color(0.40f, 0.33f, 0.26f), Low = 0.11f, High = 0.73f, Ink = new Vector2(0.26f, 0.73f), Fill = 0.90f },
            new Sheet { Name = "FireCookOff", Cols = 8, Rows = 4, Frames = 29, Snap = true, Fps = 12f, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.08f, High = 0.90f, Bands = new Vector4(0.07f, 0.28f, 0.75f, 0.75f), Ink = new Vector2(0.07f, 0.91f), Fill = 0.94f },
            new Sheet { Name = "FireTall", Cols = 8, Rows = 4, Frames = 24, Snap = true, Fps = 12f, Cycle = true, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.08f, High = 0.85f, Bands = new Vector4(0.11f, 0.28f, 0.65f, 0.75f), Ink = new Vector2(0.05f, 0.99f), Fill = 0.62f },
            new Sheet { Name = "FireGunBlast", Cols = 8, Rows = 4, Frames = 12, Snap = true, Fps = 12f, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.11f, High = 0.95f, Bands = new Vector4(0.11f, 0.38f, 0.57f, 0.75f), Ink = new Vector2(0.23f, 0.78f), Fill = 0.60f },
            new Sheet { Name = "Smoulder", Cols = 8, Rows = 4, Frames = 32, Snap = true, Fps = 12f, Cycle = true, Erode = true, Deep = true, Mood = 0.45f, Tint = new Color(0.50f, 0.47f, 0.43f), Low = 0.07f, High = 0.85f, Ink = new Vector2(0.05f, 0.92f), Fill = 0.45f },
            new Sheet { Name = "GroundRing", Cols = 8, Rows = 4, Frames = 32, Snap = true, Fps = 0f, Ground = true, Lit = 0.75f, Mood = 0.45f, Tint = new Color(0.86f, 0.78f, 0.64f), Low = 0.20f, High = 0.81f, Ink = new Vector2(0.12f, 0.88f), Fill = 0.94f },
            new Sheet { Name = "DustPuff", Cols = 8, Rows = 4, Frames = 28, Snap = true, Fps = 12f, Mood = 0.45f, Tint = new Color(0.86f, 0.78f, 0.64f), Low = 0.16f, High = 0.91f, Ink = new Vector2(0.18f, 0.82f), Fill = 0.90f },
            new Sheet { Name = "ShellPlume", Cols = 8, Rows = 4, Frames = 23, Snap = true, Fps = 12f, Erode = true, Deep = true, Mood = 0.45f, Lit = 0.8f, RampIn = 0.3f, Tint = new Color(0.50f, 0.47f, 0.43f), Low = 0.17f, High = 1.30f, Ink = new Vector2(0.02f, 0.95f), Fill = 0.90f },
            // wave 2: Low/High, Ink, Fill and the fire Bands from the firebooks printout (tw3d-board logs/firebooks-w2.log)
            new Sheet { Name = "FireLance", Cols = 8, Rows = 4, Frames = 28, Snap = true, Fps = 12f, Cycle = true, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.08f, High = 0.92f, Bands = new Vector4(0.12f, 0.30f, 0.59f, 0.75f), Ink = new Vector2(0.02f, 0.97f), Fill = 0.62f },
            new Sheet { Name = "MendSparks", Cols = 8, Rows = 4, Frames = 32, Snap = true, Fps = 12f, Cycle = true, Fire = true, Tint = new Color(1.40f, 0.78f, 0.32f), Low = 0.00f, High = 0.59f, Bands = new Vector4(0.13f, 0.25f, 0.43f, 0.75f), Ink = new Vector2(0.09f, 0.95f), Fill = 0.90f },
            new Sheet { Name = "ShellLean", Cols = 8, Rows = 4, Frames = 32, Snap = true, Fps = 12f, Mood = 0.45f, Lit = 0.8f, Tint = new Color(0.40f, 0.33f, 0.26f), Low = 0.15f, High = 0.95f, Ink = new Vector2(0.13f, 0.89f), Fill = 0.87f },   // High past its p97 0.79: the pale core must not light up (ShellPlume, bench r4)
            new Sheet { Name = "WreckSmoke", Cols = 8, Rows = 4, Frames = 28, Snap = true, Fps = 12f, Cycle = true, Erode = true, Deep = true, Mood = 0.45f, Lit = 0.6f, RampIn = 0.3f, Tint = new Color(0.50f, 0.47f, 0.43f), Low = 0.11f, High = 1.20f, Ink = new Vector2(0.02f, 0.95f), Fill = 0.43f },
            new Sheet { Name = "BloodSpurt", Cols = 8, Rows = 4, Frames = 28, Snap = true, Fps = 12f, Mood = 0.45f, Lit = 0.5f, Tint = new Color(0.62f, 0.07f, 0.05f), Low = 0.00f, High = 0.75f, Ink = new Vector2(0.00f, 1.00f), Fill = 0.98f },
            new Sheet { Name = "BloodSnipe", Cols = 8, Rows = 4, Frames = 25, Snap = true, Fps = 12f, Mood = 0.45f, Lit = 0.5f, Tint = new Color(0.62f, 0.07f, 0.05f), Low = 0.05f, High = 0.69f, Ink = new Vector2(0.00f, 1.00f), Fill = 0.87f },
            new Sheet { Name = "MuzzleBrake", Cols = 3, Rows = 4, Frames = 9, Snap = true, Fps = 12f, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.11f, High = 0.95f, Bands = new Vector4(0.11f, 0.38f, 0.57f, 0.75f), Fill = 0.80f },
            new Sheet { Name = "MuzzleStream", Cols = 3, Rows = 4, Frames = 9, Snap = true, Fps = 12f, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.11f, High = 0.95f, Bands = new Vector4(0.11f, 0.38f, 0.57f, 0.75f), Fill = 0.80f },
            new Sheet { Name = "MuzzlePop", Cols = 3, Rows = 4, Frames = 9, Snap = true, Fps = 12f, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.11f, High = 0.95f, Bands = new Vector4(0.11f, 0.38f, 0.57f, 0.75f), Fill = 0.80f },
            new Sheet { Name = "MuzzleBurst", Cols = 3, Rows = 4, Frames = 9, Snap = true, Fps = 12f, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.11f, High = 0.95f, Bands = new Vector4(0.11f, 0.38f, 0.57f, 0.75f), Fill = 0.80f },
            new Sheet { Name = "MuzzleCarbine", Cols = 3, Rows = 4, Frames = 9, Snap = true, Fps = 12f, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.11f, High = 0.95f, Bands = new Vector4(0.11f, 0.38f, 0.57f, 0.75f), Fill = 0.80f },
            new Sheet { Name = "MuzzleRifle", Cols = 3, Rows = 4, Frames = 9, Snap = true, Fps = 12f, Fire = true, Tint = new Color(1.35f, 0.62f, 0.17f), Low = 0.11f, High = 0.95f, Bands = new Vector4(0.11f, 0.38f, 0.57f, 0.75f), Fill = 0.80f },
        };

        /// <summary>IN-5 (VFX pass): how much wider a burst's far-reading parts are drawn at a zoom: 1 up to FarGrowFrom,
        /// then in step with the zoom, capped at FarGrowMax. The men grow the same way from zoom 24 (FigureMetrics.Grow).</summary>
        public const float FarGrowFrom = 80f, FarGrowMax = 2.5f;
        public static float FarGrow(float zoom) => Mathf.Clamp(zoom / FarGrowFrom, 1f, FarGrowMax);

        /// <summary>The file a book draws: `Sheets` is indexed by the enum's number, so row and enum must agree.</summary>
        public static string SheetName(Book b) => Sheets[(int)b].Name;
        public static int SheetCount => Sheets.Length;

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
        // Cycle 9: fx.smokeSoft=0.6 with fx.burstGlow=0.5 (runs 9/c99s, b1; c99-critic.md, batch9-critic.md) passed alone,
        // but the cycle 9 candidate set (smokeSoft 0.6, burstGlow 0.5, tracerInSmoke 1 with tracerShape 0) failed
        // together on weight (a0050, runs 9/d9k-critic.md: -1.0, lower in 8 of 8), so the defaults stay as they were.
        public const float DefaultSoft = 0.3f, DefaultAlpha = 0.85f;   // blind critic, cycle 5 (runs 5/w1): the one balance of six where the barrage stays heavy and the trench countable
        public const float OldSoft = 0f, OldAlpha = 1f;

        /// <summary>fx.smokeSoft, never below 0 (0 = the old look).</summary>
        public static float ReadSoft() { float v = Knobs.Get(SoftKnob, DefaultSoft); return v > 0f ? v : 0f; }

        // AOSA C58/C99: fx.burstGlow multiplies the glow of a shell's burst cloud (CombatFx). 1 = the old look and the
        // default. The cycle 9 candidate 0.5 (runs 9/b1, with fx.smokeSoft=0.6) passed alone but failed with the rest of
        // the cycle 9 set on weight (a0050, runs 9/d9k-critic.md); fx.burstGlow=0.5 still draws it.
        public const string BurstGlowKnob = "fx.burstGlow";
        public const float DefaultBurstGlow = 1f, OldBurstGlow = 1f;

        /// <summary>fx.burstGlow, never below 0 (1 = the old look).</summary>
        public static float ReadBurstGlow() => Mathf.Max(0f, Knobs.Get(BurstGlowKnob, DefaultBurstGlow));

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

        /// <summary>The tint that draws a night cloud at this value (luma at the drawings' middle ink), before fog and grade,
        /// in C59's warm grey (fx.smokeNightWarm at its old value 1).</summary>
        public static Color NightTint(float value) => NightTint(value, OldNightWarm);

        /// <summary>The same at a warmth: 1 is C59's warm grey exactly, 0 a neutral grey (AOSA C61, fx.smokeNightWarm).</summary>
        public static Color NightTint(float value, float warm)
        {
            Color hue = NightHueAt(warm);
            float luma = 0.299f * hue.r + 0.587f * hue.g + 0.114f * hue.b;
            float k = value / (luma * Mathf.Lerp(NightMidInk, NightShade, NightLit));
            return new Color(hue.r * k, hue.g * k, hue.b * k, 1f);
        }

        // AOSA C61 (juice J01, the night smoke after C59): the critic (runs 7/d59: smoke 6, readability 7) read the new smoke
        // as tan brown haze, soft-edged and even, not heavy black smoke, and the helmets blurred in the haze band at
        // (150-450, 580-900). Measured on d59-1.f12: the unlit smoke is already about 16% value (#292522), but where a burst
        // is alight it is tan (80, 54, 39)-(91, 66, 47): the burst's orange light (TWBurstLight) is ADDED to a near-black
        // warm cloud, so it dominates it. Three knobs, read once where the smoke is made:
        //   fx.smokeNightWarm  the night smoke's warmth: 1 = C59's warm grey (1, 0.89, 0.78) exactly, 0 = neutral grey; the
        //                      value (fx.smokeNight) is kept. Moonlit field only, with fx.smokeNight above 0.
        //   fx.smokeNightFire  the share of the burst's own light the night smoke takes (the shader's _BurstLit). 1 = the
        //                      old look (the shader skips the line). Moonlit field only, with fx.smokeNight above 0. The
        //                      column keeps all of it: its foot lit orange is the point of C57.
        //   fx.smokeHard       a toon-cut silhouette on the burst's cloud and the smoke (the Deep books, as fx.smokeSoft), at
        //                      the standard view: the drawing's edge stepped at half its alpha instead of the soft falloff,
        //                      so the cloud tears at hard edges. Every field (the soft edge is not a night thing). 0 = the
        //                      old look (the shader skips the line).
        // fx.smokeSoft (C52) already thins a cloud in front of any surface over 0.3 x its width in depth, but that is 0.7-3.3 m
        // above the surface at the standard view's 25 degrees, depending on the card's size, and it goes to 0 at the surface
        // rather than capping at 40%: it is the lever for the ground band, and a sweep of it (0.3, 0.5, 0.7) comes before
        // any new code there. All three at their old values (fx.smokeNightWarm=1,fx.smokeNightFire=1,fx.smokeHard=0) draw
        // the image before C61 bit for bit.
        public const string NightWarmKnob = "fx.smokeNightWarm", NightFireKnob = "fx.smokeNightFire", HardKnob = "fx.smokeHard";
        public const float DefaultNightWarm = 0.35f, DefaultNightFire = 0.35f, DefaultHard = 1f;   // critic: charcoal-umber, not tan; hard-cut toon edges
        public const float OldNightWarm = 1f, OldNightFire = 1f, OldHard = 0f;

        /// <summary>fx.smokeNightWarm, in [0, 1] (1 = C59's warm grey).</summary>
        public static float ReadNightWarm() => Mathf.Clamp01(Knobs.Get(NightWarmKnob, DefaultNightWarm));

        /// <summary>fx.smokeNightFire, in [0, 1] (1 = the old look).</summary>
        public static float ReadNightFire() => Mathf.Clamp01(Knobs.Get(NightFireKnob, DefaultNightFire));

        /// <summary>fx.smokeHard, in [0, 1] (0 = the old look).</summary>
        public static float ReadHard() => Mathf.Clamp01(Knobs.Get(HardKnob, DefaultHard));

        // AOSA C102 (juice J01, split from C53: the smallest change): the earth column reads as a smooth translucent sheet,
        // flame-lit early and curling into lobes late, which veils the men (runs 8/c98-critic.md). C53 asks for toon-stepped
        // edges instead of a soft falloff. One knob, read once where the books are made:
        //   fx.columnHard  fx.smokeHard's toon-cut silhouette (the shader's _Hard, stepped at half the drawing's alpha, torn
        //                  as the book erodes, back to the soft edge as the lens goes in) on the Column book only: not the
        //                  Splash, which draws the same sheet in water, and not the Deep books, which keep fx.smokeHard. Each
        //                  book has its own material, so no draw or material is added. 0 = the old look (the Column book
        //                  got _Hard 0 before C102, and the shader skips the line at 0).
        public const string ColumnHardKnob = "fx.columnHard";
        public const float DefaultColumnHard = 0f, OldColumnHard = 0f;   // off until a blind 2-way against the default passes (rule 6)

        /// <summary>fx.columnHard, in [0, 1] (0 = the old look).</summary>
        public static float ReadColumnHard() => Mathf.Clamp01(Knobs.Get(ColumnHardKnob, DefaultColumnHard));

        /// <summary>The _Hard a book's material is made with: fx.smokeHard on the Deep books (the burst's cloud and the smoke),
        /// fx.columnHard on the Column book, 0 on every other book. With columnHard 0 it is the value before C102 exactly.</summary>
        public static float BookHard(Book book, float smokeHard, float columnHard)
            => Sheets[(int)book].Deep ? smokeHard : book == Book.Column ? columnHard : 0f;

        /// <summary>The night smoke's hue at a warmth: C59's NightHue itself at 1 (the same floats), white at 0.</summary>
        public static Color NightHueAt(float warm) => warm >= 1f ? NightHue : Color.Lerp(Color.white, NightHue, warm);

        /// <summary>The width factor of a shell's cloud and smoke: the knob at the standard view on a moonlit field, 1
        /// among the men and on any other field. With the knob at 1 it is 1 exactly.</summary>
        public static float NightScale(float knob, float closeUp, bool moonLit) => moonLit ? Mathf.Lerp(knob, 1f, closeUp) : 1f;

        /// <summary>AOSA C59: paint the Burst and Smoke books as dark warm grey that the moon does not light (see NightKnob).
        /// Called after the biome's tints (CombatFx.ApplyTints), and only on a moonlit field; value 0 sets nothing.</summary>
        public void NightSmoke(float value) => NightSmoke(value, OldNightWarm, OldNightFire);

        /// <summary>The same at a warmth and a share of the burst's light (AOSA C61: fx.smokeNightWarm, fx.smokeNightFire).</summary>
        public void NightSmoke(float value, float warm, float fire)
        {
            if (value <= 0f) return;
            var tint = NightTint(value, warm);
            PaintNight(mats[(int)Book.Burst], tint);
            PaintNight(mats[(int)Book.Smoke], tint);
            if (mats[(int)Book.Burst] != null) mats[(int)Book.Burst].SetFloat("_BurstLit", fire);
            if (mats[(int)Book.Smoke] != null) mats[(int)Book.Smoke].SetFloat("_BurstLit", fire);
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

        // AOSA C103 (juice J01, split from C53: the column's size and dark core): two blind critics (runs 9/c102-critic.md,
        // 8/c98-critic.md) read the column as thin translucent orange-brown arcs, "water spray, sparkler, flame", and the
        // clods as uniform small dark pebbles. Three causes, from the code and the drawing:
        //   the timing  the Column book plays its 16 frames evenly over 1.8 s, so the dense column (frames 1-5) is up for
        //               0.1-0.6 s and the split arcs (frames 6-15, 10-25% cover) fill the other 1.2 s the critic sees.
        //   the light   _BurstLit is 1 on the Column (C61 left it whole), so the shell's orange light is ADDED to a dark
        //               brown and the column reads as flame (measured on c102h f50: (190, 110, 50) where lit).
        //   the clods   two DebrisRenderer bursts of 0.19-0.48 m and 0.05-0.14 m lumps, one tint, lying 30 s.
        // One knob, fx.columnSoil in [0, 1], on a moonlit field only (as C57: the day column is the size-1 book, unjudged):
        //   the column  a soil heave (SoilShape): it rises in SoilRise s (frames 0.5 -> 3.2, the dense ones with two lumps
        //               at the top, so wider at the top and ragged), holds the dense frames to SoilHold s, then collapses
        //               back into the ground over the rest of its SoilLife (height x 1.06 -> 0.4, falling faster as it
        //               goes, the frames running on to 7.6 while fx.columnEarth's erode tears it). Drawn SoilWidth as wide
        //               and SoilHeight as tall as the book's own card: an opaque column no bigger than today's, dense for
        //               1 s instead of sparse arcs for 1.8 s (the drawn cover over time is the same, 0.23 against 0.23).
        //   over men    rule 6 (C98: a bigger column lowered the men 6.0 -> 5.3; C102: an opaque one over men, 3 -> 2): the
        //               card is drawn behind a man in front of it (depth), but it covers the ground behind it out to its
        //               height over the eye's slope. CombatFx finds the nearest man behind it inside its width and caps
        //               its height so its top stops at his feet (SoilCap), down to SoilLow: a low heave, never a veil.
        //   its paint   dark umber at value SoilValue (EarthTint, the C57 paint), taking SoilFire of the burst's light.
        //   the clods   DebrisRenderer.Heave: fewer, bigger, varied (a 4x size span, most small; a third thrown as clumps
        //               of three), a darker-to-lighter spread of the mud, on real arcs that go up with the column and come
        //               down round it, lying SoilClodLife s instead of 30.
        // Between 0 and 1 the paint and the timing blend (the clods are the heave at any value above 0). No book, material,
        // mesh or pool is added: the Column book and the Clod pool already draw. 0 = the old look: nothing is painted,
        // no card has Soil, and CombatFx throws the old clods (the code before C103, bit for bit).
        public const string ColumnSoilKnob = "fx.columnSoil";
        public const float DefaultColumnSoil = 0f, OldColumnSoil = 0f;   // off until a blind 2-way against the default passes (rule 6)
        public const float SoilValue = 0.08f, SoilFire = 0.3f;           // critic: core ~0.08, dark umber #2A1E14-#4A3526, not flame-lit
        public const float SoilLife = 1.4f, SoilRise = 0.25f, SoilHold = 0.65f;   // critic: rises in 0.2-0.3 s, then falls back
        public const float SoilWidth = 0.9f, SoilHeight = 1.0f;          // rule 6: the footprint no wider or taller than today's column
        public const float SoilClodLife = 1.5f;                          // short-lived on the ground (was 30 s)
        public const float SoilLow = 0.3f;                               // the lowest the column is capped to over men (SoilCap)
        public const float SoilPeak = 1.06f * SoilHeight;                // the heave's tallest, of the card's own height (SoilShape at SoilHold)

        /// <summary>fx.columnSoil, in [0, 1] (0 = the old look).</summary>
        public static float ReadColumnSoil() => Mathf.Clamp01(Knobs.Get(ColumnSoilKnob, DefaultColumnSoil));

        /// <summary>The soil heave's frame, width and height factors (of the card's own) t seconds into a column of this
        /// life: rise in SoilRise, hold to SoilHold, collapse over the rest.</summary>
        public static void SoilShape(float t, float life, out float frame, out float wide, out float tall)
        {
            if (t < SoilRise)
            {
                float u = Mathf.Clamp01(t / SoilRise), e = 1f - (1f - u) * (1f - u) * (1f - u);   // out of the ground fast, easing at the top
                frame = Mathf.Lerp(0.5f, 3.2f, e); wide = Mathf.Lerp(0.55f, 1f, e); tall = Mathf.Lerp(0.15f, 1f, e);
            }
            else if (t < SoilHold)
            {
                float u = (t - SoilRise) / (SoilHold - SoilRise);
                frame = Mathf.Lerp(3.2f, 4.4f, u); wide = Mathf.Lerp(1f, 1.08f, u); tall = Mathf.Lerp(1f, 1.06f, u);
            }
            else
            {
                float u = Mathf.Clamp01((t - SoilHold) / Mathf.Max(0.05f, life - SoilHold));
                frame = Mathf.Lerp(4.4f, 7.6f, u); wide = Mathf.Lerp(1.08f, 1.25f, u); tall = Mathf.Lerp(1.06f, 0.4f, u * u);   // falls back, faster as it goes
            }
            wide *= SoilWidth; tall *= SoilHeight;
        }

        /// <summary>The height cap of a soil column over men (rule 6: C98's and C102's columns hid the men they stood over).
        /// The column stands on the impact and faces the eye, so it covers the ground behind it (away from the eye) out to
        /// its height over the tangent of the eye's pitch. behind is the ground distance to the nearest man behind it inside
        /// its width, tanPitch the eye's slope down to the impact, peak the column's full drawn height (m). The cap keeps the
        /// column's top at that man's feet, never below SoilLow of its height (a low heave still reads as earth); 1 with no
        /// man behind it.</summary>
        public static float SoilCap(float behind, float tanPitch, float peak)
            => peak <= 0f ? 1f : Mathf.Clamp(behind * Mathf.Max(0f, tanPitch) / peak, SoilLow, 1f);

        /// <summary>The height a card of this book is drawn at for a width, when no height is given (the drawing's aspect).</summary>
        public float CardHeight(Book book, float width) => width / Mathf.Max(0.05f, aspect[(int)book]);

        /// <summary>The value the soil column is painted at: SoilValue at 1, blended from fx.columnEarth's below it.</summary>
        public static float SoilPaintValue(float soil, float earth) => earth > 0f ? Mathf.Lerp(earth, SoilValue, soil) : SoilValue;

        /// <summary>AOSA C103: paint the Column book as the soil heave's dark umber (see ColumnSoilKnob). Called after
        /// NightEarth, on a moonlit field only; soil 0 sets nothing.</summary>
        public void SoilEarth(float soil, float earth)
        {
            if (soil <= 0f) return;
            var m = mats[(int)Book.Column];
            if (m == null) return;
            PaintNight(m, EarthTint(SoilPaintValue(soil, earth)));
            m.SetFloat("_Erode", 1f);
            m.SetFloat("_BurstLit", Mathf.Lerp(1f, SoilFire, soil));
        }

        // AOSA C108 (juice J01, split from C103: the smallest change): at night the column still reads as see-through orange
        // arcs, "flame or spray, never soil", partly over men (runs 9/batch9-critic.md). From the code: NightEarth paints it
        // #3B2A1E at value 0.22 but leaves _BurstLit 1, so the shader ADDS lerp(ink, 1, 0.3) x TWBurstLight x _Lit (0.6) to
        // that dark brown: the burst's orange outweighs the earth wherever it reaches, and on the thin late arcs (low
        // alpha, light ink) orange is all there is. C103 fixed it (columns over men 3 up / 7 equal / 0 down) but only inside
        // its whole redesign, which left the earth score flat. Its two parts as knobs of their own, on the old column,
        // on a moonlit field only (as C57; the day column and the Splash are untouched), read once in CombatFx.Awake:
        //   fx.columnBurstLit  the Column book's share of the burst's light (its _BurstLit). 1 = today (nothing is set, the
        //                      shader skips the line); C103 used SoilFire 0.3. fx.columnSoil above 0 sets its own share.
        //   fx.columnCap       how far the old column is capped over men behind it (C103's SoilCap, its top at the nearest
        //                      man's feet, never below SoilLow): the card's height x lerp(1, cap, knob). 0 = the old height (the old
        //                      Add, no search for men). With fx.columnSoil above 0 the heave's own cap applies instead.
        // No book, material, mesh or draw is added: the Column book's own material and card.
        public const string ColumnBurstLitKnob = "fx.columnBurstLit", ColumnCapKnob = "fx.columnCap";
        public const float DefaultColumnBurstLit = 1f, OldColumnBurstLit = 1f;   // off until a blind 2-way against the default passes (rule 6)
        public const float DefaultColumnCap = 1f;   // blind critic, cycle 10 (runs 10/p4c, c108.md, a0054): arc px -70%, earth +1.67 (6 of 6), men +0.17 never lower, weight -0.17
        public const float OldColumnCap = 0f;   // fx.columnPlay=1,fx.columnCap=0 is the old look
        public const float ColumnGrow = 0.35f;   // the old column's grow (CombatFx): its card ends 1.35x as tall as it is born

        /// <summary>fx.columnBurstLit, in [0, 1] (1 = today).</summary>
        public static float ReadColumnBurstLit() => Mathf.Clamp01(Knobs.Get(ColumnBurstLitKnob, DefaultColumnBurstLit));

        /// <summary>fx.columnCap, in [0, 1] (0 = the old height).</summary>
        public static float ReadColumnCap() => Mathf.Clamp01(Knobs.Get(ColumnCapKnob, DefaultColumnCap));

        /// <summary>The height factor of the old column's card: 1 at knob 0 (exactly), C103's cap at knob 1.</summary>
        public static float ColumnCapScale(float knob, float cap) => knob <= 0f ? 1f : Mathf.Lerp(1f, cap, knob);

        /// <summary>AOSA C108: the Column book's share of the burst's light (see ColumnBurstLitKnob). Called after
        /// NightEarth and before SoilEarth, on a moonlit field only; 1 sets nothing.</summary>
        public void ColumnBurstLit(float share)
        {
            if (share >= 1f) return;
            var m = mats[(int)Book.Column];
            if (m == null) return;
            m.SetFloat("_BurstLit", Mathf.Clamp01(share));
        }

        // AOSA C109 (juice J01, from C108's finding, runs 10/c108.md): the orange arcs a blind critic reads as flame or spray
        // at night, partly over men, are the Column book's own late drawing (frames 6-15, the column split into thin arcs),
        // in C57's night-earth paint, drawn 0.7-1.8 s into the old column's 1.8 s life. How the book plays (Draw): a card's
        // frame is k x Frames x Sheet.Play (k = its age over its life), so a Sheet's Play slows the whole book to end at that
        // frame; it neither stops early nor fades sooner. Every card fades over its last 35% of life and is gone at its life.
        // So a Sheet Play would stretch the heave (frames 0-5) over the whole 1.8 s. Instead this cuts the card: its frames,
        // swell, pop and glow run on the old 1.8 s clock exactly (the heave as today), but it is gone at Cut x its life, and
        // it fades (and, eroding, tears) over the last PlayFade of that shortened life, to 0 at the cut: no pop.
        //   fx.columnPlay   the part of the Column book the old dry column plays on a moonlit field (as C57), in
        //                   [MinColumnPlay, 1]. 1 = the old card (no card is cut, the old fade line runs). 0.4: gone at 0.72 s at
        //                   frame 6.4, the arcs' first frame (6) at fade 0.085. With fx.columnSoil above 0 the heave's own
        //                   timing applies (no cut); with fx.columnCap the capped column is cut the same. Splash is never cut.
        // No book, material, mesh or draw is added: the same card, gone sooner.
        public const string ColumnPlayKnob = "fx.columnPlay";
        public const float DefaultColumnPlay = 0.4f;   // blind critic, cycle 10 (runs 10/p4c, c108.md, a0054): arc px -70%, earth +1.67 (6 of 6), men +0.17 never lower, weight -0.17
        public const float OldColumnPlay = 1f;   // fx.columnPlay=1,fx.columnCap=0 is the old look
        public const float MinColumnPlay = 0.1f;
        public const float PlayFade = 0.35f;   // the cut card fades over this part of its shortened life (every card's own last 35%)

        /// <summary>fx.columnPlay, in [MinColumnPlay, 1] (1 = the old card).</summary>
        public static float ReadColumnPlay() => Mathf.Clamp(Knobs.Get(ColumnPlayKnob, DefaultColumnPlay), MinColumnPlay, 1f);

        /// <summary>The Cut a column card is added with at this knob: 0 (no cut, today exactly) at 1 or above.</summary>
        public static float ColumnPlayCut(float play) => play >= 1f ? 0f : Mathf.Clamp(play, MinColumnPlay, 1f);

        /// <summary>A card's age (seconds) at which it is gone: its life, or Cut x its life when it is cut.</summary>
        public static float CardEnd(float life, float cut) => cut > 0f ? life * cut : life;

        /// <summary>The fade of a cut card at k (its age over its FULL life): 1 until the last PlayFade of its shortened
        /// life (cut x life), then smoothly to 0 at the cut.</summary>
        public static float CutFade(float k, float cut)
        {
            float u = Mathf.Clamp01(k / Mathf.Max(0.0001f, cut));
            return 1f - Mathf.SmoothStep(0f, 1f, (u - (1f - PlayFade)) / PlayFade);
        }

        /// <summary>The Cut the card at this index holds, for the tests.</summary>
        public float CardCut(int index) => cards[index].Cut;

        /// <summary>The _BurstLit the Column book's material holds (1 when the book did not load), for the tests.</summary>
        public float ColumnBurstLitNow => mats[(int)Book.Column] != null ? mats[(int)Book.Column].GetFloat("_BurstLit") : 1f;
        readonly int maxCards;   // MaxCards, or the knob flipbook.maxCards (read in the constructor)
        readonly List<Card> cards = new List<Card>(512);
        readonly Material[] mats = new Material[(int)Book.Count];
        readonly float[] aspect = new float[(int)Book.Count];
        readonly Matrix4x4[] batch = new Matrix4x4[1023];
        // IN-4 (VFX pass): the live cards counted into their books once a frame (a stable counting sort), so Draw walks each
        // book's own cards instead of every card once per book (39 books x up to maxCards). Order within a book is kept,
        // so every batch is the same as before.
        readonly int[] bookStart = new int[(int)Book.Count + 1], bookCursor = new int[(int)Book.Count];
        int[] byBook = new int[2048];   // over MaxCards: no allocation in play (it grows only past a raised flipbook.maxCards)
        Mesh quad;
        public bool Ready { get; private set; }
        public int Alive => cards.Count;

        public FlipbookFx()
        {
            maxCards = Mathf.Max(1, Knobs.Get("flipbook.maxCards", MaxCards));
            float soft = ReadSoft();   // C52: the deep clouds' softness (the shader takes it back to 0 as the lens goes in)
            float hard = ReadHard();   // C61: the deep clouds' toon-cut edge (the same)
            float columnHard = ReadColumnHard();   // C102: the same cut on the earth column
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
                m.SetColor("_Shade", (Book)k == Book.Burst ? BurstShade : new Color(0.70f, 0.71f, 0.74f));   // a cloud is lit through: its shade is paler than the ground's; the burst's boiling cloud has a sooty underside (critique s3)
                m.SetFloat("_Lit", s.Additive || s.Fire ? 0f : s.Lit > 0f ? s.Lit : 1f);   // fire is its own light, like the additive books
                m.SetFloat("_MaskOnly", s.MaskOnly ? 1f : 0f);
                m.SetFloat("_Fire", s.Fire ? 1f : 0f);
                if ((Book)k == Book.Fireball) { m.SetColor("_Smoke", new Color(0.48f, 0.40f, 0.33f, 1f)); m.SetColor("_Core", new Color(1.6f, 1.3f, 0.8f, 1f)); }   // the shell's fireball: its soot the burst cloud's lit brown (at 0.36 it ringed each fire in dark: critique c1b), its heart yellow, not the flamethrower's white (critique c2)
                if (s.Bands.sqrMagnitude > 0f) m.SetVector("_Bands", s.Bands);
                m.SetFloat("_Rise", s.Rise);   // how far the top of the card is rotated toward umber; standing flames only   // this book's own cel cuts, else the shader's (FireBall's)
                m.SetFloat("_Erode", s.Erode ? 1f : 0f);
                m.SetFloat("_ShadeMood", s.Mood > 0f ? s.Mood : 1f);
                m.SetFloat("_Soft", s.Deep ? soft : 0f);
                m.SetFloat("_Hard", BookHard((Book)k, hard, columnHard));
                // fire is premultiplied over (One, OneMinusSrcAlpha), additive books add, everything else is straight alpha
                m.SetFloat("_SrcBlend", (float)(s.Additive || s.Fire ? UnityEngine.Rendering.BlendMode.One : UnityEngine.Rendering.BlendMode.SrcAlpha));
                m.SetFloat("_DstBlend", (float)(s.Additive ? UnityEngine.Rendering.BlendMode.One : UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha));
                m.SetFloat("_Ground", s.Ground ? 1f : 0f);
                // a ground book goes first: the columns, bursts and fire stand on it rather than under it
                m.renderQueue = s.Ground ? 3005 : s.Additive ? 3020 : s.Fire ? 3015 : 3010;
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
        public void Add(Book book, Vector3 at, float width, float life, Kind kind = Kind.None, Vector3 velocity = default, float grow = 0f, float roll = 0f, float alpha = 1f, float glow = 1f, float height = 0f, float pop = 0f, float delay = 0f, float startFrame = 0f, float soil = 0f, float soilCap = 1f, float cut = 0f)
        {
            if (!Ready) return;
            if (cards.Count >= maxCards) cards.RemoveAt(0);
            if (IsSmoke(book)) ShapeSmoke(book == Book.Burst ? BurstWeight(smokeWeight) : smokeWeight, smokeLean, aspect[(int)book], ref width, ref height, ref life, ref alpha);   // FlipbookFx.Smoke.cs
            float h = height > 0f ? height : width / Mathf.Max(0.05f, aspect[(int)book]);
            cards.Add(new Card { Pos = at, Vel = velocity, Born = Time.time + delay, Life = Mathf.Max(0.02f, life), Width = width, Height = h, Grow = grow, Roll = roll, Alpha = alpha, Glow = glow, Pop = pop, Start = startFrame, Soil = soil, Cap = soilCap, Cut = cut > 0f && cut < 1f ? cut : 0f, Book = book, Kind = kind });
        }

        /// <summary>Move, age and draw every card. Clouds before the lights, so the flash is not hidden by its own smoke.</summary>
        public void Draw(float now, Bounds bounds)
        {
            if (!Ready) return;
            float dt = Time.deltaTime;
            for (int i = cards.Count - 1; i >= 0; i--)
            {
                var c = cards[i];
                if (now - c.Born > (c.Cut > 0f ? CardEnd(c.Life, c.Cut) : c.Life)) { cards.RemoveAt(i); continue; }   // AOSA C109: a cut card goes at its cut
                if (now < c.Born) continue;   // not born yet
                if (c.Vel.sqrMagnitude > 0f) { c.Pos += c.Vel * dt; c.Vel = Vector3.Lerp(c.Vel, Vector3.zero, dt * 0.6f); cards[i] = c; }   // the throw slows; the drift on a long card stays
            }
            float far = FarBlendNow(), keep = FarKeepShare(far), keepGrow = 1f / Mathf.Sqrt(keep);   // the far fire (FlipbookFx.FarFire.cs)
            int bookCount = (int)Book.Count;
            System.Array.Clear(bookStart, 0, bookCount + 1);
            for (int i = 0; i < cards.Count; i++) bookStart[(int)cards[i].Book + 1]++;
            for (int b = 0; b < bookCount; b++) { bookStart[b + 1] += bookStart[b]; bookCursor[b] = bookStart[b]; }
            if (byBook.Length < cards.Count) byBook = new int[Mathf.NextPowerOfTwo(cards.Count)];
            for (int i = 0; i < cards.Count; i++) byBook[bookCursor[(int)cards[i].Book]++] = i;
            for (int b = 0; b < bookCount; b++)
            {
                var mat = mats[b]; if (mat == null || bookStart[b] == bookStart[b + 1]) continue;
                int frames = Sheets[b].Frames, n = 0;
                var rp = new RenderParams(mat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = false };
                for (int j = bookStart[b]; j < bookStart[b + 1]; j++)
                {
                    var c = cards[byBook[j]]; if (now < c.Born) continue;
                    if (far > 0f && Sheets[b].Fire)
                    {
                        Glow(c.Pos, c.Width, c.Height, (c.Kind & Kind.Anchored) != 0, c.Alpha);   // the halo counts all of the fire, kept or not
                        if (c.Width < FarThinWidth)
                        {
                            if (!FarKeep(c.Born, c.Life, keep)) continue;
                            c.Width *= keepGrow; c.Height *= keepGrow;   // a local copy: the card itself is not changed
                        }
                    }
                    float k = Mathf.Clamp01((now - c.Born) / c.Life);
                    float swell = 1f + c.Grow * k;
                    if (c.Pop > 0f) { float u = 1f - Mathf.Clamp01(k / 0.2f); swell *= Mathf.Lerp(1f, c.Pop, u * u * u); }   // bursts out of a point, eased
                    float fade = c.Cut > 0f ? CutFade(k, c.Cut) : 1f - Mathf.SmoothStep(0f, 1f, (k - 0.65f) / 0.35f);   // AOSA C109: a cut card fades to 0 at its cut
                    if (Sheets[b].RampIn > 0f) fade *= Mathf.Clamp01((now - c.Born) / Sheets[b].RampIn);
                    float play = Sheets[b].Play > 0f ? Sheets[b].Play : 1f;
                    float fps = Sheets[b].Fps;
                    float run = (fps > 0f ? (now - c.Born) * fps : k * frames * play) + c.Start;   // at the rate it was drawn, or spread over the life
                    float span = frames * play - 1.001f;
                    float frame = (c.Kind & Kind.HoldLast) != 0 ? Mathf.Min(k * frames, frames - 1f)
                                : Sheets[b].Cycle ? Mathf.Repeat(run, span) : Mathf.Min(run, span);
                    float bright = 1f + (c.Glow - 1f) * (1f - Mathf.Clamp01(k / 0.15f));   // the fire is out in the first sixth
                    if (c.Soil > 0f)
                    {
                        // AOSA C103: the soil heave's own timing and shape, blended by the knob (see ColumnSoilKnob)
                        SoilShape(now - c.Born, c.Life, out float soilFrame, out float wide, out float tall);
                        batch[n++] = Pack(c.Pos, c.Width * Mathf.Lerp(swell, wide, c.Soil), c.Height * Mathf.Lerp(swell, tall * c.Cap, c.Soil), Mathf.Lerp(frame, soilFrame, c.Soil), fade, bright, c.Roll, c.Kind, c.Alpha);
                    }
                    else batch[n++] = Pack(c.Pos, c.Width * swell, c.Height * swell, frame, fade, bright, c.Roll, c.Kind, c.Alpha);
                    if (n == batch.Length) { FrameBudget.Draw(rp, quad, 0, batch, n); n = 0; }
                }
                if (n > 0) FrameBudget.Draw(rp, quad, 0, batch, n);
            }
            if (far > 0f) FlushGlow(bounds);
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
            // the far fire's halo over a caller's own fire cards (a hull's tongues, the beam's pillar); the caller keeps its
            // count, so nothing is thinned here
            if (Sheets[(int)book].Fire && FarBlendNow() > 0f)
            {
                for (int i = 0; i < packed.Count; i++) { var m = packed[i]; Glow(new Vector3(m.m03, m.m13, m.m23), m.m00, m.m11, m.m20 > 0.5f, m.m21); }
                FlushGlow(bounds);
            }
        }

        /// <summary>The record TW/Flipbook reads out of the instance matrix (see the shader's header).</summary>
        public static Matrix4x4 Pack(Vector3 at, float width, float height, float frame, float fade, float bright, float roll, Kind kind, float opacity = 1f)
        {
            var m = Matrix4x4.identity;
            m.m03 = at.x; m.m13 = at.y; m.m23 = at.z;
            m.m00 = width; m.m11 = height; m.m22 = frame;
            m.m01 = fade; m.m10 = bright; m.m02 = roll;
            m.m12 = (kind & Kind.Flat) != 0 ? 2f : (kind & Kind.Upright) != 0 ? 1f : 0f;
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
