// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts; b04: 36 bolts, 8 already in flight at the fire, lanes spread in height; small round heads with long thin flame streaks; orange spark starbursts on the dummy; torso-tall ground fires; tall leaning smoke columns on ground hits; b03 beach round: 28 bolts in four low lanes from the left edge,
// fatter flame bodies, no lit disc on the dummy, wide ground bursts that flicker out, smoke as inked Lobe columns; b02: round heads with flaring tails, opening
// bolts already in flight at the fire, spark starbursts instead of a disc on the dummy, tall pointed ground flames,
// smoke as chains of round lobes) - fire bolts (lab only): a volley of comet fire bolts from
// the upper left on slightly arcing paths, staggered in height, depth and flight time, onto the dummy and the ground
// beside it. Each is a small white-hot head with a soft yellow-orange bloom, a flame tail that breaks into thin red
// streaks, and a long wavy white-grey smoke ribbon, all one Comet record (CometKind). Each hit: a small star flash, a
// starburst of thin yellow spark streaks and orange-yellow flame tongues; ground hits burst into flames about 0.9 m
// tall that flicker out, then thin curling grey smoke strands (Comet strand mode) rise, drift right along the shot,
// grow and break up. A dim brown-warm pool of light (Comet pool mode) lies on the ground just left of and in front
// of the dummy, ramping up with the volley and fading smoothly. Reference: fire_hitting_cel (spec/fire_bolts.json).
//
// Every bolt is two CelFx.Play calls: its comet (aimed a little above the hit so its gravity brings it down onto the
// hit: an arc) and its hit recipe, both shifted by the bolt's launch time, so the whole volley is spawned at the
// fire and nothing runs per frame. Kinds: Comet, Streak, Glow (three draws, no new kind).
// LOD: Close = everything; Mid = one strand, fewer sparks, tongues and flames; Far = the comet, a flash and the pool
// (and every other bolt, see FireBoltsRange).
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class FireBolts
    {
        /// <summary>Mean seconds from launch to hit.</summary>
        public const float Flight = 1.15f;   // p26: slower (REF 40-45 px/frame, p25 was 45-52)   // p21: slower (REF ~45 px/frame, 15-17 frames across)
        /// <summary>The comet's gravity (m/s2): the Range aims it ArcLift(flight) above its hit so it lands on it.</summary>
        public const float ArcGravity = 3.4f;   // p26: 5.0 x (0.95/1.15)^2: the same arc at the slower flight   // p21: REF's paths bend down harder near the hit   // b04: 2.5 -> 3.2 (the high lanes arc down)   // b03: 4 -> 2.5 (lower lanes: the reference's paths are nearly level, bending slightly down)
        public static float ArcLift(float flight) { return 0.5f * ArcGravity * flight * flight; }

        /// <summary>e01 (exact_20261117): the volley measured from the reference (fit2.py: 2D head tracks fitted with
        /// the Range's own ballistic aiming). Per bolt: launch (s after the fire; negative = already in flight at the
        /// fire), flight (s), target (0 dummy, 1 far ground, 2 near ground), launch spot relative to the hit
        /// (screen right m, up m, away m) and gravity (m/s2).</summary>
        public static readonly float[,] Volley =
        {
            { -0.421f, 1.137f, 0, -14.50f, 3.74f, -7.60f, 1.50f },   // f15-29 D hit f28   e26c: vopt2
            { -0.278f, 1.273f, 1, -15.00f, 2.97f, -10.00f, 4.57f },   // f14-34 FG hit f34   e26c: vopt2
            { 0.247f, 1.162f, 1, -15.00f, 3.08f, -5.77f, 7.00f },   // f21-45 FG hit f44   e26c: vopt2
            { 0.305f, 1.063f, 0, -14.50f, 2.56f, -3.94f, 1.96f },   // f27-43 D hit f44   e26c: vopt2
            { 0.370f, 1.630f, 0, -16.50f, 4.39f, -10.00f, 1.50f },   // f39-55 D hit f60   e26c: vopt2
            { 0.380f, 1.193f, 1, -15.00f, 3.21f, -5.33f, 1.92f },   // f25-49 FG hit f50   e26c: vopt2
            { 0.516f, 1.471f, 0, -18.50f, 2.72f, -2.54f, 1.50f },   // f33-46 D hit f57   e09: up +0.30 (register.py)   e26c: vopt2
            { 0.584f, 1.337f, 1, -15.50f, 1.82f, 5.51f, 3.70f },   // f30-57 FG hit f57   e26c: vopt2
            { 0.590f, 1.777f, 2, -17.50f, 4.72f, -11.00f, 2.09f },   // f54-81* NG hit f71   e24: launch -0.1 (REF fire px from f68)   e26c: vopt2
            { 1.127f, 1.047f, 1, -16.50f, 2.16f, -1.06f, 2.98f },   // f38-63 FG hit f64   e26c: vopt2
            { 0.926f, 1.777f, 2, -15.00f, 4.27f, -10.00f, 2.09f },   // f54-81 NG hit f85   e09: launch -0.25 (register.py)   e26c: vopt2
            { 1.354f, 1.672f, 1, -14.50f, 2.71f, -10.00f, 5.88f },   // f54-82 FG hit f83   e26c: vopt2
            { 1.410f, 1.777f, 2, -15.30f, 3.97f, -8.50f, 2.09f },   // f54-81* NG hit f90   e26c: vopt2
            { 1.654f, 1.432f, 1, -14.40f, 2.61f, -9.00f, 5.88f },   // f54-82* FG hit f88   e35: launch +0.24, flight -0.24 (rode 30 px behind row 11)   e26c: vopt2
            { 1.049f, 1.551f, 0, -13.73f, -0.46f, 21.88f, 4.10f },   // f37-71 D hit f72   e26: REF's high slow bolt coming toward the camera (fitfree.py, 9 px; was a copy of f27-43)   e26c: vopt2
            { 1.738f, 1.830f, 0, -15.50f, 2.57f, -6.80f, 1.50f },   // f74-93 D hit f99   e09: launch -0.25 (register.py)   e26c: vopt2
            { 2.070f, 1.376f, 0, -15.00f, 1.93f, 9.21f, 4.05f },   // f58-93 D hit f93   e26c: vopt2
            { 2.072f, 1.428f, 0, -16.50f, 1.09f, 1.63f, 1.50f },   // f67-90 D hit f96   e26c: vopt2
            { 2.556f, 1.171f, 0, -13.50f, -1.40f, 10.00f, 1.50f },   // f76-92 D hit f105   e26c: vopt2
            { 2.743f, 1.024f, 0, -16.00f, 1.50f, 8.38f, 1.50f },   // f76-93 D hit f102   e09: up +0.55 (register.py)   e26c: vopt2
        };
        public static readonly int Count = Volley.GetLength(0);
        /// <summary>Full flight of bolt i (s).</summary>
        public static readonly float[] FlightOf = Col(1);
        /// <summary>Fraction of its path bolt i has flown at the fire (0 = launched after the fire).</summary>
        public static readonly float[] AlongAt = MakeAlong();
        /// <summary>Birth time (s after the fire) and remaining flight (s) of bolt i.</summary>
        public static readonly float[] Launch = MakeLaunch();
        public static readonly float[] Flights = MakeFlights();
        public static readonly int[] TargetOf = MakeTarget();
        public static readonly bool[] OnGround = MakeGround();
        public static readonly float[] GravityOf = Col(6);
        static float[] Col(int c) { var a = new float[Volley.GetLength(0)]; for (int i = 0; i < a.Length; i++) a[i] = Volley[i, c]; return a; }
        static float[] MakeAlong() { var a = new float[Count]; for (int i = 0; i < Count; i++) a[i] = Mathf.Clamp01(-Volley[i, 0] / Volley[i, 1]); return a; }
        static float[] MakeLaunch() { var a = new float[Count]; for (int i = 0; i < Count; i++) a[i] = Mathf.Max(0f, Volley[i, 0]); return a; }
        static float[] MakeFlights() { var a = new float[Count]; for (int i = 0; i < Count; i++) a[i] = Volley[i, 1] - Mathf.Max(0f, -Volley[i, 0]); return a; }
        static int[] MakeTarget() { var a = new int[Count]; for (int i = 0; i < Count; i++) a[i] = Mathf.RoundToInt(Volley[i, 2]); return a; }
        static bool[] MakeGround() { var a = new bool[Count]; for (int i = 0; i < Count; i++) a[i] = Volley[i, 2] > 0.5f; return a; }
        /// <summary>Bolt i's launch spot relative to its hit: (screen right m, up m, away m).</summary>
        public static Vector3 FromOffset(int i) { return new Vector3(Volley[i, 3], Volley[i, 4], Volley[i, 5]); }
        /// <summary>How far above its hit bolt i is aimed so its gravity drops it onto the hit over its remaining flight.</summary>
        /// <summary>e04: bolt i's flame length (m) = 0.22 s of its speed / 0.92 (the shader draws 0.92 of extra.y).</summary>
        public static float BodyOf(int i) { return 0.22f / 0.92f * FromOffset(i).magnitude / FlightOf[i]; }
        /// <summary>e05: one smoke wisp per fire spot, on its first hit (REF: one thin ribbon per fire).</summary>
        public static readonly bool[] SmokeOn = MakeSmokeOn();
        static bool[] MakeSmokeOn()
        {
            // e10: one wisp per fire spot, on its first hit (REF: one strand per fire that turns into puffs)
            var a = new bool[Count]; float[] first = { 1e9f, 1e9f, 1e9f }; int[] who = { -1, -1, -1 };
            for (int i = 0; i < Count; i++) { int c = Mathf.RoundToInt(Volley[i, 2]); float h = Volley[i, 0] + Volley[i, 1]; if (c > 0 && h < first[c]) { first[c] = h; who[c] = i; } }
            for (int c = 1; c < 3; c++) if (who[c] >= 0) a[who[c]] = true;
            return a;
        }
        public static float ArcLiftOf(int i) { return 0.5f * GravityOf[i] * Flights[i] * Flights[i]; }

        static FireBolts()
        {
            // additive light on the dark field: c3 is what the pool adds at its middle (dim brown-warm, #c16a42-ish on the ground)
            CelPalette.Add("fb_pool", CelPalette.Hex(0x2A1C14), CelPalette.Hex(0x4A3424), CelPalette.Hex(0x5A4232), CelPalette.Hex(0x86664C),
                CelPalette.Hex(0x86664C), 1.1f, 4, CelPalette.Hex(0x000000));
            // the comet: deep red at the streaks' tips, red-orange, orange, yellow behind the head, a white-hot core
            CelPalette.Add("fb_comet", CelPalette.Hex(0xC58C78), CelPalette.Hex(0xFA8E7C), CelPalette.Hex(0xFBC69A), CelPalette.Hex(0xFFF1C2),   // p21b: pre-compensated for the kit's .linear store on a gamma project (wanted 853F2D / E14130 / E2874E / F9CF81)   // p21: the REF's own k-means colours (red, salmon, pale orange)   // p13: the saturated red-orange of the REF 4x crops   // d09: toward the ref #c16a42 / #f3cf86
                CelPalette.Hex(0xFFFFD2), 0.84f, 4, CelPalette.Hex(0x1E0806));   // p21b: (wanted FDEA99)   // p21: a pale yellow head, not white
            CelPalette.Add("fb_ribbon", CelPalette.Hex(0xC8B8B4), CelPalette.Hex(0xD8CCCA), CelPalette.Hex(0xE4DAD8), CelPalette.Hex(0xF0E8E4),   // p31c: pale grey smoke with a warm hint (REF), pre-compensated   // p23b: pale pink-cream (wanted A88880 / C8A8A0)   // p21b: pre-compensated (wanted 6A4E4C / 8E6A66 / B59590 / DCC8C0)   // p21: the REF's smear is a dusty pink-white line   // c05: soft grey (was near-white)
               
                CelPalette.Hex(0xFCF8F4), 1.1f, 3, CelPalette.Hex(0x3A3438));
            // hit sparks: yellow-orange, a pale yellow core (never white)
            CelPalette.Add("fb_spark", CelPalette.Hex(0x9A3A10), CelPalette.Hex(0xE0681C), CelPalette.Hex(0xFFA02E), CelPalette.Hex(0xFFD04A),
                CelPalette.Hex(0xFFE27A), 0.9f, 3, CelPalette.Hex(0x2A0C06));
            // p27h: hit dashes: bright orange to white, pre-compensated for the kit's .linear store (fb_spark's E0681C rendered B02102: a dim brown-red dash)
            CelPalette.Add("fb_dash", CelPalette.Hex(0xFFA066), CelPalette.Hex(0xFFB874), CelPalette.Hex(0xFFD48C), CelPalette.Hex(0xFFE8A8),   // e19: orange streaks (REF)
                CelPalette.Hex(0xFFF4C8), 1.0f, 3, CelPalette.Hex(0x2A0C06));   // wanted F08030 / FFB040 / FFD870 / FFF0A0, core white
            // grey wispy smoke (#726369 body)
            CelPalette.Add("fb_smoke", CelPalette.Hex(0x5A4F55), CelPalette.Hex(0x726369), CelPalette.Hex(0x8C8086), CelPalette.Hex(0xA59AA0),   // c05: the ref's #726369 grey (was near-white)
                CelPalette.Hex(0xB0A6AA), 1.1f, 2, CelPalette.Hex(0x1C171A));   // b03: two bands, a near-black ink (the reference's smoke is inked)
            // d01: the path-ribbon wisp: pale white-grey lit side, the ref's #726369 grey shade, a dark grey ink
            CelPalette.Add("fb_wisp", CelPalette.Hex(0x5E565C), CelPalette.Hex(0x7A7076), CelPalette.Hex(0xA29BA0), CelPalette.Hex(0xCBC6C8),
                CelPalette.Hex(0xE4E0E0), 1.1f, 3, CelPalette.Hex(0x2E282C));
            Comets = new CelFxRecipe[Launch.Length]; CometsMid = new CelFxRecipe[Launch.Length]; CometsFar = new CelFxRecipe[Launch.Length];
            Hits = new CelFxRecipe[Launch.Length];
            BoltLights = new CelFxRecipe[Launch.Length];
            Smokes = new CelFxRecipe[Launch.Length];
            SmokesMid = new CelFxRecipe[Launch.Length];
            int gfires = 0;
            for (int i = 0; i < Launch.Length; i++)
            {
                bool pre = AlongAt[i] > 0f;
                float hitT = Launch[i] + Flights[i];
                float bw = OnGround[i] ? (i % 3 == 0 ? 1f : 0.7f) : (hitT > 3.3f ? 0.55f * 1.9f : 0.55f * 1.2f);   // e05: REF dummy heads 32-47 px (the final volley's biggest)
                float bodyK = !OnGround[i] && hitT > 3.3f ? 1.5f : 1f;
                float trailK = (Volley[i, 0] < 0.25f || hitT > 3.3f) ? 1f : 0.6f;   // e13: REF trail area f22-34 and f94-100 high, f40-88 low
                // z02: matched native crops show this near-ground bolt is 1.57x too wide.
                // Scale flame radius alone; bw also sets swirl and smoke width.
                float flameScale = i == 12 ? 0.64f : 1f;
                Comets[i] = MakeComet(Launch[i], Flights[i], pre, 1f, bw, GravityOf[i], BodyOf(i) * bodyK, trailK, flameScale); CometsMid[i] = MakeComet(Launch[i], Flights[i], pre, 1.7f, bw, GravityOf[i], BodyOf(i) * bodyK, trailK, flameScale); CometsFar[i] = MakeComet(Launch[i], Flights[i], pre, 2.6f, bw, GravityOf[i], BodyOf(i) * bodyK, trailK, flameScale);   // d49: bigger bolts at Mid/Far (O120/O240 lost them)
                Hits[i] = MakeHit(Launch[i] + Flights[i], OnGround[i], OnGround[i] && Launch[i] + Flights[i] > 1.15f && (gfires++ & 1) == 0, DummyLightShare(i), TargetOf[i] == 2 ? 0.95f : 0.95f, hitT > 2.9f ? (TargetOf[i] == 1 ? 8f : 1f) : 2f, DomeLife(i));   // e21b: the far fire ends in a fan, the near one in a tongue
                BoltLights[i] = MakeBoltLight(Launch[i], Flights[i], OnGround[i] ? 0.5f : (hitT > 3.3f ? 0.7f : hitT < 2.05f ? 1.7f : 1.0f));   // e37b: the opening dummy bolts light most (REF f24-f40)   // e37: the mid-volley dummy bolts light the ground as the opening ones (REF f40), the final volley less (REF f90)   // e20d: the mid volley and ground-fire bolts light less (REF f60)
                Smokes[i] = MakeSmoke(Launch[i] + Flights[i], i, SmokeOn[i]);   // played on ground hits only (the film: the lobe chain)
                SmokesMid[i] = MakeSmoke(Launch[i] + Flights[i], -1);   // c01: T1 plays the b05 strand (no Lobe draw added at T1)
            }
        }

        // templates (the tests find and validate these)
        public static readonly CelFxRecipe Comet = MakeComet(0f, Flight, false);
        public static readonly CelFxRecipe DummyHit = MakeHit(Flight, false);
        public static readonly CelFxRecipe GroundHit = MakeHit(Flight, true);
        /// <summary>The soft pool under the whole volley: brightest ~2.95 s after the fire, gone by ~4.5 s.</summary>
        public static readonly CelFxRecipe Pool = MakePool(0f);
        /// <summary>The volley: bolt i's comet and its hit, shifted by Launch[i].</summary>
        public static readonly CelFxRecipe[] Comets, Hits, CometsMid, CometsFar;
        /// <summary>b03: bolt i's smoke column (the Range plays it on ground hits only), with dir = up and a little
        /// screen-right, so the column leans along the shot.</summary>
        public static readonly CelFxRecipe[] Smokes, SmokesMid;
        public static readonly CelFxRecipe Smoke = MakeSmoke(Flight, 0);
        public static readonly CelFxRecipe SmokeMid = MakeSmoke(Flight, -1);

        /// <summary>e20: bolt i's warm light on the ground, sliding under it from its launch to its hit (the Range plays it
        /// from the ground under the launch spot, aimed at the ground under the hit).</summary>
        public static readonly CelFxRecipe[] BoltLights;
        const float BoltLightHold = 0.35f;
        static CelFxRecipe MakeBoltLight(float t0, float flight, float strength = 1f)
        {
            float life = flight + BoltLightHold;
            return new CelFxRecipe("fire_bolt_light", t0 + life,
                new CelLayer
                {
                    Name = "bolt ground light", Kind = CometKind.KindName, Start = t0, Life = life, LifeJitter = 0f, Arrive = flight / life,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Gravity = 0f, Drag = 0f,
                    Size = new CelCurve(4.2f, 3.4f), SizeJitter = 0.1f,
                    Palette = "fb_pool", Core = 1f, Opacity = 0.55f * strength,   // e20c   // e20b: wider, brighter (REF f40-f90)
                    // pool mode: falloff 1.5, full by 8% of its life, fading out over the last third (after the hit)
                    Extra = new Vector4(-1f, 1.5f, 0.08f, Mathf.Clamp01((flight - 0.1f) / life)),
                });
        }

        /// <summary>e20b: a warm pulse on the ground under the dummy for dummy hit i (the Range plays it on the ground under
        /// the hit); lights the ground only, so the board does not wash out.</summary>
        public static readonly CelFxRecipe[] HitLights = MakeHitLights();
        static CelFxRecipe[] MakeHitLights()
        {
            var a = new CelFxRecipe[Count];
            for (int i = 0; i < Count; i++)
            {
                float hit = Mathf.Max(0f, Volley[i, 0]) + Volley[i, 1] - Mathf.Max(0f, -Volley[i, 0]);
                a[i] = new CelFxRecipe("fire_bolt_hit_light", hit + 1.0f, new CelLayer
                {
                    Name = "hit ground light", Kind = CometKind.KindName, Start = hit, Life = 1.0f, LifeJitter = 0.1f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(2.4f, 2.8f), SizeJitter = 0.1f,
                    Palette = "fb_pool", Core = 1f, Opacity = hit > 3.3f ? 0.2f : 0.35f,   // e37: the final volley's pulses pile up under the dummy (REF f90)   // e20c
                    Extra = new Vector4(-1f, 1.4f, 0.08f, 0.25f),
                });
            }
            return a;
        }

        const float PoolLife = 4.1f, PoolRadius = 7.0f;   // e20c: gone by f110 (REF)   // e07: REF pool peaks at f24 (0.5 s after the fire) and is gone by f120

        static CelFxRecipe MakePool(float start)
        {
            return new CelFxRecipe("fire_bolts_pool", start + PoolLife,
                new CelLayer
                {
                    Name = "warm ground light", Kind = CometKind.KindName, Start = start, Life = PoolLife, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.85f * PoolRadius, PoolRadius, 0.5f), SizeJitter = 0f,
                    Palette = "fb_pool", Core = 1f, Opacity = 0.45f,   // e20c: the late lower-left wash (REF f60-f90)   // e20: a faint base, the bolts carry the light   // p01: softer (REF's ground light is a faint wash)
                    // pool mode, falloff power 1.2, peak at 2.95 s into its life (f90), fade from 3.15 s (f95) to f137
                    Extra = new Vector4(-1f, 1.2f, 3.0f / PoolLife, 3.2f / PoolLife),   // e20c: peak f84
                });
        }

        static CelFxRecipe MakeComet(float t0, float flight, bool preflown, float k = 1f, float w = 1f, float g = ArcGravity, float bodyM = 5.0f, float trailK = 1f, float flameScale = 1f)   // p28b: w scales the head (dummy bolts 0.6: REF's are thin)   // d49: k scales the bolt for the Mid/Far tiers
        {
            return new CelFxRecipe("fire_bolt_comet", t0 + flight + Hold,
                new CelLayer
                {
                    Name = "comet", Kind = CometKind.KindName, Start = t0, Life = flight + Hold, LifeJitter = 0f, Arrive = flight / (flight + Hold),   // p27j: lives Hold s past the hit (the tail streams in, shader BOLT_HOLD), still arrives at `flight`
                    Close = 1, Mid = 1, Far = 1, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Spin = 4.5f, Gravity = g,
                // Inherited comment (kept inactive): p21: Spin = the swirl's rate (rad/s)
                    // b01 (beach, 19 m camera): ribbon 0.2 m wide, 0.75 s of its speed long (a third to half the frame)
                    // b02: a negative Stretch draws the tail behind the birth (the opening bolts were in flight before the fire)
                    Stretch = preflown ? -0.72f : (trailK >= 1f && t0 > 1.5f ? 1.3f : 0.55f),   /* e13b: the final volley's trails span the whole path (REF f94-f100) */ Size = new CelCurve(0.14f * k * (0.75f + 0.2f * Mathf.Clamp01((w - 0.45f) / 0.55f)), 0.15f * k * (0.75f + 0.2f * Mathf.Clamp01((w - 0.45f) / 0.55f))), SizeJitter = 0.2f, // Deposited history; the last volley clears its shorter trails sooner.
                      // c01: 0.2 -> 0.26-0.3 m (the smoke trail frays wider)
                    Erode = new CelCurve(0.06f, 0.2f),   // d18: stays whole (d17 dashes lost)   // c03: the ribbon breaks into pieces (was whole)
                    Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 1.0f,   // p21: a pale yellow head (REF), no white blaze   // p02: the head blazes   // b03: 2.6 -> 2.2 (the bodies read white-hot)
                    Opacity = 0.9f * trailK, Ink = 0f,   // e13: per-bolt trail strength (REF: opening + final volley trails strong, mid volley faint)
                    // b02: a round white-yellow head 0.2 m, a flame body that flares and tapers over 2.2 m, thin red
                    // streaks fading out to 5 m, a ribbon waving 0.08 m over 5.5 m
                    Extra = new Vector4(0.2f * k * w * flameScale, bodyM * k, 0.22f * w, t0 + flight > 3.3f ? Mathf.Max(0.1f, 3.80f - t0 - flight) : 0.5f)   /* e13d: the final volley's trails all clear together at 3.8 s after the fire (REF f101-f103) */, // Head radius, body length, path swirl, smoke fade seconds.
                });
        }

        /// <summary>Seconds a comet lives past its hit, its head frozen there while the smoke clears from its oldest end (= shader BOLT_HOLD).</summary>
        public const float Hold = 0.5f; // Must match shader BOLT_HOLD.

        /// <summary>e24: a ground fire burns on to the next hit at its spot (REF: lit continuously once hit) + 0.3 s.</summary>
        static float DomeLife(int i)
        {
            if (!OnGround[i]) return 0.85f;
            float at = Launch[i] + Flights[i], next = 1e9f;
            for (int j = 0; j < Count; j++) { float h = Launch[j] + Flights[j]; if (TargetOf[j] == TargetOf[i] && h > at + 1e-3f && h < next) next = h; }
            return next > 1e8f ? 0.85f : Mathf.Clamp(next - at + 0.3f, 0.85f, 1.6f);
        }

        // Overlapping hits share one reference-sized illumination peak. The GPU
        // sprites still vary independently; several point lights must not wash the board out.
        static float DummyLightShare(int index)
        {
            float at = Launch[index] + Flights[index], sum = 0f;
            for (int j = 0; j < Launch.Length; j++)
            {
                if (OnGround[j]) continue;
                float f = Flights[j];
                float proximity = Mathf.Max(0f, 1f - Mathf.Abs(at - Launch[j] - f) / 0.4f);
                sum += proximity * proximity;
            }
            return 1f / Mathf.Max(1f, sum);
        }

        static CelFxRecipe MakeHit(float hit, bool ground, bool bigFire = true, float lightShare = 1f, float domeR = 0.8f, float blades = 2f, float domeLife = 0.85f)
        {
            // b02: no glowing disc on the dummy (the critic read it as a flat white plate): a dummy hit is a starburst of
            // 24 thin bright spark streaks (6-10 frames) and a few flame tongues; a ground hit keeps a small short flash
            var r = new CelFxRecipe(ground ? "fire_bolt_ground" : "fire_bolt_dummy", hit + 2.2f,
                new CelLayer
                {
                    Name = "spark starburst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.10f, Life = ground ? 0.42f : 0.65f, LifeJitter = 0.25f,
                    Close = ground ? 8 : 10, Mid = ground ? 8 : 10,   /* e25: no firework of lines in the fire (REF dots) */   // p28e: a control film with 0 showed the spray IS this layer (the dash counter was fooled by the thin bolts' lines): REF has 10-15 bold dashes   // p28d: 100 counted 56-70 a frame beside the thin bolts' lines (REF 11-19)   // p27m: 130 counted 21-35 (REF 11-19)   // p27k: 160 counted 20-34 a frame (REF 7-19)   // p27i: the close film sits at zoom 30 = the Mid blend point, so Mid IS the count there (6 drew six dashes)   // p27g: 280 per hit wrapped the Streak ring pool in ~0.25 s and killed the live dashes   // p16: dense short dashes on the dummy   // p04: REF's ground hits are spark starbursts   // d06 (from d04): denser sprays   // c03: a fuller radial spray / dummy spark fan
                    Far = 0, Shape = CelShape.Shell, ShapeSize = new Vector3(ground ? 0.10f : 0.05f, 0f, 0f), Aim = ground ? CelAim.Up : CelAim.Back, Spread = ground ? 58f : 55f,   /* e19: back up the incoming path (REF) */
                    Speed = ground ? 2.6f : 4.0f, SpeedJitter = 0.45f, UpBias = 0f, Gravity = 2.4f, Drag = ground ? 1.5f : 1.3f,   // p27j: 9 m/s flew out of the crop in 8 frames (REF: within 150 px, 12+ frames)   // p27i: a fast burst that brakes hard (REF: out of the ball in 2 frames, then slow)   // p27e: drag 2.5 shrank them to dots by 8 frames   // p27d: slower (REF)   // c01: born on a shell (no pile-up disc at the middle), ground sparks fan upward
                    // b04: short orange dots and streaks over the torso's width (b03 lost its palette, erosion and core
                    // to a comment, so 28 default-white streaks piled up into a disc at the chest)
                    Size = new CelCurve(ground ? 0.10f : 0.16f, ground ? 0.035f : 0.06f), SizeJitter = 0.35f, Stretch = ground ? 0.025f : 0.085f,   // p28e: bolder, longer (REF)   // p27l: 4 px (REF 3-4)   // p27j: 3-4 px wide, 13 px long at birth, 6 once braked (REF 6-11)   // p27i: 9 m/s x 0.05 s = 30 px at first, 12 px once braked (REF)   // p27f: 2.6 px wide (1.5 px AA'd away), 0.14 s x 3.5 m/s = 32 px   // p27e: Size is the WIDTH (1.5 px); length = speed x 0.07 s = 8-32 px (REF 10-40)   // p27c: Stretch is seconds of motion (0.3 s drew 4 m rods)   // p22: bolder dashes   // p06: REF's dummy sparks are short bright dashes   // c01: thinner, longer streaks (b05: 0.05, 0.09)
                    Erode = new CelCurve(0.02f, 0.45f), Palette = ground ? "fb_comet" : "fb_dash", Core = ground ? 1.35f : 0.8f, Extra = new Vector4(0.45f, 0.07f, 0.08f, 0.35f), Ink = 0f,   // p27l: brighter (REF: yellow-white)   // p27j: min length 0.03 m   // p27e: a 60% tail (0 drew darts), less flicker   // p27d: yellow-white   // p10: no ink (REF's sparks are soft bright dashes)
                },
                new CelLayer
                {
                    Name = "flame tongues", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + (ground ? 0f : 0.14f), Duration = 0.08f, Life = ground ? 0.24f : 0.45f, LifeJitter = 0.25f,
                    Close = ground ? 4 : 2, Mid = ground ? 4 : 2, Far = 0,
                // Inherited comment (kept inactive): p27m: fewer, bigger   // p27j   // p27i: Mid = Close (zoom 30)   // c01: one tongue on the dummy (three lit a disc on the chest)
                    Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = ground ? CelAim.Up : CelAim.Back, Spread = ground ? 50f : 68f, Speed = ground ? 1.1f : 3.2f, SpeedJitter = 0.4f, Drag = ground ? 0f : 1.0f, Buoyancy = ground ? 0.6f : 0f,   // p27l: grow out 0.15 m and stop (REF: attached spikes)
                    Size = new CelCurve(ground ? 0.07f : 0.20f, ground ? 0.02f : 0.02f, 0.8f), SizeJitter = 0.3f, Stretch = ground ? 0.16f : 0.12f,
                // Inherited comment (kept inactive): p27m: 0.17 m, 11 px at the base   // p27l: 0.12 m wedges, 8 px at the base   // p27j: 0.08 m wedges   // p27e: thin spikes 16 px long Erode = new CelCurve(0.05f, 0.5f), Palette = ground ? "fire" : "fb_dash", Core = ground ? 1.3f : 1.7f, Ink = 0f, Extra = new Vector4(ground ? 0f : 1f, ground ? 0.12f : 0.1f, 0.35f, ground ? 0f : 0.9f),   // p27l: min 0.1 m, brighter   // p27j: a wedge wide at the ball, pointed outward (Streak extra.w)   // p27: rays round the fireball; the p05 comment had swallowed everything after Size since p05
                    Erode = new CelCurve(0.05f, 0.5f), Palette = "fb_comet", Core = 1.25f, Ink = 0f, Extra = new Vector4(ground ? 0f : 1f, 0.10f, 0.20f, ground ? 0f : 0.9f),
                },
                new CelLayer
                {
                    Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = hit, Life = 0.4f,
                    Close = 1, Mid = 1, Far = 0, Palette = "fb_comet", Core = ground ? 0.05f : (hit > 3.3f ? 0.09f : 0.14f) * lightShare * 0.65f, Extra = new Vector4(ground ? 4f : 2.5f, ground ? 0f : 0.04f, 0f, 0f),   // e30: REF's board stays beige (a local glow, not a yellow board) // Share the warm target pulse; suppress the stock flash card.
                });
            r.Layers.Add(new CelLayer
            {
                Name = "hit flash", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Start = hit, Life = 0.17f, LifeJitter = 0.04f,
                Close = 0, Mid = 0, Far = 0, Size = new CelCurve(0.20f, 0.31f, 0.5f), SizeJitter = 0.15f,
                Palette = "fb_comet", Core = 1.2f, Opacity = 0.4f, Extra = new Vector4(-4f, 0f, 0f, 0f),
            });
            for (int h = 0; h < 1; h++) r.Layers.Add(new CelLayer   // p22: a HARD ragged cel splash over the soft flash (Comet mode -4, extra.y = 1): the harsh half of a hit
            {
                Name = "hit splash hard", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.06f * h - 0.03f, ground ? 0.25f : 0.04f * h, 0f), Start = hit + 0.04f * h, Life = ground ? domeLife : (hit > 3.5f ? 1.19f : 0.95f) - 0.08f * h, LifeJitter = 0.1f,
                Close = 1, Mid = 1, Far = 0, Size = ground ? new CelCurve(domeR, domeR) : new CelCurve(1.45f - 0.22f * h, 0.90f, 0.5f), SizeJitter = 0.15f,   // p28: a 114 px quad: the splat's base 34 px, the white blob 18 px, fingers to 80 px (REF)   // p27k: 0.95 blew out   // p27j: r 35-42 px (REF)   // p22c: a big splash on the board
                Palette = "fb_comet", Core = 1.15f, Opacity = 1f, Extra = new Vector4(-4f, 1f, ground ? 2f : 0f, ground ? blades : 0f),   // e21b: blades per hit
            });
            if (!ground && hit > 3.5f) r.Layers.Add(new CelLayer
            {
                Name = "final flame remnants", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.2f, 0f),
                Start = hit + 1.0f, Life = 0.42f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.60f, 0.16f),
                Palette = "fb_comet", Core = 0.7f, Opacity = 0.68f,
                Extra = new Vector4(-4f, 1f, 0f, 1f),
            });
            // d27 (stall fracture: dummy hit -> its lingering burn; new primitive, Comet mode extra.x = -3): an orange
            // cel-blob flame behind the hit star (d28: the star rays stay on top), then a small ember that burns to ~1.4 s
            if (!ground) r.Layers.Add(new CelLayer   // p16: a soft lingering glow on the board instead of the cel-blob flame wrap (it read as orange lumps)
            {
                Name = "board glow", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, 0.08f), Start = hit + 0.1f, Life = 0.55f, LifeJitter = 0.2f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.22f, 0.14f, 0.7f), SizeJitter = 0.2f,
                Palette = "fb_spark", Core = 1.2f, Opacity = 0.08f, Extra = new Vector4(-4f, 0f, 0f, 0f),   // p26: fainter still (a ring on the board)   // p18: faint (it read as a target ring)
            });
            if (!ground) r.Layers.Add(new CelLayer   // p26: a small standing flame stays on the board after the hit (REF f33-40)
            {
                Name = "board flame", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, 0.08f), Start = hit + 0.08f, Life = 0.55f, LifeJitter = 0.2f,
                Close = 0, Mid = 0, Far = 0, Aim = CelAim.None, Speed = 0f, Gravity = 0.05f, Drag = 0f, Buoyancy = 0f, Spin = 0f,
                Size = new CelCurve(0.06f, 0.16f), SizeJitter = 0.2f, Stretch = -0.1f, Erode = new CelCurve(0.06f, 0.2f),
                Palette = "fb_comet", CoolTo = "fb_ribbon", Core = 1.7f, Opacity = 0.25f, Ink = 0f,
                Extra = new Vector4(0.14f, 0.45f, 0.02f, -0.8f),
            });
            if (!ground) for (int b = 0; b < 0; b++) r.Layers.Add(new CelLayer
            {
                Name = "flame wrap", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.05f * b - 0.03f, 0.02f + 0.1f * b, 0.08f), Start = hit + 0.06f + 0.06f * b, Life = 0.45f + 0.35f * b, LifeJitter = 0.15f,   // p01: REF's dummy is clean ~0.5 s after its last hit
                Close = 1, Mid = 1, Far = 0, Aim = CelAim.Up, Spread = 20f, Speed = 0.25f, SpeedJitter = 0.3f, Drag = 0.5f, Gravity = 0f, Buoyancy = 0.4f, Spin = 0f,
                Size = new CelCurve(0.14f, 0.36f - 0.24f * b, 0.5f), SizeJitter = 0.2f, Stretch = 0f, Erode = new CelCurve(0f, 0.75f, 1.3f),   // d42: a fan-shaped flame splash on the board (REF f96-f121), torn by noise
                Palette = "fire", Core = 1.6f, Opacity = 0.45f, Ink = 0f, Extra = new Vector4(-3f, 2.1f, 2.8f, 1.3f),   // p04: a yellow-white flash on the board
            });
            if (ground)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "hit star", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.15f, 0f), Start = hit, Life = 0.16f, LifeJitter = 0.04f,
                    Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.26f, 0.42f, 0.5f), SizeJitter = 0.15f,
                    Palette = "fb_comet", Core = 1.5f, Opacity = 0.35f, Extra = new Vector4(-4f, 0f, 0f, 0f),
                });
                // b03: a ragged burst of wide flame tongues about the dummy's torso tall, fanned out (b02's were thin
                // upright slivers), then a smaller fire that flickers on and dies about 1 s after the hit
                r.Layers.Add(new CelLayer
                {
                    Name = "ground burst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit, Duration = 0.08f, Life = 0.25f, LifeJitter = 0.25f,
                    Close = 0, Mid = 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.16f, 0f, 0f), Aim = CelAim.Up, Spread = 55f,   /* e25: off */ Speed = 1.8f, SpeedJitter = 0.4f,
                // Inherited comment (kept inactive): p11: a spray of thin flame spikes Buoyancy = 0.8f,
                    Size = new CelCurve(0.16f, 0.04f, 0.6f),   // p22: bolder spikes   // p12   // p04: small ragged tongues (REF: a spark pop with a small flame, not a yellow fan)   // c05: bigger round bursts (the ref's pop at the landing point)
                    SizeJitter = 0.4f, Stretch = 0.22f,   // p12: long spikes   // c03: round flicker flames on the ground (c01 shot carrots up into the air)   // b04: narrower fan, taller (about the torso)
                    Erode = new CelCurve(0.05f, 0.55f), Palette = "fb_comet", Core = 1.25f, Ink = 0f, Extra = new Vector4(0f, 0.12f, 0.2f, 0f),
                });
                for (int f = 0; f < 2; f++) r.Layers.Add(new CelLayer   // p16: a standing flame (Comet fire mode: extra.w < 0; a tiny gravity points its head down, so the flame body rises)
                {
                    Name = "ground flame", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.13f * f - 0.05f, 0.10f, 0.04f * f), Start = hit + 0.05f + 0.18f * f, Life = bigFire ? 1.05f - 0.2f * f : 0.46f, LifeJitter = 0.18f,
                    Close = 0, Mid = 0, Far = 0, Aim = CelAim.None, Speed = 0f, Gravity = 0.05f, Drag = 0f, Buoyancy = 0f, Spin = 0f,
                    Size = new CelCurve(0.08f, 0.22f), SizeJitter = 0.2f, Stretch = -0.1f, Erode = new CelCurve(0.06f, 0.2f),
                    Palette = "fb_comet", Core = 1.35f, Opacity = 0.60f, Ink = 0f,
                    Extra = new Vector4((bigFire ? 0.25f : 0.18f) * (f == 0 ? 1f : 0.72f), (bigFire ? 0.82f : 0.58f) * (f == 0 ? 1f : 0.8f), 0.02f, -0.8f),
                });
                if (bigFire) r.Layers.Add(new CelLayer   // d23: the lasting fire body on every other ground hit after ~f40 only (ref: 1-2 fires at once)
                {
                    Name = "ground fire", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.12f, Duration = 0.75f, Life = 0.6f, LifeJitter = 0.3f,
                    Close = 0, Mid = 0, Far = 0,
                // Inherited comment (kept inactive): p16: off, the standing flame replaces it Shape = CelShape.Disc, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Up, Spread = 50f, SpeedJitter = 0.35f, Buoyancy = 0.6f,
                    Speed = 1.2f, Size = new CelCurve(0.42f, 0.1f, 0.7f),   // p12: thin licking tongues   // p04: smaller lasting fire   // d22: low wide dome-then-shreds (ref f87-f101 zoom), not tall tongues   // d21: compact crater fire of long-lived flame tongues (16-20 f each), Mid near Close (LOD trap)   // c03: low round flames ~0.5 dummy tall (c01: 3.7 m/s carrots in a column)   // c01: as tall as the torso (b05: 0.65 m, speed 3.0)   // b04: about the torso tall (b03: under half)
                    SizeJitter = 0.3f, Stretch = 0.25f, Erode = new CelCurve(0.1f, 0.6f), Palette = "fire", Core = 1.7f, Ink = 0f, Extra = new Vector4(0f, 0.15f, 0.4f, 0f),
                });
                if (bigFire) r.Layers.Add(new CelLayer   // d38: a crater ember (cel blob, Comet mode -3): a low licking fire that outlives the burst and dies ~1.2 s after the hit (ref keeps a red ember in its smoke to ~f104)
                {
                    Name = "crater ember", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.12f, 0f), Start = hit + 0.2f, Life = 1.4f, LifeJitter = 0.15f,
                    Close = 0, Mid = 0, Far = 0,
                // Inherited comment (kept inactive): p10: off - it read as a flat orange leaf floating beside the fire Aim = CelAim.Up, Spread = 10f, Speed = 0.1f, SpeedJitter = 0.3f, Drag = 0.5f, Gravity = 0f, Buoyancy = 0.15f, Spin = 0f,
                    Size = new CelCurve(0.55f, 0.25f, 0.8f), SizeJitter = 0.2f, Stretch = 0f, Erode = new CelCurve(0f, 0.6f, 1.4f),
                    Palette = "fire", Core = 1.0f, Opacity = 0.9f, Ink = 2f, Extra = new Vector4(-3f, 1.6f, 2.6f, 0.9f),
                });
                if (hit > 2.9f) for (int b = 0; b < 2; b++)   /* e22c: two billows per late hit, apart (one fused into a blob) */ r.Layers.Add(new CelLayer   // e15: the late fires billow into warm tan puff clusters (REF f95-f128)
                {
                    Name = "smoke billow", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.60f * b - 0.32f, 0.55f + 0.25f * b, 0.05f * b),   // e38: above the fire (REF f96)   // e31: low at the fire (REF)   // e15b: three smaller lobes
                    Start = hit + 0.30f + 0.10f * b, Life = 1.62f - 0.12f * b,
                // Inherited comment (kept inactive): e38: later (the fire burns in front first, REF f96)   // e31b: lingers to ~f130 (REF) LifeJitter = 0.06f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.Up, Spread = 20f, Speed = 0.45f, SpeedJitter = 0.3f, Drag = 0.9f, Gravity = 0f, Buoyancy = 0.15f, Spin = 0f,   // e31: stays low (REF)
                    Size = new CelCurve(0.40f * domeR / 0.8f, 0.78f * domeR / 0.8f, 0.6f),   // e31: bigger (REF) SizeJitter = 0.15f,   /* e22b */ Stretch = 0f, Erode = new CelCurve(0f, 0.80f, 1.6f),
                    Palette = "fb_smoke", Core = 1f, Opacity = 1f, Ink = 0f, Extra = new Vector4(-3f, 1.9f, 1.3f, -0.62f),   // e36b: lumpier outline   /* e22b: lobes spread wider */
                });
                r.Layers.Add(new CelLayer   // e24c: embers rise off the fire for as long as it burns (REF)
                {
                    Name = "embers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = hit + 0.05f, Duration = Mathf.Max(0.3f, domeLife - 0.3f), Life = 0.5f, LifeJitter = 0.4f,
                    Close = 10, Mid = 6, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.35f * domeR, 0f, 0f), Aim = CelAim.Up, Spread = 70f,   // e34: a few (REF 4x)   // e24d: dozens, flung wide (REF)
                    Speed = 3.4f, SpeedJitter = 0.55f, Gravity = 2.0f, Drag = 0.5f, UpBias = 0f,
                    Size = new CelCurve(0.06f, 0.03f), SizeJitter = 0.4f, Stretch = 0.004f, Erode = new CelCurve(0.02f, 0.5f),   // e34: small round dots (REF)
                    Palette = "fb_comet", Core = 1.3f, Ink = 0f, Extra = new Vector4(0.45f, 0.03f, 0.08f, 0.35f),
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire core", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.2f, 0f), Start = hit, Life = 0.52f, LifeJitter = 0.15f,
                    Close = 0, Mid = 0, Far = 0, Size = new CelCurve(0.32f, 0.14f, 0.6f), SizeJitter = 0.15f,
                    Palette = "fb_comet", Core = 1.6f, Opacity = 0.7f,
                    Extra = new Vector4(-4f, 0f, 0f, 0f),   // p12: a smooth white-hot base   // b04: the flash mode b03 lost to a comment: a white-hot base
                });
                r.Layers.Add(new CelLayer
                {
                    Name = "fire glow", Kind = CometKind.KindName, Anchor = CelAnchor.Target, Start = hit, Life = 1.0f, LifeJitter = 0f,
                    Close = 0, Mid = 0, Far = 0, Size = new CelCurve(0.9f, 0.7f), SizeJitter = 0.1f,
                    Palette = "fb_pool", Core = 1f, Opacity = 0.12f, Extra = new Vector4(-1f, 1.6f, 0.08f, 0.35f),   // p11: the red disc under the fire was far too strong
                });
            }
            return r;
        }

        // b03: a smoke column per ground hit, from the kit's Lobe kind (b02's puff chains read as single beans): a
        // stream of inked lobes, lit from above in the palette's two grey bands, rising about 1.5 dummy heights, leaning
        // along dir (up and screen-right), growing and eroding away until gone ~1.3 s after the hit
        // c01: the smoke column is a chain of overlapping inked lobes laid along one S-curve (b05's strand broke into
        // flat pale squiggle pieces with no visible ink): the lobes' ink hulls merge into one outlined column that
        // rises from the fire (each lobe is born when the column's front reaches its height), leans screen-right,
        // widens and then thins toward its top, and breaks up from the top down near the end. Close only: Mid keeps
        // the b05 strand (no Lobe draw at T1), Far none.
        const int SmokeLinks = 10;   // c03: separate soft puffs curling up (c01: 26 merged into lumpy columns)
        const float SmokeHeight = 2.3f, SmokeRise = 2.4f, SmokeEnd = 2.2f;   // c03: ~1.3 dummy heights, lasting to the ref f134
        static CelFxRecipe MakeSmoke(float hit, int variant, bool first = false)
        {
            var r = new CelFxRecipe("fire_bolt_smoke", hit + 2.4f);
            if (variant < 0)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke ribbon (mid)", Kind = CometKind.KindName, Anchor = CelAnchor.Origin, Offset = new Vector3(0f, 0.1f, 0.2f), Start = hit + 0.1f, Life = 1.6f, LifeJitter = 0.1f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.Up, Spread = 6f, Speed = 1.7f, SpeedJitter = 0.15f, Drag = 0f, Gravity = 0f, Buoyancy = 0.7f, Spin = 2.2f,
                    Size = new CelCurve(0.22f, 0.62f, 0.8f), SizeJitter = 0.2f, Stretch = 4f, Erode = new CelCurve(0f, 0.9f, 2.2f),
                    Palette = "fb_smoke", CoolTo = "fb_smoke", Core = 1f, Opacity = 0.7f, Ink = 2f,
                    Extra = new Vector4(0f, -1f, 0.22f, 1.5f),
                });
                return r;
            }
            // d01: smoke as path ribbons (Comet mode extra.x = -2): two soft grey wisps per smoked hit, each a strip
            // bent along a curling path that climbs ~1.4 dummy heights, drifts screen-right, widens from a thin
            // thread to a frayed band, tears into pieces and fades (replaces the Lobe bead columns at Close)
            for (int k = 0; k < 1; k++)
            {
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke wisp", Kind = CometKind.KindName, Anchor = CelAnchor.Origin, Offset = new Vector3(0.12f * k - 0.05f, 0.05f, 0.1f * k), Start = hit + 0.06f + 0.14f * k, Life = first ? Mathf.Max(1.5f, 5.1f - hit) : Mathf.Min(1.95f - 0.22f * k, Mathf.Max(0.65f, 4.8f - hit - 0.14f * k)), LifeJitter = 0.04f,
                    Close = 1, Mid = 1, Far = 0, Aim = CelAim.None, Speed = 0f, Gravity = 0f, Drag = 0f, Buoyancy = 0f, Spin = 1.1f + 0.3f * k,
                    Size = first ? new CelCurve(1.0f, 1.4f, 1.0f) : new CelCurve(0.10f, 2.2f, 1.6f), SizeJitter = 0.15f, Stretch = 3.3f,
                    Erode = new CelCurve(0.02f, 0.62f, 2.8f),
                    Palette = "fb_wisp", Core = 1f, Opacity = 1f, Ink = 0f,   // e34: opaque (REF 4x)
                    Extra = new Vector4(-2f, 0.23f, 0.38f, 2.4f + 0.4f * k),   // d32: one broad leaning curl, not a tight corkscrew   // d07: gentler S, more lean (the wider puffs fold on tight curls)
                });
            }
            if (r.Layers.Count > 0) return r;
            float phase = variant * 1.7f, sway = 0.42f + 0.1f * (variant % 3);   // c01_3: a wider S (0.22 m was lost in the column's width)
            for (int k = 0; k < SmokeLinks; k++)
            {
                float u = k / (float)(SmokeLinks - 1), h = u * SmokeHeight;
                // lean right about 0.55 m per metre (c05: 0.3), an S of 0.22-0.34 m that grows with height
                float x = 0.55f * h + sway * Mathf.Sin(h * 2.2f + phase) * (0.3f + 0.7f * u);   // c03: tighter curl
                float born = 0.08f + h / SmokeRise;
                float life = Mathf.Max(0.45f, SmokeEnd - born);
                // radius: 0.12 m at the fire, widest (0.25 m) at 55% of the height, thinning to 0.11 m at its top
                float rad = Mathf.Lerp(0.22f, 0.42f, Mathf.Clamp01(u / 0.5f)) * (u > 0.5f ? Mathf.Lerp(1f, 0.7f, (u - 0.5f) / 0.5f) : 1f);   // c03: dummy-wide puffs
                r.Layers.Add(new CelLayer
                {
                    Name = "smoke link", Kind = CelKinds.LobeName, Anchor = CelAnchor.Origin, Offset = new Vector3(x, 0.05f + h, 0f), Start = hit + born, Life = life, LifeJitter = 0.05f,
                    Close = 1, Mid = 1, Far = 0, Shape = CelShape.Point, Aim = CelAim.Up, Spread = 25f, Speed = 0.35f, SpeedJitter = 0.3f, Drag = 0.4f, Buoyancy = 0.3f, Spin = 1.0f,
                    Size = new CelCurve(rad * 0.55f, rad * 1.15f, 0.5f), SizeJitter = 0.12f, Stretch = 0f,
                    // the top breaks up first: its erosion starts higher and the whole column goes in the last third
                    Erode = new CelCurve(0.1f * u, 1f, 3.6f - 1.2f * u), Feather = 0.2f,   // c03: softer edge
                    Palette = "fb_smoke", Core = 1f, Ink = 4f, Extra = new Vector4(1.0f, 0.12f, 0.9f, 0f),   // c03: smoother puffs (lump 0.22 read as cauliflower)   // c01_3: rounder billows (lump amp 0.16 -> 0.22)
                });
            }
            return r;
        }
    }
}
