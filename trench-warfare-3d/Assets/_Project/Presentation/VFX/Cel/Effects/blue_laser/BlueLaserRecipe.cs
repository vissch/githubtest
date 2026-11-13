// Phase: CelFx job, blue_laser (2026-11-09, lane/show/cel-blue-laser) - the thin blue laser of the owner's clip
// laser_thin_blue_cel: a held beam (about 3.3 s) on one axis broken into 3-5 pointed cyan dashes with dark gaps,
// each with a white core, ink, fine zig-zag wisps and short branch arcs, re-drawn every 2-3 frames at new places; at
// the hit a white-hot round core in a soft pale-blue bloom with a spiky white star, white and orange sparks; a soft
// bright cyan pool on the ground (no Light layer: SceneHooks.Flash blooms a white ball on the board even at core 0.22);
// the energy builds to a last volley at about 3.4 s (the clip's peak, f101), when a trail of soft tan dust puffs and
// loose black flecks rises up-right along the beam's way past the target and fades by 4.8 s, blue-white sparkles
// left on the board.
// LAB ONLY (owner, 2026-11-09): no weapon fires it; VfxLab, the Playground and the Range call Play. Energy weapon, so
// blue light is allowed (exception to "every light warm").
// Three plays of the kit: the beam (shooter -> target), the ground (level, at the foot) and the dust (along the trail).
// r04: the bloom, pool, puffs and flecks are LightningBeam dots (soft modes in its shader), so the effect draws
// LightningBeam + Glow (star, sparkles) + Streak (sparks) and no Lobe.
// LOD: Close everything; Mid fewer dashes, flashes, sparks and puffs, no branch arcs (the shader drops them) and the
// steady thread; Far the thread and the impact dot only (one LightningBeam draw), both kept a few px wide and lit
// through the fog by the shader.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class BlueLaserRecipe
    {
        // ---------------------------------------------------------------- palettes (picked off the clip's frames)
        /// <summary>The spindles: cyan body (#9fdcec), a thin white core, dark ink; c0 is the halo's light.</summary>
        public static readonly int BeamRow = CelPalette.Add("blue_laser_beam",
            // c01 (by eye, 2026-11-13): the clip's bolts are pale aqua with a white-pink head and NO dark ink; c0 is also
            // the soft bloom round them, so it is a mid cyan, not the old teal
            // c31: the body bands are the clip's cyan (#9fdcec), the white only in the core; ink paler still (no dark rim)
            CelPalette.Hex(0x98D8E6), CelPalette.Hex(0x9CDCEE), CelPalette.Hex(0xB4E8F4), CelPalette.Hex(0xCCF2FA),   // c46: paler and bluer (measured c45 body (94,181,200) vs the clip's (124,191,211))   // c42: the halo (c0) paler too: it is what saturated the body   // c41: paler still (measured c40: (116,201,214) vs the clip's (150,212,228))
            CelPalette.Hex(0xF0F4FF), 0.74f, 3, CelPalette.Hex(0x98DCE4));   // c46: a bluer core (c45 rendered (194,246,235), the clip's is (196,243,253))
        /// <summary>The impact: white-blue (star, rings, sparks).</summary>
        public static readonly int HitRow = CelPalette.Add("blue_laser_hit",
            CelPalette.Hex(0x2C6488), CelPalette.Hex(0x5AAED0), CelPalette.Hex(0x9FDCEC), CelPalette.Hex(0xD8F4FF),
            CelPalette.Hex(0xFFFFFF), 0.8f, 3, CelPalette.Hex(0x06121E));
        /// <summary>The ground glow, as it adds onto the dark field (additive: these are the light it adds).</summary>
        public static readonly int GroundRow = CelPalette.Add("blue_laser_ground",
            CelPalette.Hex(0x16304A), CelPalette.Hex(0x224466), CelPalette.Hex(0x2E5A88), CelPalette.Hex(0x3C72AC),
            CelPalette.Hex(0x3C72AC), 1.1f, 4, CelPalette.Hex(0x000000));
        /// <summary>r04: the ground pool, a soft bright cyan glow (additive, unbanded: c2 + c3 x falloff^2 at its middle).</summary>
        public static readonly int PoolRow = CelPalette.Add("blue_laser_pool",
            // b02: paler (the clip's ground glow is a whitish blue, not saturated cyan)
            CelPalette.Hex(0x1C3E62), CelPalette.Hex(0x2C5E92), CelPalette.Hex(0x4A86B4), CelPalette.Hex(0x3E6E96),   // b05: less hot middle
            CelPalette.Hex(0x8CC8F4), 0.92f, 3, CelPalette.Hex(0x000000));
        /// <summary>r04: the hit's soft bloom (additive light c2 + c3 x falloff^2) round a white-hot core.</summary>
        public static readonly int BloomRow = CelPalette.Add("blue_laser_bloom",
            // c01: more cyan (the clip's hit is an aqua disc on the board round a white point, not a blue-white ball)
            CelPalette.Hex(0x1C3E62), CelPalette.Hex(0x2C5E92), CelPalette.Hex(0x5CC4D8), CelPalette.Hex(0xB0ECF8),   // c35: aqua, not blue   // c11: whiter (clip: a lit board, not a cyan ball)
            CelPalette.Hex(0xF4FCFF), 0.9f, 3, CelPalette.Hex(0x000000));
        /// <summary>The clip's dust is a soft grey-tan; r04 puffs cover (c2 at the edge, c3 inside), no ink, no bands.</summary>
        public static readonly int DustRow = CelPalette.Add("blue_laser_dust",
            // b01: pre-encoded (sRGB of the wanted colour): the puff's cover colour came out linearised twice (tan #B49A72
            // rendered as saturated #74522B-ish orange), so these hexes are the wanted #8A7A64..#B49A72 encoded once more
                        // b04: darker, greyer (b03's rendered a bright saturated sand)
            // c30: warmer and lighter (the clip's cloud is an opaque tan-brown with shaded lobes); c2 the lit rim, c3 the body
            // c31: desaturated (c30's rendered as a saturated orange blob; the hexes are encoded twice, so they must be paler and greyer than the wanted tan)
            CelPalette.Hex(0x706F6D), CelPalette.Hex(0x7A7977), CelPalette.Hex(0x9E9894), CelPalette.Hex(0x83817F),   // c45: measured c44 (63,58,57) vs the clip's (65,61,61): greyer again   // c44: measured c43 (63,56,56) vs the clip's (65,61,61): green and blue back up, warmer lighter tops   // c43: less green   // c40: measured against the clip (hue 30, sat .17): near-neutral grey, lighter tops
            CelPalette.Hex(0x9E9894), 1.1f, 2, CelPalette.Hex(0x141010));
        /// <summary>r04: the black flecks in the dust (darker than smoke_black, so they read as the clip's ink specks).</summary>
        public static readonly int FleckRow = CelPalette.Add("blue_laser_fleck",
            CelPalette.Hex(0x050404), CelPalette.Hex(0x080606), CelPalette.Hex(0x0A0807), CelPalette.Hex(0x15100D),
            CelPalette.Hex(0x15100D), 1.1f, 2, CelPalette.Hex(0x000000));

        const float Hold = 3.4f;   // c06/c07: 3.3 -> 3.4 (vfxmatch: 3.3 ended 7 frames before the clip, 3.5 ended 5 late)           // seconds of dashes (the last dies about 3.45 s: the clip's beam ends at f105)
        const float Ramp = 0.6f;           // the thread fades in over half of this (the clip's beam builds up over its first 0.6 s)
        const float Volley = 3.2f;   // c27: 3.05 -> 3.2 (the clip's peak is f101 = 3.42 s after the fire; the bolts take 0.28 s)        // the last volley: longer dashes at once, the clip's peak (f96-f102)
        // NB: on a LightningBeam layer Ink is the beam's clip (tone.w: 1 stops the window at the target, 1.7 runs on
        // through it), not an ink width; the spindle's ink is fixed in its shader.
        const float DustAt = 2.85f;        // d05: later (every paper: ours at f89, the clip's ~f95)        // the dust trail (clip f88-f135; b02: earlier, b01's showed from f98)

        // r04: the beam is one axis broken into dashes: five lanes along it (heads at these shares of the beam,
        // window lengths picked so each dash is ~100-180 px on screen with a gap about as long), each lane re-drawn
        // every ~0.1 s (2-3 frames) at a jittered place, so 3-5 pointed dashes flicker along the beam at once.
        // c01 (by eye against the clip, 2026-11-13): the dashes are replaced by PROJECTILES. Each record is a comet bolt
        // (shader c01) whose head travels from the muzzle to the chest over its life (extra.z -> extra.w), window-long,
        // so 3-4 bolts are in flight at once with dark gaps between them, as the clip's f60/f84 show. No branch arcs
        // (extra.y 0): the clip's bolts are smooth, only their tails fray.
        static CelLayer Bolt(string name, float window, float start, int close, int mid, float width, float spread = 0.06f, float life = 0.42f, float zig = 0.5f, float forks = 0f)
        {
            return new CelLayer
            {
                Name = name, Kind = LightningBeamKind.KindName, Start = start, Duration = Hold - 0.45f - start, TimeJitter = 0.14f, Life = life, LifeJitter = 0.2f,   // c38: uneven moments (a train of shots, not a metronome)   // c31: the last arrives by Hold   // c12: the last bolt ARRIVES at Hold (clip: the hit is dead 7 frames after the last volley)
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(spread, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(width, width * 0.9f), SizeJitter = 0.5f, Stretch = window, Erode = new CelCurve(0.05f, 0.2f, 2f), Feather = 0.01f,   // c19: widths vary a lot
                Palette = "blue_laser_beam", Core = 1.2f, Opacity = 0.95f, Ink = 1f, Extra = new Vector4(zig, forks, -0.05f, 1.0f + window),
            };
        }

        /// <summary>b05: a volley bolt on its own stretch of the beam.</summary>
        static CelLayer VolleyBolt(string name, float head, float window, int close, float width)
        {
            return new CelLayer
            {
                Name = name, Kind = LightningBeamKind.KindName, Start = Volley, Duration = 0.12f, TimeJitter = 0.03f, Life = 0.28f, LifeJitter = 0.1f,   // c14: all arrive by Hold (0.38 s had bolts landing at f108)
                Close = close, Mid = 1, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(width, width * 0.9f), SizeJitter = 0.2f, Stretch = window, Erode = new CelCurve(0.05f, 0.2f, 2f), Feather = 0.01f,
                // c01: a travelling comet too (its head was parked at `head`, now unused); wider than the hold's bolts
                Palette = "blue_laser_beam", Core = 1.2f, Opacity = 0.95f, Ink = 1f, Extra = new Vector4(4.0f, 6f, -0.05f, 1.0f + window),   // c32: core 1.2
            };
        }

        /// <summary>c04: a bolt that runs on through the target (Ink is the window clip on this kind: 1.8 = 0.8 of the beam past it).</summary>
        static CelLayer Miss(CelLayer l) { l.Ink = 1.8f; l.Extra = new Vector4(l.Extra.x, 0f, -0.05f, 1.8f + l.Stretch); return l; }

        static CelLayer Dim(CelLayer l, float opacity) { l.Opacity = opacity; return l; }
        static CelLayer Jit(CelLayer l, float sizeJitter) { l.SizeJitter = sizeJitter; return l; }

        /// <summary>c19: a clump of long white spark streaks at one moment (the clip's sparks come in bursts, not a steady spray).</summary>
        static CelLayer SparkBurst(string name, float start, int close)
        {
            return new CelLayer
            {
                Name = name, Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0f, -0.12f), Start = start, Duration = 0.05f, TimeJitter = 0.02f, Life = 0.26f, LifeJitter = 0.3f,
                Close = close, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.15f, 0f, 0f), Aim = CelAim.Radial, Speed = 8f, SpeedJitter = 0.5f, UpBias = 0.3f, Gravity = 2.5f, Drag = 1.3f,
                Size = new CelCurve(0.034f, 0.014f), Stretch = 0.08f, Erode = new CelCurve(0.05f, 0.5f), Palette = "blue_laser_hit", Core = 2f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),   // c49: fatter, short again   // c48: longer   // c25: shorter dashes
            };
        }

        /// <summary>b02: a lane born off the axis (a wider spawn sphere), so a second, thinner bolt runs beside the main one.</summary>
        static CelLayer Side(CelLayer l, float spread = 0.6f) { l.ShapeSize = new Vector3(spread, 0f, 0f); l.Opacity = 0.9f; return l; }

        /// <summary>r04: a LightningBeam dot (Stretch is minus its minimum px at Far; 0.001 = none).</summary>
        static CelLayer Dot(string name, string palette, int mode, float start, float duration, float life, int close, int mid, CelCurve size, float soft, float pointsOrLump, float sharpOrCore)
        {
            return new CelLayer
            {
                Name = name, Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0.02f, -0.12f), Start = start, Duration = duration, TimeJitter = 0.02f, Life = life, LifeJitter = 0.25f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f, Size = size, SizeJitter = 0.2f, Stretch = -0.001f,
                Erode = new CelCurve(0f, 0.5f, 1.5f), Feather = 0f, Palette = palette, Core = 2.0f, Opacity = 1f, Extra = new Vector4(mode, soft, pointsOrLump, sharpOrCore),
            };
        }

        public static readonly CelFxRecipe Beam = new CelFxRecipe("blue_laser_beam", 5f,
            // b01 (beach stage): the frame's lower-left edge is at ~0.27 of the beam (the muzzle is 9 m from the eye, the
            // target 19 m; perspective packs the far half), so the lanes are laid on SCREEN like the clip's 2-3 long dashes:
            // a from off-screen (full at the frame edge) to ~0.38 of the way, b ~0.52-0.78, c ~0.86-1.0, with dark gaps
            // b02: the muzzle is lower (see Range), so the frame edge is at ~0.15 of the beam and the dummy's left edge at
            // ~0.9. Four broken bolts with gaps between, each lane's head jittered wide so the bolts land at new places
            // every 2-3 frames (b01's small jitter left them parked: a solid rod), plus a side lane near the corner
            // b03: the near dashes start from a wider sphere (0.2-0.3 m), so 2-3 offset bolts spread near the emitter and
            // converge on the dummy (they all aim at the chest); a second side lane
            // b04: longer, overlapping windows (b03's gaps left a gapped rod: iou 0.236) and 2-3 bolts side by side:
            // three side lanes from 0.18-0.3 m spheres (about 20-30 px apart near the frame edge), all converging on the chest
            // b05: 2-4 SEPARATE bolts with dark gaps (b04's overlapping windows read as one braided rod): four lanes on
            // fixed stretches of the beam (edge-0.40, 0.48-0.64, 0.74-0.88, 0.93-1.0) with small head jitter, so the gaps
            // (~0.04 of the beam, 30-80 px) stay dark; one side bolt beside the corner one (b05b: longer bolts, b05a lost hold area)
            // b06: about half as wide (b05 read as fat 20-30 px braided lenses): a thin white core in a cyan rim
            // b07: straight rods with pointed tips (shader), about 0.7x as wide, a little longer (b06b read as fat braided lenses)
            // c01: a stream of comet bolts on the axis (one every ~0.08 s, each 0.42 s muzzle -> chest, so 3-4 in flight
            // with dark gaps), a thinner parallel stream born 0.3 m beside it (the clip's f84 pair), and a sparse third
            Bolt("bolts", 0.36f, 0.00f, 6, 4, 0.26f, 0.12f, 0.55f, 4.0f, 10f),   // c47: longer (the clip's shots span ~0.4 of the corridor), one fewer to keep the gaps   // c46: fewer, a little slimmer (c45 covered 1.7x the clip's cyan px per frame)   // c45: a solid rod (1:1: the clip's plateau is 12-18 px, c44's body 8 px in a 15 px halo)   // c44: wider still (1:1: the clip's plateau is ~2x c43's)   // c43: a touch wider (blade profile)   // c39: fatter (the clip's shots are fat cyan spindles)   // c38: shorter (0.3 of the beam) so there are dark gaps between the shots   // c33: a little slimmer and shorter, born further apart   // c32: longer and wider (1:1 against the clip's f27: ours were ~8 px to its ~14)   // c31: slower (measured: c30's crossed the frame in 3 frames, the clip's in 5-6), fewer   // c30: long spindles (0.42 of the beam), wider, crossing in 0.3 s (~7 frames: the clip's f17-f23), one at a time with a second beside   // c26: a little wider   // c21: between c19 (one strand) and c20 (empty corridor)   // c19: base window 0.34, seed-scaled 0.45..1.4x in the shader   // c13: fewer, thinner (c12 merged into a band)   // c09: bigger kinks, more forks   // c08: thin crooked strands, half as wide, 1.5x longer, forks (c07 fat tubes)   // c05: thinner, straighter (c04 fat neon, bent)   // c04: wider, more wave   // c02: 40 -> 16, wider (c01 merged into one thin rod)
            Side(Bolt("bolts side", 0.28f, 0.1f, 3, 2, 0.15f, 0.06f, 0.52f, 4.0f, 10f), 0.45f),   // c47: longer   // c46: fewer   // c45/c44: wider   // c33: a clear lane beside the main one
            Side(Bolt("bolts side 2", 0.18f, 0.3f, 3, 0, 0.06f, 0.06f, 0.5f, 4.0f, 4f), 0.28f),
            // c04: a few bolts MISS and fly on past the dummy (clip f84/f101: streaks to the upper right of the board)
            Miss(Bolt("misses", 0.3f, 1.2f, 4, 2, 0.06f, 0.2f, 0.55f, 3.0f, 4f)),
            // d30: small blue-white spark ticks scattered along and beside the beam (clip f30/f64/f81); c01: fewer, parked
            Side(new CelLayer
            {
                Name = "beam specks", Kind = LightningBeamKind.KindName, Start = 0.3f, Duration = Hold - 0.3f, TimeJitter = 0.03f, Life = 0.12f, LifeJitter = 0.3f,
                Close = 60, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.7f, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(0.035f, 0.03f), SizeJitter = 0.25f, Stretch = 0.012f,   // c03: dots, not dashes Erode = new CelCurve(0.06f, 0.3f, 2f), Feather = 0.42f,
                Palette = "blue_laser_beam", Core = 0.85f, Opacity = 0.95f, Ink = 1f, Extra = new Vector4(0.3f, 0f, 0.55f, 0.57f),
            }, 0.7f),
            // b05: the last volley as two wide bolts with a gap (clip f101: one from the corner, one into the dummy)
            VolleyBolt("volley a", 0.45f, 0.36f, 1, 0.1f),
            VolleyBolt("volley b", 0.97f, 0.3f, 1, 0.09f),   // c33: one each (four at once merged into a rope at f98)   // c32: a little thinner (c31's f98 was a white rope)   // c29: shorter, fewer (c28 read as one rope corner -> board)   // c28: the clip's f100 volley bolt is bright but still thin   // c26: fatter (the clip's f101 volley is the biggest event)
            // c26: the volley's own white flash on the board (clip f101-f103)
            Dim(Dot("volley flash", "blue_laser_bloom", 1, Volley + 0.22f, 0f, 0.12f, 1, 1, new CelCurve(0.8f, 0.9f), 2.6f, 0f, 0.2f), 0.7f),   // c28: smaller (f104 read as a plain white disc)
            new CelLayer
            {
                // a dash that carries on past the dummy, up-right along the beam (clip f89-f101)
                Name = "through", Kind = LightningBeamKind.KindName, Start = 2.6f, Duration = 0.04f,   /* d16: one short burst (d14 f81-f107, d15 f89-f115: records live 0.16 s + jitter) */   // d12: only at the last volley (A/B x4: no beam past the dummy before ~f98) TimeJitter = 0.02f, Life = 0.12f, LifeJitter = 0.1f,
                Close = 14, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.03f, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                // b02: longer and further (the clip's through-bolts run on to ~(1250, 250) px)
                Size = new CelCurve(0.06f, 0.05f), SizeJitter = 0.2f, Stretch = 0.2f, Erode = new CelCurve(0.05f, 0.2f), Feather = 0.05f,
                Palette = "blue_laser_beam", Core = 1.5f, Opacity = 0.95f, Ink = 1.9f, Extra = new Vector4(0.4f, 0f, 1.0f, 1.9f),   // c01: a comet that flies on past the dummy
            },
            new CelLayer
            {
                // hidden at Close by the shader: the line that stands for the dashes at Mid and Far
                Name = "thread", Kind = LightningBeamKind.KindName, Start = Ramp * 0.5f, Life = Hold - Ramp * 0.5f + 0.25f, LifeJitter = 0f,
                // b01: none at Close (the beach shot's CloseUp is 0.91, so the shader's fade let it fill the dash gaps)
                Close = 0, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                Size = new CelCurve(0.06f, 0.05f), SizeJitter = 0f, Stretch = 1f, Erode = new CelCurve(0.05f, 0.1f), Feather = 0f,
                Palette = "blue_laser_beam", Core = 1.3f, Opacity = 0.7f, Ink = 1f, Extra = new Vector4(0.1f, 0f, 1f, 1f),
            },
            new CelLayer
            {
                // the impact dot: the star's white core up close; at Far kept 4 px in radius and lit through the fog
                Name = "dot", Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.45f), Start = 0.15f, Life = Hold + 0.15f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f,
                Size = new CelCurve(0.04f, 0.05f), SizeJitter = 0f, Stretch = -4f, Erode = new CelCurve(0.05f, 0.12f), Feather = 0f,
                Palette = "blue_laser_hit", Core = 1f, Opacity = 0.85f, Extra = new Vector4(0f, 0f, 4f, 3f),
            },
            // r04: the hit is a white-hot round core about the torso's width in a soft pale-blue bloom (no bands, no
            // bullseye): a steady soft glow under 30 flickering blooms with cores
            // b01: smaller and dimmer (r04's 0.7 m glow + 0.5 m blooms with a 0.2 m core read as a moon over the dummy):
            // a core about half a torso wide; the spiky white burst is the "rays" layer (dot mode 4)
            // b02: a white-blue glow ball about as wide as the torso (clip: ~75 x 90 px over the board): a wider, brighter
            // steady glow and larger blooms with bigger white cores
            // b04: a wider, flatter glow (1.35 m, falloff 1.9) so the pale-blue light covers the whole torso (peak area 0.72)
            // b06: a larger white-blue ball about the torso wide (b05's white-yellow ray star was smaller than the clip's glow)
            Dim(Dot("glow", "blue_laser_bloom", 1, 0.15f, 0f, Hold + 0.1f, 1, 1, new CelCurve(0.26f, 0.3f), 2.4f, 0f, 0.3f), 0.45f),   // c50: a bigger white-hot centre (the clip's core is a white blob a third of the board wide)   // c49: a little less (it buried the arcs)   // c48: hotter, a little bigger (owner: the impact stronger)   // c36: smaller still   // c35: smaller (c34 read as a ball bigger than the clip's)   // c22: a small hot centre (the clip), not a board-wide ball   // c21: smaller, dimmer; the board wash carries the hit
            // c21: the lit board: a soft rounded rectangle of light over the board (dot mode 5; z/w half extents in quad units)
            Dim(Dot("board wash", "blue_laser_bloom", 5, 0.15f, 0f, Hold + 0.05f, 1, 1, new CelCurve(0.78f, 0.78f), 0.12f, 0.72f, 0.8f), 0.5f),   // c36: the board itself carries more of the light
            Dim(Jit(Dot("board pulse", "blue_laser_bloom", 5, 0.2f, Hold - 0.1f, 0.08f, 36, 10, new CelCurve(0.78f, 0.8f), 0.12f, 0.72f, 0.8f), 0.15f), 0.4f),   // c19: the steady part lower, the pulses carry it   // c09: less dominant (clip: a lit board, not a ball)   // c01: a wider white point
            // c01: a banded aqua disc about the board's width on the chest (the clip's f60/f84 show a cel ring round the hit)
            Dim(Jit(Dot("rings", "blue_laser_hit", 6, 0.2f, Hold - 0.2f, 0.1f, 30, 6, new CelCurve(0.5f, 0.52f), 0.06f, 0.78f, 0.42f), 0.1f), 0.15f),   // c42: fainter still   // c36: faint (the rings made the hit a target icon)   // c22: the bullseye glows as two thin rings (dot mode 6), not a filled disc   // c11: the ring reads a bit more   // c02: smaller, fainter   // d55 (d51): 0.9 -> 0.75 so the board shows through   // d03: white core halved (hot 0.042 vs clip 0.025)   // b07: 1x torso (b06b: 2x the dummy)
            Dim(Jit(Dot("bloom", "blue_laser_bloom", 1, 0.15f, Hold, 0.09f, 50, 14, new CelCurve(0.22f, 0.36f, 0.6f), 2.6f, 0f, 0.14f), 0.55f), 0.85f),   // c49: a little less   // c48: more, bigger, full (the clip's core flickers white)   // c36: smaller   // c35: smaller pulses   // c23: smaller still   // c19: short, uneven pulses (clip: the hit flickers)
            // c19: a few big white flashes through the hold (clip f88: the whole board goes white for a frame or two)
            Dim(Jit(Dot("flash", "blue_laser_bloom", 1, 0.6f, Hold - 0.9f, 0.07f, 10, 3, new CelCurve(1.0f, 1.15f), 3.0f, 0f, 0.14f), 0.3f), 0.85f),   // c48: more, brighter   // c20: soft, the board shows through (c19 was a solid white ball)
            Dim(Dot("glow late", "blue_laser_bloom", 1, 1.85f, 0f, Hold - 1.7f, 1, 0, new CelCurve(0.34f, 0.38f), 2.4f, 0f, 0.2f), 0.35f),   // c35: smaller   // c23: small (it was the board-wide white ball at f84)   // d27: the clip keeps its white ball bright f81-f101 (15-17% px over lum 180; ours fell to 5-6% after f64)
            // b01: 6-10 thin white spike rays of uneven length out to 1-1.5 torso widths, re-drawn every 2-3 frames at a new turn
            // b03: a torso-wide white-core, blue-rim star on the chest: 10 rays, larger, plus a burst at the peak and a
            // faint crackle on the board after the beam
            Dim(Dot("rays", "blue_laser_hit", 4, 0.15f, Hold - 0.25f, 0.1f, 12, 8, new CelCurve(0.32f, 0.36f), 0.02f, 7f, 0.04f), 0.1f),   // c49: faint again (c48 read as a lens-flare star, not electricity)   // c48: back (the clip's hit has white spikes radiating at f40-f60)   // c36: nearly gone   // c25: faint (the clip has scattered dashes, not a ray star)   // d03: short dim rays (every paper: a lens flare)   // b07: 8-12 thin radial streaks over the ball   // b06: thinner, dimmer rays
            Dim(Dot("burst rays", "blue_laser_hit", 4, Volley, 0.12f, 0.1f, 5, 2, new CelCurve(0.7f, 0.8f), 0.02f, 9f, 0.05f), 0.25f),   // c31: fainter, over by f104 (the clip has no ray star then)   // c08: short (clip: the star is gone by f108)   // d35: a bright spiky star f98-f106 (readers: clip impact 29%/16% at f98/f106, ours 11%/3%)   // b06: 8-12 long thin radial streaks
            // b04: the crackle is short kinked lightning arcs clinging to the torso (clip f106-f132), not b03's dim ray stars
            // c30: the aftershock (clip f104-f130): a dense cluster of short THICK white-cored scribbles on the bullseye,
            // redrawn every frame, about a third of the board wide, shrinking and thinning over 1.2 s
            CrackleArc("cling", Hold + 0.02f, 0.75f, 130, 0.14f, 1.0f, 0.4f),   // c41: a touch bigger   // c35: bigger, thicker   // c33: wider cluster (the clip's covers half the board)   // c32: over the whole bullseye   // c31: thicker, more (c30's were thin and dim under a cyan blob)
            CrackleArc("cling late", Hold + 0.6f, 0.65f, 60, 0.1f, 0.95f, 0.22f),
            CrackleArc("crackle", Hold + 0.3f, 1.15f, 30, 0.045f, 0.9f),
            CrackleArc("crackle hold", 0.3f, Hold - 0.3f, 200, 0.12f, 1.0f, 0.55f),   // c50: fewer now that they are crooked   // c49: thicker, wider (c48's were thin under the bloom)   // c48: 4x the arcs, thicker, wider (owner: "more electricity" at the hit)   // c12: more and wider (the clip's arcs on the board read)
            CrackleArc("splash", 0.3f, Hold - 0.3f, 150, 0.09f, 0.95f, 0.9f, 1.3f),   // c49: wider, more, CYAN (core 1.3) so they read as arcs, not white hairs   // c48: thin tendrils splashing out to ~a board width round the hit (clip f40-f90)
            Dim(Dot("afterglow", "blue_laser_bloom", 1, Hold + 0.05f, 0f, 1.3f, 1, 1, new CelCurve(0.3f, 0.18f), 5f, 0f, 0.1f), 0.2f),   // c32: fainter still   // c31: faint (c30's read as a cyan blob)   // c30: a wider cyan haze under the clinging scribbles
            new CelLayer
            {
                Name = "star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0f, -0.12f), Start = 0.2f, Duration = Hold - 0.4f, TimeJitter = 0.03f, Life = 0.1f, LifeJitter = 0.2f,
                Close = 20, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.05f, 0f, 0f), Size = new CelCurve(0.18f, 0.26f, 0.5f), SizeJitter = 0.25f,   // c48: more, bigger   // c37: fewer, smaller star glows
                Erode = new CelCurve(0.1f, 0.7f, 1.5f), Spin = 3f, Palette = "blue_laser_hit", Core = 1.2f, Opacity = 0.45f, Extra = new Vector4(2f, 1.6f, 10f, 14f),   // c48: 0.45; Extra restored (c09's comment had swallowed it since)   // c09: 0.5 -> 0.3
            },
            new CelLayer
            {
                // blue-white sparkles left on the board after the beam (clip f105-f125)
                Name = "sparkles", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, -0.05f, -0.12f), Start = Hold + 0.1f, Duration = 1.05f, TimeJitter = 0.03f, Life = 0.16f, LifeJitter = 0.3f,
                Close = 14, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.2f, 0f, 0f), Size = new CelCurve(0.06f, 0.035f), SizeJitter = 0.4f,   // c30: fewer (the scribbles carry the aftershock)
                Erode = new CelCurve(0.05f, 0.6f), Spin = 2f, Palette = "blue_laser_hit", Core = 2f, Opacity = 0.95f, Extra = new Vector4(2f, 1.4f, 4f, 8f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0f, -0.12f), Start = 0.2f, Duration = Hold - 0.35f, Life = 0.26f,   // c11: over sooner (clip f108: a few warm sparks only)   // d03: born outside the ball, bigger, longer: a visible spray
                Close = 80, Mid = 20, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Radial, Speed = 7.0f, SpeedJitter = 0.5f, UpBias = 0.4f, Gravity = 3f, Drag = 1.6f,   // c48: more, faster; jitter/gravity/drag restored (c01's comment had swallowed them since)   // c36: fewer   // c01: 170 -> 100
                Size = new CelCurve(0.07f, 0.035f), Stretch = 0.04f, /* c50: fatter still; c49: short FAT dashes (c48's long hairs read as a star); c48: long white streaks (the clip's fly out to half a board); c37: short dashes; d12: half the streak length */ Erode = new CelCurve(0.05f, 0.5f), Palette = "blue_laser_hit", Core = 1.8f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                // d42/d50/d55: a dense spray of tiny white spark ticks round the hit through the hold (clip f30-f89: dozens at
                // once, within about one board width); with the glow thinner the board shows through as in the clip
                Name = "spray", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0f, -0.2f), Start = 0.2f, Duration = Hold - 0.1f, TimeJitter = 0.02f, Life = 0.16f, LifeJitter = 0.4f,
                Close = 160, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Radial, Speed = 5f, SpeedJitter = 0.6f, UpBias = 0.1f, Gravity = 1f, Drag = 2f,   // c49: fewer   // c48: more, longer; jitter/gravity/drag restored (c01's comment had swallowed it)   // c36: far fewer   // c01: 1200 -> 340
                Size = new CelCurve(0.035f, 0.015f), Stretch = 0.05f, Erode = new CelCurve(0.05f, 0.5f), Palette = "white", Core = 2f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),   // c49: short again   // c36: short dashes   // c21: longer hairs
            },
            SparkBurst("spark burst 1", 0.75f, 20), SparkBurst("spark burst 2", 1.45f, 24), SparkBurst("spark burst 3", 2.05f, 20), SparkBurst("spark burst 4", 2.6f, 24),   // c48: twice the bursts
            new CelLayer
            {
                // the peak's burst: a ring of long white spark lines (clip f101-f106)
                Name = "burst sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0f, -0.12f), Start = Volley + 0.15f, Duration = 0.2f, Life = 0.2f, LifeJitter = 0.2f,   // c10: shorter, fewer (clip: gone by f108)
                Close = 6, Mid = 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = CelAim.Radial, Speed = 7f, SpeedJitter = 0.3f, UpBias = 0.2f, Gravity = 2f, Drag = 1.2f,
                // b05: 8-12 thin radial white-blue streaks (clip f101 upper left), not a ring of dots
                Size = new CelCurve(0.02f, 0.01f), Stretch = 0.12f, Erode = new CelCurve(0.05f, 0.5f), Palette = "blue_laser_hit", Core = 1.8f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                // r04: orange spark streaks thrown 0.5-1 torso widths out (more, longer and brighter than r03)
                Name = "warm sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0.1f, -0.15f), Start = 0.3f, Duration = Hold - 0.4f, Life = 0.4f,
                // b04: fewer, bigger, flying up and out past the glow (b03's were hidden in the white burst)
                Close = 22, Mid = 8, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Radial, Speed = 4.6f, SpeedJitter = 0.4f, UpBias = 0.9f, Gravity = 4f, Drag = 1.4f,   // c37: fewer
                Size = new CelCurve(0.05f, 0.03f), Stretch = 0.07f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 2f, Extra = new Vector4(0.2f, 0.05f, 0.2f, 0f),
            },
            new CelLayer
            {
                // d12: blue-white cooling sparks (A/B: the clip's are blue-white with a few warm)
                Name = "cool sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0.1f, -0.15f), Start = 0.3f, Duration = Hold - 0.6f, Life = 0.4f,
                // b04: fewer, bigger, flying up and out past the glow (b03's were hidden in the white burst)
                Close = 6, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Radial, Speed = 4.6f, SpeedJitter = 0.4f, UpBias = 0.9f, Gravity = 4f, Drag = 1.4f,   // c37: fewer
                Size = new CelCurve(0.05f, 0.03f), Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f), Palette = "blue_laser_hit", Core = 2f, Extra = new Vector4(0.2f, 0.05f, 0.2f, 0f),
            },
            new CelLayer
            {
                // b05: 4-8 orange sparks flying up and out of the impact at the peak (clip f101), born outside the glow's core
                Name = "warm burst", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0.35f, -0.15f), Start = Volley + 0.12f, Duration = 0.35f, Life = 0.5f, LifeJitter = 0.2f,
                Close = 7, Mid = 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.45f, 0f, 0f), Aim = CelAim.Radial, Speed = 5.2f, SpeedJitter = 0.35f, UpBias = 1.5f, Gravity = 3f, Drag = 1.1f,
                Size = new CelCurve(0.075f, 0.045f), Stretch = 0.11f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 3f, Extra = new Vector4(0.2f, 0.05f, 0.2f, 0f),
            });

        public static readonly CelFxRecipe Ground = new CelFxRecipe("blue_laser_ground", 4f,
            new CelLayer
            {
                // r04: a soft bright cyan pool ~1.5 torso widths across (no bands, no hard rim), a LightningBeam dot on the ground
                Name = "pool", Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.05f, 0.02f, -0.8f),   /* d54: toward the shooter (clip ring band y460-520 px, ours y420-500) */ Start = 0.25f, Life = Hold, LifeJitter = 0f,   // d41: ends 0.1 s sooner (clip ring off at f106)
                // b02: a soft pale-blue ellipse about 1.5x the dummy's width (1.2 m), softer and paler than b01's puddle
                Close = 1, Mid = 1, Far = 0, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f, Size = new CelCurve(2.0f, 2.1f), SizeJitter = 0f, Stretch = -0.001f,   // c35: smaller pool
                // b03: feathered (falloff 3.2: b02's 1.5 was a flat cone with a hard rim)
                Erode = new CelCurve(0f, 0.1f), Feather = 0f, Palette = "blue_laser_pool", Core = 1f, Opacity = 0.22f, /* c36: 0.22; c35: 0.32; c31: subtler; d41: between d27 0.85 (too blue) and d29 0.62 (critics: faint) */ Extra = new Vector4(2f, 4.2f, 0f, 0f),   // b07: 2.4 m, falloff 4.2 (b06b read as a slab)   // b06: 2.9 m, falloff 3.2 (b05 read as a hard-edged disc)   // b05: 2.2 m, falloff 1.2 (a feathered halo)   // b04: wider, softer halo (1.5x the dummy)
            },
            new CelLayer
            {
                // b03: a soft blue halo standing at the feet (facing the eye), so the glow feathers up the legs
                Name = "foot halo", Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.5f, 0f), Start = 0.3f, Life = Hold, LifeJitter = 0f,
                // b07: smaller, softer and raised (b06b: its bottom cut by the ground drew a hard-edged slab under the feet)
                Close = 1, Mid = 1, Far = 0, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f, Size = new CelCurve(1.0f, 1.05f), SizeJitter = 0f, Stretch = -0.001f,
                Erode = new CelCurve(0f, 0.1f), Feather = 0f, Palette = "blue_laser_pool", Core = 1f, Opacity = 0.15f, Extra = new Vector4(1f, 5f, 0f, 0f),   // d54: dimmer (clip: dark gap between the feet glow and the ring at y440-460 px)
            });

        /// <summary>The dust trail, played along the trail (see Play): four stretches born near to far, then the flecks.</summary>
        public static readonly CelFxRecipe Dust = new CelFxRecipe("blue_laser_dust", 3f,
            // r04: 20 soft unlined tan puffs (LightningBeam puff dots that cover, each under a third of the dummy's height
            // on screen) rising up-right along the trail from mid-dummy to ~3 dummy heights past it, then thinning out
            // b01: four short gappy clumps (r04's long stretches merged into one column), each puff at most ~1/3 dummy height
            // b02: one joined trail (b01's four round clumps read as separate puffs): longer overlapping stretches with
            // more puffs, born earlier (by clip f89) and living longer so the tail f101-f136 holds its area
            // b03: translucent soft-edged lumps (opacity 0.4, edge 0.45) pooling round and just behind the dummy (a wide,
            // low clump), then a shorter, wider trail (b02's rope ran 1.5x the clip's length)
            // b04: born small and growing (b03's was full length at f89), pooling round and behind the dummy, the trail at
            // most ~3 dummy widths right (b03's mid/far stretches ran on to 6 m)
            // b04b: larger and denser (b04a's tail area was ~25% of the clip's)
            // b05: 5 separate translucent grey-tan lumpy clumps trailing up-right (clip f98-f115), short boxes with gaps
            // between them, out to ~3 dummy widths; a smaller low pool round the dummy
            // b06: 4-6 separate puffs (b05's 13 per clump merged into one opaque streak): 2-3 big lumpy puffs per clump, translucent,
            // living longer so the tail f101-f136 holds its area
            // b07: one big puff (two at the dummy) per clump on clumps 1.2 m apart, born big so the early tail is full, and
            // shorter lived (b06b ran 9 frames late and its 3-4 puffs per clump merged into one streak)
            Wide(DustStretch("dust pool", 1.3f, 1.2f, DustAt + 0.02f, 22, 8, new CelCurve(0.6f, 1.3f, 0.4f), 2.1f)),   // c44: a fifth bigger (c43 covered 45% fewer px than the clip)   // c43: twice the lobes at 2/3 the size (the clip's cloud is many small ragged lobes)   // c40: a tenth bigger   // c39: starts right of the dummy (c38 covered its body)   // c38: many smaller solid lobes (the clip's cloud is a cartoon cumulus, not a smear)   // c34: shorter lived (c33 ended 12 frames after the clip)   // c31: a little further along (the clip's cloud sits right of the dummy)   // c30: more, bigger   // c17: bigger, further (clip f101-f120: ~2x our reach)   // d05: behind-right of the dummy (d02b)
            DustStretch("dust 1", 1.4f, 0.6f, DustAt + 0.06f, 9, 3, new CelCurve(0.7f, 1.5f, 0.4f), 2.1f),   // c44: a fifth bigger   // c43: all the stretches: ~2x the lobes at ~2/3 the size
            DustStretch("dust 2", 2.5f, 0.6f, DustAt + 0.12f, 9, 3, new CelCurve(0.7f, 1.5f, 0.4f), 2.05f),
            DustStretch("dust 3", 3.8f, 0.6f, DustAt + 0.18f, 8, 3, new CelCurve(0.65f, 1.35f, 0.4f), 2.0f),
            DustStretch("dust 4", 5.2f, 0.6f, DustAt + 0.24f, 7, 2, new CelCurve(0.6f, 1.25f, 0.4f), 1.95f),
            DustStretch("dust 5", 6.6f, 0.5f, DustAt + 0.3f, 5, 1, new CelCurve(0.48f, 0.95f, 0.4f), 1.9f),
            DustStretch("dust 6", 8.0f, 0.5f, DustAt + 0.36f, 4, 0, new CelCurve(0.38f, 0.78f, 0.4f), 1.8f),
            new CelLayer
            {
                // loose separate black flecks: small hard-edged puff dots spread wider than the dust, drifting with it
                Name = "flecks", Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.1f, 3.4f), Start = DustAt + 0.05f, Duration = 0.7f, Life = 1.3f, LifeJitter = 0.35f,   // c34: shorter
                // b02: dense specks (clip ink_peak 0.053 vs b01 0.015): twice as many, larger
                Close = 60, Mid = 10, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(2.6f, 1.3f, 6.5f),   // c45: fewer (c44's were a denser pepper than the clip's)   // c41: more (the clip has black bits of every size all through the cloud)   // c30: many more (the clip has dozens of black chunks of every size)   // c17: more flecks over the whole cloud (clip f101-f120)   // d23: black flecks 64 -> 24 (every A/B: heavy black chunks, clip has few)   // d05: x2.5 dark specks (ink 0.009 vs clip 0.053) Aim = CelAim.Dir, Speed = 1.0f, SpeedJitter = 0.5f, Spread = 35f, Drag = 0.8f, Gravity = 0.4f,
                Size = new CelCurve(0.034f, 0.02f), SizeJitter = 0.8f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.6f, 3f), Feather = 0f,   // c43: smaller bits   // c41: sizes vary more
                Palette = "blue_laser_fleck", Core = 1f, Opacity = 1f, Extra = new Vector4(3f, 0.04f, 0.3f, 0f),   // c43: rounder bits (0.5 still drew triangles)   // c42: lump 0.5 (0.8 drew little black arrowheads)
            });

        /// <summary>b03: a dust stretch spread wide and low round the dummy.</summary>
        static CelLayer Wide(CelLayer l) { l.ShapeSize = new Vector3(2.3f, 0.9f, l.ShapeSize.z); l.Offset = new Vector3(0f, -0.4f, l.Offset.z); return l; }   // d05: taller (d02b)

        /// <summary>b04: short kinked lightning arcs on the board: dashes born on a 0.42 m sphere round the chest, aimed at it.</summary>
        static CelLayer CrackleArc(string name, float start, float duration, int close, float width, float opacity, float spread = 0.42f, float core = 3.0f)   // c49: core
        {
            return new CelLayer
            {
                Name = name, Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.2f, 0.05f, -0.12f), Start = start, Duration = duration, TimeJitter = 0.03f, Life = 0.12f, LifeJitter = 0.3f,   // c35: 0.12
                Close = close, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(spread, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(width, width * 0.7f), SizeJitter = 0.3f, Stretch = 0.95f, Erode = new CelCurve(0.03f, 0.2f, 2f), Feather = 0f,
                Palette = "blue_laser_beam", Core = core, Opacity = opacity, Ink = 1f, Extra = new Vector4(3.0f, 4f, 0.97f, 1.0f),   // c49: core per layer   // c41: 3.0 (whiter)   // c31: white-hot   // c12: crooked like the bolts
            };
        }

        static CelLayer DustStretch(string name, float along, float length, float start, int close, int mid, CelCurve size, float life)
        {
            return new CelLayer
            {
                Name = name, Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.3f, along), Start = start, Duration = 0.22f, TimeJitter = 0.05f, Life = life, LifeJitter = 0.1f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(1.8f, 1.0f, length), Aim = CelAim.Dir, Speed = 0.6f,   // c38: a tighter mass   // c18: a broad cloud, not a rope   // d09/d12: scattered up/down and sideways SpeedJitter = 0.4f, Spread = 20f, Drag = 1f, Buoyancy = 0.05f,
                Size = size, SizeJitter = 0.45f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.95f, 1.5f), Feather = 0f,   // c43: sizes vary more   // c34: thins sooner
                Palette = "blue_laser_dust", Core = 1f, Opacity = 0.72f, Extra = new Vector4(3f, 0.35f, 0.55f, 0.3f),   // c44: a little denser   // c43: thinner, a softer ragged rim (w 0.3), dissolves   // c39: a little thinner   // c38: solid lobes with a firm edge   // c34: a touch thinner   // c31: translucent again   // c30: opaque, crisper lobed edge (the clip's cloud is a solid billowing mass)   // c16: thinner, softer edge   // b06: alpha 0.6, lumpier
            };
        }

        /// <summary>Fire it: from the shooter to the target, the ground at the target's foot. LAB ONLY.</summary>
        public static void Play(Vector3 from, Vector3 to, Vector3 ground, uint seed)
        {
            Vector3 dir = to - from;
            CelFx.Play(Beam, from, dir, to, seed);
            Vector3 level = new Vector3(dir.x, 0f, dir.z);
            if (level.sqrMagnitude < 1e-6f) level = Vector3.forward;
            level.Normalize();
            CelFx.Play(Ground, ground, level, ground, seed + 1u);
            // the dust blows on past the target, along the beam's way
            Vector3 right = Vector3.Cross(Vector3.up, level);
            // r04: 20 degrees right and rising 0.2 (the clip's trail runs from mid-dummy up-right to ~3 dummy heights past it)
            // b01: rising 0.28 (beach stage: the clip's trail climbs to ~(1250, 250) px)
            // b03: rising 0.2 (the clip's dust pools low round the dummy)
            // b04: rising 0.12 (pooling, not a rope)
            // b05: rising 0.26 (the clip's trail at f98-f106 climbs up-right along the beam's line past the dummy)
            // b07: rising 0.36 (the clip's puffs at f98-f115 climb up-right to ~3 dummy widths)
            Vector3 trail = (level * 0.94f + right * 0.342f + Vector3.up * 0.16f).normalized;   // c18: lower (c17 climbed off the frame; the clip's cloud stays about board height)
            CelFx.Play(Dust, to, trail, to, seed + 2u);
        }

        /// <summary>The Range shot: from the low muzzle (the clip's beam enters at the lower-left corner) to the chest.</summary>
        [CelRangeEffect("blue_laser")]
        static void Range(RangeShot s)
        {
            float keep = CelFx.ZoomOverride;
            CelFx.ZoomOverride = s.Zoom;
            // b02: the clip's beam is steeper on screen (it meets the bottom edge at x ~105 of 1344, slope -0.53): the
            // muzzle sits 0.73 m above the dummy's ground instead of 1.3 m (projected: bottom edge at x ~124, slope -0.52)
            Vector3 from = s.Muzzle;
            from.y = s.Ground.y + 0.73f;
            try { Play(from, s.Target, s.Ground, (uint)s.Seed); }
            finally { CelFx.ZoomOverride = keep; }
        }
    }
}
