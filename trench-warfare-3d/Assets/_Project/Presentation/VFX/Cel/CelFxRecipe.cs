// Phase: CelFx kit (2026-11-09) - an effect as data: a list of layers, each one primitive kind spawned on a timeline,
// with its counts per LOD tier, its spawn shape, its motion, and its size, erosion and colour over the life.
// CelFx.Play(recipe, origin, dir, target, seed) spawns every layer at once (records born later wait on the GPU), so an
// effect costs CPU once, when it is played, and nothing a frame after. README.md beside this file is the guide.
//
// Recipes are plain [Serializable] classes: write them in C# (a static readonly field, see CelDemo.cs) or load them
// with JsonUtility. A layer names its kind and palette by string; the ids are looked up on first play and cached.
using System;
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    /// <summary>Where a layer's records start, in the effect's frame (right, up, forward = dir).</summary>
    public enum CelShape : byte
    {
        Point,      // at the anchor (+ Offset)
        Sphere,     // anywhere inside a sphere of ShapeSize.x
        Shell,      // on the surface of that sphere
        Disc,       // on a level disc of ShapeSize.x (a ring of radius x when ShapeSize.y > 0: between y and x)
        Box,        // inside a box of ShapeSize (right, up, forward)
        Line,       // spread along origin -> target (a beam's length, a trail)
    }

    /// <summary>Which way a layer's records fly.</summary>
    public enum CelAim : byte
    {
        None,       // they stay where they start (Speed ignored)
        Dir,        // along the effect's dir, within Spread degrees
        Radial,     // outward from the anchor (UpBias lifts them), Spread unused
        Target,     // from where they start to the target: Speed 0 = arrive exactly at the end of their life x Arrive
        Up,         // straight up, within Spread degrees
        Back,       // back along -dir (the splash off a hit, toward the shooter), within Spread degrees
    }

    /// <summary>Where a layer is anchored.</summary>
    public enum CelAnchor : byte { Origin, Target }

    [Serializable]
    public sealed class CelLayer
    {
        public string Name = "";
        /// <summary>The primitive: "Streak", "Lobe", "Glow", an added kind's Name, or "Chunks" (DebrisRenderer) / "Light" (NightLights).</summary>
        public string Kind = CelKinds.LobeName;
        // ---------------------------------------------------------------- timeline
        /// <summary>Seconds after Play the first record appears.</summary>
        public float Start;
        /// <summary>The records' births are spread evenly over this many seconds (0: all at Start).</summary>
        public float Duration;
        /// <summary>Each birth moves by up to +- this (s).</summary>
        public float TimeJitter;
        /// <summary>Seconds each record lives, x (1 +- LifeJitter).</summary>
        public float Life = 1f, LifeJitter = 0.2f;
        // ---------------------------------------------------------------- counts per tier (never rising going out)
        /// <summary>Records at Close (zoom T3/T2, 16 m and in), Mid (T1, 30 m) and Far (O120 and out); blended between.</summary>
        public int Close = 1, Mid = 1, Far = 0;
        // ---------------------------------------------------------------- where and which way
        public CelAnchor Anchor = CelAnchor.Origin;
        /// <summary>Added to the anchor in the effect's frame: x right, y up, z along dir.</summary>
        public Vector3 Offset;
        public CelShape Shape = CelShape.Point;
        public Vector3 ShapeSize = Vector3.one;
        public CelAim Aim = CelAim.None;
        public float Speed, SpeedJitter = 0.25f;
        /// <summary>Cone half-angle (degrees) for Dir, Up and Back.</summary>
        public float Spread = 15f;
        /// <summary>Radial: how much the outward direction leans up (0: a sphere, 1: a fountain).</summary>
        public float UpBias;
        /// <summary>Target: the share of the life at which it arrives (1: at its death; 0.5: halfway, then it carries on).</summary>
        public float Arrive = 1f;
        public float Gravity, Drag, Buoyancy;
        /// <summary>Spin (rad/s; a lobe's boil, a glow's turn), x (1 +- SizeJitter) like the size.</summary>
        public float Spin;
        // ---------------------------------------------------------------- look
        /// <summary>Size (m) over the life: a streak's width, a lobe's or a glow's radius.</summary>
        public CelCurve Size = new CelCurve(1f, 1f, 1f);
        public float SizeJitter = 0.2f;
        /// <summary>Size.w: a streak's trail (seconds of its speed), a lobe's vertical squash (1 round).</summary>
        public float Stretch;
        /// <summary>The erosion threshold over the life (0 whole .. 1 gone) and the feather of its edge.</summary>
        public CelCurve Erode = new CelCurve(0f, 0.6f, 2f);
        public float Feather = 0.08f;
        /// <summary>The palette by name (CelPalette), and the one it cools into over its life ("" = none) from CoolFrom on.</summary>
        public string Palette = "white", CoolTo = "";
        public float CoolFrom = 0.35f;
        /// <summary>The HDR multiplier of the core band (1: white, 3-6 blooms), opacity (additive kinds), ink width (px at Close).</summary>
        public float Core = 2f, Opacity = 1f, Ink = 2f;
        /// <summary>The kind's own parameters (see its shader's header): Lobe (lump freq, lump amp, light up-bias, -),
        /// Glow (mode, falloff/ring width, star points, sharpness), Streak (tail taper, min length, flicker, -),
        /// Chunks (DebrisRenderer.Piece, burn 0..1, -, -), Light (reach m, -, -, -).</summary>
        public Vector4 Extra;

        [NonSerialized] internal int kindId = int.MinValue, paletteId = -1, coolId = -1;

        /// <summary>The count at a LOD blend (0 Close, 1 Mid, 2 Far): linear between the tiers, then the quality's share.
        /// Never more going out as long as Close >= Mid >= Far (Validate holds the recipe to it).</summary>
        public int CountAt(float lod, float quality)
        {
            lod = Mathf.Clamp(lod, 0f, 2f);
            float n = lod <= 1f ? Mathf.Lerp(Close, Mid, lod) : Mathf.Lerp(Mid, Far, lod - 1f);
            if (n <= 0f) return 0;
            return Mathf.Max(1, Mathf.FloorToInt(n * Mathf.Max(0f, quality) + 0.5f));
        }
    }

    [Serializable]
    public sealed class CelFxRecipe
    {
        public string Name = "";
        /// <summary>How long the effect runs, s (for the stage and the stills; Play does not need it).</summary>
        public float Length = 3f;
        public List<CelLayer> Layers = new List<CelLayer>();

        public CelFxRecipe() { }
        public CelFxRecipe(string name, float length, params CelLayer[] layers) { Name = name; Length = length; Layers.AddRange(layers); }

        /// <summary>Everything wrong with the recipe, one line each; empty when it is fine. The tests run it on every recipe.</summary>
        public List<string> Validate(CelFxRenderer renderer = null)
        {
            var bad = new List<string>();
            for (int i = 0; i < Layers.Count; i++)
            {
                var l = Layers[i];
                string at = Name + "[" + i + (string.IsNullOrEmpty(l.Name) ? "" : " " + l.Name) + "]: ";
                if (l.Close < l.Mid || l.Mid < l.Far) bad.Add(at + "counts rise going out (Close " + l.Close + ", Mid " + l.Mid + ", Far " + l.Far + ")");
                if (l.Far < 0) bad.Add(at + "a negative count");
                if (l.Life <= 0f) bad.Add(at + "Life must be above 0");
                if (CelPalette.Find(l.Palette) < 0) bad.Add(at + "no palette called " + l.Palette);
                if (!string.IsNullOrEmpty(l.CoolTo) && CelPalette.Find(l.CoolTo) < 0) bad.Add(at + "no palette called " + l.CoolTo);
                if (renderer != null && renderer.KindId(l.Kind) == -1) bad.Add(at + "no kind called " + l.Kind);
            }
            return bad;
        }

        /// <summary>The most records any one play spawns at Close, High quality (for budgets).</summary>
        public int MaxRecords()
        {
            int n = 0;
            foreach (var l in Layers) n += l.Close;
            return n;
        }
    }
}
