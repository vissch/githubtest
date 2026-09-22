// Phase: B2 (presentation-only reusable painted material vocabulary)
//
// One sheet of painted surfaces, not one texture each. Every one of these is grey — the kit tints it with
// _BaseColor — so storing them in RGB24 was paying three bytes to say one thing, five times over, in five separate
// textures that each cost their own binding. They are now the layers of a single R8 Texture2DArray: a third of the
// memory, one binding, and a new surface costs 64 KB rather than a texture and a material property.
//
// Tiling is why this is an array and not an atlas. These are sampled by mesh UV and repeat across a plank or a wall,
// and a tile packed into a corner of an atlas bleeds into its neighbours the moment it wraps.
//
// The shader takes the layer as _Pigment (a float, -1 meaning "use _BaseMap instead", which is what the ground and
// the imported sheets do) and the sheet itself as the global _PigmentSheet, set once by BattlefieldKit.
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public static class BattlefieldPigment
    {
        /// <summary>The order here IS the layer index the shader is given, so new surfaces go on the end.</summary>
        public enum Surface { Timber, Earth, Concrete, Canvas, Bark, Rust, Stone, Sacking }

        public const int Size = 256;
        public const int Count = 8;

        /// <summary>Every surface as one mipmapped array, baked once. R8: these are grey and the kit tints them.</summary>
        public static Texture2DArray Sheet()
        {
            var sheet = new Texture2DArray(Size, Size, Count, TextureFormat.R8, true)
            { name = "Painted surfaces", wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Bilinear, anisoLevel = 4 };
            var pixels = new Color[Size * Size];
            for (int layer = 0; layer < Count; layer++)
            {
                var surface = (Surface)layer;
                for (int y = 0; y < Size; y++)
                for (int x = 0; x < Size; x++)
                {
                    float value = Value(surface, (x + .5f) / Size, (y + .5f) / Size);
                    pixels[y * Size + x] = new Color(value, value, value, 1f);
                }
                sheet.SetPixels(pixels, layer);
            }
            sheet.Apply(true, true);   // mips, and drop the CPU copy: nothing reads these back
            return sheet;
        }

        /// <summary>The painted value at one point of one surface, centred on about 1.0 so a tint passes through it.</summary>
        static float Value(Surface surface, float u, float v)
        {
            float value = .91f + Mathf.PerlinNoise(u * 21f, v * 17f) * .09f;
            if (surface == Surface.Timber || surface == Surface.Bark)
            {
                float grain = v * 6f + Mathf.Sin(u * 9f + v * 5f) * .12f + Mathf.Sin(u * 23f) * .035f;
                float stroke = Mathf.Repeat(grain, 1f);
                if (stroke < .055f && Mathf.Sin(u * 17f + v * 9f) > -.65f) value = .44f;
                else if (stroke < .115f) value = 1.13f;
                float knot = new Vector2((u - .68f) * 3.9f, (v - .42f) * 8f).magnitude;
                if (knot < .13f || Mathf.Abs(knot - .65f) < .075f) value = .43f;
                if (surface == Surface.Timber)
                {
                    if (v < .025f) value = .48f;
                    else if (v < .06f) value = 1.15f;
                    if ((u < .065f || u > .935f) && Mathf.Abs(v - .5f) < .035f) value = .27f;
                }
            }
            else if (surface == Surface.Earth)
            {
                float seam = v - (.45f + Mathf.Sin(u * 19f) * .075f + Mathf.Sin(u * 43f) * .025f);
                if (Mathf.Abs(seam) < .018f || (Mathf.Abs(seam + .27f) < .014f && u > .26f)) value = .37f;
                else if (seam > .018f && seam < .052f) value = 1.22f;
                float cut = Mathf.Repeat(u * 13f + Mathf.Sin(v * 18f) * .19f, 1f);
                if (v < .35f && cut < .07f) value = .48f;
            }
            else if (surface == Surface.Canvas)
            {
                float seam = Mathf.Abs(v - .51f - Mathf.Sin(u * 18f) * .008f);
                if (seam < .009f) value = .48f;
                if (seam < .025f && Mathf.Repeat(u * 42f, 1f) < .14f) value = .55f;
                if (v > .72f && Mathf.Sin(u * 25f + v * 5f) > .96f) value = .67f;
            }
            else if (surface == Surface.Rust)
            {
                // Pitted, streaked metal: broad patches where the paint has gone, dark pits inside them, and rust
                // running DOWN from each patch, because water does. Helmets, tins, buckets, corrugated iron.
                float patch = Mathf.PerlinNoise(u * 6.5f + 3.1f, v * 6.5f);
                float pit = Mathf.PerlinNoise(u * 47f, v * 47f + 9f);
                if (patch > .56f)
                {
                    value = Mathf.Lerp(1.06f, .62f, Mathf.InverseLerp(.56f, .82f, patch));
                    if (pit > .70f) value *= .74f;                                   // the pits themselves
                }
                float run = Mathf.PerlinNoise(u * 24f + 17f, v * 3.2f);              // stretched down the surface
                if (run > .66f && patch > .40f) value *= Mathf.Lerp(1f, .80f, (run - .66f) * 3f);
                if (Mathf.Repeat(v * 9f, 1f) < .045f && patch < .5f) value *= 1.07f; // a rolled edge catching light
            }
            else if (surface == Surface.Stone)
            {
                // Broken block: a few straight fracture lines with a bright chipped edge on one side, over a speckle.
                float speck = Mathf.PerlinNoise(u * 38f + 5f, v * 38f);
                value *= Mathf.Lerp(.94f, 1.05f, speck);
                float a = Mathf.Repeat(u * 3f + v * 1.7f + Mathf.Sin(v * 11f) * .06f, 1f);
                float b = Mathf.Repeat(v * 2.6f - u * .9f + Mathf.Sin(u * 13f) * .05f, 1f);
                if (a < .022f || b < .019f) value = .52f;                            // the fracture
                else if (a < .046f || b < .040f) value = 1.16f;                      // its chipped lip
                if (speck > .80f) value *= .82f;                                     // knocked-out grains
            }
            else if (surface == Surface.Sacking)
            {
                // Hessian: a coarse warp and weft you can count, unlike Canvas, which is a seamed sheet. Sandbags
                // read as woven rather than smooth when the camera is among the men.
                float warp = Mathf.Repeat(u * 34f, 1f), weft = Mathf.Repeat(v * 34f, 1f);
                float over = (warp < .5f) == (weft < .5f) ? 1.06f : .93f;            // which thread is on top
                value *= over;
                if (warp < .10f || weft < .10f) value *= .90f;                       // the gaps between threads
                if (Mathf.PerlinNoise(u * 9f + 21f, v * 9f) > .74f) value *= .94f;   // worn, dirt-darkened patches
            }
            else
            {
                float crack = u - (.55f + Mathf.Sin(v * 16f) * .09f + Mathf.Sin(v * 39f) * .025f);
                if (Mathf.Abs(crack) < .012f && v > .26f) value = .31f;
                else if (crack > .012f && crack < .024f && v > .26f) value = 1.19f;
                if (Mathf.PerlinNoise(u * 23f + 7f, v * 23f) > .73f) value *= .65f;
                if (v > .94f) value = 1.12f;
            }
            return value;
        }
    }
}
