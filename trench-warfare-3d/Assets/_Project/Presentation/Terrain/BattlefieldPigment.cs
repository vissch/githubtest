// Phase: B2 (presentation-only reusable painted material vocabulary)
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public static class BattlefieldPigment
    {
        public enum Surface { Timber, Earth, Concrete, Canvas, Bark }

        // Shared mipmapped pigment, baked once. Uses the shader's existing base-map sample.
        public static Texture2D Bake(Surface surface)
        {
            const int size = 256;
            var texture = new Texture2D(size, size, TextureFormat.RGB24, true)
            { name = "Painted " + surface, wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Bilinear, anisoLevel = 4 };
            var pixels = new Color[size * size];
            for (int y = 0; y < size; y++)
            for (int x = 0; x < size; x++)
            {
                float u = (x + .5f) / size, v = (y + .5f) / size;
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
                else
                {
                    float crack = u - (.55f + Mathf.Sin(v * 16f) * .09f + Mathf.Sin(v * 39f) * .025f);
                    if (Mathf.Abs(crack) < .012f && v > .26f) value = .31f;
                    else if (crack > .012f && crack < .024f && v > .26f) value = 1.19f;
                    if (Mathf.PerlinNoise(u * 23f + 7f, v * 23f) > .73f) value *= .65f;
                    if (v > .94f) value = 1.12f;
                }
                pixels[y * size + x] = new Color(value, value, value, 1f);
            }
            texture.SetPixels(pixels); texture.Apply(true, true); return texture;
        }
    }
}
