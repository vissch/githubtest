// Phase: Playground (2026-09-26, lane/show/playground) — each LOD's colour matched to LOD0's
// Tripo bakes each LOD's texture on its own, so the LODs of one model differ in colour: every switch shifted colour 2.4-7x
// what a one-degree turn of the camera does (critic r7, measured by lodpop's noise floor). Each LOD's mean surface
// colour (its atlas under its UVs, times its vertex colours, weighted by area) is compared with LOD0's and the ratio
// goes on that LOD's material as a tint. A far LOD that carries its colour in its vertices (no UVs) counts those.
// Everything is compared as the shader sees it. The project renders in Gamma colour space, so the atlas and the vertex
// colours both reach the shader as stored, and are compared as stored.
// What it buys is small, measured (docs/22): the mean colour is only ~5/255 of the ~11/255 block-averaged colour change
// at a switch; the rest is the shape (lines and shading move with the different sculpt). On for figures, off for vehicles.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Playground
{
    public static class LodTint
    {
        public static Color MeanColour(IEnumerable<Mesh> meshes, Texture2D atlas)
        {
            double r = 0, g = 0, b = 0, area = 0;
            bool readable = atlas != null && atlas.isReadable;
            foreach (var m in meshes)
            {
                if (m == null || !m.isReadable) continue;
                var v = m.vertices; var uv = m.uv; var col = m.colors; var t = m.triangles;
                bool hasUv = uv.Length == v.Length, hasCol = col.Length == v.Length;
                for (int i = 0; i < t.Length; i += 3)
                {
                    float a = Vector3.Cross(v[t[i + 1]] - v[t[i]], v[t[i + 2]] - v[t[i]]).magnitude;
                    if (a <= 0f) continue;
                    Color c = Color.white;
                    if (hasUv && readable) c = atlas.GetPixelBilinear((uv[t[i]].x + uv[t[i + 1]].x + uv[t[i + 2]].x) / 3f, (uv[t[i]].y + uv[t[i + 1]].y + uv[t[i + 2]].y) / 3f);
                    if (hasCol) c *= (col[t[i]] + col[t[i + 1]] + col[t[i + 2]]) / 3f;
                    r += c.r * a; g += c.g * a; b += c.b * a; area += a;
                }
            }
            return area > 0 ? new Color((float)(r / area), (float)(g / area), (float)(b / area), 1f) : Color.white;
        }

        /// <summary>The tint that brings a LOD's mean colour to the reference's, per channel, kept within a third either way.</summary>
        public static Color Match(Color reference, Color lod)
        {
            float f(float want, float have) => Mathf.Clamp(have > 1e-4f ? want / have : 1f, 0.75f, 1.33f);
            return new Color(f(reference.r, lod.r), f(reference.g, lod.g), f(reference.b, lod.b), 1f);
        }
    }
}
