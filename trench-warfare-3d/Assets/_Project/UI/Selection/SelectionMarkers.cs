// Phase: B6 (implemented) — what selection looks like on the field, as Dust Front draws it (its developer's GIFs,
// 2026-09-23 research): under each selected unit a flat hexagon of two thin off-white brackets with gaps at two
// corners, lying on the ground so the camera's pitch flattens it; an inspected enemy gets the same in rust; above each
// a thin health bar, dark track and red fill, facing the camera. The unit (or knot) under the cursor gets faint brackets. All
// instanced (two draws per colour), textures painted in code, URP Unlit made transparent the way CombatFx does it.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.UI
{
    public sealed class SelectionMarkers : System.IDisposable
    {
        public static readonly Color Ours = new Color32(0xE8, 0xE1, 0xD2, 235);      // --tw-text-bright
        public static readonly Color Theirs = new Color32(0xB5, 0x52, 0x3A, 235);    // --tw-enemy
        public static readonly Color Hover = new Color32(0xE8, 0xE1, 0xD2, 90);
        public static readonly Color Danger = new Color32(0xE0, 0x2B, 0x2B, 235);    // --tw-alarm: ours under our own shells
        public static readonly Color Track = new Color32(0x0E, 0x0F, 0x10, 210);
        public static readonly Color FillOurs = new Color32(0xD9, 0x48, 0x5A, 255);  // Dust Front's pinkish red
        public static readonly Color FillTheirs = new Color32(0xE0, 0x2B, 0x2B, 255); // --tw-alarm
        public const float BarWidthPerSize = 0.8f, BarHeightM = 0.12f, GroundLift = 0.06f;

        readonly Mesh ground, bar;
        readonly Texture2D hex;
        readonly Material hexOurs, hexTheirs, hexHover, hexDanger, track, fillOurs, fillTheirs;
        readonly List<Matrix4x4> batch = new List<Matrix4x4>(256);
        Matrix4x4[] array = new Matrix4x4[256];
        static readonly Bounds Everywhere = new Bounds(Vector3.zero, Vector3.one * 100000f);

        public SelectionMarkers()
        {
            ground = Quad(true); bar = Quad(false);
            hex = HexBrackets(128);
            var unlit = Shader.Find("Universal Render Pipeline/Unlit");   // kept in player builds (Resources/ShaderKeep), as CombatFx uses it
            hexOurs = Transparent(unlit, Ours, hex); hexTheirs = Transparent(unlit, Theirs, hex); hexHover = Transparent(unlit, Hover, hex); hexDanger = Transparent(unlit, Danger, hex);
            track = Transparent(unlit, Track, null); fillOurs = Transparent(unlit, FillOurs, null); fillTheirs = Transparent(unlit, FillTheirs, null);
            fillOurs.renderQueue = fillTheirs.renderQueue = track.renderQueue + 1;   // the fill over its track
        }

        public void Dispose()
        {
            foreach (var o in new Object[] { ground, bar, hex, hexOurs, hexTheirs, hexHover, hexDanger, track, fillOurs, fillTheirs })
                if (o != null) Object.Destroy(o);
        }

        /// <summary>Draw this frame's markers: brackets under the selected (and the hovered), bars over the selected.</summary>
        public void Draw(List<ScreenUnit> selected, List<ScreenUnit> hovered, System.Func<ScreenUnit, float> hpFraction, Camera cam)
        {
            if (cam == null) return;
            batch.Clear(); foreach (var u in hovered) batch.Add(GroundTrs(u)); Flush(ground, hexHover);
            for (int pass = 0; pass < 2; pass++)
            {
                batch.Clear();
                foreach (var u in selected) if (u.Ours == (pass == 0)) batch.Add(GroundTrs(u));
                Flush(ground, pass == 0 ? hexOurs : hexTheirs);
            }
            // bars: the track, then each side's fill scaled to its health from the left end
            var face = cam.transform.rotation;
            Vector3 right = cam.transform.right;
            batch.Clear();
            foreach (var u in selected) batch.Add(Matrix4x4.TRS(BarCentre(u), face, new Vector3(BarWidth(u), BarHeightM * BarScale(u), 1f)));
            Flush(bar, track);
            for (int pass = 0; pass < 2; pass++)
            {
                batch.Clear();
                foreach (var u in selected)
                {
                    if (u.Ours != (pass == 0)) continue;
                    float k = Mathf.Clamp01(hpFraction(u));
                    if (k <= 0f) continue;
                    float w = BarWidth(u), inset = BarHeightM * BarScale(u) * 0.25f;
                    float fw = (w - 2f * inset) * k;
                    Vector3 c = BarCentre(u) - right * ((w - 2f * inset) * 0.5f - fw * 0.5f);
                    batch.Add(Matrix4x4.TRS(c, face, new Vector3(fw, BarHeightM * BarScale(u) - 2f * inset, 1f)));
                }
                Flush(bar, pass == 0 ? fillOurs : fillTheirs);
            }
        }

        /// <summary>While a strike is aimed: rust brackets under the enemy it covers, red under ours in its reach.</summary>
        public void DrawTargets(List<ScreenUnit> enemy, List<ScreenUnit> ours)
        {
            batch.Clear(); foreach (var u in enemy) batch.Add(GroundTrs(u)); Flush(ground, hexTheirs);
            batch.Clear(); foreach (var u in ours) batch.Add(GroundTrs(u)); Flush(ground, hexDanger);
        }

        static float BarScale(ScreenUnit u) => u.Vehicle ? 1.6f : Mathf.Max(1f, u.Size * 0.6f);
        static float BarWidth(ScreenUnit u) => u.Size * BarWidthPerSize;
        static Vector3 BarCentre(ScreenUnit u) => u.World + Vector3.up * (u.Vehicle ? 2.4f : u.Size * 1.05f);   // over the head: the body centre is half a figure up
        static Matrix4x4 GroundTrs(ScreenUnit u)
        {
            float body = u.Vehicle ? UnitPicker.VehicleBodyM : UnitPicker.ManBodyM * u.Size / 1.1f;
            return Matrix4x4.TRS(new Vector3(u.World.x, u.World.y - body + GroundLift, u.World.z), Quaternion.identity, new Vector3(u.Size, 1f, u.Size));
        }

        void Flush(Mesh mesh, Material mat)
        {
            if (batch.Count == 0) return;
            if (array.Length < batch.Count) array = new Matrix4x4[Mathf.NextPowerOfTwo(batch.Count)];
            batch.CopyTo(array);
            Graphics.RenderMeshInstanced(new RenderParams(mat) { worldBounds = Everywhere, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = false }, mesh, 0, array, batch.Count);
        }

        /// <summary>A unit quad centred on the origin: on the ground (XZ) or facing +Z (the bar, rotated to the camera).</summary>
        static Mesh Quad(bool onGround)
        {
            var m = new Mesh { name = onGround ? "tw-select-ground" : "tw-select-bar" };
            m.vertices = onGround
                ? new[] { new Vector3(-0.5f, 0f, -0.5f), new Vector3(0.5f, 0f, -0.5f), new Vector3(0.5f, 0f, 0.5f), new Vector3(-0.5f, 0f, 0.5f) }
                : new[] { new Vector3(-0.5f, -0.5f, 0f), new Vector3(0.5f, -0.5f, 0f), new Vector3(0.5f, 0.5f, 0f), new Vector3(-0.5f, 0.5f, 0f) };
            m.uv = new[] { new Vector2(0, 0), new Vector2(1, 0), new Vector2(1, 1), new Vector2(0, 1) };
            m.triangles = new[] { 0, 2, 1, 0, 3, 2 };   // clockwise seen from above (ground) and from the camera (the bar faces away from it)
            m.RecalculateNormals(); m.RecalculateBounds();
            return m;
        }

        /// <summary>
        /// The bracket texture: a hexagon outline (flat top), 1/40 of the size thick, anti-aliased, white on clear, with
        /// gaps at the top-right and bottom-left corners so it reads as two brackets, as Dust Front draws it.
        /// </summary>
        public static Texture2D HexBrackets(int n)
        {
            var t = new Texture2D(n, n, TextureFormat.RGBA32, true) { wrapMode = TextureWrapMode.Clamp, filterMode = FilterMode.Trilinear, anisoLevel = 4, hideFlags = HideFlags.HideAndDontSave };
            var px = new Color32[n * n];
            float r = 0.46f, thick = 1f / 40f;
            for (int y = 0; y < n; y++) for (int x = 0; x < n; x++)
            {
                float u = (x + 0.5f) / n - 0.5f, v = (y + 0.5f) / n - 0.5f;
                float d = Mathf.Abs(HexSdf(u, v, r));
                float a = Mathf.Clamp01((thick - d) * n * 0.9f);
                float ang = Mathf.Atan2(v, u) * Mathf.Rad2Deg; if (ang < 0f) ang += 360f;
                if (Mathf.Abs(Mathf.DeltaAngle(ang, 60f)) < 16f || Mathf.Abs(Mathf.DeltaAngle(ang, 240f)) < 16f) a = 0f;   // the two gaps
                px[y * n + x] = new Color32(255, 255, 255, (byte)Mathf.RoundToInt(a * 255f));
            }
            t.SetPixels32(px); t.Apply(true, true);
            return t;
        }

        /// <summary>Signed distance to a flat-topped regular hexagon of circumradius r (negative inside).</summary>
        public static float HexSdf(float x, float y, float r)
        {
            // Inigo Quilez's hexagon (flat top and bottom at +-apothem, vertices at 0, 60, ... degrees); the apothem is r cos 30
            const float k0 = -0.8660254f, k1 = 0.5f, k2 = 0.57735027f;
            float px = Mathf.Abs(x), py = Mathf.Abs(y);
            float dot = 2f * Mathf.Min(k0 * px + k1 * py, 0f);
            px -= dot * k0; py -= dot * k1;
            float a = r * 0.8660254f;
            px -= Mathf.Clamp(px, -k2 * a, k2 * a); py -= a;
            return new Vector2(px, py).magnitude * Mathf.Sign(py);
        }

        static Material Transparent(Shader shader, Color color, Texture tex)
        {
            var m = new Material(shader) { enableInstancing = true, color = color };
            m.SetFloat("_Surface", 1f);
            m.SetFloat("_Blend", 0f);
            m.SetFloat("_ZWrite", 0f);
            m.SetInt("_SrcBlend", (int)BlendMode.SrcAlpha);
            m.SetInt("_DstBlend", (int)BlendMode.OneMinusSrcAlpha);
            m.EnableKeyword("_SURFACE_TYPE_TRANSPARENT");
            m.SetOverrideTag("RenderType", "Transparent");
            m.renderQueue = (int)RenderQueue.Transparent + 10;
            if (m.HasProperty("_BaseColor")) m.SetColor("_BaseColor", color);
            if (tex != null) { m.mainTexture = tex; if (m.HasProperty("_BaseMap")) m.SetTexture("_BaseMap", tex); }
            m.hideFlags = HideFlags.HideAndDontSave;
            return m;
        }
    }
}
