// Phase: C1 (unit look, look-06) - ONE LONG CARD for the flamethrower's stream, from the muzzle to the target.
//
// What this replaces, and why it had to be replaced rather than tuned. The jet was a CHAIN of flipbook cards -
// Flamethrower.Link/Span laid four of the pack's stream drawing along the aim, with a nine-card bright core inside
// it. Three rounds of work went into making those links fuse: more overlap, closer cels, per-link breath, a
// different book for the head. The master's verdict on the pictures never moved: "a thin pale-yellow stick from the
// muzzle and a SEPARATE blob of orange fire a few metres out, with no taper joining them". The reason is structural.
// Every link is a quad with its own drawing, and a drawing has its own closed contour; the union of five contours is
// read by the eye as five things however much they overlap, and wherever two of them do not quite meet there is a
// waist that reads as a gap. No amount of overlap turns a row of objects into one gesture.
//
// So the stream is GEOMETRY now: a ribbon strip whose spine runs from the nozzle to the target along the aim, with
// the arc hung off it, widening from 1.55 m at the mouth to 3.90 m at the head (the owner's widths), and the fire is
// computed across it by TW/Flame Jet (URP). One mesh, one draw, one silhouette, and the taper is continuous by
// construction: segment i's far end IS segment i+1's near end, so there is no spacing to get wrong.
//
// It is rebuilt EVERY FRAME, outside Flamethrower's RootEvery gate. A chain link is a card dropped into a flipbook
// and left to live out its life; this is a shape that has to be where the man is pointing right now, and it is also
// what lets the shader's scroll be the only moving part.
//
// Pure statics for the shape (Spine, Across, HalfAt) so FlameShapeTests can measure it: EditMode cannot render, and
// the master's complaint has always been about the shape, not about the pixels.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Presentation.Tactical
{
    public static class FlameJetCard
    {
        /// <summary>
        /// How many quads the ribbon is built from. Sixteen, because the arc is a curve and a strip of eight showed
        /// its chords; past about twenty nothing in the picture changes and every jet costs four more vertices.
        /// </summary>
        public const int Segments = 16;

        /// <summary>The stream's thickness at the mouth and at the head, in metres. The owner's numbers.</summary>
        public const float Mouth = 1.55f, Head = 3.90f;

        /// <summary>Half the stream's thickness at u along the run, in metres.</summary>
        public static float HalfAt(float u) => Mathf.Lerp(Mouth, Head, Mathf.Clamp01(u)) * 0.5f;

        /// <summary>
        /// Segment i of segs along a run of len metres. near and far are how far past the mouth the segment's two
        /// ends sit; half is the stream's half-thickness at its NEAR end. Segment i's far IS segment i+1's near, so
        /// the card is continuous: no spacing, no overlap, no gap, and nothing to measure a waist in.
        /// </summary>
        public static void Spine(int i, int segs, float len, out float near, out float far, out float half)
        {
            near = len * i / segs;
            far = len * (i + 1) / segs;
            half = HalfAt(i / (float)segs);
        }

        /// <summary>
        /// Which way is ACROSS the ribbon: perpendicular to the run and to the line of sight, so the card presents
        /// its width to the eye. When the stream is aimed at or away from the camera that cross product collapses,
        /// and rather than giving up on the card (which is what the old chain existed to cover) it falls back to the
        /// camera's right and then to up. A degenerate aim therefore still draws a stream, seen end-on - which is
        /// why the whole fallback chain could go.
        /// </summary>
        // The mesh, the noise and the material outlive a Play session unless something puts them back: they are
        // HideAndDontSave, so leaving Play does not destroy them and the next session would draw into a dead Mesh.
        static FlameJetCard() => SceneStatics.Register(nameof(FlameJetCard), Reset);

        /// <summary>Drop everything Play built. Called when Play ends (SceneStatics).</summary>
        static void Reset()
        {
            if (material != null) Object.DestroyImmediate(material);
            if (noise != null) Object.DestroyImmediate(noise);
            if (mesh != null) Object.DestroyImmediate(mesh);
            material = null; noise = null; mesh = null;
            pos.Clear(); vu.Clear(); shape.Clear(); tris.Clear();
            cards = 0; drawn = 0;
        }

        public static Vector3 Across(Vector3 along, Vector3 toEye)
        {
            Vector3 a = along.sqrMagnitude > 1e-8f ? along.normalized : Vector3.forward;
            Vector3 c = Vector3.Cross(a, toEye);
            if (c.sqrMagnitude > 1e-6f) return c.normalized;
            c = Vector3.Cross(a, Vector3.up);
            if (c.sqrMagnitude > 1e-6f) return c.normalized;
            return Vector3.Cross(a, Vector3.right).normalized;
        }

        static readonly List<Vector3> pos = new List<Vector3>(512);
        static readonly List<Vector2> vu = new List<Vector2>(512);
        static readonly List<Vector4> shape = new List<Vector4>(512);
        static readonly List<int> tris = new List<int>(768);
        static Mesh mesh;
        static Material material;
        static Texture2D noise;

        /// <summary>
        /// What the last frame actually built, for a capture rig to print beside its pictures. "jets=0 quads=0" while
        /// the sim is burning men is the same class of bug Tools/flamefight exists to catch one level up: the stream
        /// is asked for and nothing draws it.
        /// </summary>
        public static string Report() => "cards=" + cards + " quads=" + (tris.Count / 6) + " drawn=" + drawn
                                       + " material=" + (material != null);
        static int cards, drawn;

        /// <summary>Start a frame's worth of streams. Called once a frame before any Push.</summary>
        public static void Begin()
        {
            pos.Clear(); vu.Clear(); shape.Clear(); tris.Clear(); cards = 0;
        }

        /// <summary>
        /// The SAG's profile along the run: 0 at both ends, deepest a little past the middle, and still falling
        /// through the last quarter. Flamethrower.Arc is this same curve, which is why it lives here - the card and
        /// everything still drawn as a flipbook card have to hang off ONE centreline or the stream reads as two.
        /// </summary>
        public static float Sag(float u) => Mathf.Sin(Mathf.Pow(Mathf.Clamp01(u), 0.78f) * Mathf.PI);

        /// <summary>
        /// One stream. mouth is the nozzle, aim the unit direction, len the run in metres; sag is the cross-run
        /// displacement at the curve's deepest point (Sag scales it along the run); phase scrolls the shader's noise
        /// so two jets are never the same picture; alpha and the glow ramp set the brightness.
        /// </summary>
        public static void Push(Vector3 mouth, Vector3 aim, float len, Vector3 toEye, Vector3 sag,
                               float phase, float alpha, float glowMouth, float glowHead)
        {
            if (len <= 0.05f || alpha <= 0.01f) return;
            cards++;
            Vector3 along = aim.sqrMagnitude > 1e-8f ? aim.normalized : Vector3.forward;
            Vector3 across = Across(along, toEye);
            for (int i = 0; i < Segments; i++)
            {
                Spine(i, Segments, len, out float near, out float far, out float halfNear);
                float u0 = i / (float)Segments, u1 = (i + 1) / (float)Segments;
                float halfFar = HalfAt(u1);
                Vector3 a = mouth + along * near + sag * Sag(u0);
                Vector3 b = mouth + along * far + sag * Sag(u1);
                var s0 = new Vector4(halfNear, phase, alpha, Mathf.Lerp(glowMouth, glowHead, u0));
                var s1 = new Vector4(halfFar, phase, alpha, Mathf.Lerp(glowMouth, glowHead, u1));
                int v0 = pos.Count;
                pos.Add(a - across * halfNear); vu.Add(new Vector2(-1f, u0)); shape.Add(s0);
                pos.Add(a + across * halfNear); vu.Add(new Vector2(1f, u0)); shape.Add(s0);
                pos.Add(b + across * halfFar); vu.Add(new Vector2(1f, u1)); shape.Add(s1);
                pos.Add(b - across * halfFar); vu.Add(new Vector2(-1f, u1)); shape.Add(s1);
                tris.Add(v0); tris.Add(v0 + 1); tris.Add(v0 + 2);
                tris.Add(v0); tris.Add(v0 + 2); tris.Add(v0 + 3);
            }
        }

        /// <summary>
        /// Hand this frame's streams to the renderer. One draw, however many jets are burning. EVERY camera, not the
        /// one the jets were stepped against: TankCapture renders Camera.main into a RenderTexture of its own and a
        /// per-camera draw never reached it - the first run of the card came back with sixteen quads built, the draw
        /// issued, the material alive, and not one blue pixel in the picture.
        /// Through FrameBudget, like every other draw in the game, so rule 7 and frame_budget_draws can see it.
        /// </summary>
        public static void Draw(Camera cam)
        {
            if (tris.Count == 0) return;
            Build();
            if (material == null) return;      // no shader in this build: draw nothing rather than draw magenta
            mesh.Clear();
            mesh.SetVertices(pos); mesh.SetUVs(0, vu); mesh.SetUVs(1, shape); mesh.SetTriangles(tris, 0);
            var bounds = new Bounds(pos[0], Vector3.one * 200f);
            mesh.bounds = bounds;
            FrameBudget.Draw(new RenderParams(material) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = false },
                             mesh, 0, Matrix4x4.identity);
            drawn++;
        }

        static void Build()
        {
            if (mesh == null) mesh = new Mesh { name = "Flame jet", hideFlags = HideFlags.HideAndDontSave };
            if (material != null) return;
            var shader = Shader.Find("TW/Flame Jet (URP)");
            if (shader == null) return;
            // NightLights' own recipe for the flame noise, kept local on purpose: that file builds its torches' mesh
            // and material and is not ours to grow a public helper on for one caller.
            const int n = 64;
            var px = new Color32[n * n];
            for (int y = 0; y < n; y++)
            for (int x = 0; x < n; x++)
            {
                float c = Tile(x / (float)n * 4f, y / (float)n * 4f, 4) * .55f + Tile(x / (float)n * 9f, y / (float)n * 9f, 9) * .45f;
                byte b = (byte)(Mathf.Clamp01(c) * 255f); px[y * n + x] = new Color32(b, b, b, 255);
            }
            noise = new Texture2D(n, n, TextureFormat.RGBA32, true) { name = "Flame jet noise", wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Bilinear, hideFlags = HideFlags.HideAndDontSave };
            noise.SetPixels32(px); noise.Apply(true, true);
            material = new Material(shader) { hideFlags = HideFlags.HideAndDontSave };
            material.SetTexture("_Noise", noise);
        }

        static float Tile(float x, float z, int period)
        {
            int x0 = Mathf.FloorToInt(x), z0 = Mathf.FloorToInt(z);
            float tx = x - x0, tz = z - z0;
            tx = tx * tx * (3f - 2f * tx); tz = tz * tz * (3f - 2f * tz);
            int xa = x0 % period, xb = (xa + 1) % period, za = z0 % period, zb = (za + 1) % period;
            return Mathf.Lerp(Mathf.Lerp(Hash(xa + period * 131, za), Hash(xb + period * 131, za), tx),
                              Mathf.Lerp(Hash(xa + period * 131, zb), Hash(xb + period * 131, zb), tx), tz);
        }

        static float Hash(int a, int b)
        {
            uint h = (uint)a * 0x9E3779B1u ^ (uint)b * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12;
            return (h & 0xFFFF) / 65535f;
        }
    }
}
