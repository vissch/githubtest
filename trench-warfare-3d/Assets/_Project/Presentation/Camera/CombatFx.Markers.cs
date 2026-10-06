// Phase: VFX pass (2026-10-01) — part of CombatFx (see CombatFx.cs for the event dispatch and the shared pools). The
// called-strike target markers: where support fire was called, seen by both sides until its last payload lands
// (MarkerLife). A disc is a rim round the ground it will fall on; a corridor is its two edges and its ends.
// They were one flattened sphere (or a run of boxes) at 35 % over the whole area for 10 s, which at night on a 25 m
// barrage was an opaque yellow plate over the field the shells were landing in (VFX round 1: "opaque yellow targeting
// overlays", a readability veto). No fill at all: any fill floats 0.4 m up and reads past the rim on ground falling
// away from the camera (round 3). Each rim piece sits on its own ground sample, so a rim follows a slope instead of
// cutting into it; the rim thins away over its last seconds instead of blinking out.
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>How long a marker shows when its ability says nothing better.</summary>
        public const float MarkerSeconds = 10f;
        /// <summary>The rim's width in metres, and its opacity.</summary>
        public const float MarkerRimWidth = 0.55f, MarkerRimAlpha = 0.55f;
        const int RimPieces = 48;

        Material rimMine, rimTheirs;

        /// <summary>The rim's width at a marker's time left: full, then thinning to nothing over its last three seconds.</summary>
        public static float MarkerRim(float left) => MarkerRimWidth * Mathf.Clamp01(left / 3f);

        /// <summary>How long an ability's marker shows: until its last payload has landed (warm-up and spread) and half a
        /// second more, never under 3 s or over 20. A strafe's corridor lay on the field for seconds after the pass.</summary>
        public static float MarkerLife(int ability, float tickSeconds)
        {
            if (!TW.Sim.Match.OffMapAbilitySystem.TryGetStats(ability, out var s)) return MarkerSeconds;
            return Mathf.Clamp((s.WarmupTicks + s.SpreadTicks) * tickSeconds + 0.5f, 3f, 20f);
        }

        void MarkerMaterials()
        {
            if (rimMine != null) return;
            var unlit = Shader.Find("Universal Render Pipeline/Unlit");
            if (unlit == null) unlit = Shader.Find("Unlit/Color");
            rimMine = Transparent(unlit, new Color(1f, 0.85f, 0.3f, MarkerRimAlpha));
            rimTheirs = Transparent(unlit, new Color(1f, 0.2f, 0.15f, MarkerRimAlpha));
        }

        void DestroyMarkerMaterials()
        {
            foreach (var mat in new[] { rimMine, rimTheirs }) if (mat != null) Destroy(mat);
        }

        float MarkerGround(float x, float z) => RenderGround.Sample(Host.Local.Map, x, z) + 0.4f;

        void DrawMarkers(float now, Bounds bounds)
        {
            if (markers.Count == 0) return;
            MarkerMaterials();
            for (int pass = 0; pass < 2; pass++)
            {
                bool mine = pass == 0;
                var rpRim = new RenderParams(mine ? rimMine : rimTheirs) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off };
                // the rims
                batch.Clear();
                for (int i = 0; i < markers.Count && batch.Count < 1000; i++)
                {
                    var m = markers[i];
                    if (m.Mine != mine) continue;
                    float w = MarkerRim(m.Until - now);
                    if (w <= 0.01f) continue;
                    if (m.Length <= 0f)
                    {
                        float r = Mathf.Max(0.5f, m.Radius - w * 0.5f), chord = 2f * Mathf.PI * r / RimPieces * 1.04f;
                        for (int k = 0; k < RimPieces; k++)
                        {
                            float a = k * (2f * Mathf.PI / RimPieces);
                            var dir = new Vector3(Mathf.Cos(a), 0f, Mathf.Sin(a));
                            var at = m.Pos + dir * r; at.y = MarkerGround(at.x, at.z);
                            batch.Add(Matrix4x4.TRS(at, Quaternion.LookRotation(new Vector3(-dir.z, 0f, dir.x)), new Vector3(w, 0.06f, chord)));
                        }
                        continue;
                    }
                    var rot = Quaternion.LookRotation(m.Dir);
                    var side = new Vector3(m.Dir.z, 0f, -m.Dir.x);
                    float half = Mathf.Max(0.5f, m.Radius - w * 0.5f);
                    for (float s = 0f; s < m.Length && batch.Count < 1000; s += MarkerSegment)
                    {
                        float len = Mathf.Min(MarkerSegment, m.Length - s);
                        var mid = m.Pos + m.Dir * (s + len * 0.5f);
                        for (int e = -1; e <= 1; e += 2)
                        {
                            var at = mid + side * (half * e); at.y = MarkerGround(at.x, at.z);
                            batch.Add(Matrix4x4.TRS(at, rot, new Vector3(w, 0.06f, len)));
                        }
                    }
                    for (int e = 0; e < 2; e++)   // the two ends
                    {
                        var at = m.Pos + m.Dir * (e == 0 ? 0f : m.Length); at.y = MarkerGround(at.x, at.z);
                        batch.Add(Matrix4x4.TRS(at, rot, new Vector3(m.Radius * 2f, 0.06f, w)));
                    }
                }
                if (batch.Count > 0) Flush(cube, rpRim);
            }
        }
    }
}
