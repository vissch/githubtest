// Phase: CelFx orbital laser reference match (2026-11-14) - diffuse energy lighting for the painted surfaces.
// Eight bounded records are uploaded on emission; the shader evaluates their envelopes with CelFx's clock.
using System;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class OrbitalLaserLighting
    {
        public const int Capacity = 8;
        static readonly Vector4[] positions = new Vector4[Capacity];
        static readonly Vector4[] colours = new Vector4[Capacity];
        static readonly Vector4[] axes = new Vector4[Capacity];
        static readonly Vector4[] timing = new Vector4[Capacity];
        static readonly int PositionsId = Shader.PropertyToID("_OrbitalLightPositions");
        static readonly int ColoursId = Shader.PropertyToID("_OrbitalLightColours");
        static readonly int AxesId = Shader.PropertyToID("_OrbitalLightAxes");
        static readonly int TimingId = Shader.PropertyToID("_OrbitalLightTiming");
        static readonly int CountId = Shader.PropertyToID("_OrbitalLightCount");
        static int count;

        static OrbitalLaserLighting()
        {
            SceneStatics.Register(nameof(OrbitalLaserLighting), ResetSession);
        }

        /// <summary>Add a diffuse light above a ground centre. Tint is linear radiance; values above one are valid.
        /// Born uses CelFx.Now, so stills, captures and overlapping effects use the same clock as their geometry.</summary>
        public static void Add(Vector3 centre, float born, float life, Color tint, float radius)
            => Add(centre, born, life, tint, radius, Vector3.right);

        public static void Add(Vector3 centre, float born, float life, Color tint, float radius, Vector3 right)
        {
            if (life <= 0f || radius <= 0f) return;
            if (count == Capacity)
            {
                for (int i = 1; i < Capacity; i++) Copy(i, i - 1);
                count--;
            }
            positions[count] = new Vector4(centre.x, centre.y + 1.1f, centre.z, radius);
            colours[count] = new Vector4(tint.r, tint.g, tint.b, 0f);
            right.y = 0f;
            right = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            axes[count] = new Vector4(right.x, right.y, right.z, 0f);
            timing[count] = new Vector4(born, life, Mathf.Min(0.12f, life * 0.25f), Mathf.Min(0.16f, life * 0.25f));
            count++;
            Upload();
        }

        /// <summary>Remove expired entries permanently, so they cost no pixels and cannot return after a clock rewind.</summary>
        public static void Prune(float now)
        {
            int kept = 0;
            for (int i = 0; i < count; i++)
            {
                if (now >= timing[i].x + timing[i].y) continue;
                if (kept != i) Copy(i, kept);
                kept++;
            }
            if (kept == count) return;
            Array.Clear(positions, kept, count - kept);
            Array.Clear(colours, kept, count - kept);
            Array.Clear(axes, kept, count - kept);
            Array.Clear(timing, kept, count - kept);
            count = kept;
            Upload();
        }

        static void Copy(int from, int to)
        {
            positions[to] = positions[from];
            colours[to] = colours[from];
            axes[to] = axes[from];
            timing[to] = timing[from];
        }

        static void Upload()
        {
            if (count > 0)
            {
                Shader.SetGlobalVectorArray(PositionsId, positions);
                Shader.SetGlobalVectorArray(ColoursId, colours);
                Shader.SetGlobalVectorArray(AxesId, axes);
                Shader.SetGlobalVectorArray(TimingId, timing);
            }
            Shader.SetGlobalFloat(CountId, count);
        }

        /// <summary>Drop the bounded light history and disable its shader path when Play ends.</summary>
        public static void ResetSession()
        {
            count = 0;
            Array.Clear(positions, 0, Capacity);
            Array.Clear(colours, 0, Capacity);
            Array.Clear(axes, 0, Capacity);
            Array.Clear(timing, 0, Capacity);
            Shader.SetGlobalFloat(CountId, 0f);
        }
    }
}
