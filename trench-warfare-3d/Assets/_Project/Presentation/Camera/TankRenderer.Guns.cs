// Phase: VFX pass (owner, 2026-09-28: "each class unit has a different vfx specific to that class ... think of mortars") —
// part of TankRenderer. Every machine's gun drew the same shot: one 4.5 m blast, one heavy straight tracer, the same smoke,
// whether it was the Tusk's 37 mm, the Pavise's long gun or the Kettle's mortar lobbing over a trench. GunFor sizes the
// blast, the round and the smoke by the machine, and an indirect gun (the Kettle's mortar, the Salvo's rockets) throws its
// round in an ARC: a short streak up and over onto the burst. The sim fires and bursts on consecutive ticks (the round has
// no flight: TankGunnery adds the impact the tick it fires), so the arc is quick (ArcSeconds), and it never holds the burst
// back: men do not die before the shell is seen to land. fx.classArms 0 draws every gun as before.
using UnityEngine;
using TW.Sim;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        /// <summary>A machine's gun as drawn: its blast's width (m), the round's tracer width, the smoke's size (x the old),
        /// and whether the round goes up and over (an indirect gun).</summary>
        public struct GunLook
        {
            public float Blast, Tracer, Smoke; public bool Arc;
            public static readonly GunLook Old = new GunLook { Blast = 4.5f, Tracer = 2.2f, Smoke = 1f };
        }

        public const float ArcSeconds = 0.16f;   // the arc's whole flight on screen
        public const int ArcSegments = 8;

        public static GunLook GunFor(byte archetype)
        {
            switch (archetype)
            {
                case VehicleArchetype.Tusk: return new GunLook { Blast = 3.0f, Tracer = 1.6f, Smoke = 0.8f };                 // a 37 mm
                case VehicleArchetype.Pavise: case VehicleArchetype.Banner: return new GunLook { Blast = 6.0f, Tracer = 2.8f, Smoke = 1.35f };   // the long guns
                case VehicleArchetype.Kettle: return new GunLook { Blast = 2.6f, Tracer = 1.4f, Smoke = 1.2f, Arc = true };   // the mortar over its back
                case VehicleArchetype.Salvo: return new GunLook { Blast = 3.2f, Tracer = 1.2f, Smoke = 1.6f, Arc = true };    // a box of rockets
                default: return GunLook.Old;   // the Maw's and the Pincer's 6-pdrs, and anything not named
            }
        }

        /// <summary>The point `t` (0..1) of the arc from `from` to `to`: a parabola whose top stands `apex` over the straight line.</summary>
        public static Vector3 ArcPoint(Vector3 from, Vector3 to, float apex, float t) => Vector3.Lerp(from, to, t) + Vector3.up * (4f * apex * t * (1f - t));

        /// <summary>How high an indirect round climbs over a throw of `distance` m.</summary>
        public static float ArcApex(float distance) => Mathf.Clamp(distance * 0.3f, 6f, 40f);

        /// <summary>The indirect round's arc as ArcSegments short tracers, each shown a little after the one before, so a
        /// bright streak runs up and over onto the burst in ArcSeconds.</summary>
        void ThrowArc(Vector3 from, Vector3 to, byte team, float width)
        {
            var fx = Fx(); if (fx == null) return;
            float apex = ArcApex(Vector3.Distance(from, to));
            for (int i = 0; i < ArcSegments; i++)
            {
                float a = i / (float)ArcSegments, b = (i + 1) / (float)ArcSegments;
                fx.AddTracer(ArcPoint(from, to, apex, a), ArcPoint(from, to, apex, b), team, width, a * ArcSeconds);
            }
        }
    }
}
