// Phase: A5c (2026-09-28) — each machine drives in a way of its own (owner: "make sure the vehicle has its own unique
// animation flares, in speed rhythm, style"). Before this every tracked machine rode one spring (omega 7), one engine
// buzz (41 rad/s) and no response at all to gathering way, braking or turning; every walker stepped with one gait. The
// sim now gives each machine momentum and a pivot share of its own (VehicleProfile.Accel/Brake/PivotSpeed); this is the
// picture's half: how the body answers it. One row per machine, read once when its view is made (View.Style).
//  - the ride: pitch and roll spring (Omega, Zeta below 1 rocks on after a stop), heave spring (HeaveOmega);
//  - squat and dive: pitch per m/s^2 of way gathered or shed (a cushion noses down into it: negative);
//  - lean: roll per (rad/s x m/s) through a turn, out on springs, in (negative) on a cushion or a gunship;
//  - the engine: a buzz (Rumble*, its own note), a slow beat at full throttle (Chug*: a big engine's surge), and how
//    much it revs as it pulls away (Rev: the exhaust quickens before the speed shows);
//  - the running gear's rhythm: a pitch nod per ClatterEvery metres travelled, so it beats faster the faster it goes;
//  - a walker: its swing time and step height as shares of WalkerGait's, and the thump each footfall puts on the body.
using UnityEngine;
using TW.Sim;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        public struct DriveStyle
        {
            public float Omega, Zeta, HeaveOmega;          // pitch and roll spring, its damping ratio, heave spring
            public float Squat, Lean;                      // rad per m/s^2 gathered; rad per (rad/s x m/s) of turn
            public float RumbleAmp, RumbleThrottle, RumbleRate;   // m of heave at idle, m more at full throttle, rad/s
            public float Clatter, ClatterEvery;            // rad of pitch nod, per this many metres of travel
            public float Chug, ChugHz;                     // m of heave at full throttle, and its beat
            public float Rev, Puff;                        // throttle per m/s^2 gathered; exhaust interval share
            public float Swing, Arc, Stomp;                // walker: swing time share, step height share, m/s per footfall
        }

        /// <summary>The ride before this (2026-09-28): every machine without a row of its own still rides it.</summary>
        public static readonly DriveStyle PlainStyle = new DriveStyle
        {
            Omega = 7f, Zeta = 1f, HeaveOmega = 10f, RumbleAmp = 0.006f, RumbleThrottle = 0.01f, RumbleRate = 41f,
            ClatterEvery = 1f, Puff = 1f, Swing = 1f, Arc = 1f,
        };

        public static DriveStyle StyleFor(byte archetype)
        {
            var s = PlainStyle;
            switch (archetype)
            {
                // a landship: slow to settle, rears as it gets going, a deep slow engine beating under it, heavy puffs
                case VehicleArchetype.Maw:
                    s.Omega = 4.5f; s.Zeta = 0.8f; s.HeaveOmega = 7f; s.Squat = 0.09f; s.Lean = 0.04f;
                    s.RumbleAmp = 0.010f; s.RumbleThrottle = 0.012f; s.RumbleRate = 24f; s.Clatter = 0.005f; s.ClatterEvery = 0.95f;
                    s.Chug = 0.035f; s.ChugHz = 1.1f; s.Rev = 0.5f; s.Puff = 1.4f; break;
                // the light tank: stiff, a high buzz, a quick patter off its short links, quick thin puffs
                case VehicleArchetype.Tusk:
                    s.Omega = 9f; s.Zeta = 0.9f; s.HeaveOmega = 13f; s.Squat = 0.035f; s.Lean = 0.035f;
                    s.RumbleAmp = 0.004f; s.RumbleThrottle = 0.007f; s.RumbleRate = 62f; s.Clatter = 0.004f; s.ClatterEvery = 0.42f;
                    s.Rev = 0.35f; s.Puff = 0.75f; break;
                // the assault tank: sits down hard on its tail as it lunges into a charge
                case VehicleArchetype.Breaker:
                    s.Omega = 6f; s.Zeta = 0.7f; s.HeaveOmega = 9f; s.Squat = 0.055f; s.Lean = 0.05f;
                    s.RumbleAmp = 0.008f; s.RumbleThrottle = 0.012f; s.RumbleRate = 33f; s.Clatter = 0.006f; s.ClatterEvery = 0.7f;
                    s.Chug = 0.02f; s.ChugHz = 1.8f; s.Rev = 0.6f; s.Puff = 0.9f; break;
                case VehicleArchetype.Brute:
                    s.Omega = 6.5f; s.Zeta = 0.85f; s.HeaveOmega = 9f; s.Squat = 0.05f; s.Lean = 0.045f;
                    s.RumbleAmp = 0.007f; s.RumbleThrottle = 0.01f; s.RumbleRate = 36f; s.Clatter = 0.005f; s.ClatterEvery = 0.6f;
                    s.Chug = 0.012f; s.ChugHz = 1.5f; s.Rev = 0.45f; break;
                // a loaded rack: soft, under-damped, rocks on after it stops and leans in a turn
                case VehicleArchetype.Salvo:
                    s.Omega = 4.8f; s.Zeta = 0.35f; s.HeaveOmega = 8f; s.Squat = 0.07f; s.Lean = 0.06f;
                    s.RumbleAmp = 0.006f; s.RumbleThrottle = 0.009f; s.RumbleRate = 30f; s.Clatter = 0.004f; s.ClatterEvery = 0.8f;
                    s.Chug = 0.01f; s.ChugHz = 1.3f; s.Rev = 0.4f; s.Puff = 1.1f; break;
                // the ambulance: tall and bouncy, rolls out in a turn, jolts along
                case VehicleArchetype.Mercy:
                    s.Omega = 5.5f; s.Zeta = 0.3f; s.HeaveOmega = 8f; s.Squat = 0.05f; s.Lean = 0.10f;
                    s.RumbleAmp = 0.005f; s.RumbleThrottle = 0.008f; s.RumbleRate = 48f; s.Clatter = 0.009f; s.ClatterEvery = 0.55f;
                    s.Rev = 0.5f; s.Puff = 0.8f; break;
                // on a cushion: floats, noses down into the way it gathers, banks into a turn, a turbine's whine
                case VehicleArchetype.Skimmer:
                    s.Omega = 3f; s.Zeta = 0.55f; s.HeaveOmega = 6f; s.Squat = -0.05f; s.Lean = -0.07f;
                    s.RumbleAmp = 0.003f; s.RumbleThrottle = 0.004f; s.RumbleRate = 70f; s.Rev = 0.6f; s.Puff = 0.6f; break;
                case VehicleArchetype.Hopper:
                    s.Omega = 3f; s.Zeta = 0.5f; s.HeaveOmega = 5f; s.Squat = -0.06f; s.Lean = -0.09f;
                    s.RumbleAmp = 0.003f; s.RumbleThrottle = 0.004f; s.RumbleRate = 70f; s.Rev = 0.6f; s.Puff = 0.6f; break;
                // the walkers: how they step, not how they ride (their tilt comes off their feet)
                case VehicleArchetype.Pincer:  s.Swing = 0.8f;  s.Arc = 0.85f; s.Stomp = 0.25f; s.HeaveOmega = 12f; s.RumbleAmp = 0.003f; break;   // six legs scuttle
                case VehicleArchetype.Kettle:  s.Swing = 1.0f;  s.Arc = 1.25f; s.Stomp = 0.55f; s.HeaveOmega = 9f; break;                          // high-stepping, bouncy
                case VehicleArchetype.Censer:  s.Swing = 0.75f; s.Arc = 1.0f;  s.Stomp = 0.3f;  s.HeaveOmega = 12f; s.RumbleAmp = 0.003f; break;   // quick and skittery
                case VehicleArchetype.Pavise:  s.Swing = 1.3f;  s.Arc = 0.8f;  s.Stomp = 1.3f;  s.HeaveOmega = 8f; break;                          // plants each foot hard
                case VehicleArchetype.Banner:  s.Swing = 1.25f; s.Arc = 1.4f;  s.Stomp = 0.45f; s.HeaveOmega = 9f; break;                          // tall, stately steps
                case VehicleArchetype.Redoubt: s.Swing = 1.4f;  s.Arc = 0.7f;  s.Stomp = 1.7f;  s.HeaveOmega = 7f; s.RumbleAmp = 0.009f; break;    // the ponderous blockhouse
                case VehicleArchetype.Croaker: s.Swing = 0.9f;  s.Arc = 1.5f;  s.Stomp = 1.2f;  s.HeaveOmega = 8f; break;                          // two legs, lunging strides
            }
            return s;
        }

        /// <summary>What the style adds to the ride this frame, on top of the ground: squat or dive and the turn's lean
        /// into the springs' targets (pitch, roll), and the engine's beat and the running gear's nod beside them
        /// (View.PitchFx, View.Bob), which are drawn but not sprung, so a fast rhythm is not smoothed away.</summary>
        static void Flair(View v, float now, ref float pitch, ref float roll)
        {
            var st = v.Style;
            float speed = Mathf.Abs(v.Speed);
            pitch += Mathf.Clamp(st.Squat * v.Accel, -0.12f, 0.12f);
            roll += Mathf.Clamp(-st.Lean * v.YawRate * speed, -0.12f, 0.12f);
            float moving = Mathf.Clamp01(speed / 0.6f);
            float beat = v.Stalled ? 0f : st.Chug * v.Throttle * Mathf.Sin((now * st.ChugHz + v.Slot * 0.29f) * Mathf.PI * 2f);
            v.PitchFx = st.Clatter * moving * Mathf.Sin(v.Travel / Mathf.Max(0.05f, st.ClatterEvery) * Mathf.PI * 2f) + beat * 0.4f;
            v.Bob = beat;
        }
    }
}
