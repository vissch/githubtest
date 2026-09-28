// Phase: VFX pass (owner, 2026-09-28: "each class unit has a different vfx specific to that class" and "the most common vfx
// first ... we have to optimize this") — part of CombatFx. The census (VfxEventCensusTests) puts small arms at about 99 % of
// every event the picture draws, and until now every one of them - rifle, SMG, MG, sniper, pistol, a tank's hull gun - drew
// the same flare, tracer, spurt and smoke. The shooter's class is read from the world (w.Archetype[shooter]); the sim is
// untouched. ArmsFor scales what the shot path already draws: no card, chunk or random draw is added, so the shared
// UnityEngine.Random stream and the card budget are as they were. fx.classArms 0 draws every class as the rifle (the old look).
// Also here: the view (camera, zoom, unit scale) read once a frame for the shot and hit paths, which ran Camera.main and
// TryGetComponent<IZoomSource> for each of about sixty events a second.
using UnityEngine;
using TW.Sim;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        public const string ClassArmsKnob = "fx.classArms";
        float classArms = 1f;   // knob fx.classArms (Awake): 1 each class its own look, 0 all as the rifle

        public static float ReadClassArms() => Mathf.Clamp01(Knobs.Get(ClassArmsKnob, 1f));

        /// <summary>How a class's small arms draw, as multiples of the rifle's: the muzzle flare's size, the tracer's width (1 = the
        /// usual round), the dirt spurt's size, the muzzle smoke's size, and whether a case flies from the breech. FlareLife is the
        /// flare card's life (s); Flared and Smoked say whether THIS round draws its flare card and its smoke puff at all.</summary>
        public struct ArmsLook
        {
            public float Flare, Tracer, Spurt, Smoke, FlareLife; public bool Case, Flared, Smoked;
            public static readonly ArmsLook Rifle = new ArmsLook { Flare = 1f, Tracer = 1f, Spurt = 1f, Smoke = 1f, FlareLife = 0.18f, Case = true, Flared = true, Smoked = true };
        }

        /// <summary>A machine gun's stream (the census: 72 % of every shot in a mixed battle, 7 rounds a second a gun). A flare
        /// card of 0.18 s and a smoke puff of 1.1-1.9 s for every round kept about ten puffs alive a gunner and filled the chunk
        /// pool (420) in a firefight; the flare lives 0.3 s on every second round and one bigger puff on every third, which
        /// reads as the same burning muzzle and the same hanging smoke for half and a third of the cards.</summary>
        static ArmsLook Stream(ArmsLook look, uint round)
        {
            look.Flared = (round & 1u) == 0u; look.FlareLife = 0.3f;
            look.Smoked = round % 3u == 0u; look.Smoke *= 1.45f;
            return look;
        }

        /// <summary>The look of a shot by the shooter's class. Every third round of a machine gun is a tracer round, drawn
        /// heavier (by the tick and the gunner, so the same round every run).</summary>
        public static ArmsLook ArmsFor(byte archetype, uint tick, int shooter)
        {
            uint round = tick + (uint)shooter;
            bool tracerRound = round % 3u == 0u;
            switch (archetype)
            {
                case InfantryArchetype.Assault:   // a submachine gun: small quick flares, light rounds
                    return Look(0.7f, 0.8f, 0.8f, 0.6f, true);
                case InfantryArchetype.Machinegunner:   // a long flare, a smoking barrel, a stream with every third round bright
                    return Stream(Look(1.35f, tracerRound ? 1.7f : 0.9f, 1.1f, 1.3f, true), round);
                case InfantryArchetype.Sniper:   // one big flash, a long bright round, a heavy kick of dirt
                    return Look(1.6f, 1.9f, 1.5f, 1.6f, true);
                case InfantryArchetype.Officer:  // a carbine
                    return Look(0.85f, 0.9f, 0.9f, 0.8f, true);
                case InfantryArchetype.Shield:   // a pistol behind the plate
                    return Look(0.6f, 0.7f, 0.7f, 0.5f, true);
                case InfantryArchetype.Jetpack:  // a machine pistol
                    return Look(0.75f, 0.8f, 0.8f, 0.6f, true);
                case VehicleArchetype.Maw: case VehicleArchetype.Tusk: case VehicleArchetype.Breaker: case VehicleArchetype.Skimmer:
                    // a hull or coaxial machine gun: heavier than a man's, tracer rounds as the MG's, no case in the open
                    return Stream(Look(1.25f, tracerRound ? 1.8f : 1.0f, 1.2f, 1.1f, false), round);
                default:
                    return ArmsLook.Rifle;   // rifleman, para, repair man, and anything not named
            }
        }

        static ArmsLook Look(float flare, float tracer, float spurt, float smoke, bool brass) =>
            new ArmsLook { Flare = flare, Tracer = tracer, Spurt = spurt, Smoke = smoke, FlareLife = 0.18f, Case = brass, Flared = true, Smoked = true };

        ArmsLook ArmsNow(SimWorld w, SimEvent e) =>
            classArms > 0f && e.A >= 0 && e.A < w.Archetype.Length ? ArmsFor(w.Archetype[e.A], e.Tick, e.A) : ArmsLook.Rifle;

        // the view once a frame (task: the shot and hit paths read it per event)
        int viewFrame = -1; Camera viewCam; float viewZoom, viewScale = 1f;

        /// <summary>The main camera, its zoom and the men's drawn scale at that zoom, read once a frame.</summary>
        void ViewNow(out Camera cam, out float zoom, out float scale)
        {
            if (viewFrame != Time.frameCount)
            {
                viewFrame = Time.frameCount;
                viewCam = Camera.main;
                viewZoom = viewCam != null && viewCam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
                viewScale = units != null ? units.UnitScale * Mathf.Clamp(viewZoom / Mathf.Max(1f, units.GrowFromZoom), 1f, units.MaxGrow) : 1f;
            }
            cam = viewCam; zoom = viewZoom; scale = viewScale;
        }
    }
}
