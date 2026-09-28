// Phase: VFX pass (owner, 2026-09-28: "each class unit has a different vfx specific to that class" and "the most common vfx
// first ... we have to optimize this") — part of CombatFx. The census (VfxEventCensusTests) puts small arms at about 99 % of
// every event the picture draws, and until now every one of them - rifle, SMG, MG, sniper, pistol, a tank's hull gun - drew
// the same flare, tracer, spurt and smoke. The shooter's class is read from the world (w.Archetype[shooter]); the sim is
// untouched. ArmsFor scales what the shot path already draws: no card, chunk or random draw is added, so the shared
// UnityEngine.Random stream and the card budget are as they were. fx.classArms 0 draws every class as the rifle (the old look).
// Critique lin5 (22/100: "size multipliers on cards too small to see"): a class is now told apart by its round's LIFE and
// STREAK as well as its width (a sniper's long white streak that hangs, a pistol's short spit, a machine gun's dashes with
// every third round a real tracer), and the classes are spread further apart (a sniper's flare 2.4, an officer's crisp pop).
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
        /// flare card's life (s); Flared and Smoked say whether THIS round draws its flare card and its smoke puff at all.
        /// TracerLife and Streak: the round's time on screen and its streak's length, x the usual (TracerSeconds, 6-10 m).</summary>
        public struct ArmsLook
        {
            public float Flare, Tracer, Spurt, Smoke, FlareLife, TracerLife, Streak; public bool Case, Flared, Smoked;
            public static readonly ArmsLook Rifle = new ArmsLook { Flare = 1f, Tracer = 1f, Spurt = 1f, Smoke = 1f, FlareLife = 0.18f, TracerLife = 1f, Streak = 1f, Case = true, Flared = true, Smoked = true };
        }

        /// <summary>A machine gun's stream (the census: 72 % of every shot in a mixed battle, 7 rounds a second a gun). A flare
        /// card of 0.18 s and a smoke puff of 1.1-1.9 s for every round kept about ten puffs alive a gunner and filled the chunk
        /// pool (420) in a firefight; the flare lives 0.3 s on every second round and one bigger puff on every third, which
        /// reads as the same burning muzzle and the same hanging smoke for half and a third of the cards. The rounds between
        /// the tracer rounds are short quick dashes, so the stream reads broken, a bright round in every three.</summary>
        static ArmsLook Stream(ArmsLook look, uint round, bool tracerRound)
        {
            look.Flared = (round & 1u) == 0u; look.FlareLife = 0.3f;
            look.Smoked = round % 3u == 0u; look.Smoke *= 1.45f;
            if (tracerRound) { look.TracerLife = 1.3f; look.Streak = 1.2f; }
            else { look.TracerLife = 0.6f; look.Streak = 0.5f; }
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
                case InfantryArchetype.Assault:   // a submachine gun: small quick flares, short quick rounds
                    return Look(0.7f, 0.8f, 0.8f, 0.6f, true, 0.18f, 0.6f, 0.45f);
                case InfantryArchetype.Machinegunner:   // a long flare, a smoking barrel, a broken stream with every third round bright
                    return Stream(Look(1.35f, tracerRound ? 1.7f : 0.7f, 1.1f, 1.3f, true, 0.18f, 1f, 1f), round, tracerRound);
                case InfantryArchetype.Sniper:   // one big flash that hangs, a long white round, a heavy kick of dirt
                    return Look(2.4f, 1.9f, 1.5f, 1.6f, true, 0.3f, 2.2f, 1.6f);
                case InfantryArchetype.Officer:  // a carbine: a crisp pop, hardly a wisp
                    return Look(1.1f, 0.9f, 0.9f, 0.3f, true, 0.12f, 0.8f, 0.7f);
                case InfantryArchetype.Shield:   // a pistol behind the plate: a short spit
                    return Look(0.6f, 0.7f, 0.7f, 0.5f, true, 0.18f, 0.6f, 0.3f);
                case InfantryArchetype.Jetpack:  // a machine pistol: tiny flares that flicker three times (CombatFx.Close.cs)
                    return Look(0.5f, 0.8f, 0.8f, 0.6f, true, 0.18f, 0.55f, 0.4f);
                case VehicleArchetype.Maw: case VehicleArchetype.Tusk: case VehicleArchetype.Breaker: case VehicleArchetype.Skimmer:
                    // a hull or coaxial machine gun: heavier than a man's, tracer rounds as the MG's, no case in the open
                    return Stream(Look(1.25f, tracerRound ? 1.8f : 0.8f, 1.2f, 1.1f, false, 0.18f, 1f, 1f), round, tracerRound);
                default:
                    return ArmsLook.Rifle;   // rifleman, para, repair man, and anything not named
            }
        }

        static ArmsLook Look(float flare, float tracer, float spurt, float smoke, bool brass, float flareLife, float tracerLife, float streak) =>
            new ArmsLook { Flare = flare, Tracer = tracer, Spurt = spurt, Smoke = smoke, FlareLife = flareLife, TracerLife = tracerLife, Streak = streak, Case = brass, Flared = true, Smoked = true };

        ArmsLook ArmsNow(SimWorld w, SimEvent e) =>
            classArms > 0f && e.A >= 0 && e.A < w.Archetype.Length ? ArmsFor(w.Archetype[e.A], e.Tick, e.A) : ArmsLook.Rifle;

        /// <summary>The flare's base width (x the unit scale and the class) up close: the old 1.05 + 0.4 r at the standard view,
        /// 1.8 + 0.5 r among the men, where the old card was half a man and under a sixth of the frames (critique lin5).</summary>
        public static float FlareBase(float random, float closeUp, bool classLooks) =>
            classLooks ? Mathf.Lerp(1.05f, 1.8f, closeUp) + random * Mathf.Lerp(0.4f, 0.5f, closeUp) : 1.05f + random * 0.4f;

        /// <summary>The flare's life and its day glow up close (x 1.45 and 1.6 to 2.6 among the men): additive on snow at 1.6 was erased.</summary>
        public static float FlareLifeAt(float life, float closeUp, bool classLooks) => classLooks ? life * Mathf.Lerp(1f, 1.45f, closeUp) : life;
        public static float FlareGlow(bool night, float closeUp, bool classLooks) => night ? 3.2f : classLooks ? Mathf.Lerp(1.6f, 2.6f, closeUp) : 1.6f;

        /// <summary>A heavy round keeps its weight up close: TracerLook thins every round to 0.3 among the men, which took a
        /// 1.9 x sniper's round to a hair; a round over 1 x regains up to twice that.</summary>
        public static float TracerWidthAt(float width, float closeUp) => width > 1f ? width * Mathf.Lerp(1f, 2f, closeUp) : width;

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
