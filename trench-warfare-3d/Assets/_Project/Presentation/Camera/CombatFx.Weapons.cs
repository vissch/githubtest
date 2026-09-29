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
                    return Look(0.8f, 0.8f, 0.8f, 0.6f, true, 0.18f, 0.6f, 0.45f);
                case InfantryArchetype.Machinegunner:   // a long flare, a smoking barrel, a broken stream with every third round bright
                    return Stream(Look(1.35f, tracerRound ? 1.3f : 0.7f, 1.1f, 1.3f, true, 0.18f, 1f, 1f), round, tracerRound);
                case InfantryArchetype.Sniper:   // one big flash, a thin white-hot needle that lingers, a heavy kick of dirt
                    return Look(1.5f, 1.3f, 1.5f, 1.6f, true, 0.22f, 2.2f, 1f);
                case InfantryArchetype.Officer:  // a carbine: a crisp pop, hardly a wisp
                    return Look(0.8f, 0.9f, 0.9f, 0.3f, true, 0.12f, 0.8f, 0.7f);   // smaller and quicker than the SMG's cone (critique r7)
                case InfantryArchetype.Shield:   // a pistol behind the plate: a short spit
                    return Look(0.6f, 0.7f, 0.7f, 0.5f, true, 0.18f, 0.6f, 0.3f);
                case InfantryArchetype.Jetpack:  // a machine pistol: tiny flares that flicker three times (CombatFx.Close.cs)
                    return Look(0.5f, 0.8f, 0.8f, 0.6f, true, 0.18f, 0.55f, 0.4f);
                case VehicleArchetype.Maw: case VehicleArchetype.Tusk: case VehicleArchetype.Breaker: case VehicleArchetype.Skimmer:
                    // a hull or coaxial machine gun: heavier than a man's, tracer rounds as the MG's, no case in the open
                    return Stream(Look(1.25f, tracerRound ? 1.4f : 0.8f, 1.2f, 1.1f, false, 0.18f, 1f, 1f), round, tracerRound);
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
        /// <summary>The most a class's muzzle book glows by day: FlareGlow's close-up 2.6 was for the additive Muzzle card; a
        /// premultiplied fire book at that clips its core, body and fringe to one white bloom on snow (iso4).</summary>
        public const float ClassBookDayGlow = 1.6f;
        /// <summary>The brake's growth by day: with the glow capped its cross shows, 2.5 men wide at 1.25 (r19 "a firework").</summary>
        public const float BrakeDay = 0.8f;
        /// <summary>The brake's height by day, x scale: its side jets thrown 3 men wide read as a firework (r20).</summary>
        public const float BrakeDayTallest = 1.0f;
        /// <summary>The SMG's burst by day: its fat cone and petals 2 men long each read as a firework (r20, r21).</summary>
        public const float BurstDay = 0.75f;

        public static float FlareGlow(bool night, float closeUp, bool classLooks) => night ? 3.2f : classLooks ? Mathf.Lerp(1.6f, 2.6f, closeUp) : 1.6f;

        /// <summary>A heavy round keeps a little weight up close (TracerLook thins every round to 0.3 among the men); x2 made a
        /// sniper's round a lightsaber and the MG's a glass bar (critique lin6), so 1.15.</summary>
        public static float TracerWidthAt(float width, float closeUp) => width > 1f ? width * Mathf.Lerp(1f, 1.15f, closeUp) : width;

        /// <summary>By day among the men a round's streak is halved: 6 m cream bars read as poles lying on the snow (lin6).</summary>
        public static float DayStreakAt(bool night, float closeUp, bool classLooks) => night || !classLooks ? 1f : Mathf.Lerp(1f, 0.5f, closeUp);

        /// <summary>By day up close the flare is drawn from the gun-blast fire book, whose soot fringe shows on snow; the additive
        /// Muzzle card is light added to white and vanished (lin5, lin6). Past this closeness only.</summary>
        public const float DayFlareCloseUp = 0.3f;
        public static bool DayFlare(bool night, float closeUp, bool classLooks) => classLooks && !night && closeUp >= DayFlareCloseUp;

        /// <summary>The class's own muzzle book (critique la5: one book scaled can never make classes differ in shape), or
        /// null for the rifle, which keeps the old flare (Muzzle, or GunBlast by day up close).</summary>
        public static FlipbookFx.Book? MuzzleBookOf(ArmsKind kind)
        {
            switch (kind)
            {
                case ArmsKind.Sniper: return FlipbookFx.Book.MuzzleBrake;
                case ArmsKind.Mg: case ArmsKind.HullMg: return FlipbookFx.Book.MuzzleStream;
                case ArmsKind.Pistol: case ArmsKind.MachinePistol: return FlipbookFx.Book.MuzzlePop;
                case ArmsKind.Smg: return FlipbookFx.Book.MuzzleBurst;
                case ArmsKind.Carbine: return FlipbookFx.Book.MuzzleCarbine;
                default: return null;
            }
        }

        /// <summary>A class book's width up close, x its flare: the brake's cross and the MG's tongue a little bigger to read at
        /// 25 px, the pop a little smaller (critique r6).</summary>
        /// <summary>The widest a class book is drawn, x the men's scale.</summary>
        public const float ClassBookMost = 4.5f;

        /// <summary>Up close a class book is drawn this much bigger than the flare it replaces: its drawing uses the middle
        /// of its cell, and at the flare's size a petal was 3 px (critique r12: about 3x too small to show its shape).</summary>
        public const float BookGrow = 2.5f, BookGrowNight = 2.0f;   // at night a little smaller: its glow is dimmed as well (r13 bonfires, r14 flecks)
        /// <summary>A class book is capped by its HEIGHT (across the barrel), not its length: a width cap cut the MG's tongue,
        /// whose identity is its length, and let nothing stop a fan ballooning (critique r14).</summary>
        public const float ClassBookTallest = 1.6f;

        public static float MuzzleScale(FlipbookFx.Book book, float closeUp)
        {
            float k = book == FlipbookFx.Book.MuzzleBrake ? (SceneMood.Night ? 1.25f : BrakeDay) : book == FlipbookFx.Book.MuzzleStream ? 1.4f : book == FlipbookFx.Book.MuzzlePop ? 0.9f
                    : book == FlipbookFx.Book.MuzzleBurst && !SceneMood.Night ? BurstDay : 1f;
            return Mathf.Lerp(1f, k * (SceneMood.Night ? BookGrowNight : BookGrow), closeUp);
        }

        /// <summary>A flame nearly end-on (just short of the Star pop) blooms into a soft ellipse: its glow x0.7 there.</summary>
        public static float GrazeGlow(Vector3 barrel, Vector3 camForward)
        {
            float d = Mathf.Abs(Vector3.Dot(barrel, camForward));
            return d > 0.45f && d <= 0.6f ? 0.7f : 1f;
        }

        /// <summary>A man firing away from the eye hides his own flare behind his body (critique la4): up close its card is
        /// pushed further out along the barrel and lifted, in proportion to how squarely he faces away.</summary>
        /// <summary>A flame seen end-on (the barrel along the eye's line) is a sheet edge-on: drawn as a pop (a Star) instead.</summary>
        public static bool EndOn(Vector3 barrel, Vector3 camForward, bool classLooks) => classLooks && Mathf.Abs(Vector3.Dot(barrel, camForward)) > 0.75f;

        /// <summary>A three-quarter view (|dot| 0.6 to 0.75): the class's book drawn foreshortened (x0.6) with a small star on it,
        /// so the shape survives; past 0.75 only the star (critique r7: end-on stars hid every book for men facing the eye).</summary>
        public static bool ThreeQuarter(Vector3 barrel, Vector3 camForward, bool classLooks)
        {
            float d = Mathf.Abs(Vector3.Dot(barrel, camForward));
            return classLooks && d > 0.6f && d <= 0.75f;
        }

        public static Vector3 FlareClear(Vector3 barrel, Vector3 camForward, float scale, float flare, float closeUp, bool classLooks)
        {
            if (!classLooks || closeUp <= 0f) return Vector3.zero;
            float away = Mathf.InverseLerp(0.3f, 0.8f, Vector3.Dot(barrel, camForward)) * closeUp;
            return Vector3.up * (0.3f * scale * away) + barrel * (flare * 0.45f * away);
        }

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
