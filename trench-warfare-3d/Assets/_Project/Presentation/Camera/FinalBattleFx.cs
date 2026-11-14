// Phase: final CelFx battle integration (2026-11-14) - individual presentation events, never reference volleys.
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Presentation;
using TW.Presentation.VFX.Cel;
using TW.Presentation.VFX.Cel.Effects;

namespace TW.Presentation.Tactical
{
    public enum FinalBurstRoute { Legacy, Explosion, Artillery, Incendiary }

    public static class FinalBattleFx
    {
        public const float FlamePacketSeconds = .4f;
        static readonly CelFxRecipe FlamePacket = FlamethrowerRecipe.TimeScaled(FlamethrowerRecipe.Recipe, .12f, 0f);
        public static int Shots, Impacts, Bursts, Flames, BeamFrames;
        static FinalBattleFx() { SceneStatics.Register(nameof(FinalBattleFx), Reset); }
        static void Reset() { Shots = Impacts = Bursts = Flames = BeamFrames = 0; }
        static void Count(ref int counter, string route) { if (counter++ == 0) Debug.Log("FINAL_BATTLE_VFX " + route); }

        public static FinalBurstRoute Route(BlastShape shape, int source, bool water)
        {
            if (water) return FinalBurstRoute.Legacy;
            if (shape == BlastShape.Incendiary) return FinalBurstRoute.Incendiary;
            if (shape != BlastShape.Shell) return FinalBurstRoute.Legacy;
            return (source >= 0 && source <= SourceId.AbilityMax) || source == SourceId.Ship ||
                (source >= SourceId.EmplacementBase && source < SourceId.CookOff)
                ? FinalBurstRoute.Artillery : FinalBurstRoute.Explosion;
        }

        public static void Shot(Vector3 muzzle, Vector3 hit, uint seed, float delay)
        {
            float clock = CelFx.Clock;
            try
            {
                CelFx.Clock = CelFx.Now + Mathf.Max(0f, delay);
                CelFx.Play(BulletImpacts.Tracer, muzzle, hit - muzzle, hit, seed);
                Count(ref Shots, "shot");
            }
            finally { CelFx.Clock = clock; }
        }

        public static void Impact(Vector3 hit, Vector3 toward, float feetY, uint seed, bool damaged = true)
        {
            var ground = new Vector3(hit.x, feetY, hit.z);
            if (damaged) BulletImpacts.PlayImpact(hit - toward * 2f, hit, ground, ground, seed, true, 1);
            else CelFx.Play(BulletImpacts.Hit, hit - toward * 2f, toward, hit, seed);
            Count(ref Impacts, "impact");
        }

        public static bool Burst(BlastShape shape, int source, bool water, Vector3 at, Vector3 dir, uint seed)
        {
            var route = Route(shape, source, water);
            if (route == FinalBurstRoute.Legacy) return false;
            if (dir.sqrMagnitude < .001f) dir = Vector3.forward;
            if (route == FinalBurstRoute.Artillery) CelFx.Play(ArtilleryRecipe.Target, at, dir, at, seed);
            else if (route == FinalBurstRoute.Incendiary) CelFx.Play(FireBolts.GroundHit, at, dir, at, seed);
            else ExplosionRecipe.PlayShell(at, dir, seed);
            Count(ref Bursts, route == FinalBurstRoute.Artillery ? "artillery" : route == FinalBurstRoute.Incendiary ? "incendiary" : "explosion");
            return true;
        }

        public static void Flame(Vector3 muzzle, Vector3 target, uint seed)
        {
            FlamethrowerRecipe.Play(FlamePacket, muzzle, target, seed);
            Count(ref Flames, "flame");
        }

        public static void BeamCharge(Vector3 at, float strength, float seconds)
            => OrbitalLaserRecipe.PlayBattleCharge(at, strength, seconds);

        public static void Beam(Vector3 head, Vector3 right, float seconds, uint seed)
        {
            OrbitalLaserRecipe.PlayBattleFrame(head, right, seconds, seed);
            Count(ref BeamFrames, "orbital_beam");
        }
    }
}
