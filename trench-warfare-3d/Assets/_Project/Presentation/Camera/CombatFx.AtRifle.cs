// Phase: VFX pass (goal fire-at-rifle-look) — the anti-tank rifle's miss at a hull. TankRenderer.cs:1499 already
// draws every VehicleArmourHit (CombatFx.Close.cs ArmsKind.AtRifle: a spark, a flash and plate flakes), but
// DirectFire.cs adds no event for a round that misses, so an AT shot that missed a tank left no mark at all. A
// small ring remembers each AT shot at a hull until either its VehicleArmourHit lands (drawn already; this only
// settles the entry) or a tick passes with none — the spurt past the hull. Never settle inside the Shot case
// itself (CombatFx.cs): within a tick Shot precedes VehicleArmourHit, so the miss can only be known a tick later.
using UnityEngine;
using TW.Sim;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        struct HullShot { public int Target; public uint Tick; public Vector3 To; public Vector3 Dir; public float Scale; public float Delay; public bool InUse; }

        const int HullShotRing = 32;
        readonly HullShot[] hullShots = new HullShot[HullShotRing];
        int hullShotNext;

        /// <summary>Shots at a hull taken, and how many were settled each way: the probe `tw eval` reads
        /// (AtHullShots == AtHullStrikes + AtHullSpurts).</summary>
        public static int AtHullShots, AtHullStrikes, AtHullSpurts;

        /// <summary>Whether an AT rifle's shot at a hull that was not struck this tick draws the spurt past it —
        /// pure, so a test can pin it without a world.</summary>
        public static bool SpurtsPastHull(byte archetype, bool hullTarget, bool struckThisTick)
            => hullTarget && !struckThisTick && KindOf(archetype) == ArmsKind.AtRifle;

        /// <summary>CombatFx.cs's Shot case: an AT rifle fired at a hull. Recorded, not drawn yet — its
        /// VehicleArmourHit (if any) arrives on the same tick or the next, settled in Update.</summary>
        void NoteHullShot(SimWorld w, SimEvent e, Vector3 from, Vector3 to, float scale, float delay)
        {
            int slot = hullShotNext;
            hullShotNext = (hullShotNext + 1) % hullShots.Length;
            if (hullShots[slot].InUse) AtHullShots--;   // the ring is full: drop the oldest entry unresolved and do not count it (the identity stays true)
            hullShots[slot] = new HullShot { Target = e.B, Tick = e.Tick, To = to, Dir = (to - from).normalized, Scale = scale, Delay = delay, InUse = true };
            AtHullShots++;
        }

        /// <summary>CombatFx.cs's OnSimEvent: a VehicleArmourHit settles the newest matching unresolved shot — the
        /// hit side is already drawn by TankRenderer, so this only clears the entry.</summary>
        void NoteHullHit(SimEvent e)
        {
            for (int i = 0; i < hullShots.Length; i++)
            {
                if (!hullShots[i].InUse || hullShots[i].Target != e.A) continue;
                hullShots[i].InUse = false;
                AtHullStrikes++;
                return;
            }
        }

        /// <summary>Update(): every shot whose tick has passed with no VehicleArmourHit — the round went past the
        /// hull. A spurt where it crossed and a puff of dust on the ground under it, jittered off the shot's own
        /// tick and target (never UnityEngine.Random: no class draw takes from the shared stream).</summary>
        void ResolveHullShots(uint tick)
        {
            if (classArms <= 0f || books == null || !books.Ready) return;
            for (int i = 0; i < hullShots.Length; i++)
            {
                if (!hullShots[i].InUse || hullShots[i].Tick >= tick) continue;
                var s = hullShots[i];
                hullShots[i].InUse = false;
                AtHullSpurts++;
                uint h = (uint)s.Tick * 31u + (uint)s.Target;
                books.Add(FlipbookFx.Book.Spurt, s.To, 1.1f * s.Scale, 0.5f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (((h & 1u) != 0u) ? FlipbookFx.Kind.Mirror : 0),
                    grow: 0.3f, alpha: 0.85f, pop: 0.3f, delay: s.Delay);
                Vector3 ground = s.To + s.Dir * 2.5f;
                if (Host != null && Host.Local != null)
                {
                    ground.y = RenderGround.Sample(Host.Local.Map, ground.x, ground.z);
                    books.Add(FlipbookFx.Book.Puff, ground, 1.6f * s.Scale, 0.6f, ((h & 2u) != 0u) ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                        grow: 0.4f, alpha: 0.75f, pop: 0.3f, delay: s.Delay);
                }
            }
        }
    }
}
