// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-8, look spec L05) — part of CombatFx. Behind fx.recipes,
// the shells of an off-map barrage are seen coming in: the sim schedules every payload ahead (OffMapAbilitySystem.
// Scheduled, read here and never written), so a ShellFall card (a shell streaking down onto its point, 8 frames at
// 12 fps) is laid IncomingLead before each shell's tick and ends as the shell lands (on game time: at a raised
// TimeScale the streak runs slow against the sim). Read once per sim tick; up close
// and at the standard view only (culled from IncomingFarZoom: at the overview the burst is the read). The Kettle's and
// the tanks' rounds land the tick they fire, so they have no incoming (a SIM flight time is owner question Q5).
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        public const float IncomingLead = 7f / 12f;     // the ShellFall book reaches its last frame (the streak on the ground) 7/12 s in
        const float IncomingCardLife = 1f;               // held a little past the landing, so the fade (the last third) starts after it
        public const float IncomingFarZoom = 120f;
        uint incomingTick = uint.MaxValue;
        OffMapAbilitySystem offMap;

        /// <summary>Whether a scheduled payload is a shell seen coming in (not a strafe's rounds, a gas or smoke canister, a
        /// beam or a drop): pure, so a test can hold it.</summary>
        public static bool IsIncoming(int kind, int ability) => kind == (int)PayloadKind.Shell && ability != (int)OffMapAbilityId.StrafeRun && ability != (int)OffMapAbilityId.Beam;

        /// <summary>How many ticks ahead of its landing a shell's streak is laid.</summary>
        public static uint IncomingLeadTicks(float tickSeconds) => (uint)Mathf.Max(1, Mathf.RoundToInt(IncomingLead / Mathf.Max(1e-4f, tickSeconds)));

        void TickIncoming()
        {
            if (recipes < 0.5f || books == null || !books.Ready) return;
            var w = Host.Local.World;
            uint tick = w.Tick;
            if (tick == incomingTick) return;
            uint from = incomingTick == uint.MaxValue || tick < incomingTick || tick - incomingTick > 8 ? tick : incomingTick + 1;   // catch up a few ticks, never replay a history
            incomingTick = tick;
            if (offMap == null) offMap = w.GetSystem<OffMapAbilitySystem>();
            if (offMap == null || !offMap.Scheduled.IsCreated || offMap.Scheduled.Length == 0) return;
            var cam = Camera.main;
            if (cam != null && cam.TryGetComponent<IZoomSource>(out var zs) && zs.CurrentZoom >= IncomingFarZoom) return;
            uint lead = IncomingLeadTicks(w.Config.TickSeconds);
            for (int i = 0; i < offMap.Scheduled.Length; i++)
            {
                var p = offMap.Scheduled[i];
                if (!IsIncoming(p.Kind, p.Ability) || p.Tick < from + lead || p.Tick > tick + lead) continue;
                Vector3 at = (Vector3)p.Pos; at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z);
                // caught up late (a frame that stepped several ticks): start the streak that far into its fall, not later
                int behind = (int)(tick + lead) - (int)p.Tick;   // >= 0 here; signed, so it cannot wrap
                float late = behind * w.Config.TickSeconds;
                books.Add(FlipbookFx.Book.ShellFall, at, 5f, IncomingCardLife, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, glow: SceneMood.Night ? 1.6f : 1f, startFrame: late * 12f);
            }
        }
    
        // L23: smouldering craters, a pool of SmoulderPool at most (a long card each)
        public const int SmoulderPool = 24;
        public const float SmoulderShare = 0.3f;
        const float SmoulderDelay = 2f;   // the burst's own smoke first
        readonly float[] smoulderUntil = new float[SmoulderPool];

        /// <summary>Whether a heavy burst at this place leaves its crater smouldering: 30 % of them, by a hash of the place
        /// (not a random draw, so the same shell smoulders every run). Pure, so a test can hold it.</summary>
        public static bool Smoulders(float x, float z) => Hash01(x, z, 41) < SmoulderShare;

        /// <summary>L23 (fx.recipes): a heavy dry burst may leave a thread of smoke standing in its crater for 20-40 s,
        /// fading as it goes; close up and at the standard view only (a pool of long cards costs).</summary>
        void SmoulderCrater(Vector3 p, float r, float far)
        {
            if (far > 1f || !Smoulders(p.x, p.z)) return;
            float now = Time.time;
            int slot = -1;
            for (int k = 0; k < SmoulderPool; k++) if (smoulderUntil[k] <= now) { slot = k; break; }
            if (slot < 0) return;   // the pool is full: the oldest threads are still standing
            float life = Mathf.Lerp(20f, 40f, Hash01(p.x, p.z, 43));
            smoulderUntil[slot] = now + SmoulderDelay + life;   // the card lives from its delay on
            books.Add(FlipbookFx.Book.Smoulder, p - Vector3.up * 0.3f, Mathf.Clamp(r * 0.6f, 2.5f, 5f), life, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored,
                alpha: 0.7f, delay: SmoulderDelay, startFrame: Hash01(p.x, p.z, 47) * 31f);
        }

        /// <summary>L24 (fx.recipes): wire cut (WireBreached: pos, scalar the gap's width): earth kicked up along the gap and
        /// the wire's snap, a spike of light (scattered round the cut: the event carries no heading). No sparks thrown (they take random draws).</summary>
        void OnWireBreached(SimEvent e)
        {
            if (recipes < 0.5f || books == null || !books.Ready) return;
            Vector3 at = (Vector3)e.Pos; at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z);
            float half = Mathf.Clamp(e.Scalar * 0.5f, 0.5f, 6f);
            for (int k = 0; k < 3; k++)
            {
                float a = Hash01(at.x, at.z, 50 + k) * 6.2832f, d = half * Hash01(at.x, at.z, 60 + k);
                books.Add(FlipbookFx.Book.Spurt, at + new Vector3(Mathf.Cos(a) * d, 0f, Mathf.Sin(a) * d), 1.2f, 0.5f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (k == 1 ? FlipbookFx.Kind.Mirror : 0));
            }
            books.Add(FlipbookFx.Book.Star, at + Vector3.up * 0.6f, 1f, 0.08f, glow: SceneMood.Night ? 3f : 1.6f);
        }

        /// <summary>L11 (fx.recipes): a paratrooper down (DropLanded: pos): the dust he lands in. The canopy is a mesh and
        /// owner question Q8.</summary>
        void OnDropLanded(SimEvent e)
        {
            if (recipes < 0.5f || books == null || !books.Ready) return;
            var cam = Camera.main;
            if (cam != null && cam.TryGetComponent<IZoomSource>(out var zs) && zs.CurrentZoom >= IncomingFarZoom) return;
            Vector3 at = (Vector3)e.Pos; at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z);
            books.Add(FlipbookFx.Book.DustPuff, at + Vector3.up * 0.1f, 2f, 28f / 12f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, alpha: 0.7f);
        }
    }
}
