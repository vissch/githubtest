// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-8, look spec L05) — part of CombatFx. Behind fx.recipes,
// the shells of an off-map barrage are seen coming in: the sim schedules every payload ahead (OffMapAbilitySystem.
// Scheduled, read here and never written), so a ShellFall card (a shell streaking down onto its point, 8 frames at
// 12 fps) is laid IncomingLead before each shell's tick and ends as the shell lands. Read once per sim tick; up close
// and at the standard view only (culled from IncomingFarZoom: at the overview the burst is the read). The Kettle's and
// the tanks' rounds land the tick they fire, so they have no incoming (a SIM flight time is owner question Q5).
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        public const float IncomingLead = 8f / 12f;     // the ShellFall book's length: the streak meets the ground on its last frame
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
                float early = (p.Tick - (tick + lead)) * w.Config.TickSeconds;   // 0 unless it was caught up late (negative then)
                books.Add(FlipbookFx.Book.ShellFall, at, 5f, IncomingLead, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, glow: SceneMood.Night ? 1.6f : 1f, delay: Mathf.Max(0f, early));
            }
        }
    }
}
