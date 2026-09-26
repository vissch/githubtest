// Phase: B6 / docs/21 phase 5 (implemented) — aiming an off-map ability: the state between "armed" and the command.
// A point ability fires where the map is clicked. A line ability (a corridor: the strafe, the beam, the smoke screen,
// the creeping barrage, the HE line and box, creeping gas) is press-drag-release: the press is where the line starts,
// the drag its heading and length (Shift snaps the heading to 15 degrees; a drag under MinDrag fires the ability's
// own length straight up the player's field), Tab cycles the patterns the ability offers. What the player is aiming
// is a Shape the effects draw (CombatFx.Abilities.cs) and the readout counts under (AimReadout). No input in here:
// TestPanel feeds it the mouse and the keys, the tests feed it points.
using UnityEngine;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Presentation.Tactical
{
    /// <summary>What is being aimed: a disc round a point, or a corridor from Start along Dir.</summary>
    public struct AimShape
    {
        public bool Line;
        public Vector3 Start, Dir;
        public float Length, HalfWidth, Radius, StepMetres;
        public int Pattern;
        public Vector3 End => Start + Dir * Length;
    }

    public sealed class AbilityAim
    {
        /// <summary>A drag shorter than this is a click: the line runs its own length straight up the field.</summary>
        public const float MinDrag = 2f;
        public const float SnapDegrees = 15f;
        /// <summary>The disc drawn for an ability with no radius of its own (gas drifts).</summary>
        public const float PointFallbackRadius = 8f;

        public OffMapAbilityId Armed { get; private set; } = OffMapAbilityId.None;
        public int Pattern { get; private set; }
        public bool Dragging { get; private set; }
        public Vector3 Start { get; private set; }
        public Vector3 Current { get; private set; }
        /// <summary>Shift held: the heading snaps (TestPanel sets it each frame; the effects and the readout read it).</summary>
        public bool Snap { get; set; }

        bool TryStats(out AbilityStats s)
        {
            s = default;
            return Armed != OffMapAbilityId.None && OffMapAbilitySystem.TryGetStats((int)Armed, out s);
        }

        /// <summary>The armed ability, in this pattern, is a corridor rather than a point or a disc.</summary>
        public bool IsLine => TryStats(out var s) && OffMapAbilitySystem.IsLine(s, Pattern);

        public void Arm(OffMapAbilityId id) { Armed = id; Pattern = 0; Dragging = false; }
        public void Cancel() { Armed = OffMapAbilityId.None; Pattern = 0; Dragging = false; }

        /// <summary>The next pattern the ability offers (Tab). False when it has only the one.</summary>
        public bool CyclePattern()
        {
            if (!TryStats(out var s)) return false;
            for (int k = 1; k <= AbilityArgs.PatternMask; k++)
            {
                int next = (Pattern + k) % (AbilityArgs.PatternMask + 1);
                if (next == Pattern) break;
                if (s.Offers(next)) { Pattern = next; Dragging = false; return true; }
            }
            return false;
        }

        /// <summary>The map pressed at a point: a point ability fires (the command comes back, and the aim is over); a
        /// line ability starts its drag there.</summary>
        public bool Press(Vector3 ground, uint tick, byte player, out SimCommand cmd)
        {
            cmd = default;
            if (!TryStats(out var s)) return false;
            if (!OffMapAbilitySystem.IsLine(s, Pattern))
            {
                cmd = Command(ground, 0, 0, tick, player);
                Cancel();
                return true;
            }
            Start = ground; Current = ground; Dragging = true;
            return false;
        }

        public void Drag(Vector3 ground) { if (Dragging) Current = ground; }

        /// <summary>The drag released: the line fires from where it was pressed, along the drag.</summary>
        public bool Release(Vector3 ground, uint tick, byte player, out SimCommand cmd)
        {
            cmd = default;
            if (!Dragging || !TryStats(out var s)) { Dragging = false; return false; }
            Current = ground;
            int heading = HeadingOf(Start, ground, Snap, player);
            int length = LengthOf(Start, ground, s);
            cmd = Command(Start, heading, length, tick, player);
            Cancel();
            return true;
        }

        SimCommand Command(Vector3 at, int heading, int length, uint tick, byte player)
            => new SimCommand { Tick = tick, Player = player, Type = CommandType.SupportFire, A = (int)Armed, B = AbilityArgs.Pack(heading, Pattern, length), Pos = new float3(at.x, 0f, at.z) };

        /// <summary>The heading of a drag in degrees: 0 is +Z (up the field), 90 is +X, clockwise from above. A drag under
        /// MinDrag is straight up the player's own field (player 1 looks down -Z). Snapped to SnapDegrees on request.</summary>
        public static int HeadingOf(Vector3 from, Vector3 to, bool snap, byte player = 0)
        {
            float dx = to.x - from.x, dz = to.z - from.z;
            if (dx * dx + dz * dz < MinDrag * MinDrag) return player == 1 ? 180 : 0;
            float deg = Mathf.Atan2(dx, dz) * Mathf.Rad2Deg;
            if (snap) deg = Mathf.Round(deg / SnapDegrees) * SnapDegrees;
            int h = Mathf.RoundToInt(deg);
            return ((h % 360) + 360) % 360;
        }

        /// <summary>The length of a drag in whole metres, clamped to what the ability allows; 0 (the ability's own
        /// length) for a drag under MinDrag.</summary>
        public static int LengthOf(Vector3 from, Vector3 to, in AbilityStats stats)
        {
            float d = Vector2.Distance(new Vector2(from.x, from.z), new Vector2(to.x, to.z));
            if (d < MinDrag) return 0;
            return Mathf.RoundToInt(Mathf.Clamp(d, OffMapAbilitySystem.MinLength, stats.Length));
        }

        /// <summary>What is being aimed under the cursor: a disc for a point ability; for a line the corridor of the
        /// drag in hand, or, before the press, the ability's full length from the cursor straight up the field.</summary>
        public bool Shape(Vector3 cursor, byte player, out AimShape shape)
        {
            shape = default;
            if (!TryStats(out var s)) return false;
            shape.Pattern = Pattern; shape.StepMetres = s.StepMetres;
            if (!OffMapAbilitySystem.IsLine(s, Pattern))
            {
                shape.Start = cursor; shape.Radius = s.Radius > 0f ? s.Radius : PointFallbackRadius;
                return true;
            }
            Vector3 from = Dragging ? Start : cursor, to = Dragging ? Current : cursor;
            int heading = HeadingOf(from, to, Snap, player), length = LengthOf(from, to, s);
            float3 dir = AbilityArgs.Heading(heading);
            shape.Line = true; shape.Start = from; shape.Dir = new Vector3(dir.x, 0f, dir.z);
            shape.Length = length > 0 ? length : s.Length; shape.HalfWidth = OffMapAbilitySystem.HalfWidthOf(s, Pattern);
            return true;
        }
    }
}
