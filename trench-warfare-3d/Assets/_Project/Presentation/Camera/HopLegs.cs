// Phase: Playground (2026-09-28) / the battle (2026-10-07) — a hopping machine's legs through a hop
// The Bullfrog's legs are welded into its hull and skinned (WeldedLegRig). What the six leg bones of each side do through
// a hop was worked out in the Playground over thirteen critic rounds (Playground/Runtime/HopDrive.cs, docs/22); the owner
// asked for the same legs in the battle (2026-10-07: "we need to animate his legs as well as if he jumps ... we already
// did this at some point that needs to carry through"). So the two share this: Drives says how far each motion is at a
// phase of the hop, Pose turns the bones by them. HopDrive adds what only the Playground has (the body stood on its
// skinned feet, kicks, the belly's squash); TankRenderer.HopLegs.cs drives it from the battle's own hop.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class HopLegs
    {
        public const float Crouch = 0.18f, Flight = 0.5f;   // shares of a hop; the landing takes the rest
        public const float Push = 0.4f;   // how far the hind legs unfold on the ground, pushing off

        /// <summary>The six bones of each side (0 left, 1 right) by index in the rig, -1 where it has none.</summary>
        public sealed class Bones
        {
            public readonly int[] Thigh = new int[2], Shin = new int[2], Foot = new int[2], Arm = new int[2], Fore = new int[2], Hand = new int[2];
            public Bones(WeldedLegRig legs)
            {
                string[] sides = { "L", "R" };
                for (int s = 0; s < 2; s++)
                {
                    Thigh[s] = legs.Bone("Thigh_" + sides[s]); Shin[s] = legs.Bone("Shin_" + sides[s]); Foot[s] = legs.Bone("Foot_" + sides[s]);
                    Arm[s] = legs.Bone("Arm_" + sides[s]); Fore[s] = legs.Bone("Fore_" + sides[s]); Hand[s] = legs.Bone("Hand_" + sides[s]);
                }
            }
        }

        /// <summary>The legs through a hop at phase u, as six drives (0..1): extend, the hind legs unfolding (straight
        /// out in the first tenth of the flight, folding back up by two thirds of it); trail, those legs swung from
        /// pushing down to trailing straight back behind (propped on them from below, the body rose 2.5 m, twice the hop,
        /// before it left the ground); tuck, the forelegs swept back under it leaving the ground; reach, the forelegs out
        /// forward and down for the landing; absorb, the hind knees folding as it gathers and lands.</summary>
        public static void Drives(float u, out float extend, out float trail, out float open, out float tuck, out float reach, out float absorb)
        {
            extend = trail = open = tuck = reach = absorb = 0f;
            if (u < Crouch)
            {
                float c = u / Crouch;
                absorb = 0.5f * Mathf.Sin(Mathf.Min(c / 0.6f, 1f) * Mathf.PI);   // gathers
                // and pushes off: the hind legs start to unfold with the feet still on the ground, which stands the body
                // up on them (unfolding only in the air, it rose with its legs still folded, critic g10)
                extend = Push * Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.6f, 1f, c));
                // leaning back as it pushes, so it leaves along a diagonal instead of standing up on stilts first (g11)
                trail = 0.6f * Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.75f, 1f, c));
            }
            else if (u < Crouch + Flight)
            {
                float k = (u - Crouch) / Flight;
                // the swing back leads the rest of the unfold: unfolding first, the legs propped it up to 1.9 m
                // (and waits for it: still unfolding while it swung, the legs stood it 0.7 m over the arc at k = 0.04)
                extend = Mathf.Lerp(Push, 1f, Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.07f, 0.2f, k))) * (1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.35f, 0.7f, k)));
                trail = Mathf.Lerp(0.6f, 1f, Mathf.SmoothStep(0f, 1f, k / 0.07f));
                // the knee opens from its push to its trail after the swing: opening while it swung dropped the ankle
                open = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.07f, 0.2f, k));
                tuck = Mathf.SmoothStep(0f, 1f, k / 0.15f) * (1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.3f, 0.5f, k)));
                // from 0.3, over the end of the tuck: from 0.45 the legs sat at rest mid-flight (g10)
                reach = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.3f, 0.8f, k));
            }
            else
            {
                float q = (u - Crouch - Flight) / (1f - Crouch - Flight);
                reach = 1f - Mathf.SmoothStep(0f, 1f, q / 0.4f);
                absorb = Mathf.Sin(Mathf.Min(q / 0.5f, 1f) * Mathf.PI);
            }
        }

        /// <summary>Pose the bones from the drives (and splay: knocked out, the legs sprawl; brace: firing, the forelegs
        /// set forward). Pitch turns about the body's right (+ swings a bone's far end from forward to down to back),
        /// splay about its forward, outward on each side. extendL and extendR: each side's own unfold (a kick is one
        /// leg's). The caller solves the rig (WeldedLegRig.Solve).</summary>
        public static void Pose(WeldedLegRig legs, Bones b, float extendL, float extendR, float trail, float open, float tuck, float reach, float absorb, float splay, float brace)
        {
            for (int s = 0; s < 2; s++)
            {
                // sprawled 45 degrees out (20 left it sitting up, g10)
                float out_ = (s == 0 ? -1f : 1f) * 45f * splay;
                float ex = s == 0 ? extendL : extendR;
                // the roll outside the pitch: a sprawled leg folds and kicks in its own plane, flat along the ground (the
                // pitch outside swung a sprawled leg's kick straight up over the back like a tail, critic g13)
                Quaternion R(float pitch, float roll = 0f) => Quaternion.AngleAxis(roll, legs.Forward) * Quaternion.AngleAxis(pitch, legs.Right);
                // (the forelegs do not fold as it gathers or lands: turned about the shoulder the hands left the ground and
                // the body stood up on the elbows, +0.2 m in the crouch)
                // trailing, the thighs ride up 32 degrees: the body leaves nose up, and at 20 that swung the trailing feet
                // into the ground, which stood it 0.7 m over its arc
                // trailing, the leg opens nearly straight behind (knee 100 degrees; at 65 it zig-zagged, the knee up and the
                // foot down, g10); pushing, it presses down; absorbing, the knee folds (5 degrees did not show)
                Set(legs, b.Thigh[s], R(ex * Mathf.Lerp(-22f, 25f, trail) + 12f * absorb, out_));
                Set(legs, b.Shin[s], R(ex * Mathf.Lerp(55f, 100f, open) - 15f * absorb + 20f * splay));
                Set(legs, b.Foot[s], R(ex * Mathf.Lerp(25f, 60f, open)));
                Set(legs, b.Arm[s], R(-38f * reach + 24f * tuck - 15f * brace - 10f * splay, out_));
                Set(legs, b.Fore[s], R(-12f * reach + 20f * tuck));
                Set(legs, b.Hand[s], R(20f * reach - 10f * tuck));   // (35: the palm behind the wrist sheared)
            }
        }

        static void Set(WeldedLegRig legs, int bone, Quaternion q) { if (bone >= 0) legs.Pose[bone] = q; }
    }
}
