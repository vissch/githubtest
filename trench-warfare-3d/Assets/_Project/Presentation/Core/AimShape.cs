// Phase: B6 / docs/21 phase 5 (implemented) — what the player is aiming (AbilityAim's Shape) and the hook that hands it
// to the effects: SceneHooks.AimPreview is a TryAimShape the aim's owner sets (TestPanel today; an AbilityTargeting
// controller in TW.UI later, docs/21) and CombatFx.Abilities draws from, so the effects never know who owns the aim.
// Here in the core assembly because SceneHooks lives here and the camera assembly is above it.
using UnityEngine;

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

    /// <summary>The aim now, for the effects to draw: false when nothing is armed or the cursor is off the map.</summary>
    public delegate bool TryAimShape(out AimShape shape);
}
