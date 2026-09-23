// Phase: B6 (stubs) — what B6 still owes after the battle HUD landed: depends on MissionRunner (A6) and world-space icons.
// DeployBar, TrenchWidget, AbilityBar and ObjectiveTracker are implemented (HudView's cards, TrenchOrderCluster,
// the support cards and ObjectiveTracker.cs); these two wait for the mission script and the unit-state pass.
using UnityEngine;

namespace TW.UI
{
    /// <summary>Dialogue strip driven by MissionTriggerFired events and the mission's dialogue table.</summary>
    public sealed class MissionDialogue : MonoBehaviour { void Start() => throw new System.NotImplementedException("Phase B6: MissionDialogue"); }

    /// <summary>Suppression / pinned / bogged icons above units (world-space canvas or instanced sprites).</summary>
    public sealed class UnitStateIcons : MonoBehaviour { void Start() => throw new System.NotImplementedException("Phase B6: UnitStateIcons"); }
}
