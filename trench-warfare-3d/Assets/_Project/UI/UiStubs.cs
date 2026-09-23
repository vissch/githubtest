// Phase: B6 (stubs) — what B6 still owes after the battle HUD landed: depends on MissionRunner (A6) and world-space icons.
// DeployBar, TrenchWidget, AbilityBar and ObjectiveTracker are implemented (HudView's cards, TrenchOrderCluster,
// the support cards and ObjectiveTracker.cs), and the dialogue strip is HudDialogue + HudCommentary; this one waits
// for the unit-state pass.
using UnityEngine;

namespace TW.UI
{
    /// <summary>Suppression / pinned / bogged icons above units (world-space canvas or instanced sprites).</summary>
    public sealed class UnitStateIcons : MonoBehaviour { void Start() => throw new System.NotImplementedException("Phase B6: UnitStateIcons"); }
}
