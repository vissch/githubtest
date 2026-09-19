// Phase: B6 (stubs) — depends on: SimHost.Issue (P0), TrenchOrders (A3), OffMapAbilitySystem (A5), MissionRunner (A6)
using UnityEngine;

namespace TW.UI
{
    /// <summary>Five deploy slots with cost and cooldown; issues DeployUnit.</summary>
    public sealed class DeployBar : MonoBehaviour { void Start() => throw new System.NotImplementedException("Phase B6: DeployBar"); }

    /// <summary>Per-trench widget: ">>" advance, "↑" roster with class toggles, lock, "↩" fallback, hold fire.</summary>
    public sealed class TrenchWidget : MonoBehaviour { void Start() => throw new System.NotImplementedException("Phase B6: TrenchWidget"); }

    /// <summary>Off-map ability bar with cooldowns and point/line/area/heading reticles; issues SupportFire.</summary>
    public sealed class AbilityBar : MonoBehaviour { void Start() => throw new System.NotImplementedException("Phase B6: AbilityBar"); }

    /// <summary>Objective order and capture progress, wave banner, silver HUD.</summary>
    public sealed class ObjectiveTracker : MonoBehaviour { void Start() => throw new System.NotImplementedException("Phase B6: ObjectiveTracker"); }

    /// <summary>Dialogue strip driven by MissionTriggerFired events and the mission's dialogue table.</summary>
    public sealed class MissionDialogue : MonoBehaviour { void Start() => throw new System.NotImplementedException("Phase B6: MissionDialogue"); }

    /// <summary>Suppression / pinned / bogged icons above units (world-space canvas or instanced sprites).</summary>
    public sealed class UnitStateIcons : MonoBehaviour { void Start() => throw new System.NotImplementedException("Phase B6: UnitStateIcons"); }
}
