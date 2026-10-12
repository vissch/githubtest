# The paratroop landing question is now readable from the sim (Z011.2, sim half)

From: `lane/sim/review3-seam`. To: the show lane.

The sim's landing rule for a paratroop drop is no longer locked inside the command handler:

- `OffMapAbilitySystem.CanLand(int player, float3 at)` is public, a pure read (no sim state touched).
- `MatchSim.CanDropLand(int player, float3 at)` is the way in for show code. A sim without abilities answers yes.

The show half of Z011.2 is yours: `TestPanel.Update` asks `CanDropLand` before `aim.Press` for an ability with
`Men > 0`, leaves the aim armed when the answer is no, and `AbilityAim.Shape` carries a `Refused` flag so
`CombatFx` draws the disc in the refusal colour.

The sim's `Reject` stays: the HUD's answer can be up to `InputDelay` ticks stale, so the sim must still refuse.

No state, no hashed value and no `ReplayRecorder.FormatVersion` moved.
