# To every lane: the Proving Ground's SIM seam is on `lane/sim/proving-ground` (not landed)

The owner asked for a test level with every unit, a wave picker and a full unit panel (`decisions.md`, "The Proving
Ground"). Its SIM half is four commits on `lane/sim/proving-ground` (worktree `githubtest-proving-sim`), gated green,
pushed, and **waiting for the owner's word to land**. Nothing here is on the integration branch yet.

What lands with it, and what you would have to rebase over:

| Surface | Change |
|---|---|
| Replay format | v15 -> **v17** (`Sim/Core/Replay.cs`); pins in SimHashTests, FactionRosterTests, LoadoutTests. If your lane bumps the format too, the later lane renumbers. |
| Hash chain | `120 SapperSystem;` after `110 TrenchOrdersSystem;` (SimHashTests' pinned string). |
| Archetype ids | 21-36 are taken (`Sim/Core/RosterEntry.cs`): Brute 21, Croaker 22, Hopper 23, Mercy 24, Frog 25, Sentry 26, AT rifle 27, Death Battalion 28, Mark IV 29, Mark V 30, A7V 31, Renault FT 32, Whippet 33, Austin 34, Sapper 35, Flamethrower 36. All are `UnitDefinitions.All` entries in no faction's slot or pool. A test that needs a free id: 50 and up. |
| `InfantrySpec` | + `MineCharges` (int), `NeverPinned` (bool), appended. |
| `SimConfig` | + `Endless` (bool; `SectorControl` never names a winner), written to the replay header after `HeroPity1`. |
| Commands | `CommandType.UnitAbility` is consumed by `SapperSystem`: `b` = `UnitAbilityId` in the low byte (`LayMine` 13, `LayTripwire` 14), `AbilityArgs` above it. Any other id is rejected. |
| Events | `SapperOrdered`, `SapperLaying` appended after `RocketFired`. |
| `FlowFieldManager` | `MaxGoals` 16 -> 32; `TryGetGoal` (-1 when full, no throw), `Retarget`. |
| Behaviour | A machine with `HealPerSecond` heals; any shooter with `HuntsArmour` fires armour-piercing at a plate it beats; `NeverPinned` holds suppression below Pinned; a weapon with `SetsBurning` lights the man and the ground it hits; `MineSystem`'s trigger reads the match's vehicle profiles (a Skimmer or Salvo no longer trips a mine with the Maw's footprint). |
| Deleted | the never-registered stub `SpecialAbilitiesSystem` (the enum `UnitAbilityId` stays in `Sim/Units/SpecialAbilities.cs`). |

For the SHOW lanes: the sixteen have no names, tooltips, portraits or models yet (`UnitLook` answers "Vehicle"; a machine
without a `TankRenderer.Machines` row is drawn as the Maw). That is the Proving Ground's SHOW work
(`lane/show/proving-ground`, not started): plan of record `~/.claude/plans/can-you-make-a-misty-dove.md`, summarised in
`decisions.md` and `tasks.md`.

Delete this note once the lane has landed and every lane has rebased over it.
