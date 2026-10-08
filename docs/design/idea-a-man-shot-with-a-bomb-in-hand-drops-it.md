# A man shot with a bomb in hand drops it
Design page for the board item `idea-a-man-shot-with-a-bomb-in-hand-drops-it` (stage game-design, 2026-10-08). Read in the checkout githubtest-frog-house, detached at 2bad681c; every "today" names the file it was read in. Sketch: `shown.jpg` in the stage's evidence.

## 1. The rule
A man who decides to bomb a trench no longer lets go at once: he draws the bomb and holds it lit for a short wind-up, then throws as he does today. If he is killed inside that wind-up, by anything, the bomb falls where he stood, burns for a second and goes off there, among whoever is next to him. It is the same bomb with the same burst, and it does not care whose men they are.

## 2. What the player sees and does
He presses nothing: it is a thing to watch and a risk to weigh.
1. His men close on a trench. One stops, arm back, fuse sputtering (the spark the flying bomb already has, `Presentation/Camera/CombatFx.Grenades.cs`).
2. A shot from the trench drops him. The bomb rolls off his hand and lies sputtering at his feet, lit warm on the dark ground.
3. One second. The men beside him flinch (picture only; they do not run).
4. The burst. Two or more dead together already go up as a fountain (`Presentation/Core/DeathGags.cs`, `heap = Density >= 2`), so "they all fly" is the gag that exists.
What it costs him: a tight knot of assault men under rifle fire can lose several to one of its own bombs. The defender gains a reason to shoot the bomber first; the attacker a reason to pin the trench (smoke, a machine gun) before the bombers go in.

## 3. Numbers (all placeholders: the balance stage sweeps them)
| Number | Placeholder | Reasoning |
|---|---|---|
| Wind-up, decision to release | 0.6 s = 12 ticks | Today 0 (`Sim/Combat/DirectFire.cs`, `Throws` adds the bomb to the air the tick it decides). Long enough to see and to be shot in; a fifth of `GrenadeCooldownSeconds` 3 s. Tick rate 20 (`Sim/Core/SimConfig.cs`). |
| Throw cycle | unchanged, 3 s | The cooldown still starts at the decision, so bombs per minute do not change; each lands 0.6 s later. |
| Fuse of a dropped bomb | 1.0 s = 20 ticks | The pitch's "a second"; about a bomb's longest flight today (1.2 s at 22 m, `CombatTables.GrenadeFlightTicks`). |
| Its burst | 110 damage, 4.5 m, suppression 50 | `GrenadeDamage`, `GrenadeRadius`, `GrenadeSuppression` in `Sim/Combat/CombatTables.cs`, untouched. Falloff as any burst: `1 - 0.75 x dist / radius` (`Sim/Combat/Blast.cs`), so 110 at his feet, about 73 at 2 m. |
| Chance that an armed death drops it | 100 % | No roll: one cause, one effect, readable. The sweep counts drops a match and may ask for less. |
| Who can drop one | whoever can throw | `CombatTables.GrenadesFor`: Rifle, Frog, DeathBattalion, Para, Jetpack, Shield, Sapper 2; Assault 4; Officer 1; the rest 0. |
Not found: a man's hit points (not read), so "how many die at 2 m" is the sweep's to answer; how close a squad stands in the open.

## 4. Edge cases
1. **In a trench.** A man in a trench never throws (`Throws` refuses `UnitFlags.InTrench`), so no drop starts in one. A drop near a trench goes through `BlastSystem` like any burst, bay rules and all.
2. **Usually only his own side.** He throws from at least `GrenadeMin` 5 m; the burst reaches 4.5 m. The man he aimed at is out of reach by rule.
3. **Dies after the release.** Unchanged: the thrown bomb goes off where it lands, "thrower alive or not" (`DirectFire.cs` header).
4. **Pinned or wounded in the wind-up.** He still throws. Only death drops it.
5. **A blast kills him.** The bomb stays where he stood, his body flies. If that burst was a dropped bomb, his own drops a second later: a chain, bounded by the bombs men carry (the bomb is spent at the decision, as today).
6. **His slot is refilled within the second.** `SimWorld.Spawn` bumps `Generation`; the wind-up entry checks slot and generation as `grenadeGen` does, and a dropped bomb holds its place and team by value, never the slot.
7. **Map edge.** It falls where he stood, which is on the map; nothing flies.
8. **3,000 units.** At most one wind-up a man and one list walked a tick on the main thread, like `flying` today. No search.
9. **Replay.** Wind-ups and dropped bombs are sim state, in the hash beside `flying`; no roll, no clock but the tick.
10. **`fx.deathAbsurd` 0.** The drop is a sim rule and still happens; only how the men fly follows the knob.

## 5. What it needs
Sim first (lane/sim):
- `DirectFireSystem`: a wind-up list (slot, generation, the bomb, release tick) between `Throws` and `flying`; each tick, in order: thrower dead or slot refilled -> dropped list (his last living position, fuse); release tick reached -> `flying` as today.
- Two events appended to `SimEventType`: the bomb drawn (thrower, target, ticks to release) and the bomb dropped (dead man, position, fuse ticks). `GrenadeThrown` moves to the release tick, same fields.
- The dropped bomb is an `Impact` with `SourceId.Grenade` (2002), no direction, `Player` = the dead man's team; friendly fire is already on in `Blast.cs` ("friendly fire is on").
- **Seam: replay format.** `ReplayRecorder.FormatVersion` is 37 (`Sim/Core/Replay.cs`); this is a behaviour change with new hashed state and appended events, so the next free number at landing. No archetype id, no ability id, no file layout changes.
- Tests: `GrenadeTests` gains the drop, the chain, the refilled slot; `AssaultLadderTests` re-run with the wind-up.
Show (after the owner picks a concept):
- `AnimationController` starts `Clip.Throw` on the drawn event so the swing ends on the release (today the `threw` latch starts it on `GrenadeThrown`).
- `CombatFx.Grenades`: the lump and fuse spark in the hand, then lying on the ground for the fuse, with a warm light; the burst is the grenade's small one.
- A flinch for the men within reach. No HUD element: nothing to press, nothing to read.

## 6. What it must not break
- **The assault** (tasks.md, "The assault"; decisions 2026-09-28/29): the ladder went red twice on small range changes. A 0.6 s later bomb is such a change: the ladder is the gate.
- **No chaos the player cannot read** (decision 2026-09-28, no constant bombardment): one visible cause, a second of warning, rare.
- **Deaths are launched bodies, presentation only, seeded** (decisions 2026-09-26, 2026-09-28): the sim adds a burst, the flying stays in `DeathGags`.
- **The bomb's safety distances** (`GrenadeMin`, `GrenadeFriend`; decision 2026-09-29) and the 17.6 m throw: untouched.
- **Determinism** (goals.md, hard limits): list order is insertion order; nothing reads the frame.
Cut: men running from the bomb, picking it up or kicking it away, a drop on a wound, a chance roll, a HUD callout, bombs dropped by trench men.

## 7. Open to the owner
1. **Every throw gets a 0.6 s wind-up, so there is a moment to be shot in?** Default: yes. Without it the rule has no "bomb in hand" to exist in (today the bomb leaves the tick he decides).
2. **Do the men beside the dropped bomb try to get away in that second?** Default: no, they flinch and fly; running is a second mechanic and spoils the joke.
