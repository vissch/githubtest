# The balloon: a shot frog deflates and whizzes off

Item `idea-a-shot-frog-deflates-and-whizzes-off-lik`, stage game-design, 2026-10-08. Show lane only. Paths below are under `trench-warfare-3d/Assets/_Project/`. Every number marked (p) is a placeholder for the balance and motion stages.

## 1. The rule
When a frog on foot is shot dead while standing in the open, now and then he does not fall: he swells up on the spot, lets go with a raspberry, and whizzes off in three zigzag hops, smaller on each, then drops as a flat skin. It is a death, not an ability: nobody triggers it, it changes no fight, and only the frog infantryman can die this way.

## 2. What the player sees and does
1. A rifle round kills a standing frog. He stays on his feet for a beat and his throat sac and belly swell.
2. Raspberry. He shoots off backwards from the gun, low and fast, turning over.
3. Hop two goes off to one side, hop three to the other; he is visibly smaller on each, with a puff of air behind him.
4. He comes down flat: a frog-shaped skin on the mud, which lies and sinks like any other body.
The player does nothing. In the standard view the whole thing lasts about 3.7 s (p) and stays inside 12 m (p) of where he died.

## 3. Numbers (all placeholders)
- **How often: 1 in 8** (0.12 x the knob, up to 1) of qualifying deaths (p). The gags that exist are not rare (`DeathGags.PuntShare` 0.45, the plank 0.2 of standing gas deaths, `Presentation/Core/DeathGags.cs`); "rare" here means well under the plank. Rolled before the jig, heavy-punt and punt split, so a balloon replaces one of those.
- **The swell: 0.4 s** (p), under `DeathGags.DelayCap` 0.6.
- **Three hops: 6 / 4 / 2.5 m far, 3 / 2.2 / 1.2 m high** (p). At the body's gravity 14 (`VATRenderer.ThrowGravity`) that is 1.3 + 1.1 + 0.8 s in the air. Under `FarCap` 26 and `HighCap` 11.
- **The zigzag: each hop turned 50 to 70 degrees** (p) off the last, left then right or right then left by the seed.
- **Size: 1.0, 0.8, 0.6, 0.5** (p) at the swell and on hops one to three; the skin lies at 0.5 size and height 0.12 (the pancake's `RestQ`, the lowest `VatTint` holds).
- Cost to the player: none. The Frog keeps the rifleman's numbers (25 silver, 100 hp, 3.0 m/s, `Sim/Match/UnitDefinitions.cs`).

## 4. Edge cases
- **In a trench:** no balloon. `DeathGags.Choose` returns no gag for a shot man in a trench ("a shot never throws a man out of his trench"), and that stays. A balloon that starts in the open may fly over a trench and land in one.
- **Headshot, prone, other causes:** a headshot keeps the helmet pop; a prone frog keeps the flop; blast, gas, fire, crush, claw and beam keep their own gags.
- **Map edge and water:** landings are clamped half a metre inside the map (`FallenFlight.OnGround`); a hop that lands under the water level ends the flight there, as a wet first landing already ends bounces.
- **The slot is refilled the same tick:** the gag reads the dead frog's kind from his `DeathRecord.Archetype`, not from the slot.
- **3,000 units:** one body in the existing pool of 600 (`VATRenderer.MaxFallen`), no pool of its own; air puffs only within `CombatFx.GagDustReach` (120 m). A whole frog wave shot at once gives about one balloon in eight, each on its own path.
- **On a heap:** the skin joins the pile through `FallenFlight.EndAt`, like any gagged body.
- **Replay and capture:** every roll is `Dice(seed, tick, salt)` with new salts, so a replay and a still die the same way.
- **Knob:** at `fx.deathAbsurd` 0 nothing changes (`AtIntensityZeroChoosingChangesNothing` must still pass); at 2 the hops grow but the caps hold.
- **Far view:** the far frog is 300 triangles (decisions.md, 2026-10-06); the swell may not read there, the zigzag must.

## 5. What it needs
- **Sim: nothing.** No rule, no event, no archetype id, no ability id, no replay version, no file format. `DeathGag` and `GagPlan` are used only under `Presentation/` and `Tests/Show/`.
- Show, choice: `DeathGag.Balloon` added at the end of the enum; `GagInput` gets the dead man's archetype (it has none today); a branch in `DeathGags.Choose`.
- Show, path: `FallenFlight` bounces carry straight on and shrink to 0.35 of the throw; the balloon needs each arc on its own bearing and height.
- Show, body: a per-body size over time (the fallen draw already scales a charred body, `VATRenderer.Fallen.cs`), and the flat rest through `RestQ`.
- Show, the swell: today's squash keeps the volume, so taller means thinner (`Shaders/VAT_URP.shader`); a round swell needs a frog clip or a new shader term. The concept stage shows both; the pick decides.
- Show, effects: air puffs along the path (`FlipbookFx.Book.Puff`); no blood splats, no gore trail and no blood card for this gag (`CombatFx.GagBlood` draws them for any gag with arcs today).
- Show, the raspberry: a sound hook for deaths was not found (`Presentation/Audio/EventAudioRouter.cs` is 10 lines and names no sim event). See question 2.
- Tests: `DeathGagTests` (only a frog, never in a trench, the share, the caps, no allocation), `FallenFlightTests` (the zigzag lands on the ground, inside the map).
- See it: `TW.Editor.DeathLab.Scene("shot", x, z)` with `DeathGags.Pin`; a way to force one gag was not found, the motion stage needs one.

## 6. What it must not break
- Deaths are VAT launched bodies, presentation only and seeded, no ragdolls (decisions.md, 2026-09-26 and 2026-09-28).
- Knob 0 is today's deaths exactly; the knob stays a developer knob with no slider (decisions.md, 2026-09-28, 2026-09-30).
- The GORE slider: the balloon is bloodless at every GORE setting, so it adds nothing GORE 0 must hide.
- The trench rule for shot men, and every cap in `DeathGags` (`NoGagPassesTheCaps`).
- `VatTint` and `VatPad` are full (tasks.md, Infantry rendering): the size must not go into either.
- Concepts first (goals.md): nothing of the look is built before the owner has picked.

## 7. Open to the owner
1. **How often?** 1 in 8 of the frogs shot standing in the open (default) / 1 in 4 / 1 in 20.
2. **The raspberry:** a comic word "PFFRRT" through the rare `ComicWords` (default: it exists, and its gap of 8 s keeps it rare) / wait for a sound.

Cut: frogs in a trench, the machines of the frog faction (Bullfrog, Croaker, Hopper), a balloon from other causes, the skin as a pickup or a mark, any new sound system.
