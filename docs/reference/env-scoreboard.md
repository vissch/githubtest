# Environment scoreboard

The loop's scoreboard. `visual-score.md` is the older day/night log against the reference images and stays as the
history; this file is what the ten-minute critique loop reads and writes from 2026-09-24 onward.

## How it works

**The zoom ladder.** The same ten criteria are scored three times, at three camera tiers. Work happens at ONE tier at
a time - the active tier. When every line of the active tier is at the bar, the tier is closed and the loop moves one
step in, to the next tier, and scores the same ten lines again from there. Detail that only exists at T1 is a lie the
moment the owner rolls the wheel, and detail that only exists at T3 is invisible in the game as it is actually played,
so both get scored and neither is allowed to pay for the other.

| Tier | Camera | What fills the frame |
|---|---|---|
| T1 STANDARD | zoom 30, 25 deg lens, 25 deg pitch, yaw +21 | the position: two trench lines and the ground between them |
| T2 MID | zoom 14-18 | a platoon and the bay it holds |
| T3 CLOSE | zoom 6-9, camera about 1.1 m up among the men, 42 deg lens | one man, one sandbag course, one puddle |

**The bar.** A tier is closed when every line is at least **8** and the tier totals at least **85 / 100**.
8 means a viewer would call it the same style as the target. 10 is not the goal and chasing it wastes rounds.

**The biomes.** Three fields are in scope and each gets its own board. The active biome is named in the state block
below. A change made for one biome must be checked against the other two before it is committed - most of this
machinery is shared, and the last three regressions in this project were a biome-specific fix leaking sideways.

## The ten criteria

| # | Criterion | What a 8 looks like |
|---|---|---|
| 1 | Silhouette and read | every shape separates from the ground and from the shape behind it; no man lost against a bank |
| 2 | Ground surface | no visible tile repeat at the tier's texel scale; relief, wear and variety across a frame's worth of ground |
| 3 | Liquid | water, ice or molten reads as that substance at this distance: depth, edge, motion and what it does with light |
| 4 | Trench kit and structures | revetment, bags, boards, dugouts: irregular, settled into the ground, not a repeated unit |
| 5 | Props, clutter and falloff | the field carries the right amount of stuff for the tier, and density falls off without a visible edge |
| 6 | The men | materials, wear, contact with the ground, poses; the fallen and the living both hold up at this distance |
| 7 | Light and shadow | key light, pooled light, contact shadow and glint; nothing is flat and nothing is a black hole |
| 8 | Colour and depth | palette holds, values separate near from far, aerial perspective reads, no mush and no candy |
| 9 | Effects and motion | bursts, smoke, weather and small life are the right size, speed and lifetime for this distance |
| 10 | Cohesion | the frame looks like ONE place: no seams, no popping, no dead empty quarter, no element from another game |

## Standing constraints on every round

These are not scored. They are conditions of a round being allowed to land at all.

- **Readability never drops.** If the change makes the men, the trench lines or the orders harder to see in play, it
  is reverted, whatever it did to the other nine lines.
- **The budget never rises.** `BattlefieldProps.DrawCalls` and `SubmittedVertices` at the STANDARD view must not go
  up. Detail added for T2 or T3 is switched by `_TWClose` / `SceneHooks.CloseUp` and costs nothing at T1. A round that
  raises the standard-view cost pays for it somewhere else in the same round or it does not land.
- **Sim untouched.** Presentation only. Nothing under `Assets/_Project/Sim` unless the owner asked for it, because it
  is hashed and it is replayed.
- **Measured, not asserted.** Every score change names the capture it was read from. A score moved on an argument
  rather than on a picture is written as `-` and the reason is logged.

## State

```
ACTIVE TIER   : T1 STANDARD
ACTIVE BIOME  : NightMud
LOOP          : every 10 min, started 2026-09-24 01:09, runs 8 h
```

## Boards

### NightMud (the shipped field)

| # | Criterion | T1 | T2 | T3 |
|---|---|---|---|---|
| 1 | Silhouette and read | - | - | - |
| 2 | Ground surface | - | - | - |
| 3 | Liquid | - | - | - |
| 4 | Trench kit and structures | - | - | - |
| 5 | Props, clutter and falloff | - | - | - |
| 6 | The men | - | - | - |
| 7 | Light and shadow | - | - | - |
| 8 | Colour and depth | - | - | - |
| 9 | Effects and motion | - | - | - |
| 10 | Cohesion | - | - | - |
| | **Total / 100** | **-** | **-** | **-** |

### Winter (`BattlefieldParams.WinterLine`, `Biome.Winter`)

| # | Criterion | T1 | T2 | T3 |
|---|---|---|---|---|
| 1 | Silhouette and read | - | - | - |
| 2 | Ground surface | - | - | - |
| 3 | Liquid | - | - | - |
| 4 | Trench kit and structures | - | - | - |
| 5 | Props, clutter and falloff | - | - | - |
| 6 | The men | - | - | - |
| 7 | Light and shadow | - | - | - |
| 8 | Colour and depth | - | - | - |
| 9 | Effects and motion | - | - | - |
| 10 | Cohesion | - | - | - |
| | **Total / 100** | **-** | **-** | **-** |

### Coast (`BattlefieldParams.Landing`)

| # | Criterion | T1 | T2 | T3 |
|---|---|---|---|---|
| 1 | Silhouette and read | - | - | - |
| 2 | Ground surface | - | - | - |
| 3 | Liquid | - | - | - |
| 4 | Trench kit and structures | - | - | - |
| 5 | Props, clutter and falloff | - | - | - |
| 6 | The men | - | - | - |
| 7 | Light and shadow | - | - | - |
| 8 | Colour and depth | - | - | - |
| 9 | Effects and motion | - | - | - |
| 10 | Cohesion | - | - | - |
| | **Total / 100** | **-** | **-** | **-** |

Known going in, from the last rounds of `visual-score.md` and from the winter commit `1c8e67e`, so the first
scoring round does not pretend to discover them: the winter ice is flatter than the snow beside it (luma spread
0.061 against 0.161) and has no glare, which is exactly what criterion 3 is for; the coast has no obstacles, no
wire and no defender's trench on the sand at all, which is criteria 4 and 5; the flipbook drawings' ink outlines do
not follow zoom, which will cost criterion 1 at T3; a single rifle shot is marginally legible at T1, criterion 9.

## Log

One row per round. `Lines` names the criteria the round moved. A round that finds nothing worth doing writes a row
saying so rather than inventing work.

| Round | Time | Tier | Biome | Worst thing found | What was done | Lines | Gate | Left undone |
|---|---|---|---|---|---|---|---|---|
| 0 | 2026-09-24 01:09 | T1 | NightMud | - | scoreboard opened | - | n/a | first round scores T1 NightMud from a capture before changing anything |
