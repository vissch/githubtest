# Goals: what the game is for, and what matters now

**Draft of 2026-10-07, put to the owner as a decision brief; until he has answered it, the rows of
[decisions.md](decisions.md) win wherever this page and a row disagree.** This page gathers what is already written
down; it decides nothing. The ideas agent (the skill tw-ideas) reads it first, and an idea names the goal it serves.
When a row of decisions.md changes a goal, change the line here in the same commit.

## What the game is

- A 3D remake of the 2D tug-of-war RTS Trench Warfare 1917 that keeps its identity: attrition along one axis,
  deployment slots, trench commands, off-map support fire, silver per second. ([overview](../00-overview.md), Vision.)
- The core loop is the trench fight: garrison, fire step, suppression and pinning, advance and fall back, barrage
  with craters, gas that sinks into trenches. ([plan review](../11-plan-review.md), section 4.)
- **The fun is in how units die.** "lots of the fun arrives as units die, we need to make this more absurd": slapstick
  and gore, cartoon physics. (Decision of 2026-09-28.)
- **The look is a painted cartoon mudfield at night.** Night is the default; every light is warm; Dust Front is a
  craft reference, not a style to copy. (Decisions of 2026-09-21 and 2026-09-29.)
- **It is watched, not read.** What is shown to the owner is a picture or a film first, words second; that holds
  for the board, for a decision and for an idea. (Decision of 2026-10-03.)

## What matters now (newest first)

1. **The frog faction is the main faction**: "the most important and instantly playable models in the game
   currently". Its units, their jobs, their looks and their effects come first. (Decision of 2026-10-06.)
2. **Matches must end, and be balanced when they do**: of the matches that end, each faction wins 40 to 60 %. Most
   matches do not end yet; that is the open problem. (Decisions of 2026-10-06.)
3. **The review's fixes**: the six top review fixes run first, then he looks at what changed. (Decision of
   2026-10-07, on its own decision lane until that lands.)
4. **The way we work**: agents that check themselves, a day's budget that holds, handoffs that are indexed, a board
   he can act on. (The plan of 2026-10-07 for the systematic priorities, on the Drive.)

## Hard limits

- Windows x64 only. 3,000 units at most. Same-build determinism: nothing in the sim may depend on the frame rate,
  the machine or the order of drawing. Two developers.
- Sim and show are two lanes; a feature that needs both is split, sim first.
- **Concepts first**: an effect, an animation or a character starts with concepts or references he picks from;
  nothing of it is built before he has picked. (Decision of 2026-10-06.)
- Nothing lands without his word.

## What he has said no to, as a kind

Read the list itself (the context of the ideas tool); these are the patterns in it.

- Chaos the player cannot read: no constant bombardment on the field. (2026-09-28.)
- Physics for its own sake: deaths are launched bodies, not ragdolls. (2026-09-26.)
- Being asked twice: his answer on the board is the decision, and queues its work with no second yes. (2026-10-06.)
