# act-braced-weapons — evidence

What the owner asked for: braced weapons (the machine gun team, the Sentry, the AT rifle) set up before they fire
instead of shooting from the hip like riflemen. The sim is unchanged; only the drawn pose is.

## What changed in the look
- Stopped in the open, a braced man is drawn flat on the bipod. The set-up is the stance ladder's own
  StandToKneel -> KneelToProne, seen between his stop and his first shot.
- He then fires FireMG (the machine gunner, 2, and the Sentry, 26) or FireProne (the AT rifle, 27), reloads prone and
  does not stand up to aim.
- In a trench he rests the gun on the parapet instead: Clip.Unsling once, then he fires from the knee.
- Coming off the gun to move he slings it (Clip.Sling) before the first walking step.

## The knob
`fx.braced`: 1 is the new look and the default, 0 is today's behaviour exactly. The before captures are filmed with
`fx.braced = 0` from the same build as the after captures.

## The captures
Filmed on commit 129a0907 plus this leg's CombatFx.Close change, with `TW_FIRING_UNITS=2,26,27` (the machine
gunner, the Sentry, the AT rifle) at the game camera, standard zoom:

| File | Rig | Field | fx.braced |
|---|---|---|---|
| `InfantryFiringStills-Winter-before.png` | InfantryFiringStills | Winter (day) | 0 |
| `InfantryFiringStills-Winter-after.png` | InfantryFiringStills | Winter (day) | 1 |
| `InfantryFiringStills-NightMud-before.png` | InfantryFiringStills | NightMud (night) | 0 |
| `InfantryFiringStills-NightMud-after.png` | InfantryFiringStills | NightMud (night) | 1 |
| `TracerStills-NightMud-after.png` | TracerStills | its own night moment | 1 (the default) |

Each firing sheet is one row an archetype (2, 26, 27), fourteen frames left to right over the eight seconds the
squad walks into its own fire. What to look at: in the `after` rows the men go down to the bipod between their stop
and their first shot, and the muzzle flashes sit a man's height lower than in the matching `before` row.

TracerStills has no unit knob (it films riflemen under smoke), so there is one run only: it shows where a tracer now
leaves the gun, nothing to pair it with.

DeathStills and EffectFilmStills were not filmed: neither rig fires a braced archetype, so they would show the same
picture twice.

## Open look call
The Sentry (26) carries a frontal plate. Prone behind the plate may read badly; if the captures show that, leaving the
Sentry on the parapet/knee instead is a look call for the owner, not a blocker.
