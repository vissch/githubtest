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

## The captures (leg 2)
Before/after pairs at the game camera, standard zoom, day and night, from the Tests/Stills rigs: TracerStills,
InfantryFiringStills, DeathStills, EffectFilmStills. They go in this folder, named `<rig>-<day|night>-<before|after>.png`.

Not filmed yet: this leg is the code and the EditMode test (BracedWeaponTests, 11 cases; 10 of them red on the
commit before it).

## Open look call
The Sentry (26) carries a frontal plate. Prone behind the plate may read badly; if the captures show that, leaving the
Sentry on the parapet/knee instead is a look call for the owner, not a blocker.
