# Mixamo clip manifest (owner download of 2026-09-22, 205 clips)

Measured straight from the FBX files (`trench-warfare-3d/Tools/fbxscan.py`: hips curve and stack length; no import). Hips height is in Mixamo centimetres: about 100 standing, 72 to 92 stooped, 39 to 46 kneeling or squatting, 13 prone. "Travel" is root motion left in the clip (the baker strips it; the sim moves the man). "Loop" means the last pose returns to the first. Rigs: R rifleman, A assault, M machine-gunner, S sniper, O officer (future). Verdict: **use** is in the first bake, **spare** is a variant to add when there is atlas room, **ditch** is not baked.

| Clip | State | Stance | s | Loop | Travel cm | Hips cm | Rigs | Verdict | Note |
|---|---|---|---|---|---|---|---|---|---|
| Rifle Aim To Down | Aim.Down | stand | 1.5 | no | 15.1 | 97.8..101.7 | RAMS | use | lower it when the target is lost |
| Rifle Aim To Kneel | Aim.Down | kneel | 0.8 | no | 13.4 | 38.9..49.9 | RAMS | use | kneeling |
| Rifle Down To Aim | Aim.Up | stand | 0.87 | no | 11.2 | 99.6..101.7 | RAMS | use | raise the rifle when a target appears (34 deg turn: strip) |
| Rifle Kneel To Aim | Aim.Up | kneel | 0.8 | no | 8.3 | 38.9..48.2 | RAMS | use | kneeling |
| Emerging | Cover.Leave | stand | 2.3 | no | 338.5 | 84.0..95.4 | RAMS | use | out from cover into a run (339 cm, 74 deg turn: strip) |
| Taking Cover | Cover.Take | stand | 2.27 | no | 137.3 | 88.9..100.4 | RAMS | use | run in and turn against the wall (137 cm, 90 deg: strip) |
| Death Crouching Headshot Front | Death.Kneel | kneel | 1.9 | no | 96.8 | 11.1..85.5 | RAMS | use | from the squat |
| Rifle Kneel Hit To Back | Death.Kneel | kneel | 1.9 | no | 129.3 | 15.4..44.1 | RAMS | use | from one knee, thrown back |
| Rifle Run To Dying | Death.Run | stand | 2.47 | no | 401.3 | 17.0..85.9 | RAMS | use | shot at a run (401 cm: strip) |
| Death From Back Headshot | Death.Stand | stand | 3.7 | no | 98.5 | 15.7..97.6 | RAMS | use |  |
| Death From Front Headshot | Death.Stand | stand | 2.83 | no | 39.7 | 13.9..99.5 | RAMS | use | drops on the spot (40 cm) |
| Death From Right | Death.Stand | stand | 3.3 | no | 90.2 | 14.2..97.6 | RAMS | use | shot from the right |
| Death From The Back | Death.Stand | stand | 2.97 | no | 117.5 | 17.2..101.1 | RAMS | use | shot from behind |
| Death From The Front | Death.Stand | stand | 3.43 | no | 108.9 | 18.8..106.2 | RAMS | use | shot from the front |
| Rifle Death | Death.Stand | stand | 3.8 | no | 108.2 | 16.7..100.2 | RAMS | use | 3.8 s, rifle held |
| Rifle Hit To Back | Death.Stand | stand | 1.77 | no | 143.9 | 17.5..101.7 | RAMS | use | thrown back (144 cm: strip): shell |
| Walking To Dying | Death.Walk | stand | 3.07 | no | 167.9 | 23.7..98.8 | RAMS | use | shot while walking |
| Walking To Dying (1) | Death.Walk | stand | 2.9 | no | 245.4 | 23.6..103.6 | RAMS | use | variant |
| Dive Roll | Dive | stand | 2.37 | yes | 374.8 | 17.3..104.2 | RA | use | dive to the ground and up (375 cm travel: strip); shell landing close |
| Rifle Rubbing Eyes | Fidget.Kneel | kneel | 1.83 | no | 2.2 | 38.9..46.3 | RAMS | use | kneeling fidget; at night and in gas |
| Check Shoe | Fidget.Stand | stand | 4.7 | no | 5.0 | 97.9..101.3 | RAO | use | rear line only; ends in a different foot stance, crossfade back |
| Inspecting | Fidget.Stand | stand | 3.17 | yes | 0.0 | 100.2..100.8 | RAMSO | use | looks the rifle over |
| Rifle Idle (4) | Fidget.Stand | stand | 5.93 | yes | 0.1 | 81.3..100.9 | RAMS | use | dips to 81 cm: a stretch; play once as a fidget |
| Turning Right 45 Degrees (1) | Fidget.Stand | stand | 1.0 | yes | 0.0 | 100.0..102.5 | RAMS | spare | a foot shuffle in place (no yaw): a fidget |
| Firing Rifle (4) | Fire.CrouchWalk | crouch | 1.17 | yes | 0.0 | 75.2..79.2 | RAM | use | firing while stooped (hips 77) |
| Fire Rifle | Fire.Kneel | kneel | 1.03 | yes | 0.0 | 46.3..47.7 | RAMS | use | kneeling shot (hips 46) |
| Prone Firing Rifle | Fire.Prone | prone | 0.83 | yes | 0.0 | 13.0..13.2 | RMS | use | 0.83 s prone shot |
| Prone Firing Rifle (1) | Fire.Prone | prone | 0.43 | yes | 0.0 | 13.0..13.0 | M | use | 0.43 s: the machine-gun burst loop |
| Firing Rifle (3) | Fire.Run | stand | 1.0 | yes | 0.0 | 91.9..98.1 | RA | use | firing while running (hips 92 = Rifle Run) |
| Shoot Rifle | Fire.Run | stand | 0.93 | yes | 0.0 | 85.3..97.6 | RA | use | run-and-shoot variant |
| Firing Rifle (5) | Fire.Sprint | stand | 0.57 | yes | 0.0 | 82.8..91.0 | A | use | hip fire at a sprint (0.57 s cycle): assault only |
| Firing Rifle | Fire.Stand.Aimed | stand | 0.27 | yes | 0.0 | 100.1..100.2 | RS | use | 0.27 s single aimed shot: the sniper and the rifleman |
| Firing Rifle (1) | Fire.Stand.Aimed | stand | 1.17 | yes | 0.0 | 100.1..100.2 | RS | use | 1.17 s aimed shot with a settle |
| Firing Rifle (2) | Fire.Walk | stand | 3.27 | yes | 0.0 | 94.2..99.8 | RA | use | firing while walking (hips 95 = Walking) |
| Firing Rifle (6) | Fire.Walk | stand | 1.37 | yes | 0.0 | 93.7..99.4 | RA | use | variant |
| Firing Rifle (7) | Fire.Walk | stand | 1.33 | yes | 0.0 | 94.2..99.9 | RA | use | variant |
| Rifle Prone Hit Reaction | Hit.Prone | prone | 1.23 | yes | 0.0 | 13.0..13.9 | RAMS | use | and, held on its last frame, the prone death (none in the set) |
| Hit Reaction (2) | Hit.Run | stand | 0.6 | yes | 0.2 | 82.8..91.8 | RAMS | use | 0.6 s: hit while running, keeps going |
| Hit Reaction (1) | Hit.Stand | stand | 2.3 | yes | 0.0 | 83.5..93.7 | RAMS | use | 2.3 s |
| Hit Reaction (3) | Hit.Stand | stand | 2.43 | yes | 0.0 | 85.4..95.2 | RAMS | use | 2.4 s variant |
| Hit Reaction | Hit.Stand.Heavy | stand | 3.1 | yes | 0.0 | 63.2..100.9 | RAMS | use | 3.1 s stagger to 63 cm and back: a heavy hit |
| Walking Hit Reaction | Hit.Walk | stand | 1.37 | yes | 0.6 | 93.7..99.1 | RAMS | use | hit while walking |
| Crouch Idle | Idle.Crouch | kneel | 6.7 | yes | 0.0 | 46.3..46.6 | RAMS | use | hips 46: squat, 6.7 s |
| Idle Crouching | Idle.Crouch | kneel | 2.1 | yes | 0.0 | 46.2..46.4 | RAMS | use | variant |
| Idle Crouching Aiming | Idle.Crouch.Aim | kneel | 2.1 | yes | 0.0 | 46.2..46.4 | RAMS | use | aimed from the squat |
| Rifle Kneel Idle | Idle.Kneel | kneel | 1.7 | yes | 0.0 | 38.9..38.9 | RAMS | use | hips 39: one knee down |
| Prone Idle | Idle.Prone | prone | 4.93 | yes | 0.0 | 13.0..13.2 | RAMS | use | 4.9 s |
| Idle | Idle.Stand | stand | 2.1 | yes | 0.0 | 96.9..97.6 | RAMSO | use | unarmed-style stance; use for the rear line when the rifle is slung |
| Idle Aiming | Idle.Stand.Aim | stand | 2.1 | yes | 0.0 | 97.8..97.9 | RAMS | use | aimed variant |
| Rifle Aiming Idle | Idle.Stand.Aim | stand | 3.1 | yes | 0.0 | 100.1..100.4 | RAMS | use | aimed, target held, not firing |
| Rifle Idle | Idle.Stand.Ready | stand | 8.57 | yes | 0.0 | 101.8..102.3 | RAMS | use | primary ready idle, looks about (8.6 s) |
| Rifle Idle (1) | Idle.Stand.Ready | stand | 2.83 | yes | 0.0 | 100.0..100.3 | RAMS | use | short variant (2.8 s), for the phase-offset pool |
| Rifle Idle (2) | Idle.Stand.Ready | stand | 7.7 | yes | 0.0 | 101.7..102.8 | RAMS | use | variant, head turns |
| Rifle Idle (3) | Idle.Stand.Ready | stand | 10.63 | yes | 0.0 | 97.3..102.0 | RAMS | use | variant, shifts weight (10.6 s) |
| Jump | Jump | stand | 2.77 | yes | 396.7 | 78.2..105.3 | - | ditch | 4 m running jump |
| Jump Backward | Jump | stand | 0.9 | yes | 0.0 | 93.3..112.8 | - | ditch | hop cycle |
| Jump Forward | Jump | stand | 0.9 | yes | 0.0 | 81.4..129.9 | - | ditch | hop cycle |
| Rifle Jump | Jump | stand | 2.27 | yes | 160.4 | 71.7..121.0 | - | ditch | running long jump, no situation |
| Rifle Jump (1) | Jump | stand | 0.6 | no | 0.9 | 104.1..135.9 | - | ditch | hop in place |
| Rifle Jump In Place | Jump | stand | 1.93 | yes | 0.2 | 76.3..125.8 | - | ditch | hop in place |
| Block With Rifle | Melee.Block | stand | 1.63 | yes | 0.0 | 88.4..100.4 | RA | use |  |
| Rifle Block | Melee.Block | stand | 2.8 | yes | 0.0 | 86.7..93.2 | RA | spare | 2.8 s variant |
| Upward Rifle Butt Strike | Melee.Crouch | kneel | 2.2 | yes | 0.0 | 55.7..56.5 | RA | use | butt strike from a crouch (hips 56) |
| Bayonet Stab | Melee.Stand | stand | 3.27 | yes | 0.0 | 74.2..89.8 | RA | use | 3.3 s |
| Rifle Punch | Melee.Stand | stand | 2.77 | yes | 0.0 | 71.5..92.6 | RA | use | butt to the face |
| Rifle Turn And Kick | Melee.Stand | stand | 2.77 | no | 88.5 | 88.8..107.7 | A | spare | turns first (47 deg) |
| Smash | Melee.Stand | stand | 3.0 | yes | 0.0 | 82.6..92.4 | RA | use | overhead |
| Left Cover Sneak W_ Rifle | Move.Cover.Left | crouch | 1.13 | yes | 0.0 | 89.5..95.4 | RAMS | use | sidestep along cover, rifle up |
| Right Cover Sneak W_ Rifle | Move.Cover.Right | crouch | 1.13 | yes | 0.0 | 89.4..95.4 | RAMS | use |  |
| Moving Backward In Crawl Position | Move.Crawl.Back | crouch | 1.7 | yes | 0.0 | 34.7..46.3 | RAMS | spare | hands and knees (hips 43), no forward partner |
| Moving Backward In Prone Position | Move.Prone.Back | prone | 1.9 | yes | 0.0 | 12.2..27.3 | RAMS | use | backing out of fire |
| 3e5cc2dade57d51f535dee9a2fe94j7f2 | Move.Prone.Fwd | prone | 3.27 | yes | 115.9 | 13.0..24.0 | RAMS | use | UNNAMED FILE: a belly crawl forward, 116 cm travel (strip). Rename to Prone Crawl Forward |
| Backwards Rifle Run | Move.Run.Back | stand | 0.6 | yes | 0.0 | 96.2..101.0 | RAMS | spare | rifle up variant |
| Run Backward | Move.Run.Back | stand | 0.5 | yes | 0.0 | 89.2..97.1 | RA | use |  |
| Run Backwards | Move.Run.Back | stand | 0.53 | yes | 0.0 | 96.5..101.0 | - | ditch | duplicate |
| Run Backward Left | Move.Run.Diag | stand | 0.5 | yes | 0.0 | 89.2..96.8 | - | ditch | diagonal |
| Run Backward Right | Move.Run.Diag | stand | 0.5 | yes | 0.0 | 92.8..99.3 | - | ditch | diagonal |
| Run Forward Left | Move.Run.Diag | stand | 0.5 | yes | 0.0 | 91.6..101.4 | - | ditch | diagonal |
| Run Forward Right | Move.Run.Diag | stand | 0.5 | yes | 0.0 | 92.6..100.4 | - | ditch | diagonal |
| Rifle Run | Move.Run.Fwd | stand | 0.73 | no | 0.1 | 83.7..95.0 | RAMS | use | the run (hips 85) |
| Rifle Run (1) | Move.Run.Fwd | stand | 0.67 | yes | 0.0 | 92.3..96.3 | RAMS | use | variant (hips 92) |
| Run Forward | Move.Run.Fwd | stand | 0.5 | yes | 0.0 | 84.8..95.6 | RA | spare | 8-way set |
| Run Left | Move.Run.Left | stand | 0.5 | yes | 0.0 | 95.0..102.4 | RA | use |  |
| Run Right | Move.Run.Right | stand | 0.5 | no | 0.0 | 88.9..97.4 | RA | use |  |
| Strafe | Move.Run.Side | stand | 0.67 | yes | 0.0 | 88.7..95.4 | RA | use | a running sidestep (0.67 s) |
| Idle To Running | Move.Run.Start | stand | 0.9 | no | 197.5 | 80.5..100.2 | RA | spare | 0.9 s variant |
| Rifle Start Run | Move.Run.Start | stand | 1.73 | no | 312.6 | 83.4..100.7 | RAMS | use | idle -> run (313 cm travel: strip; play at the sim speed ramp) |
| Rifle Backward Run To Stop | Move.Run.Stop | stand | 1.53 | no | 213.9 | 95.7..103.1 | - | ditch | covered |
| Rifle Run To Stop | Move.Run.Stop | stand | 1.47 | no | 293.9 | 85.3..101.9 | RAMS | use | run -> idle (294 cm: strip) |
| Sprint Backward | Move.Sprint.Back | stand | 0.5 | yes | 0.0 | 93.7..101.1 | - | ditch | nobody sprints backwards |
| Sprint Backward Left | Move.Sprint.Diag | stand | 0.5 | yes | 0.0 | 93.5..101.4 | - | ditch | diagonal |
| Sprint Backward Right | Move.Sprint.Diag | stand | 0.5 | yes | 0.0 | 92.5..97.6 | - | ditch | diagonal |
| Sprint Forward Left | Move.Sprint.Diag | stand | 0.5 | yes | 0.0 | 94.2..99.1 | - | ditch | diagonal |
| Sprint Forward Right | Move.Sprint.Diag | stand | 0.5 | no | 0.0 | 92.1..96.6 | - | ditch | diagonal |
| Sprint Forward | Move.Sprint.Fwd | stand | 0.5 | yes | 0.0 | 88.6..94.9 | RA | use | flat out, rifle in one hand |
| Sprint Left | Move.Sprint.Side | stand | 0.5 | yes | 0.0 | 92.3..101.2 | - | ditch |  |
| Sprint Right | Move.Sprint.Side | stand | 0.5 | yes | 0.0 | 89.2..101.3 | - | ditch |  |
| Rifle Crouch Backward Walk | Move.Stoop.Back | crouch | 2.0 | yes | 0.0 | 89.8..93.0 | RAMS | use | backing off while covering |
| Walk Crouching Backward | Move.Stoop.Back.Low | crouch | 1.0 | yes | 0.0 | 71.5..74.9 | RAMS | use |  |
| Walk Crouching Backward Left | Move.Stoop.Diag | crouch | 1.0 | yes | 0.0 | 68.9..72.5 | - | ditch | diagonal |
| Walk Crouching Backward Right | Move.Stoop.Diag | crouch | 1.0 | yes | 0.0 | 71.5..75.2 | - | ditch | diagonal |
| Walk Crouching Forward Left | Move.Stoop.Diag | crouch | 1.0 | yes | 0.0 | 71.6..75.2 | - | ditch | diagonals: the nearer cardinal is used |
| Walk Crouching Forward Right | Move.Stoop.Diag | crouch | 1.0 | yes | 0.0 | 67.7..72.2 | - | ditch | diagonal |
| Crouch Walking | Move.Stoop.Fwd | crouch | 1.17 | yes | 143.9 | 75.4..79.3 | - | ditch | not in place (144 cm travel) and covered |
| Crouched Walking | Move.Stoop.Fwd | crouch | 1.7 | yes | 0.2 | 75.2..79.8 | RAMS | spare | lower stoop (78), arms swing more |
| Rifle Crouch Walk | Move.Stoop.Fwd | crouch | 1.97 | yes | 0.0 | 87.1..91.8 | RAMS | use | primary in-trench and under-fire walk |
| Rifle Crouch Walk (1) | Move.Stoop.Fwd | crouch | 1.93 | yes | 0.0 | 86.4..92.7 | RAMS | use | variant |
| Walk Crouching Forward | Move.Stoop.Fwd.Low | crouch | 1.0 | yes | 0.0 | 71.6..74.7 | RAMS | use | hips 72, the lowest stoop: below a parapet |
| Crouch Walk Strafe Left | Move.Stoop.Left | crouch | 1.23 | yes | 0.0 | 74.6..82.3 | RAMS | use | hips 81 |
| Walk Crouching Left | Move.Stoop.Left.Low | crouch | 1.0 | yes | 0.0 | 72.6..75.4 | RAMS | use | sidling along a wall |
| Crouch Walk Strafe Right | Move.Stoop.Right | crouch | 1.2 | yes | 0.0 | 72.9..81.5 | RAMS | use |  |
| Walk Crouching Right | Move.Stoop.Right.Low | crouch | 1.0 | yes | 0.0 | 71.3..75.3 | RAMS | use |  |
| Crouched Run | Move.Stoop.Run | crouch | 0.77 | yes | 1.1 | 0.4..0.5 | - | ditch | the copy already in the project, hips at 0: same clip as (1) |
| Crouched Run (1) | Move.Stoop.Run | crouch | 0.77 | yes | 0.0 | 87.8..94.4 | RAMS | use | the crouched run (hips 88) |
| Crouch Run Backwards | Move.Stoop.Run.Back | crouch | 0.5 | yes | 0.0 | 86.2..90.7 | RA | use |  |
| Crouch Strafe Run Left | Move.Stoop.Run.Side | crouch | 0.5 | yes | 0.0 | 79.7..83.3 | RA | spare | mirror of Crouched Strafe Run |
| Crouched Strafe Run | Move.Stoop.Run.Side | crouch | 0.6 | yes | 0.0 | 80.6..88.0 | RA | use |  |
| Backwards Rifle Walk | Move.Walk.Back | stand | 1.27 | yes | 0.0 | 97.2..102.4 | RAMS | spare | variant, rifle up |
| Walk Backward | Move.Walk.Back | stand | 1.0 | yes | 0.0 | 85.0..91.0 | RAMS | use | covering a withdrawal |
| Walking Backwards | Move.Walk.Back | stand | 1.4 | yes | 0.0 | 90.9..100.2 | RAMS | spare | variant |
| Start Walking Backwards | Move.Walk.Back.Start | stand | 2.03 | no | 177.3 | 91.7..100.2 | RAMS | spare |  |
| Walk Backward Left | Move.Walk.Diag | stand | 1.0 | yes | 0.0 | 88.7..95.6 | - | ditch | diagonal |
| Walk Backward Right | Move.Walk.Diag | stand | 1.0 | yes | 0.0 | 85.0..91.4 | - | ditch | diagonal |
| Walk Forward Left | Move.Walk.Diag | stand | 1.0 | yes | 0.0 | 85.1..91.5 | - | ditch | diagonal |
| Walk Forward Right | Move.Walk.Diag | stand | 1.0 | yes | 0.0 | 84.1..92.6 | - | ditch | diagonal |
| Rifle Walk | Move.Walk.Fwd | stand | 1.3 | yes | 0.0 | 96.6..103.0 | RAMS | use | rifle low, walking: the default march |
| Walk Forward | Move.Walk.Fwd | stand | 1.0 | yes | 0.0 | 85.5..90.9 | RA | spare | 8-way set, forward |
| Walk With Rifle | Move.Walk.Fwd | stand | 1.1 | yes | 0.0 | 96.1..101.0 | RMS | use | rifle at the ready, walking (arms still) |
| Walk With Rifle (2) | Move.Walk.Fwd | stand | 3.37 | yes | 0.0 | 96.1..102.1 | RMS | spare | 3.4 s long cycle of the same |
| Walking | Move.Walk.Fwd | stand | 1.37 | yes | 0.0 | 94.3..99.1 | O | use | the plain walk: officer / slung rifle |
| Walking (1) | Move.Walk.Fwd | stand | 7.0 | no | 211.5 | 91.6..97.8 | - | ditch | 7 s with 2 m of travel: not a cycle |
| Walk With Rifle (1) | Move.Walk.Fwd.Low | stand | 1.43 | yes | 0.0 | 84.2..90.7 | RA | use | lower, wary walk (hips 85) |
| Strafe Left | Move.Walk.Left | stand | 1.03 | yes | 0.0 | 94.2..101.8 | RAMS | spare | variant |
| Walk Left | Move.Walk.Left | stand | 1.0 | yes | 0.0 | 82.0..91.1 | RAMS | use |  |
| Strafe Right | Move.Walk.Right | stand | 1.43 | yes | 0.0 | 96.4..100.6 | RAMS | spare | variant |
| Walk Right | Move.Walk.Right | stand | 1.0 | yes | 0.0 | 86.8..96.2 | RAMS | use |  |
| Rifle Side Step | Move.Walk.Side | stand | 1.17 | yes | 0.0 | 95.8..101.9 | RAMS | use | a slow sidestep, rifle up: edging along the fire step |
| Rifle Side Step (1) | Move.Walk.Side | stand | 1.5 | yes | 0.0 | 96.3..101.9 | RAMS | spare | variant |
| Rifle Backward Walk To Stop | Move.Walk.Stop | stand | 2.87 | no | 174.3 | 99.2..102.4 | - | ditch | covered |
| Rifle Walk To Stop | Move.Walk.Stop | stand | 3.57 | no | 181.1 | 96.3..103.1 | RAMS | use | 3.6 s: only the last 1.2 s is used |
| Stop Walking With Rifle | Move.Walk.Stop | stand | 2.27 | no | 123.2 | 98.8..102.9 | RAMS | spare | 2.3 s, turns 45 deg |
| Walk Backwards Stop | Move.Walk.Stop | stand | 1.37 | no | 91.6 | 94.5..100.2 | RAMS | spare |  |
| Prone Roll | Prone.Roll | prone | 1.7 | yes | 120.3 | 12.9..22.6 | RAMS | use | roll aside from a shell (120 cm travel: strip) |
| Roll Left | Prone.Roll | prone | 2.5 | yes | 127.3 | 12.7..23.5 | RAMS | spare | 2.5 s variant |
| Roll Left (1) | Prone.Roll | prone | 1.5 | yes | 118.7 | 13.0..26.7 | RAMS | spare | 1.5 s variant |
| Dodging | React.Duck | stand | 1.63 | yes | 0.0 | 79.0..100.6 | RAMS | use | ducks to 79 cm: near miss |
| Rifle Shielding Face | React.Shield | stand | 4.3 | no | 61.8 | 40.9..102.4 | RAMS | use | arm over the face, drops to a crouch (4.3 s): shell burst close, gas |
| Fall Over | React.Trip | stand | 2.0 | no | 79.9 | 11.2..100.4 | RAMS | use | trips flat (ends at 11 cm): mud, wire; get up with Prone To Kneel + Kneel To Stand |
| Reload | Reload.CrouchWalk | crouch | 3.67 | yes | 0.2 | 83.7..94.9 | RAM | use | stooped reload (hips 85) |
| Prone Reloading | Reload.Prone | prone | 6.4 | yes | 0.0 | 11.0..14.1 | RMS | use | 6.4 s: the MG belt change |
| Reload (1) | Reload.Stand | stand | 4.1 | yes | 0.1 | 93.7..99.3 | RA | use | 4.1 s variant, legs shift (walk-and-reload) |
| Reloading | Reload.Stand | stand | 3.3 | yes | 0.0 | 98.9..100.7 | RAS | use | 3.3 s; the sniper plays it at 2x as the bolt |
| Grab And Put Back Rifle | Sling | stand | 7.5 | yes | 0.0 | 97.1..103.2 | - | ditch | 7.5 s combined cycle; the two halves are covered |
| Grab Rifle And Put Back | Sling | stand | 6.17 | yes | 0.0 | 99.6..103.0 | - | ditch | duplicate |
| Grab Rifle From The Side And Put Back | Sling | stand | 5.03 | yes | 0.0 | 98.2..103.8 | - | ditch | duplicate |
| Put Back Rifle | Sling.Away | stand | 3.27 | no | 24.9 | 99.5..102.6 | - | ditch | duplicate of Rifle Put Away |
| Put Back Rifle (1) | Sling.Away | stand | 2.57 | no | 20.9 | 97.9..103.3 | - | ditch | duplicate |
| Put Back Rifle Behind Shoulder | Sling.Away | stand | 3.97 | no | 26.6 | 97.0..103.2 | - | ditch | duplicate |
| Rifle Put Away | Sling.Away | stand | 2.3 | no | 0.2 | 98.8..101.8 | RAMSO | use | sling: arriving in the rear trench |
| Grab Rifle From Back | Sling.Ready | stand | 2.9 | no | 24.9 | 99.7..103.1 | - | ditch | duplicate of Rifle Pull Out with 25 cm root motion |
| Grab Rifle From Behind Shoulder | Sling.Ready | stand | 3.53 | no | 26.6 | 99.9..103.3 | - | ditch | duplicate |
| Grab Rifle From The Side | Sling.Ready | stand | 2.47 | no | 20.9 | 101.2..104.7 | - | ditch | duplicate |
| Rifle Pull Out | Sling.Ready | stand | 2.03 | no | 0.3 | 101.2..102.2 | RAMSO | use | unsling: rear line -> alarm |
| Crouch To Standing With Rifle | Stance.CrouchToStand | kneel | 1.1 | no | 0.7 | 45.5..101.2 | RAMS | use | from the squat |
| Rifle Kneel To Prone | Stance.KneelToProne | kneel | 1.97 | no | 0.4 | 12.5..47.6 | RAMS | use | 2.0 s |
| Rifle Kneel To Stand | Stance.KneelToStand | kneel | 1.23 | no | 23.1 | 38.8..101.9 | RAMS | use | 1.23 s |
| Rifle Crouch Idle To Walk | Stance.KneelToStoop | kneel | 2.4 | no | 46.5 | 38.9..92.3 | RAMS | use | kneel -> stooped walk |
| Rifle Prone To Kneel | Stance.ProneToKneel | prone | 1.97 | no | 10.8 | 13.0..47.5 | RAMS | use | 2.0 s |
| Rifle Stand To Kneel | Stance.StandToKneel | stand | 0.93 | no | 23.0 | 38.9..101.9 | RAMS | use | 0.93 s |
| Rifle Idle To Crouch | Stance.StandToStoop | stand | 2.43 | no | 53.3 | 74.5..101.7 | RAMS | use | stand -> stooped walk (53 cm travel: strip) |
| Crouch Walk Backwards Stop | Stance.StoopToKneel | crouch | 1.37 | no | 96.4 | 45.2..90.7 | RAMS | spare | backing off then kneeling; 96 cm travel |
| Rifle Crouch Walk To Kneel | Stance.StoopToKneel | crouch | 2.73 | no | 140.2 | 38.9..92.3 | RAMS | use | stooped -> kneel (140 cm travel: strip) |
| Rifle Crouch Walk To Idle | Stance.StoopToStand | crouch | 2.93 | no | 53.5 | 86.7..101.9 | RAMS | use | stooped -> stand |
| Single Step Right | Step | stand | 1.53 | yes | 52.0 | 97.4..100.2 | - | ditch | 52 cm root motion single step |
| Stepping Forward | Step | stand | 2.0 | yes | 51.9 | 97.3..100.3 | - | ditch | 52 cm root motion single step |
| Toss Grenade | Throw | stand | 3.2 | no | 0.0 | 84.3..100.6 | RA | use | 3.2 s; the release is at ~1.4 s |
| Jump Up | Trench.ClimbOut.A | stand | 0.53 | no | 0.8 | 69.5..97.6 | RAMS | use | take-off: the first half of climbing the parapet |
| Jump Loop | Trench.ClimbOut.B | stand | 1.0 | yes | 0.0 | 96.9..97.2 | RAMS | use | airborne hold while the sim lifts the man |
| Jump Down | Trench.ClimbOut.C | stand | 0.67 | no | 0.8 | 77.9..97.6 | RAMS | use | landing |
| Jumping Down | Trench.DropIn | stand | 2.93 | no | 126.1 | 66.6..165.9 | RAMS | use | starts 0.66 m up and lands: the drop into a trench (126 cm travel: strip) |
| Jumping Down (1) | Trench.DropIn | stand | 2.83 | no | 172.6 | 76.3..163.3 | RAMS | use | variant (0.63 m) |
| Crouching Turn 90 Left | Turn.Crouch.L90 | kneel | 1.27 | no | 1.3 | 45.9..56.5 | RAMS | use |  |
| Crouching Turn 90 Right | Turn.Crouch.R90 | kneel | 1.27 | no | 1.3 | 46.2..63.4 | RAMS | use |  |
| Rifle Turn | Turn.Stand | stand | 1.7 | no | 7.6 | 99.8..101.7 | RAMS | use | standing turn set (8): 45/90/135/180 each way; the 90s and 180s are wired |
| Rifle Turn (1) | Turn.Stand | stand | 1.53 | no | 7.8 | 98.4..101.9 | RAMS | use |  |
| Rifle Turn (2) | Turn.Stand | stand | 1.97 | no | 21.8 | 98.8..102.0 | RAMS | spare |  |
| Rifle Turn (3) | Turn.Stand | stand | 1.87 | no | 30.0 | 99.0..102.1 | RAMS | spare |  |
| Rifle Turn (4) | Turn.Stand | stand | 1.3 | no | 6.5 | 100.1..102.1 | RAMS | use |  |
| Rifle Turn (5) | Turn.Stand | stand | 1.7 | no | 26.9 | 101.0..102.0 | RAMS | use |  |
| Rifle Turn (6) | Turn.Stand | stand | 1.47 | no | 28.3 | 100.4..102.0 | RAMS | use |  |
| Rifle Turn (7) | Turn.Stand | stand | 1.87 | no | 22.4 | 98.7..102.8 | RAMS | spare |  |
| Left Turn | Turn.Stand.L90 | stand | 1.5 | no | 26.7 | 91.8..100.2 | - | ditch | covered |
| Turn 90 Left | Turn.Stand.L90 | stand | 1.0 | no | 1.9 | 94.8..97.6 | O | use | plain 1 s turns for the slung / officer rig |
| Turning Right 45 Degrees | Turn.Stand.R45 | stand | 2.0 | no | 0.7 | 96.3..100.3 | - | ditch | covered by the rifle set |
| Right Turn | Turn.Stand.R90 | stand | 1.8 | no | 45.7 | 97.5..100.1 | - | ditch | covered |
| Turn 90 Right | Turn.Stand.R90 | stand | 1.0 | no | 1.9 | 95.2..98.0 | O | use |  |
| Turning Right 90 Degrees | Turn.Stand.R90 | stand | 2.4 | no | 1.1 | 97.9..100.2 | - | ditch | covered |
| Rifle Crouch Turn | Turn.Stoop | crouch | 1.43 | no | 20.4 | 79.7..84.7 | RAMS | use | stooped turn set (8): 45/90/135/180 each way; only the 90s and 180s are wired |
| Rifle Crouch Turn (1) | Turn.Stoop | crouch | 1.23 | no | 30.3 | 81.1..88.4 | RAMS | use |  |
| Rifle Crouch Turn (2) | Turn.Stoop | crouch | 1.5 | no | 38.0 | 81.1..87.9 | RAMS | spare |  |
| Rifle Crouch Turn (3) | Turn.Stoop | crouch | 1.5 | no | 39.4 | 80.3..87.7 | RAMS | spare |  |
| Rifle Crouch Turn (4) | Turn.Stoop | crouch | 1.27 | no | 18.9 | 81.0..89.9 | RAMS | use |  |
| Rifle Crouch Turn (5) | Turn.Stoop | crouch | 1.47 | no | 4.5 | 81.1..91.5 | RAMS | use |  |
| Rifle Crouch Turn (6) | Turn.Stoop | crouch | 2.07 | no | 13.2 | 81.1..87.1 | RAMS | spare |  |
| Rifle Crouch Turn (7) | Turn.Stoop | crouch | 1.47 | no | 27.6 | 80.2..85.0 | RAMS | use |  |

ditch 46, spare 28, use 131

Seconds of animation: ditch 85 s, spare 45 s, use 274 s.
