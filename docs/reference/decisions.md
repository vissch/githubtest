# Owner decisions

What the owner has decided, newest last, so nobody re-asks or quietly undoes one. These win over any doc, plan or
code comment that disagrees; fix the other place. **A new decision is written here in the same turn it is made**
(AskUserQuestion answer, or the owner's own words), with the date and where it came from. The story behind it can
go in `agent-memory.md`.

Sources: `docs/11-plan-review.md` §1, `docs/archive/handoff-2026-09-22.md`, and the dated log in `agent-memory.md`.

## Platform, scope, team
| Date | Decision |
|---|---|
| 2026-09-20 | Ship **Windows x64 only**. Determinism is same-build (Burst `FloatMode.Strict`, floats, no fixed point). |
| 2026-09-20 | Online multiplayer: **keep the door open**. Lockstep, replays and loopback stay; real transport comes after Mission 3. |
| 2026-09-20 | Two developers, one can do art. Entities / Entities Graphics are out of the project. |
| 2026-09-20 | **3,000 units maximum.** `SimConfig.MaxSlots` = 3584. Rendering is the full VAT path. |
| 2026-09-21 | 8 unit types per player, full 3D units, not sprites. The owner makes the art. |

## Look and view
| Date | Decision |
|---|---|
| 2026-09-21 | **Standard view: everything is built for it.** fov 25, pitch 25, zoom 30, turned 21° toward the enemy, following the battle along Z. |
| 2026-09-21 | Visual standard: painted cartoon mudfield. Night is the default look (`docs/reference/battlefield-night.jpeg`). |
| 2026-09-21 | Placement rule: **nothing is ruled or evenly spread.** Lines wander, point features cluster (one big, a few medium, many small). Trenches are bent. |
| 2026-09-21 | Battlefield: river is an obstacle with fords and a bridge; trees, stumps and wrecks give cover, block, and are destructible; fixed seed and params per mission. |
| 2026-09-21 | Unit art budget: full model 1,200-1,500 verts (max 2,000), far model 250-400 beyond 170 m, vehicles 3,000-5,000. |
| 2026-09-22 | Imported props drawn 2-3x their imported size. Bunkers, field guns and observation stands only at each side's back edge or on the far (-X, fog) side, openings toward the fog. |
| 2026-09-23 | **Machines are gigantic and infantry 25% shorter.** `VehicleSize` Walker 2.5, Tank 1.7 (`Sim/Nav/VehicleKinematics.cs`); `VATRenderer.UnitScale` 1.125. |
| 2026-09-23 | **Winter troops keep khaki.** No greatcoat, no second palette. Warm pixels on the men in winter are intended; do not tune them away. |
| 2026-09-26 | **Man-made props are true to the soldier; machines stay giant.** Doors, crates, lanterns, guns, carts, houses and sandbags get strict bounds against the drawn man (`FigureMetrics`, 2.0 m at close zoom; the readability grow is a men-only exception). Walkers and tanks keep `VehicleSize` as a stylised exception. Rocks, debris, mounds, stumps and logs vary freely. Supersedes the 2026-09-22 "2-3x" rule for man-made props. (Overhaul directive, AskUserQuestion.) |

## Levels
| Date | Decision |
|---|---|
| 2026-09-22 | The sea lies beyond the **enemy** line. Deployed reinforcements **ride the boats in**, not instant spawns. |
| 2026-09-23 | **Coast and snow levels are the focus, coast first**, each a real `BattlefieldParams` preset with its own mission card, not a reskin. The volcano/lava level is deprioritised. |

## Destruction and combat
| Date | Decision |
|---|---|
| 2026-09-23 | Shelters lose sandbags but stand, and reduce artillery damage. |
| 2026-09-24 | **The trench always stands and always gives limited protection.** No trench cave-in. A shell in a man's own bay does 0.7 damage (`BlastRules.TrenchBayFactor`). |
| 2026-09-24 | Buildings go into the sim with cover, artillery protection and collapse damage, but **never block movement**. |
| 2026-09-24 | Explosions are directional in look **and** damage. The ground is permanently dynamic and a hole grows with repeated hits. |
| 2026-09-24 | Walkers limp as they lose legs; tank tracks degrade by degrees. |
| 2026-09-24 | Shelters are not exempt from small-arms wear: bags go first (~1 min of one MG), then the shell at concrete's rate. |
| 2026-09-24 | House chunk mask widened to 96 chunks (`HouseKit.MaxChunks`), so ruins come down a course at a time. |
| 2026-09-26 | **Death physics are VAT launched bodies, not physics ragdolls.** The blast throw arc scales with how many men died nearby, bodies tumble in flight and pile on earlier bodies; presentation only, seeded so replays agree. (Overhaul directive, AskUserQuestion.) |
| 2026-09-26 | **Mines and tripwires are laid by a sapper unit during the match**, not in a preparation phase: a new `InfantryArchetype.Sapper` in both faction pools (after `lane/show/units-meta` lands its ten slots), walking out on a `UnitAbility` command. (Overhaul directive, AskUserQuestion.) |

## Interface
| Date | Decision |
|---|---|
| 2026-09-23 | UI Toolkit HUD in the Dust Front style: keep the bottom-bar positions, full scope including main menu, mission select, pause, settings, debrief. Placeholder art now, artist textures later. |
| 2026-09-23 | Unit selection the way Dust Front does it, with ten control groups. An armoury screen showing unit cutouts. |
| 2026-09-24 | Hover card beside the cursor over a unit. |

## Process
| Date | Decision |
|---|---|
| 2026-09-23 | Single player runs **one** sim world. The two-world check is an opt-in determinism canary (`feature-flags.md`). |
| 2026-09-23 | Performance fidelity bar: a change that keeps the image lands freely; one that should be "indistinguishable" needs before/after captures and a critic. Measure in the editor **and** a Windows player build. |
| 2026-09-25 | Navigation docs live in the repo (`CLAUDE.md` → `docs/reference/`), checked by `Tools/codemap.py`. |
| 2026-09-26 | **The six-phase overhaul (scale audit, contextual scatter, modular trench, death animations, commander abilities, home front + campaign) is done by one session on both lanes:** SIM seams on `lane/sim/overhaul` first, alone and gated; SHOW work on `lane/show/overhaul`; worktree `githubtest-overhaul`. Plan of record: `docs/21-overhaul-2026-09.md`. (AskUserQuestion.) |

## Open: waiting on the owner
Do not build any of these without asking. Ask with AskUserQuestion, then move the answer up.
- **M1.5 fun-gate playtest** with both developers is still not done. docs/11 says nothing after it is scheduled until it passes; work has continued on the owner's word.
- **Sim protection from shelters:** map-generator shelter positions, or trench-bay protection? `NavLayer.Bunker` is never set today.
- **Do houses give sim cover?** Needs a hash change.
- **Where the ruins set goes.** It is cut, imported and tested, and nothing places it (`BattlefieldComposer` only uses Houses and Military).
- **Explosions batches B and C** (sky flash, shock ring, per-weapon recipes; scar layer, smouldering craters, haze). Batch A shipped; B and C wait for a go.
- **Not scaled with the giant machines:** trench cross width, slope limit, turn rates, speeds, `MaxGrow`. Balance, not geometry.
- **Forward+** renderer: the perf pass was to switch and let the owner judge the night look. Not recorded as done.
- **Repo hygiene** from the maintainability audit: Git LFS for FBX and `.bytes`, removing `github-test1/`, whether CI builds Windows.

## Plans that live outside the repo
These memory entries cite plan files that exist only on the workstation in `~/.claude/plans/`. Their substance is
summarised in the docs named; the files themselves are not readable from another machine or lane.
| Plan file | Topic | Where the substance is |
|---|---|---|
| https-www-youtube-com-watch-v-virkcw0kxf-spicy-muffin.md | UI Toolkit HUD and menus | `docs/17-ui-art-spec.md`, this page (Interface) |
| check-the-current-unity-generic-leaf.md | Performance pass | `docs/05-performance-budgets.md` |
| it-seems-that-the-harmonic-cosmos.md | Destruction v2 | `docs/16-destruction.md` |
| for-the-trench-warfare-lovely-cerf.md | Explosions batches A-C | `docs/16-destruction.md`, the open item above |
| pasted-content-id-f06f-system-directive-dazzling-clover.md | The six-phase overhaul (2026-09-26) | `docs/21-overhaul-2026-09.md`, this page |
