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
| 2026-09-23 | **Machines are gigantic and infantry 25% shorter.** `VehicleSize` Walker 2.5, Tank 1.7 (`Sim/Nav/VehicleKinematics.cs`); `FigureMetrics.UnitScale` 1.125 (`Presentation/Core/FigureMetrics.cs`, read by the renderer and the picker). |
| 2026-09-27 | **Lower LODs are derived from LOD0**, not taken from the art tool's own lower-poly sculpts (Tripo bakes each level as a different model and texture, so every switch popped). `frogrig.py` / `tank3split.py` `TW_DERIVE=12` by default; measured in docs/22. |
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
| 2026-09-28 | **Two new machines are battle units that the Unit Sandbox fields and no faction does** (owner: make them "battle units", keep the original game; the sandbox is how they are tested). The Skimmer (archetype 19, a hovercraft) and the Salvo (20, a half-track rocket truck) are in `UnitDefinitions.All`, in no `FactionRoster` slot or pool. The Skimmer drives as a **tracked** machine (`ChassisKind.Tracked`, bog and ditch chances zero, bridges 3.5 m): `ChassisKind.Wheeled` has no movement or module rules of its own yet (the agent's choice, 2026-09-28). The unit table's fingerprint is hashed, so this is replay format v10. |
| 2026-09-28 | **The Salvo's shot is a rack of twelve rockets in the sim, each landing on its own tick** (owner, through the coordinator: the damage and the drawn rockets land together). The one shell became twelve delayed bursts (format v11). Calibrated to the harm the shell did, not its kills: over 20 seeds on 25 men 2.5 m apart the rack takes 23,628 hp off them (the shell 23,819) and kills 164 (the shell 225), the same damage spread over more men (the agent's choice; raising it to match the kills overshoots the harm by a third). (Superseded 2026-09-28 by the critic's round: 16 rockets, one per tube.) |
| 2026-09-28 | **The Salvo stands off; the Skimmer and Salvo keep clear of the Maw** (the critic's round, the agent's choices): the Salvo holds where it is while it has a target (`TankSpec.StandOff`), rather than drive on into the enemy's lines like every other machine, and its rack is 16 rockets, one per tube of the model (the harm per shot measured unchanged). The Skimmer and the Salvo keep 1.2 m more room (`VehicleProfile.Clearance`) because the Maw is drawn wider than its footprint; the Maw itself is left as it shipped. Replay format v12. |
| 2026-09-28 | **The balance critic's round** (the agent's choices on the critic's report; numbers in docs/22): the Salvo answers machines with its rockets (a rack may take a machine as its mark; its bursts test the deck) rather than giving the rockets armour-piercing, and carries the Tusk's hull machine gun (110 m) for the 60 m its rockets cannot reach; cost stays 380. Its hold gives up after 32 s on a mark that loses nothing. The Skimmer hunts light machines with a 12 mm machine gun (only a machine flagged `HuntsArmour` does: a shipped Tusk's 9 mm is not let at a Maw's 8 mm side), costs 180, holds at 90 m and looks down into a trench within 20 m. The rack's weight is unchanged. The shipped Kettle's halted-hull blind spot is left as the original game has it. |
| 2026-09-28 | **Critic round 3:** a machine's stand-off hold counts hit points its mark loses to anyone, not only to it (a burst carries no shooter slot; crediting only its own hits would need one, a wider sim change), and holds only on the first goal it was given, so a later order, ability or hero's call that sends it elsewhere is obeyed. Players have no move order for a machine today (the agent's choices). |
| 2026-09-28 | **Critic round 4:** a Salvo's rockets still in their tubes when it dies (or its slot is reused) are never fired; a rack's landing points are clamped to the map; the rockets' trails are one ribbon mesh drawn through `FrameBudget` (not flipbook cards, which one rack could flood), with URP's Particles/Unlit kept in the build by `Resources/ShaderKeep/KeepParticlesUnlit.mat`; the Skimmer is drawn hovering (presentation only). Replay format v15 (the agent's choices on the critic's findings). |
| 2026-09-28 | **Deaths are slapstick and gore** (owner: "lots of the fun arrives as units die, we need to make this more absurd"). Cartoon physics (big arcs, flips, bounces and skids, pancakes under tracks, helmets popping off) and more dismemberment and blood; the GORE slider still turns blood and flesh down. Presentation only and seeded, as the 2026-09-26 rule says; still no ragdolls. How absurd is a developer knob, `fx.deathAbsurd` (0 is exactly today's deaths, 1 the new look, 2 ludicrous), with **no settings slider**; it stays at 0 until the owner has seen the captures. (AskUserQuestion.) |
| 2026-09-28 | **Better low-poly models for deaths are break-apart parts only**: body parts cut in Blender from the soldier figures already in the repo, hull chunks and a scrap heap cut from the battle vehicle models already in the repo. The base figures, the VAT atlases and every machine's LODs stay as they are; no VAT rebake for this. (AskUserQuestion.) |
| 2026-09-28 | **A wreck breaks in stages and then disappears**, and each stage changes the game: whole wreck (blocks, 50 % cover), broken wreck (blocks, about 35 %), scrap pile (does not block, about 15 %), gone. Explosions, heavy machines ramming it, sustained machine-gun fire and deliberate fire wear it down. Implements the 2026-09-21 "wrecks … are destructible". A SIM change with a replay format bump, landed as its own lane before the drawing. (AskUserQuestion.) |
| 2026-09-28 | **Deliberate fire at a wreck is automatic only**: a gun with no living target shoots a wreck that shelters enemies. No new player command and no HUD work. (AskUserQuestion.) |
| 2026-09-28 | **Breaking a wreck does not lower its salvage value**: a wreck is worth what it was when the machine died (`WreckRecord` and the debrief unchanged). (AskUserQuestion.) |

## Interface
| Date | Decision |
|---|---|
| 2026-09-23 | UI Toolkit HUD in the Dust Front style: keep the bottom-bar positions, full scope including main menu, mission select, pause, settings, debrief. Placeholder art now, artist textures later. |
| 2026-09-23 | Unit selection the way Dust Front does it, with ten control groups. An armoury screen showing unit cutouts. |
| 2026-09-24 | Hover card beside the cursor over a unit. |
| 2026-09-27 | **The battle bar may shrink at 16:9.** With ten roster slots and seven support cards it is 1,732 px wide and draws at 0.91 on a 16:9 screen (`HudView.BarFit`), rather than narrowing the infantry cards to fit (180 to 154 px). `HudLayoutTests` holds it at no less than 0.9. Revisit with the HUD's next layout pass. |

## Process
| Date | Decision |
|---|---|
| 2026-09-23 | Single player runs **one** sim world. The two-world check is an opt-in determinism canary (`feature-flags.md`). |
| 2026-09-23 | Performance fidelity bar: a change that keeps the image lands freely; one that should be "indistinguishable" needs before/after captures and a critic. Measure in the editor **and** a Windows player build. |
| 2026-09-25 | Navigation docs live in the repo (`CLAUDE.md` → `docs/reference/`), checked by `Tools/codemap.py`. |
| 2026-09-26 | **The six-phase overhaul (scale audit, contextual scatter, modular trench, death animations, commander abilities, home front + campaign) is done by one session on both lanes:** SIM seams on `lane/sim/overhaul` first, alone and gated; SHOW work on `lane/show/overhaul`; worktree `githubtest-overhaul`. Plan of record: `docs/21-overhaul-2026-09.md`. (AskUserQuestion.) |
| 2026-09-27 | **SIM landing order: the overhaul first.** `lane/sim/overhaul` and `lane/show/overhaul` land together (`land.py --carry-sim`, under the 2026-09-26 "one session on both lanes" decision); `lane/sim/units-meta` and `lane/show/units-meta` rebase onto it afterwards and renumber the ability id 10 clash (ParaDrop vs StrafeRun). Merging waits until every session has pushed. |
| 2026-09-27 | **The overhaul lands as one merge commit** (owner's choice), the one exception to "no merge commit on integration": its 140 commits hold 12 merges of its SIM lane, and a rebase would replay them and re-raise every conflict they resolved. The merge is made on a branch cut from integration, so integration stays the first parent. Every other lane rebases. |
| 2026-09-27 | **aosa lands as one merge commit too** (owner's choice): its 75 commits turn constants into run-time knobs in the code integration and the overhaul rewrote, so a rebase would stop at 26 of them in the same regions; one merge resolves 11 files once. |
| 2026-09-27 | **units-meta lands as one merge commit on the overhaul** (owner's choice; a rebase would stop at 16 of its 20 commits). Its SIM work is redone in the merge: `ParaDrop` becomes ability 12 (`StrafeRun` 10 and `Beam` 11 keep theirs), its random streams 17-21, `FormatVersion` 9 (the overhaul's 8 + 1), its events after the overhaul's. Every faction may call the overhaul's six abilities and Brass alone the paratroopers (`FactionRoster.AbilityMask`; the owner may split them per faction). Keys: the digit row is the ten deploy slots, F5 F6 F7 arm HE, gas and the drop, C M V B the four line abilities. The HUD shows the drop card to both sides, as on units-meta's own branch, and the sim refuses it to Iron. |
| 2026-09-28 | **Go for a VFX pass over every ability and unit action, explosion batches B and C included** (sky flash, shock ring, per-weapon recipes; scar layer, smouldering craters, haze): catalogue every event, design its look at close, standard and far zoom, generate new sheets on the desktop (ComfyUI, `tw-vfx-sheets`), implement on `lane/show/pipe-vfx`. Lands only on the owner's word. (Owner, in the pipeline session.) |
| 2026-09-28 | **The FlipbookFx ordinal bug is fixed inside the VFX pass**, as its own commit on `lane/show/pipe-vfx`: `Book.Core/Head/Bloom` drew the wrong Fire rows (`Sheets` out of order with the enum; the tw3d-board finding BUG-desktop-20260928-flipbook-ordinals). A test holds `Sheets[(int)b].Name == "Fire"+b`; before/after flameshots go to the owner. (Owner, AskUserQuestion.) |

## The Proving Ground (a test level with every unit)
| Date | Decision |
|---|---|
| 2026-09-28 | **The five playground prototypes (Brute, Croaker, Hopper, Mercy, Frog) become stand-in battle units** with placeholder stats: machines first, the Frog as a VAT figure last. Archetypes 21-25 in `UnitDefinitions.All`, no faction's pool. (AskUserQuestion.) |
| 2026-09-28 | **The idea units get cheap stand-ins where the sim can carry them** (a spec or a weapon the sim already reads: Sentry, AT rifle, Death Battalion, the historical armour drawn as shipped models; the Sapper on `MineSystem`, the Flamethrower on `BurningSystem`); the rest stay idea cards in the level's panel. Archetypes 26-36. (AskUserQuestion.) |
| 2026-09-28 | **Allied selection in the level = an in-match panel (spawn 1/5/10 at the rally) plus a launch screen that picks any ten for the deploy bar**, so the real deploy path (cost, cooldown, boats) is testable too. (AskUserQuestion.) |
| 2026-09-28 | A Proving Ground match never ends: `SimConfig.Endless`, so support calls and the bombardment keep going after a tester's wave takes an HQ (the agent's choice; replay v16). The Hopper drives as a legless walker profile (strides trenches and wire, never bogs) and is drawn hovering: the sim has no flight, and it is shot at ground level (the agent's choice). |

## How units move and fight
| Date | Decision |
|---|---|
| 2026-09-28 | **Units spread over the map instead of walking in rows on set paths, and go out of their way to attack each other** (the owner, playing the game). Built on `lane/sim/nav-engage` as lanes, trench walls crossed anywhere, a corrected flow field and `EngageSystem` (`tasks.md`, Movement and Infantry combat; replay v18). The choices made on the way are the agent's and are listed under Open. |
| 2026-09-28 | **Men are deployed across the whole width of the field** (the owner, told what it costs): reinforcements on foot come up each on his own lane (`Lane.Of` in `SimWorld.Deploy`), and men off a boat fan out to theirs. The cost is accepted: a garrison is thin. Ten men on the greybox field (284 m wide) stand some thirty metres apart in their trench, so what reaches men by radius reaches fewer of them: a hero's rally (20 m) found two of twelve where it found three or more, and a Breaker's guns had the one man before it down before its claws reached him. Two tests had their garrisons made thicker for it (HeroTests forty men, BreakerTests a knot put down behind the trench); the rules themselves are unchanged. |

## Open: waiting on the owner
- **Spread and the fight (2026-09-28), the agent's choices, each one a constant or a rule to turn:** (1) a man climbs
  out of and drops into a trench anywhere, so ladders are no longer the only way (`FlowField.CanStepInfantry`); (2) a
  garrison stays in its trench and fights from the step: only men in the open go after the enemy; (3) nobody stands
  in the open to duel across wire or a trench, and men in an enemy trench are stormed, not hunted; (4) he hunts within
  70 m and stops to shoot at half his weapon's range (45 m under a `>>` order), kneeling; (5) mud costs a path twice
  dry ground, not four times; (6) machines keep to lanes too but do not hunt (their guns choose their own targets).
  Measured in the test battle (both sides ordered forward every 5 s): fights in the open are now fought out to the
  last man, and the side caught in front of the other's trench line loses. (Deployment across the whole width was
  one of these and is decided: the table above.)
- **The Sapper's pool (2026-09-28):** the 2026-09-26 row puts `InfantryArchetype.Sapper` in both faction pools; the
  Proving Ground seam keeps him, and every stand-in, out of every pool until the owner says which of the sixteen the
  shipped factions field. Adding one to a pool is one line in `FactionRoster.Pools` (a seam commit).
Do not build any of these without asking. Ask with AskUserQuestion, then move the answer up.
- **M1.5 fun-gate playtest** with both developers is still not done. docs/11 says nothing after it is scheduled until it passes; work has continued on the owner's word.
- **Sim protection from shelters:** map-generator shelter positions, or trench-bay protection? `NavLayer.Bunker` is never set today.
- **Do houses give sim cover?** Needs a hash change.
- **Where the ruins set goes.** It is cut, imported and tested, and nothing places it (`BattlefieldComposer` only uses Houses and Military).
- **Not scaled with the giant machines:** trench cross width, slope limit, turn rates, speeds, `MaxGrow`. Balance, not geometry.
- **Forward+** renderer: the perf pass was to switch and let the owner judge the night look. Not recorded as done.
- **Record live matches for debugging?** A recorder needs a hash every tick, which single player turns off for
  speed. Options: pay for per-tick hashing, or a commands-only replay format (a seam change). Audit R9.
- **A CI check for the docs and tools** (`validate.py`, `Tools/selftest.py`, Python only), required before a lane
  lands on the integration branch? Audit R10.
- **`main`** is at afc6fe8, far behind the integration branch, and is what a fresh clone checks out. Fast-forward it,
  or make the integration branch the default?
- **Repo hygiene** from the maintainability audit: Git LFS for FBX and `.bytes`, removing `github-test1/`, whether CI builds Windows.
- **The house kits' tan in a night ruin** (asset playground): a shelled Boilerhouse's chunks show its tan plaster
  (atlas 82/68/57), which reads as dark brown crates under the night light. Palette/source art; the cut faces
  themselves are fixed in the playground's prototype and, since 2026-09-27, in `housesplit.py`; the game's committed
  chunks were cut before it and are not regenerated (most sets' split settings are unrecorded; re-cutting changes
  every building). Owner: re-cut the sets, or leave them. Measured 2026-09-27 (loop 2): those brown faces are FLAT (colour
  spread under 2/255 over an 80x120 px patch of a shelled ruin), painted so by the kit; repainting them with masonry is
  an art call, not a split fix.
- **Faction abilities after the merge (2026-09-27):** every faction may call the overhaul's six abilities and Brass
  alone the paratroopers (`FactionRoster.AbilityMask`). Split them per faction? And the HUD shows the drop card to an
  Iron player, whose call the sim then refuses: hide cards the side's faction cannot call (`HudView.Offered`)?
- **Heroes in the stress bench:** `HeroSystem.TeamMask` is 1, so the stress preset's player side can get a hero who
  takes the front trench over the top (about tick 860 on ShelledForest). Bench runs compared across builds carry that
  event; the preset's tests run without heroes. Turn heroes off in the stress preset itself?
- **The bench report's frame budget twice:** `frame_draw_calls`/`frame_vertices`/`frame_indirect_draws` (overhaul) and
  `frame_budget_*` (aosa) read the same `FrameBudget`. Keep one set of names?
- **The `-land` branches on origin** (`lane/show/*-land`, pushed by `land.py` when the lanes landed on 2026-09-27) are
  copies of what is on the integration branch. Delete them once every lane session has caught up
  (`docs/inbox/2026-09-27-all-lanes-landed.md`).
- **The frog's far LOD size:** 300 tris ships (pop overlap 0.855 with the shoulders kept); 380 tris 0.865, 460 0.868
  (measured 2026-09-27 on the derived LODs; the source level and the rig made no difference).
- **The Skimmer and the Salvo (sandbox units, 2026-09-28):** the Salvo beats a Tusk 6 in 10 (the balance critic expected
  the Tusk to win at a price); the Skimmer cannot touch a Tusk (all its plate is 10 mm or more, 12 mm or more at front and
  sides: the Skimmer's gun is 12 mm); a halted Kettle whose path does not point at its targets never fires (the original
  game's machine, left alone). Still to check in Play: rockets reaching the ground on their burst's frame, the trails at
  night (alpha 0.7), the hover over trenches, both far LODs at 170 m, and the ribbon shader in a Windows build.

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
| lots-of-the-fun-cuddly-floyd.md | Absurd deaths and the wreck life cycle (2026-09-28) | this page (Destruction and combat), `docs/16-destruction.md` as it is built |
