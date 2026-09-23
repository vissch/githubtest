# 16. Destruction: everything breaks, for nothing

Owner's brief (2026-09-23): "every explosion has real impact: parts fall off tanks, limbs fly away, dirt, buildings
fall into pieces, trees break", and it must be cheap on the CPU and the GPU. This is the design that landed, what it
costs, and what is still open.

## The rule

**The sim decides what breaks. Presentation decides how the pieces fly. The pieces cost the CPU nothing a frame.**

- What breaks is already in the deterministic sim and hashed: `BlastSystem` (damage and knock), `DeformationSystem`
  (craters, tree → broken tree → stump, wire, wrecks), `VehicleModulesSystem` (modules, crew, fire, cook-off, legs).
  Nothing in this feature adds sim state or changes the replay format.
- How a piece flies is a 96-byte record written once, on the event, into a `GraphicsBuffer`. `TW/Debris (URP)`
  integrates the flight in the vertex shader from the record and the clock. There is no per-frame CPU work per piece,
  no PhysX (banned, `docs/05`), no GameObject, no allocation after load.

## The renderer: `Presentation/Camera/DebrisRenderer.cs` + `Shaders/Debris_URP.shader`

A record is an arc: `P0, Born, V0, LandT, LandY, Axis, Spin, Rot0, Tint (a = burn), Scale, Life, Mode`.

- `LandT` and `LandY` are solved at the throw against the **drawn** ground (`RenderGround.Sample`) at the place the
  piece comes down: two Newton steps (`DebrisMath.Landing`). The shader then plays the arc, one bounce (0.45 of the
  sideways speed, 0.30 of the fall), settles the piece flat as it stops, and past `Life` sinks it into the mud over 3 s
  and scales it to nothing.
- `Mode 1` is a hinge: a tree top or a slab going over about its foot, accelerating, with a small bounce back.
- Ten kinds of piece, one procedural mesh each (30–90 vertices, smoothed outline normal in UV3 like the kit):
  clod, shard, plank, rubble, sandbag, plate, crown, limb, helmet, rifle. One pool per kind, a ring each:

  | piece | pool | shadows |
  |---|---|---|
  | Clod | 1024 | no |
  | Shard | 512 | no |
  | Plank | 384 | yes |
  | Rubble | 512 | yes |
  | Sandbag | 256 | yes |
  | Plate | 256 | yes |
  | Crown | 64 | yes |
  | Limb | 256 | no |
  | Helmet | 128 | no |
  | Rifle | 128 | no |

  3,520 records, 330 KB, allocated once. Past a pool's cap the oldest piece is overwritten, so a barrage never grows
  anything. One `RenderMeshIndirect` per kind that has anything alive: at most 10 draws (each with its outline pass).
- Spend follows the look point: `DebrisMath.Share(CameraShake.DistanceToLook)` throws all of a burst's pieces under the
  eye, half beyond 55 m, a quarter beyond 120 m.
- Every throw is seeded from its place and the event tick (`DebrisRng`), so a replay and a capture agree.
- The shader reads its record with `GetIndirectInstanceID_Base`, not `GetIndirectInstanceID`. On D3D `SV_InstanceID`
  starts at 0 for every command, so the pool's `startInstance` has to be added; the `_Base` helper does that (and does
  nothing extra on Vulkan, where the id already includes it). With the plain helper every pool read the Clod pool's
  first records, which is why on the first evening only clods ever appeared: everything else drew at the positions of
  the oldest clods, long sunk.
- The mesh bounds are padded 0.5 m for the outline hull; the rest height (`Lift`) takes that padding off again, or a
  plank would come to rest 0.37 m in the air.
- The look is TW/Toon's: two-step light, shade tint, shadows, lamps, mist, fog, ink outline; `Tint.a` makes a piece glow
  with embers that cool over the first half of its life (armour off a cooked-off tank).

## What throws what

| event | where | pieces |
|---|---|---|
| `Explosion` (dry) | `CombatFx` | 8 + 2.2 r clods (fist to head, lie 30 s) + 4 + r small fast ones; the old chunk cubes cut from 17 to 10 |
| `Death` by a shell that throws him high | `CombatFx.Gibs` | 30 % of them: 1–2 limbs, 22 % the head, the helmet, 60 % the rifle, five dark lumps (`DebrisRenderer.Gore` 0 turns the lumps and limbs off) |
| `VehicleCrushed` (a man, b = 2) | `CombatFx` | helmet, a limb, lumps, low and slow |
| `PropChanged` Tree → BrokenTree | `CombatFx.TreeBreaks` | the crown hinges off the 2.7 m snag away from the newest burst (1.3 s), splinters |
| `PropChanged` → Stump | `CombatFx.TreeBreaks` | the snag shatters: 14 charred shards, clods |
| `PropChanged` → Log (crushed) | `CombatFx.TreeBreaks` | shards |
| `VehicleArmourHit` holed | `TankRenderer` | 3 burning plates, on top of the existing sparks and horn shear |
| `VehicleLegLost` | `TankRenderer` | 4 plates (the leg itself is still `ThrowLeg`, the parented-part path) |
| `VehicleCookOff` | `TankRenderer` | 14 burning plates, 60 s, on top of `Wreckify`'s turret/cupola/horns |
| `Explosion` inside a prop's radius | `PropDestruction` (pending) | see below |

The limbs a man loses are also cut from his figure: `VATRenderer.AddFallen(..., gib)` writes a bit per limb into the
instance record's spare float (`VatInstance.Pad`), and `VAT_URP` clips any vertex whose limb id (mesh UV1.x) is in the
mask, in all four passes, so a triangle across the root is cut at its middle. **The bake does not write UV1 yet**
(VATBaker is in another session's hands today): until it does the mask is inert and the man is thrown whole while
his limbs fly. The baker's change is one line per vertex, `LimbOf(boneName)`: head 1, left arm 2, right arm 3, left leg
4, right leg 5; helmet vertices 1; rifle 0.

`TankRenderer`'s own parented-part debris (turret, cupola, horns, tracks, legs) is unchanged and now capped at
`MaxLoose = 160` pieces, oldest at rest first; a track slid off is never dropped (it goes back when mended).

## Buildings and the rest of the kit: `Presentation/Terrain/PropDestruction.cs`

Every drawn prop that is not the sim's has a material class and a strength. Four things wear it down:

| what | how |
|---|---|
| a burst (`Explosion`) | BlastSystem's falloff (`1 - 0.75 d/R`, R = 1.15 x the blast radius); power = radius / 6 m, from a grenade (0.15) to the HE barrage (1.3) |
| a tank's AP round (`VehicleFired`, scalar 0) | a 1.3 m strike of 1.1 where the round comes down |
| a tank's tracks | every sim tick, from the tick state: a vehicle moving faster than 0.25 m/s flattens every crushable prop within 2.4 m of its centre |
| (small arms) | nothing yet: a shot's event carries no impact point |

A hit that does not finish a prop throws chips (splinters, chips of stone, twigs) off the side facing the strike, as
many as the harm. At nothing the prop is hidden and its volume becomes rubble, planks, sacks or plates. A crushed prop's
pieces are pushed out low along the track, with a little dust.

| class | strength | breaks into | a tank flattens it |
|---|---|---|---|
| **shelters**: dugout, concrete shelter, sod shelter, pillbox, MG nest, armoured stand, the earth roof | never collapse | sheds sandbags, 16 a shelter, up to 5 a hit, then only earth; concrete ones lose chips too | no |
| stone that is not a shelter: ruin, well, wall stub, slab, barricade | 2.0 | rubble | no |
| light timber: planks, duckboards, ladders, crates, boards, doors, signs, markers, stakes, timber hedgehogs, posts, knife rests | 0.8 | planks | yes |
| revetment 1.2, trench floor 1.5 | | planks | no |
| a lone sack | 0.9 | sacks | yes |
| parapets, sandbag walls, gabions | 0.9-1.0 | sacks | no |
| sheet iron, wire fence | 1.0 | plates | yes |
| guns, limbers, shell stacks, a turret, the aeroplane | 1.3 | plates | no |
| scrub, grass, reeds, poppies, branches | 0.3 | twigs | yes |

Shelters are the owner's rule (2026-09-23): "the shelter should have the sandbags blown off, but otherwise stay fine,
shelters are reducing artillery damage". The bags the imported shelter meshes carry are part of those meshes and stay
drawn; what flies are thrown sacks off the top, and the loose bags, gabions and crates stood round a shelter are
ordinary props and do go.

Collapsed props are remembered by **module + position quantised to 0.25 m** (never by index), and
`BattlefieldProps.Suppress` is asked for every instance it composes, so a prop stays down across the recompose every
crater triggers, and nothing outside a strike can ever be hidden. The same predicate holds the static fallen tree-top
back for 4.8 s, while the thrown crown falls and lies.

The hooks on `BattlefieldProps`: `Within(module, centre, radius, list)`, `Hide(module, page, slot)`, and
`Func<Module, Matrix4x4, bool> Suppress`, asked in the composer's emit callback. `GreyboxTerrainView` adds the component.

Seen in Play (2026-09-23 10:00): a barrage on a concrete shelter; it stands with its earth roof, 22 bags came off,
221 chip hits elsewhere, 281 props down. A Maw let loose flattened 19 props along its path. Console clean. On the way:
`BattlefieldComposer` used to re-check every site after a crater and drop one whose ground no longer fitted, so a
shelled shelter vanished whole, bags, crates and all. A site now keeps its height instead.

**The sim does not know about shelters.** `NavLayer.Bunker` exists and nothing sets it; BlastSystem protects men in a
trench, a shell hole or lying down, and nobody for being under a roof. So "shelters reduce artillery damage" is not
true in the game today. It needs the sim to know where shelters are (the composer that places them is presentation),
then a protection factor in BlastSystem, and it changes the replay hash.

## What a shell does to the men and machines that live through it

Owner, 2026-09-23: "make the explosions more impactful ... for the units". Presentation only, like everything above:
the sim's answer to a blast is unchanged (damage, suppression, `SimWorld.Knock`), so nothing here moves the replay hash.

- **Blown off his feet** (`AnimationController`, rung 3). A man upright in the open whom BlastSystem throws faster than
  `BlownDownKnock` (4 m/s: about the inner 60 % of the burst) is spun to face the way it throws him, lifted
  (`Hop`, up to 1.1 m over `HopSeconds` 0.5 s, drawn by `VATRenderer`'s job as a Y offset), and goes down on his face
  away from it (`Trip`, "Fall Over", which ends face down). He lies there 1 to 2.5 s (`KnockedUntil`), then `GetUp`
  (at 1.8x if the sim is already moving him), or stays flat if the sim has flattened him for the fire. A man already
  diving from a shell he heard keeps his dive, lifted the same way, and stays down as long; a fall started over the
  top of a dive stood him back up first. A man thrown more gently dives away with it, lifted a little (up to 0.35 m).
  The daze is counted from when he is back up: counted from the fall, getting up (2.3 s) used it all.
- **Dazed.** After a knockdown, and for half of the men who shield their face from a burst on top of them, a man
  standing still spends 1.5 to 5 s on one knee (`DazedUntil`), rubbing his eyes once (`FidgetRubEyes`). A shot still
  cuts through: the sim decides when he fires.
- **The wave.** Inside 0.85 of the radius (where the sim throws men) and for everyone it killed, the burst is the
  tick's. Past that, each man's reaction waits `(d - 0.85 r) / WaveSpeed` (60 m/s: a readable ripple, not the 340 m/s
  pressure front that is one tick for everybody), plus a tick for half of them, and it reaches `WaveReach` (12 m) past
  the radius, where the sim's reach stops. Past r + 3 m it is a flinch at most, and a man on a target mostly keeps it.
- **Grime.** Every man within r + 6 m gains `0.5 (1 - d / (r + 6))^2` of mud and soot (to 1), which wears off over five
  minutes (`GrimeFade`). It reaches `VAT_URP` in the record's spare float (`VatPad`: limbs in bits 0-5, grime 6-13, a
  per-man seed 14-21), where it splashes mud up from the boots in blotches placed in his own space (they stay put as
  he moves, and differ man to man), higher the dirtier he is, and greys the rest. The fallen keep what they wore.
  The splashes are a smooth value noise, stretched: a hash per 5 cm cell drew them as squares (pixel camouflage).
- **Hulls** (`TankRenderer.Blasted`). A burst within r + 4 m + half the hull kicks the hull's pitch and roll springs
  away from it (the near side up, about 3 degrees) and heaves it up (about 12 cm), harder the nearer and the bigger
  the shell; the dirt it threw comes down on the deck a third of a second later. Walkers ride the same springs. The main gun and a strike on armour
  now light the ground through `SceneHooks.Flash` (NightLights' pooled lights, which never let a gun take a light
  from a brighter shell), and a strike is felt through the camera.
- **The camera** (`CameraShake`). Every kick waits for the sound (`Arrival`, 343 m/s: a shell 100 m from the lens is felt
  0.3 s after its flash) and shoves the picture along the ground away from the burst, a critically damped spring of
  about a fifth of a second, on top of the old thump and rumble.
- **The hull's springs are now solved exactly** (`TankRenderer.Spring.Step`: `x(t) = (x0 + (v0 + w x0) t) e^(-w t)`).
  The explicit step goes unstable once `w dt` nears 2, which one long frame reaches (0.33 s at w 10); a blast's kick
  during a stall then grew every frame and a Maw was drawn 3 km up (seen in Play). The exact step only ever decays.

Seen in Play (2026-09-23, 20 50 on the greybox, one unannounced 8 m shell queued into BlastSystem in both worlds, six
men standing still at 1.5 to 16 m, all alive at 8 damage): the three nearest blown off their feet (lifted 0.27 to
0.35 m), down 1 to 1.5 s, up, one kneeling to rub his eyes before he stood; the 6 m man thrown into a dive (0.16 m);
the 9 m and 16 m men ducked a poll later; grime 0.28 to 0.34 near, 0.07 at 9 m, none at 16 m; desync False.

Tests: `Tests/EditMode/BlastReactionTests.cs` (the knockdown through to the daze, the wave's timing and reach, grime by
distance, the Pad packing, the kick's delay, a hull's spring through twenty long frames).

## Cost

- CPU: a throw is two ground samples and a 96-byte write; a shell burst is about 30 throws. Nothing per frame per
  piece; per frame one `SetData` per pool that had a throw and ≤ 10 draw submissions.
- GPU: ≤ 3,520 instances × ≤ 90 vertices ≈ 300 k vertices worst case, all pools full; typically a few thousand.
  Shadows only for the big kinds.
- Memory: 330 KB of records + ten small meshes.
- Draw calls: +10 at most (each with its outline pass), against a field already at 374 vs the < 300 ceiling
  (`docs/05`). The debris does not make that worse in proportion; the fix for the 80-material kit is separate.
- Measure p95/p99, not the mean: an explosion frame is a tick frame, and the tick already carries a ~400 KB
  allocation claude-68 is chasing.

## Tests

`Tests/EditMode/DebrisTests.cs`: the landing solve on flat and stepped ground, the arc never below its rest height,
one bounce then rest, continuity at the landing, the look-point share, the seeded generator, and that the record is
the 96 bytes the shader declares with a pool for every piece under half a megabyte.

## Seen in Play

- 2026-09-23 first Play: shell bursts throw dark clods that arc, bounce once and lie on the field (700–1,300 alive
  after a barrage, 6–8 draws); gore lumps at blast deaths; console clean. The other nine pools were invisible: the
  indexing fault above. Fixed the same night together with the rest-height padding, a brighter `Mud` clod tint
  (the old one read black at night) and a softer `_EmberColor` (the old one read as a lantern). The fixed pools have
  not yet been seen in Play (the editor was handed on for two gates); first thing next session is the row test
  (`poolcheck.sh` in claude-b7's scratchpad: one big burning piece of every kind, filmed).
- 2026-09-23 03:00, second Play: the row test shows all ten kinds, arcing, tumbling, glowing and coming to rest;
  console clean. It also found a hinge fault: a tree top toppled about its break 2.7 m up and stayed there, lying
  level in mid-air. `Mode 1` now drops the pivot from `P0.y` to `LandY` (the ground at the pivot, plus a quarter of
  the scale) as it goes over, so a snapped top swings and falls onto the ground; a slab hinged at its foot is
  unchanged. The crown lies about 3.5 s after its fall before it sinks (it was sinking before it had landed).
- Biome seam for claude-0a's lava and snow fields: `DebrisRenderer.Biome` (a static `Color`, default white with
  a = 0) is pushed as the global `_DebrisBiome` every frame. rgb multiplies every piece's tint; a is a floor under the
  ember glow, so on lava every piece smoulders. A biome sets it in one line.
- `CombatFx` pruned nine per-frame lists with `RemoveAll` and a lambda closing over `now`: a closure and a delegate a
  call, every frame. Replaced by `Prune(list, at, static (x, at) => ...)`, an in-place compaction with a cached
  delegate; zero allocation.

## Open

Done 2026-09-23, gated and seen in Play:
- Limbs: `VATBaker` writes the limb id to UV1.x (0 body, 1 head and helmet, 2/3 arms, 4/5 legs; both figures rebaked,
  only the two mesh assets changed, atlases untouched at 70.2 MB). A dead man with both legs gone was filmed beside a
  whole one: the cut is clean.
- Stumps: the fallen are drawn with their own material, `Cull Off`, and a back face returns a dark wet red, unlit, so
  a cut shows a wound, not the field through a hollow shell. No new draw. Trap found on the way: `new Material(m)` copies
  only the properties the shader declares; `_PosMin`/`_PosSize` are not declared and have to be set on the copy, or
  every vertex decodes to the origin.
- Props: the hooks above and `PropDestruction` in.
- Craters: `GreyboxTerrainView` rebuilds at most `MaxChunkRebuilds` (2) dirty chunks a frame, round the field from
  where it stopped.
- Lava: `DebrisRenderer.LavaLevel` (static, -10000 = off) is pushed as `_DebrisLava`; a piece whose rest height is at
  or below it goes under within 1.2 s of landing and flares as it goes. claude-0a sets it from `BiomeProfile.MoltenLevel`
  in `Atmosphere.PushBiome`, the one authority (the terrain's `_TWHeat.w`).
- Gore: `GameSettings.Camera.Gore` (0..1) is applied to `DebrisRenderer.Gore` by `SettingsApplier.ApplyCamera`, with a
  GORE slider under Controls.

Still open:
1. Owner decision: whether a collapsed shelter stops giving cover in the sim (a hash bump; `docs/PLAN.md` already says
   the bomber run "destroys bunkers (2 hits)").
2. Lava, seen in Play 2026-09-23 after claude-0a's 3cff58e (debris takes `TWHemisphere` and `TWGroundBounce`): burning
   plates and rubble read against the bright molten ground by their ink outline and ember seams, and pieces that land
   in the melt go under within about a second, as designed. Nothing to change in the debris. Seen in the same barrage,
   not debris: `CombatFx` still throws pale blue water splashes where a shell lands in the lava pool.
3. Close-up limb stumps are small at the gameplay zoom; if a still asks for more, a dark cap mesh at the joint.
