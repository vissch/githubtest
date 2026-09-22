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

## Buildings and the rest of the kit: `Presentation/Terrain/PropDestruction.cs` (waiting on a BattlefieldProps hook)

Every drawn prop that is not the sim's has a material class and a strength, and a burst inside its radius wears it
down with BlastSystem's falloff (`1 - 0.75 d/R`, R = 1.15 × the blast radius, one field-gun burst = 1.0 at the
centre): concrete and stone (bunker, ruin, pillbox, sod shelter, MG nest, stand, well, wall stub, slab, barricade) at
2.0 take two hits; timber (dugout, roof, revetment, duckboards, ladder, crates, boards, doors, signs, stakes, posts,
knife rests) at 0.8–1.5; bags (parapets, sacks, gabions) at 0.9–1.0; iron (field gun, limber, shell stack, sheets,
the aeroplane) at 1.3; scrub at 0.3. At nothing the instance is hidden and its volume becomes rubble, planks, sacks or
plates thrown away from the burst, with two dust puffs and, for stone, a camera thump.

Collapsed props are remembered by **module + position quantised to 0.25 m** (finer than the 2 m picket spacing;
never by index), and `BattlefieldProps.Suppress` is asked for every instance it composes, so a prop stays down across
the recompose every crater triggers, and nothing outside a blast radius can ever be hidden. The same predicate holds
the static fallen tree-top back for 1.4 s while the crown falls.

Needs from `BattlefieldProps` (claude-68 is adding them): `Within(module, centre, radius, list)`, `Hide(module, page,
slot)`, `Func<Module, Matrix4x4, bool> Suppress` consulted in `Put` and the anonymous emit path.

**Gameplay is untouched on purpose:** a bunker cell stays `NavLayer.Bunker` after its roof has gone; the sim's cover
comes from its own props. Whether a collapsed shelter should stop giving cover (and what a trench collapse does) is an
owner decision and a v3 hash bump; `docs/PLAN.md` already specifies "destroys bunkers (2 hits)" for the bomber run.

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

## Open

1. VATBaker: write limb ids to UV1.x (one line per vertex) and rebake (`TW/VAT/Bake Infantry`, ~1 min).
2. BattlefieldProps hook → drop `PropDestruction.cs` in (it is written, in claude-b7's scratchpad).
3. Craters: `GreyboxTerrainView` rebuilds every dirty 32 m chunk the same frame with no budget; a 12-shell barrage
   can dirty most of the field at once. Cap it at a chunk or two a frame (file is in claude-68's hands today).
4. The see-through at a limb cut (the figure is single-sided): a dark cap needs a second small mesh or `Cull Off` on
   the fallen buffer only.
5. Owner decisions: gore level (`DebrisRenderer.Gore` is a static, 0..1, meant for a settings toggle); whether
   collapsed shelters lose their cover in the sim.
