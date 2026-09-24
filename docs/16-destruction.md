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

## The village houses: `Presentation/Terrain/HouseKit.cs`

Owner, 2026-09-23: the building blocks were "not broken up small enough"; try the Tripo sheet of six ruined village
houses, cut into small pieces in Blender, and put them in the centre of the map near the water on each side.

- **Cut in Blender** (`Tools/housesplit.py`, headless Blender 5.0). The sheet already comes as Tripo parts. The tool
  groups them into six houses by overlapping boxes, squares each house to the axes and scales it to metres: the
  two-storey ones stand about 7 m. Parts under 1.4 m join the part they touch. Every part longer than 2.4 m is cut
  along its longest axis until it fits, and each cut is capped with a face that takes its UVs from the skin it closes.
  Slivers under 24 triangles join their nearest chunk.
- **What it makes.** Six houses of 9 to 21 chunks each: 83 chunks, 28 to 217 triangles, about 6,000 triangles for all six.
  Each chunk is its own FBX in `Resources/Env/Houses`, pivot at the middle of its base. PropDestruction measures a
  hit to the pivot, so each chunk is hit on its own. `houses.json` gives each chunk's offset in its house, its bounds,
  and stone or timber, read from the painted colour. The sheet is graded like the other sets and is the atlas's seventh
  cell (`Tools/envatlas.py`).
- **One matrix a house.** A chunk is drawn at `house * Translate(offset)`. The chunks are unnamed modules, so no
  PropLayout look or hand edit jitters one out of its house.
- **What rests on what.** `HouseKit.Solve` works it out from the bounds. A chunk whose foot is within 0.35 m of the
  house's floor stands on the ground. Any other chunk rests on the lower chunks it overlaps and reaches within 0.25 m of.
  A chunk with none in reach rests on the highest chunk under it. A chunk over nothing at all counts as grounded.
- **Destruction.** Stone chunks have strength 1.4, timber chunks (beams, boards, tiles) 1.0; tanks do not flatten
  either. A chunk that breaks does not turn into rubble on the spot as other props do (owner, 2026-09-23, "destruction
  feel"): it comes away whole and flies as itself, its own mesh and material drawn by `PropDestruction.Fly` (one
  `RenderMesh` a loose chunk, at most `MaxLoose` 48; past that it goes to rubble on the spot). A hit throws it away
  from the burst and up, tumbling over the axis across the throw, with a few chips off it. It lands on the drawn
  ground by the lowest corner of its box, bounces once, slides to rest, lies `LooseLie` 18 s and sinks over 2.5 s. At
  its first touch it breaks up there: most of its rubble or planks, and the dust of it coming down.
- **Storey by storey.** When a chunk goes, `Shaken` looks at everything it carried `FallDelay` 0.45 s later, a quarter
  more for each storey up, with a jitter of a quarter of that, so a storey's beams let go one after another. A chunk
  still standing with nothing under it (`Drop`) tips outward from the middle of its house and falls whole, the dust of
  the mortar letting go where it was; what it carried is looked at in turn.
- **Placed** by `BattlefieldComposer.PlaceHamlets` once per map, like the sites. The houses are shared out three to a
  bank in a seeded order. They stand beside the road over the bridge in the middle of the map, on its far side from
  the camera: every view of the field looks from +X, so a house on the near side hid the crossing. Each keeps 4.5 m
  plus half its size off the bridge's line, and 1.5 m plus half its size back from the first dry metre of the bank.
  Its front, the sheet's front (local +Z), faces the camera, turned up to 30 degrees either way; turned from it, a
  house showed only a blank back or side. The ground test is Landmarks' `Room`, with the sim's props kept clear by a
  metre. A house stays at the height it was built at when a crater opens under it.
- **Grade.** The sheet is graded warmer and brighter than the other sets (saturation kept 0.66, value 1.14), to the
  stone set's mean value: at 0.50 and 0.88 the plaster and tiles went to one dark grey at night.

- **The rear's military buildings** (owner, 2026-09-23: "military buildings for in the back of the allied troops").
  A second Tripo sheet (`Downloads/stylized+watchtower+3d+model.zip`) came as one welded object; `housesplit.py` with
  `TW_LOOSE=1` splits it into its loose parts first, then groups and cuts it like the village: a watchtower (18 chunks),
  a guard post (14), a command post (17) and a concrete blockhouse (16), 65 chunks at the village's scale, in
  `Resources/Env/Military` with the atlas's eighth and last cell. `HouseKit.Load(set, ...)` loads a set; the kit's
  `Houses` holds both sets in one array (a chunk's House indexes it) and each set has its own material pair. Placed by
  `PlaceRear`: behind team 0's rear fire trench, from the map's back edge to four metres short of the trench (the rear
  is some sixteen metres deep), on the far side of the supply road from the camera, fronts to the camera, three metres
  clear of each other; trench sites keep six metres clear of any building. Concrete and heavy timber: stone chunks 2.2,
  timber 1.4. Seen in Play (19:20): all four stand as a compound behind the allied line; a hit at the watchtower's legs
  took twelve of its eighteen chunks and the platform tipped and fell after them.

**The sim knows nothing of the houses.** They give no cover, block no one and stop no shot, like the rest of the kit.
Making them count needs the house footprints in `MapData` (cover and `Blocked` cells, removed as chunks go), which
changes the replay hash. That is the same owner decision as the shelters below.

Cost (optimised 2026-09-23, owner: "is it viable on all buildings"). Three changes:
- **One mesh a house, chunks masked.** `HouseKit.BuildWhole` combines a house's chunks into one mesh at load, each
  vertex tagged with its chunk in UV1.x (the chunk FBXs import readable for it, `EnvKitImport` v3). The house is one
  instance of that mesh; `BattlefieldProps.MaskOf` (PropDestruction: the chunks remembered as destroyed) gives each
  visible instance a bitmask, passed in a `MaterialPropertyBlock` array, and TW/Toon's `_CHUNKMASK` variant collapses a
  masked chunk's vertices to the origin in all four passes. The chunk modules stay as bookkeeping (`Module.Drawn`
  false): placed, hit, hidden and remembered exactly as before, never drawn. A house chunk is always remembered, past
  `MaxRemembered` too, or its house would draw it again. `Resources/Env/Houses/HouseMask.mat` keeps the variant in a build.
- **One material** for every chunk and one, with the keyword, for every house.
- **Loose chunks batched**: a flying or lying chunk is its house's whole mesh with every other chunk masked off, so all
  the loose chunks of one house type are one instanced draw.

Measured at the standard view on the village, 120 frames a state, shelling on:

| houses drawn | batches | SetPass | render CPU |
|---|---|---|---|
| none | 314 | 223 | 3.85 ms |
| a draw per chunk (shared material already) | 491 | 345 | 5.10 ms |
| one mesh a house | 342 | 250 | 4.34 ms |

The village went from +177 batches to +28. The cost now grows with house types, not with houses or with damage: a
village of thirty houses from these six types draws the same. A house type holds at most 24 chunks (the mask is a
float's whole-number range).

Seen in Play (2026-09-23 18:00, seed 1917): all six houses placed, three a bank round the bridge, each whole and
the right way up. The ambient bombardment had already brought two of them down before the test. A barrage-sized
strike on the bell-tower house took every chunk, with rubble, planks and dust. A 0.8 m hit at one corner of another
took that corner and the chunk it carried, and five of its nine chunks stood. With the loose chunks (18:40): a 4 m
shell at the foot of the chimney house threw four chunks whole, tumbling, out of the dust; an upper piece left with
nothing under it tipped and fell half a second later; the house stood with its front out and the chunks lay in the
mud beside it among their rubble.

## The rest of the kit (2026-09-23)

Owner: "lets see what other items we can make destructable in the scene", then the recommended set. Every drawn prop
that is not the sim's now has a rule. Added:

| Props | Rule |
|---|---|
| the three stumps, the shell-torn trunk | Plank, Hp 1.3 |
| fallen log | Plank, Hp 1.3, flattened by a tank |
| pile of stones | Rubble, Hp 1.2, flattened |
| boulder | Rubble, Hp 3.2 (a heavy shell close by) |
| spent shell cases | Plate, Hp 0.4, flattened |
| dud shell | Plate, Hp 0.5, flattened, **cooks off** |
| helmet, mess kit, spade, ammo tin, boots, rifle, bucket, hung tins, tins on the wire, rag | Hp 0.2, flattened, **thrown whole** |

**Cook-off.** A dud that is hit or run over goes up 0.35 to 0.7 s later: `SceneHooks.CookOff` (CombatFx draws a shell
burst's flash, fire and smoke at a fraction of the size, sparks, clods, a light and a camera kick) and a `Strike` of
2.6 m and 0.8 round it, so the next dud in reach goes in turn. Drawn only: the sim never hears of it, and no man is hurt.

**Thrown whole.** What a man drops is not broken up: a burst throws it as itself, on the loose-chunk path (it flies,
bounces once, lies 18 s, sinks), each kind in one instanced draw of its own mesh. These things are drawn only while the
camera is close (`Module.MaxDistance`), so they are thrown only then; at the standard view one is simply gone. Loose
places are kept back for house chunks (16 of the 48).

**Sliced kit props.** The well, the wall stub, the biplane and the field gun come apart like the houses. They are cut from
their own FBX (`Tools/housesplit.py` with `TW_LOOSE=1 TW_ONE=1 TW_KEEP=1 TW_SCALE=1`: one building however many
parts, the prop's own pivot and facing kept) into `Resources/Env/<set>/Chunks/`, with the set's `houses.json` beside:

| Prop | Set | Cut, m | Chunks | Tris (whole was) | Rule |
|---|---|---|---|---|---|
| Well | Siege | 0.9 | 19 | 863 (513) | stone Hp 1.2 / timber Hp 0.8, from the paint |
| WallStub | Stones | 1.0 | 10 | 606 (218) | all stone, Hp 1.2 (its brick reads warm to the classifier) |
| Biplane | Weapons | 1.9 | 16 | 1246 (566) | Plank, Hp 0.7 |
| FieldGun | Weapons | 0.8 | 11 | 625 (381) | Plate, Hp 1.1 |

The cut faces are what the triangles grow by. They keep their set's atlas cell, so the full atlas does not matter.
`BattlefieldKit.Slice` links each module to its building (`Module.Sliced`, and `Drawn = false`). The module is still
placed, named and hand-edited as itself, and `BattlefieldProps.Put` puts the building's whole mesh and its chunks at
the prop's matrix, after the look's styling, so a scaled or turned prop comes apart at its own size. Holding or moving
one in the editor recomposes, so its chunks follow. The sliced module has no rule of its own; its chunks do.
`HouseKitTests.A_Sliced_Kit_Prop_Puts_Every_Vertex_Of_The_Whole_Prop_Back_Where_It_Was` holds the slicing to the
original (a turned or mirrored export fails it).

Seen in Play (2026-09-23 19:55, night, rain). On this map: 22 wall stubs, the biplane and 6 field guns are placed (the
stubs and the biplane on the land beyond the field), plus 7 duds and 85 helmets; no well. The field gun at (79.6, 4.2)
stands at about 4 to 6.5 times its size. A 4 m hit at its side took one of its 11 chunks and threw 9 dropped things. An
8 m, 1.6 strike on its middle threw the barrel, the shield and the wheels whole, tumbling out of the dust, with the tins
and helmets round it (`Captures/gun_flying.png`). A direct hit on a dud queued its cook-off, and it went up with a burst
of smoke (`CookedOff` counted it). A 1.5 m hit at 0.7 only chips a dud (0.39 of harm), as its Hp says. No errors.

## What a shell does to the men and machines that live through it

Owner, 2026-09-23: "make the explosions more impactful ... for the units". Presentation only, like everything above:
the sim's answer to a blast is unchanged (damage, suppression, `SimWorld.Knock`), so nothing here moves the replay hash.

- **Blown off his feet** (`AnimationController`, rung 3). A man upright in the open whom BlastSystem throws faster than
  `BlownDownKnock` (4 m/s: about the inner 60 % of the burst) is spun to face the way it throws him, lifted
  (`Hop`, 0.23 to 0.73 m life-size over `HopSeconds` 0.5 s, drawn by `VATRenderer`'s job as a Y offset times his drawn scale), and goes down on his face
  away from it (`Trip`, "Fall Over", which ends face down). He lies there 1 to 2.5 s (`KnockedUntil`), then `GetUp`
  (at 1.8x if the sim is already moving him), or stays flat if the sim has flattened him for the fire. A man already
  diving from a shell he heard keeps his dive, lifted the same way, and stays down as long; a fall started over the
  top of a dive stood him back up first. A man thrown more gently dives away with it, lifted a little (up to 0.23 m life-size).
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

### The burst seen from close by (second round, batch A)

Owner, 2026-09-23: the next phase in three batches, reported one at a time; this is the first (bugs and the close-up
view). Presentation only.

- **A cook-off's fireball lasts.** `TankRenderer` added six flames to `flames` at the event, but that list is rebuilt
  from the burning hulls every frame, so the fireball drew for one frame. A `Fireball` has its own clock and is added to
  the flames every frame it lives: it bursts out of a point over the first 15 % of its life, rises, and shrinks away
  after 45 %. One big tongue (1.5 s), six smaller ones, a flash card and five black smoke cards after it. Sized to the
  hull: the numbers are drawn for a 2.5 m half-length (`DrawnForHalfLength`) and multiplied by `Model.HalfLength / 2.5`,
  so a Maw (4.34 m since `VehicleSize.Tank`) burns 1.74 times as big. It rises from 0.7 of the hull's height.
- **Dirt is lumps.** The dirt CombatFx throws was Unity's cube, which read as cubes from close by. It is a lump from
  `DebrisRenderer.Lump` (now public); water drops are spheres.
- **Smoke from close by.** `Flipbook_URP` fades a card where it meets the ground or a wall (the scene's depth behind it,
  over 0.8 m) and as it comes through the lens (its eye depth under a quarter of its width): a card wider than the picture
  was a wall of smoke for seconds. Among the men (`SceneHooks.CloseUp`) a burst draws 5 smoke cards, not 7, up to 30 %
  smaller and shorter-lived. The burst's low CPU smoke balls go a pass fainter every quarter of CloseUp and are gone
  among the men: from close by they were glass spheres with a hard rim.
- **The flash.** The Flash card goes from 3.2 r to 1.6 r wide and its glow halves as the lens goes in; NightLights' light
  card comes down to 55 %, and the hole's ember glow to half its size and 70 % of its strength (it was a 4 m pink ball
  standing in the crater for up to 12 s).
- **Night tint.** Smoke and the burst cloud take 45 % of the mood's shade tint (`Sheet.Mood`, `_ShadeMood`), not all of
  it: at night the burst cloud was saturated blue. Smoke is a warm grey (0.50, 0.47, 0.43).
- **Chunk smoke drifts with the wind** (`_TWWind`), not towards a fixed -Z.
- **The knockdown hop is life-size metres** times the man's drawn scale (UnitScale times the zoom growth), so it stays in
  proportion to him after claude-14's UnitScale 1.5 to 1.125, and still reads when the men are drawn large far out.
- **The capture rig drew every close still as the standard view.** `CaptureRig` switches TacticalCamera off for a set,
  and the camera's OnDisable puts `SceneHooks.CloseUp` and `_TWClose` to 0. `Pose` now sets both for the shot's zoom.
  Any close still taken with the rig before 2026-09-23 20:40 had the close-up band (grit, footprints, litter, the
  effects' close sizes) switched off.

Seen in Play (2026-09-23, greybox, night and rain; an unannounced 8 m shell at 20 50 filmed at zoom 5 and zoom 30, and a
Maw held, set alight and filmed through its cook-off): the flash is a white core with the clods dark against it, not a
white-out; the smoke thins to drawn curls near the lens; the ember sits down in the hole; the burst cloud is a soft grey;
the fireball stands white-hot out of the hull's top, taller than the machine, for about a second. Desync False, console
clean. Shots: `shots/batchA` in claude-6f's scratchpad.

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

## The ground remembers, except where a trench stands in it (2026-09-24)

Owner, in two messages the same day. First: the fun in this game is seeing large damage on the troops and the
environment, so make everything as destructible as possible, at every zoom, with directional explosions, an
environment that is blown away and on some level *permanently* changed, and "the more explosions on 1 place on the
battlefield the bigger the hole should become". Then, a correction that shapes the whole feature: "since this is a
trench game the ground near the trench wont be able to take alot of degradation. the trench allways need to stand
and allways give limited protection."

Those two pull in opposite directions, and the resolution is a guard band rather than a compromise on either: the
open field is *more* malleable than it was, and the five metres around a trench are untouchable.

### The hole that grows (`Sim/Terrain/CraterStamp.cs`, `MapData.Holes`)

A crater used to be a subtractive cosine bowl and nothing else, so twenty shells on one spot dug twenty bowls
inside each other: deeper and deeper, never wider. Now every runtime stamp goes through `ApplyDynamic`, which looks
for the hole it landed in (`MergeShare` 0.75 of the larger radius) and **widens** it:
`R' = min(MaxRadius 12 m, sqrt(R^2 + GrowShare 0.6 * r^2))`, carrying `DepthShare` 0.6 of its own depth into the
middle. A 3 m shell repeated on one spot reaches 12 m across in 26 rounds and stops there. `MapData.Holes` is the
record (cap 512, oldest forgotten; its ground stays dug), and the merge keeps the *first* centre, because the
heightfield already holds the old bowl.

**Rims.** Spoil is thrown up in a ring `RimWidth` 0.3 of R wide outside the lip. The bound is the interesting part:
a rim added afresh per shell would build a mountain over a three-minute barrage, so each hole carries `RimUp` and a
stamp adds only the difference between what it has already thrown up and what its depth now deserves
(`RimShare` 0.25 of depth, capped at `MaxRim` 0.5 m). Eighty shells on one spot raise half a metre of spoil in
total, not forty.

**Depth is bounded** by `MapData.Bedrock` (the map's lowest generated ground less 1.5 m) and by
`WaterLevel - MaxUnderWater`, so a shell can never make ground impassable — the rule `BattlefieldTests` has
guarded since A4.

### The trench guard band

`MapData.CellTrenchDist` is a byte per 1 m height cell: decimetres to the nearest trench or ladder cell, built once
by `BuildTrenchDistance` (trenches never move). `CraterStamp.Hard()` turns it into a scale on every carve and every
rim: **zero** within `TrenchKeep` 1 m, ramping to full over the next `TrenchGuard` 4 m. So the parapet and the metre
behind it never move, the next four metres dig progressively less, and only past five metres is the field soft. A
hole may still grow its *radius* into the band — its bowl is simply flat there — so a shell hole can never join a
trench or undercut one. Nav cells are marked `Crater` only where the ground actually moved, so cover never claims a
hole the band refused to dig.

**There is no trench cave-in, by decision.** A direct hit on a bay throws sandbags, revetment planks and a scorch
mark, which is `PropDestruction`'s business and already worked; what makes it *hurt* is `BlastRules.TrenchBayFactor`
below. `NavLayer`, `CellTrenchId`, `TrenchCells` and the garrison's posts are untouched by a shell.

`DynamicGroundTests` covers it. The test worth keeping is
`TheGroundDigsMoreTheFurtherItIsFromATrench`: it fires a 16 m shell straight into a trench and shows the ground
dropping 0.00, 0.34, 0.96 and 1.81 m at 1, 2, 4 and 6 m out — *rising* as the bowl's own falloff weakens, which is
the only shape of evidence that tells a guard band apart from a shell that was simply weaker out there.

**The trap this landed with, and the reason the authored field is a test.** `CellTrenchDist` is allocated in the
`MapData` constructor, and a `NativeArray` starts at zero. Zero means "touching a trench", so during generation —
before `BuildTrenchDistance` runs — every cell read as banded and `CraterStamp` refused to dig the whole map. The
generated battlefield came out flat and completely unshelled. Nothing in the feature's own tests noticed, because
they all run on the playtest map, where the band is built; `BattlefieldTests.Generator_MakesAShelledWoodWithARiver`
caught it with "shell holes: expected greater than 200, but was 0". The array now initialises to 255, and
`DynamicGroundTests` pins that default on a bare `MapData`.

### The burst that knows which way it was going (`Sim/Combat/Blast.cs`)

`Impact` carries `Dir` (the shell's flight, flattened to XZ; zero for a cook-off or a round coming straight down),
`Shape` (`BlastShape`: shell, masonry, cook-off) and `Rubble` (metres of mound a collapse heaps up, which
`DeformationSystem` turns into a `CraterKind.Mound` stamp that raises the ground instead of digging it). The
`Explosion` event carries the direction in `Dir.xz` and the shape in `Dir.y`, so presentation can lean the picture
without guessing from the weapon id. Every producer fills it: tank and walker HE from the muzzle (a Kettle's
indirect round deliberately gets none — it comes down steeply, and its directional look is a tall thin column, not
a lean), the gunboat from the hull to where the shell lands, the off-map battery up the field from behind its own
line, the ambient shells alternating (the draw is taken *after* every existing roll, so not one shell moved).

`BlastRules` gathers what a burst is cut down by, in one place:

| What | Factor | Was |
|---|---|---|
| a shell in a man's own trench bay | `TrenchBayFactor` **0.7** | 1.0 — a direct hit in the bay was unprotected |
| the same trench, past `BayMetres` 12 m | `TrenchTraverseFactor` 0.5 | 0.35 |
| his trench, the shell out in the field | `TrenchOutsideFactor` 0.35 | 0.35 |
| in the open, when the shell went off *inside* a trench | `FieldShadow` 0.45 | 1.0 |
| a shell hole, or lying down | 0.6 / 0.7 | unchanged |
| broken ground in the line (`HeightfieldRaycast`) | `TerrainShadow` 0.55 | 1.0 |
| the far side of a directional burst | `1 + DirBias` 1.3, near side 0.7 | 1.0 both |
| falling masonry, on a man in a trench | `MasonryTrenchFactor` 0.5, knock halved | — |
| the floor under all of it | `MinThrough` 0.08 | — |

The 0.7 is the owner's rule made arithmetic: **a trench always gives limited protection**, so even a shell in your
own bay leaves you three tenths better off than standing in the open.

Two deliberate restraints. **Suppression is leaned but not shaded**: keeping your head down is what a man does
whether or not the parapet saved him, and the M1.5 fun gate tuned a barrage's suppressive weight against the old
numbers — shading it here would quietly make garrisons far harder to pin, which nobody asked for. And **the terrain
ray is bounded**: it is asked only past `ShadowFrom` 0.4 of the radius, only when there is a crater or trench cell
at one end to cast a shadow at all, and at most `MaxRaycasts` 48 times per impact, taken in slot order so the cap
falls the same way on every machine. `BlastSystem.LastRaycasts` exposes the count and `DirectionalBlastTests` holds
it to the cap.

### Tanks fail by degrees, like the walkers always have (`Sim/Units/VehicleModules.cs`)

A walker has limped at 16 % a leg since the crabs landed, and the owner liked it. A tank's track was a switch:
whole, or thrown and the tank stuck where it stood. Now both are curves.

| Module | Health | Speed |
|---|---|---|
| track, each | >= `TrackFullAbove` 0.8 | 1.0 |
| | 0.2 to 0.8 | `TrackWorstFactor` 0.35 rising to 1.0 (0.78 at the 0.6 a repair leaves) |
| | < `TrackThrownBelow` 0.2 | 0, and `Immobilised` — it used to take reaching exactly zero |
| both together | | the worse sets the pace, and a mismatch costs `TrackMismatch` 0.15 more: it crabs |
| engine | >= 0.7 / 0.2-0.7 / < 0.2 | 1.0 / `EngineWorstFactor` 0.45 rising / stalled |
| walker legs | per leg gone | `max(LegFloor 0.25, 1 - LegLoss 0.16 * legs)`, unchanged |

`VehicleTrackHit` and `VehicleStalled` now fire on crossing those thresholds rather than on reaching zero, so the
flag and the picture agree: `TankRenderer` sheds the track off the hull exactly when the sim stops calling it a
track. `MendWorst` prefers a module that has actually failed, at the same thresholds. `TankMobilityTests` is the
curve as a table, so a tuning pass shows up in the gate rather than in a playtest.

### Replay format v4

`TerrainHashSystem` (`Sim/Match/`, order 50) steps nothing and folds `MapData.Hash` into the tick hash, closing the
gap docs/03 had recorded and deferred "to the next replay-format break". This is that break:
`ReplayRecorder.FormatVersion` 3 -> 4. It was cheap — no replay file is tracked and no test pins a literal hash
(every hash assertion in the suite is A-against-B), so the whole cost was one constant.

Gate at this point: **EditMode 337/337, PlayMode 15/15** — including the three-thousand-unit lockstep stress run and the two-peer sync under latency, jitter and loss, which is what says the ground and the new blast chain are still deterministic.
