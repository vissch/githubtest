# 19 — The coastal level and the snow level: what exists today

Written 2026-09-23, after the owner moved the volcano level down the list and put **the coastal level and the
snow level** at the top, coast first.

This is an inventory, not a plan of record. Everything in it was read out of the code or measured, and where a
number appears it is the number in the file rather than an intention. It supersedes nothing; for winter it
**statuses** the W1–W10 list in `18-biomes.md` rather than restating it.

## The headline

**Neither level exists.** There is exactly one generated battlefield —
`BattlefieldParams.ShelledForest` (`Sim/Terrain/BattlefieldGenerator.cs:28`) — and one mission card,
`ShelledWood1917`. The coast is a 36 m strip appended behind that map's far trench, and snow is a repaint of the
same ground. What *does* exist is a surprising amount of machinery underneath both, most of it built, tested and
unused at the scale a level would need.

| | Coast | Snow |
|---|---|---|
| A map of its own | no | no |
| A mission card | no | no |
| Sim systems | **yes** — `SeaLandingSystem`, 305 lines, 8 tests | none, and none needed: winter is a repaint |
| Presentation | **yes** — ocean, shore, craft, backdrop, shader | **yes** — snow term, snowfall, full biome profile |
| Blocking problem | the map is too small for its own boats | none blocking; the open items are art |

---

## 1. The coastal level

### 1.1 What is built

**Sim — authoritative and hashed.**

| Where | What |
|---|---|
| `Sim/Terrain/MapData.cs` | `SeaSide` / `SeaTeam` / `SeaAway` / `SeaStartZ` / `ShoreZ` / `SeaLevel` / `HasSea` / `Offshore(z)`. Already **side-agnostic**: only the generator hardcodes side 1 |
| `Sim/Terrain/BattlefieldGenerator.cs` | `BattlefieldParams.Sea`, `SeaMargin` (36 m), `ShoreAt` (19 m), `BeachHeight()` — dry sand and dunes, then the waterline, then shallows; noise ribs running with the shore; starts at exactly the rear ground's height so the beach joins the field without a step |
| `Sim/Match/SeaLanding.cs` (305) | `SeaLandingSystem` + `ISeaLift`. 6 craft × 8 berths; a paid deploy goes aboard instead of spawning; run in at 7 m/s from 96 m, ground at 2.2 m of draught, ramp down over 16 ticks, men out every 3 ticks, retract at 4.5 m/s. Stores runs at 800 ticks then every 1100. Three sim-owned gunboats shell inland every 460 ticks (300 damage, 7.5 m, 80 suppression) |
| `Sim/Core/ISeaLift.cs` | the seam `SimWorld.Deploy` offers each paid unit to |

**Presentation.**

| Where | What |
|---|---|
| `Ocean.cs` (139) | one mesh, one draw, one material. Runs `Reach` **1150 m** out and `Flank` 780 m past each side. Graded grid — 2 m rows at the breakers, 100 m in the open — about 4,000 vertices over ~1.5 km². Per-vertex depth, waterline proximity and a buried flag, baked because depth only changes when the ground does. Swell 0.62 m / 34 m / 4.2 m/s, held here because the boats ride the same water |
| `Shore.cs` (36) | the single answer to "how high is the ground out there": `Bed()`, `Shape()`, `UnderWater()`. Skirt, sea mesh and backdrop props all ask it, so the three agree to the centimetre |
| `LandingCraftView.cs` (309) | draws craft and gunboats where the sim says they are |
| `BattlefieldBackdrop.cs` (303) | continues the trenches off the flanks, belts the land edges |
| `Shaders/Sea_URP.shader` (196) | vertex-baked depth, two Gerstner swells, surf cut from one wave phase per pixel |

**Tests.** `Tests/EditMode/LandingTests.cs` (171), 8 tests: the map runs on into sand and water without moving
the battle; a deploy puts a craft in the water instead of a man on the field; the craft grounds and puts its men
on the sand; the landed men walk inland; the other side still walks up from its own rear; more men than the boats
can carry still arrive; the landings are the same on every machine; a craft carries either men or one tank.

### 1.2 The blocking problem, measured

`SeaMargin` is a **36 m const**, and `ShoreZ` is `Length + ShoreAt`, so ShelledForest has **17 m of water in Z**.
Against that:

| | metres seaward of the waterline |
|---|---|
| water the **map** has | **17** |
| a landing craft appears at (`StandOff`) | 96 |
| a gunboat lies at (`ShipStandOff` ± its stagger) | 90 – 270 |

So **every run-in and every gunboat is outside the map** — past the heightfield and outside the nav grid. It is
not invisible, because `Ocean.Reach` draws water 1150 m out; it is *un-simulated ground*. Nothing throws, because
`CellOf` and `Height.Sample` both clamp, which is exactly why this was never noticed. The consequence for a level
is that the approach — the thing a landing level exists to show — happens off the map.

**Second, unfixed and needing a decision:** `ShipSpread` is 110 m *along* the coast, so three gunboats sit at
−110 / 0 / +110 ±30 about the centre — a fleet **250 m wide on a 90 m map**. Two of the three are off the side
today. Correcting it moves ship positions, which are hashed sim state on the shipped map, so it is a gameplay
change rather than a widening and is left for the owner.

### 1.3 What is missing

- **No preset and no mission card.** (`BattlefieldParams.Landing` and a widened `SeaMargin` are staged.)
- **Nothing is composed on the sand.** `BattlefieldComposer`, `BattlefieldKit` and `PropLayout` never read
  `SeaStartZ`, `ShoreZ` or `Offshore`. The beach is bare heightfield with noise ribs — no obstacles, no wire, no
  wreckage, no cover. A man landing walks up an empty slope.
- **Beach obstacles are promised in a comment and do not exist.** `BattlefieldGenerator.cs:60` says the dry part
  "has to be wide enough to land a company on and to hold the beach obstacles". There are none. The *art* is
  there — `BattlefieldKit.cs:376` builds a `TimberHedgehog` — it is simply never placed on a beach.
- **No coastal biome.** `Biome { NightMud, Lava, Winter }`. A coast currently borrows whichever look is set.
- **The defender has no shore line.** Trenches are placed by the layout at both ends; nothing digs in *above the
  beach*, which is the one piece of geometry a coastal defence needs.

---

## 2. The snow level

### 2.1 Status of W1–W10 from `18-biomes.md`

| | Item | State |
|---|---|---|
| W1 | Snow on every up-facing surface | **built** — `TWSnowAmount` in the shared header; terrain, props and men all take it from one rule |
| W2 | Frozen shell holes: pale, translucent, cracked ice | **colour only.** `LiquidTint` (0.52, 0.60, 0.72, 0.75) lands the hue; the surface still *ripples and flows*, because that lives in the vertex normal and the flow map. Right colour, wrong motion |
| W3 | Snow drifts sculpted into the ground | **not built** |
| W4 | Blizzard, wind-driven and streaking | **partly** — `Rain.AsSnow` with `Snowfall` 0.85; it falls nearly vertically |
| W5 | Distance disappears fast in white-blue fog | **built** — `Depth` 150 m, `Mist` 0.64/0.75/0.90 at density 0.72 |
| W6 | Real mountains on the horizon | **not built.** The one item needing real geometry. `Haze` is already held *darker* than the sky (0.63, 0.70, 0.79) specifically so this stays buildable |
| W7 | Icicles on dugout lips and structures | **not built** |
| W8 | Men's breath | **not built** (planned in the close-up work) |
| W9 | Footprints | **not built** (planned; winter raises its value sharply) |
| W10 | Ice glare | **partly** — `SnowSparkle` 0.15, `SnowColor` alpha 0.10 keeps fresh snow matt so the glare belongs on the ice; there is no ice yet, so the one high-contrast element the image can have is missing |

### 2.2 Settled, and not to be reopened

**Winter troops keep khaki.** No greatcoat, no second cloth palette (owner, 2026-09-23). Winter's residual warm
was measured to be the **men** — not wire, duckboards or smoke, which measure cold at (114, 128, 152) against a
soldier's (109, 107, 86). That warm is therefore the **intended** look. Do not tune winter toward the reference's
0.2% warm by any means, and do not file the gap as a defect again.

### 2.3 What is missing structurally

- **No winter map.** `Winter` does not appear anywhere in `Sim/` — it is purely a presentation biome, so today
  the snow level is ShelledForest repainted: a wood, a river and a water table, in a blizzard.
- `Flooding` is already dropped to 0.25 "what water there is has frozen", but the river and the water table are
  still generated, so a frozen field still has a flowing river across no man's land.

---

## 3. Order of work

1. **Coast, geometry first.** `SeaMargin` as a parameter (zero keeps every existing map identical), a `Landing`
   preset with enough water for the boats, and a test that asserts a map contains the water its own landing
   system uses. *Staged.*
2. **Coast, the beach as a place.** Compose on the sand: obstacles from the existing hedgehog module, wire,
   wreckage and cover, keyed off `Offshore`. This is what turns a slope into a level.
3. **Coast, the defender's line.** A trench above the beach rather than the layout's ends.
4. **Snow, the map.** A winter preset: no river, no water table, thinner wood.
5. **Snow, W2 ice.** Ripple and flow to zero with cracks in their place — the same liquid surface a third time,
   and the item that gives the image its only high-contrast element.
6. **Snow, W6 mountains.** The only item on either list needing real geometry.

## 4. Two standing cautions

- **A metric with no opposing metric will be gamed.** From `18-biomes.md`, learned twice: the single-hue number
  climbed to 97% while the picture went monochrome. Any new coast or snow metric ships with its opposite.
- **The map clamps, so being off it is silent.** `CellOf` and `Height.Sample` both clamp, which is why a fleet
  outside the map produced no error for as long as it existed. When something coastal looks wrong, check whether
  it is *on the map* before checking what it looks like.
