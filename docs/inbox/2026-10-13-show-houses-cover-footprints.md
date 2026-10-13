# Houses give cover: the sim needs the hamlet footprints (from lane/sim/houses-cover)

The sim now knows what a house is and gives a man on foot behind one 35 % cover from small arms (the owner,
2026-10-07: "A little: 35 % less, like a man in a shell hole"). `MapData.Houses` holds the footprints and
`MapData.AddHouse(new HouseDef { Pos, Yaw, Half })` adds one (centre, radians, half extents in metres).

**Nothing places one yet.** `BattlefieldComposer.PlaceHamlets` (SHOW) puts kit houses by the bridge and its own
comment says the sim knows nothing of them. Please hand each placed house to `map.AddHouse` with the same centre,
yaw and half extents as the kit piece, after the trenches are cut. It changes no nav cell and no cell cover: only
`DirectFire` reads it.

It is in the tick hash (appended at the end of `MapData.Hash`), so rerun the determinism, replay and hash tests
with the change, and note it is a seam: the footprints must be identical on both machines.

Replay format is v38 on `lane/sim/houses-cover`, renumbered at landing.
