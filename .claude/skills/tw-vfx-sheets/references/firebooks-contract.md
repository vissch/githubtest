# What the game accepts: Tools/firebooks.py and FlipbookFx

## firebooks.py
`cd trench-warfare-3d && python Tools/firebooks.py [packdir] [outdir]`. The defaults are `G:/My Drive/vfx/sheets` and
`Assets/_Project/Resources/VFX`.

- **Table-driven, not filename-driven.** `BOOKS = {BookName: (exact pack filename, mode, root_left, keep)}` and
  `FRAMES_IN = {BookName: n}` are typed by hand. The script never parses `_CxR_`, `_Nf`, `_loop` or `_piv`.
- **Grid hardcoded to 8 × 4.** Each cell is sliced as `h/4 × w/8` and resized to 256 × 256 (LANCZOS), so a
  non-square cell is squashed. Alpha below 6/255 is zeroed.
- **Value mode:** `luma` (0.299/0.587/0.114) for orange sheets; `light` = (max + min) / 2 for saturated blue, green
  or red. Max-channel was rejected.
- **`root_left`** (directional books) slides every cell so the drawing's root sits at x = 0.
- **`keep = (first, last)`** is inclusive. Kept frames are packed from cell 0 and the rest stay transparent.
- **Output:** always a 2048 × 1024 RGBA image of 8 × 4 cells of 256. RGB is grey = value × alpha (premultiplied);
  A is alpha. The shader reads `ink = luma(rgb) / max(a, 0.06)`. A colour sheet must never go into
  `Resources/VFX` directly.
- **Printout:** ink p03/p97 → `Low`/`High`; extent → `Ink`; `Fill`; a band histogram; share-matched `Bands`.
  Paste all of these into the `Sheets` row.
- **Hash guard:** only FireBall, FireColumn and FireBurst are hashed (`UNCHANGED` / `*** CHANGED ***`). Every run
  rewrites every book, so check the rest with `git diff --stat -- Assets/_Project/Resources/VFX`.

## Current books
| Book | Pack file | Mode | Root-left | Keep |
|---|---|---|---|---|
| FireBall | orange_fire_explosion_8x4_12fps_32f | luma | | |
| FireColumn | orange_ground_explosion_8x4_12fps_30f | luma | | |
| FireBurst | orange_center_explode_8x4_12fps_32f | luma | | |
| FireJet | blue_direction_explosion_8x4_12fps_29f | light | yes | |
| FireBlast | purple_fire_explosion_8x4_12fps_32f | light | | |
| FireFan | green_direction_explosion | light | yes | (8, 23) |
| FireStand | blue_fire_explosion | light | | (2, 17) |
| FirePool | blood_ground_1_8x4_12fps_32f | light | | (16, 31) |
| FireCore | purple_direction_explosion | light | yes | (6, 27) |
| FireBloom | blue_ground_explosion_8x4_12fps_27f | light | | (4, 24) |
| FireHead | orange_explosion_8x4_12fps_26f | luma | | (4, 22) |

`G:\My Drive\vfx\sheets` also holds `chlorine_billow`, `chlorine_vent` and `smoke_bank` (2026-09-25). No game code uses them yet; the game's Gas is a tinted `Puff`.

## Registering a book: FlipbookFx.cs (Presentation/Camera, SHOW lane)
1. Add a `Book` enum entry.
2. Add a `Sheets` row **at the same position**. The table is indexed as `Sheets[(int)book]`, so its order must match the enum.
3. Fill the row: `Cols = 8, Rows = 4, Frames = last − first + 1, Fire = true, Snap = true, Fps = 12f`, a `Tint`, and `Low / High / Bands / Ink / Fill` from the printout. `Cycle = true` for loops.
4. Textures load by `Resources.Load("VFX/" + Name)`. `Ready` needs every sheet, so one missing PNG disables all flipbooks.
5. After moving `Low`/`High`, re-measure `Bands`. A core cut of 1.00 means that band is switched off; this happened once to FireStand, which made it the dimmest fire in the build.
6. The fire hierarchy the owner signed off, measured by firebooks: about 15 % soot, 16 % red rim, 46 % orange body and 23 % pale heart. FireJet is the exception at 12 % core, because it is one long card. A darker cut read as blood on mud at night.

## Seeing it
- `bash Tools/flameshots <prefix>`: Play in GreyboxCorridor, `Random.InitState(20250925)`, four shots (jet, cook, stand, wall) into `Tools/flame-shots/<prefix>_<shot>.png`.
- `python Tools/flamecheck.py <png>`: exits 1 if the shot has no fire. The fire mask is `(r−b ≥ 60 & lum ≥ 70) | (lum ≥ 190 & r−b ≥ 15)`, and the default floor is 15,000 px.
- Capture the same build twice first: the difference between the two is the noise floor.
