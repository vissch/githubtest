# Concept — a rocketed man lands head first, legs kicking

The idea the owner accepted on 2026-10-08: a man blown high comes down head first and sticks in the mud to his
waist; his legs kick for three seconds, then droop. He picked option **A, waist deep**, on 2026-10-09. This is
the lane's record of the concept stage, round 2. The pictures live on the board, not here:
`evidence/idea-a-rocketed-man-lands-head-first-legs-kic/concept/` (`shown.jpg`, five pages, `notes.md`,
`checks.txt`, `luminance.txt`, `probes.json`, `seed-diff.png`, `greyscale-check.jpg`).

## The four options

| | Above the mud | Kick | Ends |
|---|---|---|---|
| **A  Waist deep** *(his pick)* | two legs, 0.905 m | 3.0 s | folds to 0.50 m and hangs: an M |
| **B  Dart** | 1.60 m, stiff, leaning 10° | quivers 2.0 s | tips over and lies flat, as today |
| **C  Boots only** | two boots, 0.446 m | 3.0 s, one boot drops off | the boots stay |
| **D  Out the other side** | A, plus a 0.445 m helmeted head 1.45 m on | 3.0 s | as A |

C is nearly `DeathGag.Boots`, which the game already plays for a Beam death at absurdity ≥ 0.5
(`Assets/_Project/Presentation/Core/DeathGags.cs:127`, `BootsFrom` `:82`). B ends flat on the mud, which is what
happens today — it un-plants the man the idea wants planted.

## Scale

fov 25, pitch 25, zoom 30 → the camera sits 78.12 m out, 34.64 m of field fill 900 px → 25.98 px per metre. A
man is `BakeHeightM` 1.78 × `UnitScale` 1.125 × `Grow(30)` 1.25 = 2.5031 m = 65.03 px
(`Assets/_Project/Presentation/Core/FigureMetrics.cs:13-26`). Every page is drawn at that scale and carries a
rifleman measured back off its own pixels, ±2 px.

## What the build stage inherits

1. **A far row is needed.** The standard view's camera sits 78.12 m out, past `FallenNearDistance` 70 m
   (`Assets/_Project/Presentation/Units/VATRenderer.Fallen.cs:32`), so a planted man there is drawn by the far
   model's Death rows. Without a far row for the kick, the gag plays only when the player is zoomed in.
2. **The sink swallows him.** `SinkDepth` is 1.1 m over `SinkSeconds` 2.5 s (same file, `:31`). A's legs stand
   0.905 m out of the lip, so the gag must opt out of sinking, or begin sinking only once the legs have drooped.
3. `FallenSeconds` 30 s (`:30`) leaves room for the gag's 3.5 s.

Building the gag — the new `DeathGag`, its clip and its far row — is the build stage, not this one.

## Checks this stage ran

Both scripts live in the board folder and are rerunnable (`python gen.py`, then `python measure.py`).

- `checks.txt` — one `FIELD_SEED` for all five pages (non-man difference bbox `None` against A for B, C, D, E);
  the rifleman 67 px on all five pages against the 65.03 px bar; A's silhouette 35 px (1.35 m) against today's
  fallen body at 33 px (1.27 m), the 0.45 m mud crown counted in and said so; `shown.jpg` 398,911 bytes at
  `sheet.png`'s own size, by quality only. **0 failed.**
- `luminance.txt` — every carrying feature of the planted man probed off `shown.jpg`'s own JPEG pixels against
  that page's ground box and far-field box, all inside the LIFE SIZE band, bar 2.50:1. Worst row: B, tunic over
  the far field, 2.82:1. **0 rows below the bar.**
- Pages A–D are drawings (PIL, no editor). Page E is the only rendered picture: `today-crop.png`, cut 1:1 from
  `today.png`, sidecar `today.json` (frame 3033, `pose_error_m` 0, `men_in_frame` 4, `fallen` 1).
