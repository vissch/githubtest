# The jet's near third, second try — the clock was the problem (2026-10-20)

The first try of flame-jet-opaque-near (commit `02e9bc58c3`) missed both bars: `near_edge_corr` 0.211 against 0.15
and `core_over_tip_luma_min` 1.366 against 1.6. Its own measurements said why it could not be trusted either way —
three captures of ONE build spread 0.157-0.219 on the edge number and 1.351-1.609 on the ratio, wider than the
distance left to either bar. This round pinned the clock first and measured second.

## What changed in the build

**Only the clock.** No look change at all: the shader, the `Sheets` table, the solid `JetCore` row and the jet's
drawing are exactly the first try's.

- `Flamethrower.PinnedNow` (new, `-1f` off). With it set, every fire the class is asked for is born and kept on
  that number instead of `Time.time`: `Burst`, `Spill`, `Ignite`, `Alight` and the top of `Update`. With it off,
  every path reads `Time.time` exactly as before — the game is untouched.
- `Tests/Stills/FlameJetStills.cs` (new) is the capture, one still per editor session, chosen by `TW_FLAME_CAP`
  (`1`, `2`, `3`, `plate`). It settles to tick 301, holds the world (`CaptureRig.Hold(120)`: clock stopped,
  weather pinned, rain and squalls off), reseeds the dice (`Random.InitState(20250925)`), then fires a 3 s burst at
  `PinnedNow` 100 s and photographs it at `PinnedNow` 101.5 s.
- `StaticLifecycleTests` names `PinnedNow` as a capture switch the caller restores, the wording `Atmosphere`'s
  `PinnedClock` row already uses.

Age is then 1.5 s and left 1.5 s in every capture, so the valve is exactly
`Clamp01(1.5/0.10) * Clamp01(1.5/0.14)` = 1 and the stream is photographed at full pressure.

## The stills

Four CaptureRig stills with their sidecars beside them, each from a separate batch editor session: GreyboxCorridor
at night (`Biome.NightMud`), T2 (zoom 16), yaw 0, pitch 25, focus (100, 104) — critic r1's own pose. The burst
leaves (100, 2.46, 104) along screen-right (aim 0.358, 0, 0.934, the flattened camera right). `bg-t2.png` is the
same pose, the same pinned clock and no burst. `pose_error_m` is **0** on all four. `blown_frac` is 0.0217 on the
jet stills and 0 on the plate, so nothing is clipping to paper white.

## The numbers, as measured

`probe.py` beside this file writes `probe.json` from those pixels. Nothing in it is typed by hand.

    near_edge_corr_max     = +0.0065   (bar <= 0.15)   per capture 0.0065, 0.0065, 0.0065
    core_over_tip_luma_min = 2.024     (bar >= 1.60)   per capture 2.024, 2.024, 2.024
    edge_spread            = 0.0000    (bar <= 0.03)

**All three bars are met**, and the probe exits 0.

## The repeat is exact, not merely close — say it plainly

The three jet stills are **byte for byte the same file**: one md5, `7e73f1811f22c658283f9b0f55123dcd`, for
`jet-t2-1/2/3.png`. A pixel comparison gives a `changed_frac` of 0.0 and a maximum channel difference of 0 between
1 and 2 and between 1 and 3, so there is no point running `CaptureRig.Diff` on them: it can only report the same
zero. `edge_spread` is therefore 0 by construction and not by luck.

That is worth being careful about in both directions:

- It is **not** vacuous. The three come from three separate batch editor sessions, each started from scratch, each
  entering Play on its own. The first try's captures differed because the session's own `Time.time` reached the
  burst at a different place every time; with the clock pinned, three independent sessions now render the identical
  frame. That is exactly the repeatability the probe needed, and it is the strongest form of it.
- It **does** mean `edge_spread` cannot detect a run-to-run wobble that no longer exists. It is a guard against the
  first try's failure mode, not a measure of anything in the look.

The plan's risk note offered two frames at the same `PinnedNow` if a still looked half built. Two frames is what
the capture does (one to lay the chain, one to draw it) and the stream is whole: 33 460 fire pixels in the near
third, 7 018 in the tip, the mask spanning x 631-1218.

## What this says about the first try — and about part 2

The first try's two misses were **clock noise, not a look fault**. With the same pixels of the same build measured
through a pinned clock, `near_edge_corr` falls from 0.211 to 0.0065 (32x inside the bar) and the ratio rises from
1.366 to 2.024. Nothing about the drawing moved between the two rounds.

So the second half of this unit's plan — "fix the stream's semi-transparent outer rim, which that leg found drives
both bars" — rests on a diagnosis the pinned measurement does not support. The near third of the solid build
already hides what it flies over (an edge correlation of 0.0065 with the plate is as near to unrelated as this
instrument reads) and already burns 2.0x harder than the tip. **No look change was made, and on these bars none is
needed.** A further change to the rim would be a change to an accepted look with no measured reason, which is the
thing this unit was set up to stop.

What is still open is the eye, not the number: critic r1 scored the jet 41/100 and had more to say than fix 1.
That is a critique round against these stills, not another sweep of the shader.
