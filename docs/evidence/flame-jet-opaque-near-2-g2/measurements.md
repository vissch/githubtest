# The white loop in the jet's orange tip — part 1: the baseline and the bar (2026-10-24)

The owner looked at the previous round's stills (`../flame-jet-opaque-near-2/`) and said the orange tip carries a
white loop that reads as a card stroke, not soot. This part measures the stroke on the current build and fixes the
bar the after stills must clear. No look change is in this commit.

## The baseline needs no new capture

Two facts, both checked here, not assumed:

- **The tree has not touched `Flamethrower.cs` since `ee49e7ee4b`** (`git log -1 -- .../Flamethrower.cs`), the
  commit this round starts from. So `../flame-jet-opaque-near-2/jet-t2-1/2/3.png` **are** three captures of the
  current build. They are copied in here unchanged as `before-1/2/3.png` with their sidecars.
- **The plate fires no burst**, so no look change to the jet can move it. `bg-t2.png` is copied in unchanged and
  is the plate for both the before and the after measurements.

The three before stills are byte for byte the same file (one md5, `7e73f1811f22c658283f9b0f55123dcd`). That is not
an accident and not a copy: each came from a separate batch editor session, and the previous round pinned the
jet's own clock (`Flamethrower.PinnedNow`, born at 100 s, photographed at 101.5 s, the world held by
`CaptureRig.Hold(120)` and the dice reseeded with `Random.InitState(20250925)`), so three independent sessions
render the identical frame. Every Perlin and `Mathf.Repeat` phase in the drawing is a function of 101.5 alone; no
session clock enters the picture. See `../flame-jet-opaque-near-2/measurements.md` for the full account.

## What the loop is, measured

`probe.py --spread` on the before stills, the fire mask being flamecheck.py's own rule:

    before-1/2/3.png   x 631..1218 (588 px)   near 33 460 px   last-third 25 591 px
    white 1 217 px  ->  tip_white_frac 0.0476, 0.0476, 0.0476      spread 0.0000

White is luma >= 215 and r - b <= 45: bright and **not** warm, so a Core card's cream crescent counts and hot
orange fire does not. The last third's mean luma is 140.4, so it is the orange band, not a hot area. The last
**sixth** holds **0** white px, so the white is not the tip glowing: it sits inside the orange band. 807 of the
1 217 px survive a 7x7 opening, so it is a narrow connected band several pixels wide — a stroke — rather than
scattered speckle.

## Who draws there

The core chain past the hot third (`Flamethrower.cs` ~826-852) draws `Book.Core` at `alpha: valve * drawnWeight`
and `glow: glow * 1f`, flat all the way to the tip, and the file's own comment calls Core "a cream crescent hook,
the brightest value in the picture". The scraps past the head (~770-795) use the same book at the same full alpha.
The owner's 2026-10-19 reversal (heat at the mouth, orange to the tip, soot past it) was applied to the envelope
(`alpha 0.95 - 0.12u`), to the boil and to the ground light, but never to these two. That is the defect part 2
fixes.

## The bar

    WHITE_BAR  = 0.0238   = 0.5 * 0.0476, half the smallest of the three before values
    TIP_PX_REF = 25 591   the before median of last-third fire px

Both come straight out of the `--spread` run above; nothing is typed by hand. Halving is what puts the bar outside
the spread of three captures of the current build: that spread measures 0.0000, and a bar a few thousandths under
the baseline would still sit inside any honest noise band.

The three guards against faking it:

| Bar | Want | Why |
|---|---|---|
| `tip_white_frac_max` | <= 0.0238 | the stroke is gone, at the worst of the three |
| `tip_white_spread` | <= 0.01 | a number inside the run-to-run noise cannot pass |
| `tip_px_ratio` | 0.85..1.15 | the tip cannot be deleted, shortened or dimmed out of the fire mask |
| `near_edge_corr_max` | <= 0.15 | the near third stays an opaque body (carried over unchanged) |

## State after part 1

    python docs/evidence/flame-jet-opaque-near-2-g2/probe.py --spread   -> exit 0
    python docs/evidence/flame-jet-opaque-near-2-g2/probe.py           -> exit 1, "not captured yet"

Exit 1 is correct here: there are no after stills yet. Part 2 changes the drawing, recaptures the three stills into
this folder as `jet-t2-1/2/3.png` and writes the result below.
