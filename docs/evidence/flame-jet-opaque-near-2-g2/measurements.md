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

# Part 2: the change is made, and it cannot be judged here (2026-10-24)

Steps 4-6 of the plan are built and green. Step 7's captures then showed two things that stop this unit, both
measured in this checkout today, with the three after stills byte identical as the pinned clock promises
(md5 `ae11fd4b166c1a7f7d93dd5fe98ca545`, one per batch editor session).

## 1. The committed baseline does not reproduce on this build

`recapture-unchanged.png` is a capture of the **unchanged** tree (`git checkout --` on `Flamethrower.cs`, nothing
else in the working tree, same harness, same session recipe). Against `before-1.png`, which part 1 took from the
previous round's folder on the strength of `git log` alone:

| | fire span | last-third px | white px | `tip_white_frac` |
|---|---|---|---|---|
| `before-1/2/3.png` (previous round) | x 631..1218, 588 px | 25 591 | 1 217 | 0.0476 |
| `recapture-unchanged.png` (today, same code) | x 631..1139, 509 px | 34 948 | 3 032 | 0.0868 |

The sidecars agree on the pose to the metre and on `luma_mean` to 0.002, so this is not a different shot: the
stream itself is 79 px shorter and the last third is a fatter, brighter part of it. Nothing under
`Assets/_Project/Presentation`, `Tests/Stills` or `Resources` changed between `ee49e7ee4b` and this commit
(`git log ee49e7ee4b..HEAD -- ...` is empty), so part 1's "the before stills **are** three captures of the current
build" is false for a reason this leg has not found.

That makes both numbers part 1 derived from those stills invalid: `WHITE_BAR` 0.0238 is half a baseline that this
build does not produce, and `TIP_PX_REF` 25 591 puts `tip_px_ratio` at **1.366** for the **unchanged** build, which
is outside 0.85..1.15. So `probe.py` cannot exit 0 for any change to the drawing, and the bar may not be moved
after the fact (the plan's own rule). The unit is blocked on it; the question is in
`docs/reference/decisions.md` under "Open: waiting on the owner".

## 2. Steps 4 and 5 move nothing the probe can see

With `CoreCool` in and the scraps dimmed, the three after stills measure **exactly** the unchanged build's numbers:

    jet-t2-1/2/3.png   x 631..1139 (509 px)   last-third 34 948 px   white 3 032 px (0.0868)
    tip_white_frac_max 0.0868   tip_white_spread 0.0000   tip_px_ratio 1.366   near_edge_corr_max +0.0176

The harness is not blind: a deliberate gross version of the same rule (`CoreCool` returning **0** past the hot
third) produced a different image (md5 `d2293a20…`), so the capture does see this code. The cut as planned is
simply below what the fire mask and the white rule register.

Where the white actually is, binned along the stream (both builds, tenths of the fire span):

    before-1.png    0.059 0.132 0.087 0.066 0.082 0.152 0.085 0.085 0.000 0.000
    after (today)   0.050 0.111 0.115 0.062 0.074 0.097 0.152 0.089 0.100 0.004

White is 5-15% of the fire in **every** tenth of the stream, with its peak in the sixth or seventh - past the hot
third but well short of the tip. Part 1 attributed the loop to the core chain's flat alpha past the hot third; on
these numbers that chain is at most a part of it, and the plan's own next suspects (`Book.Bloom` at the terminus,
then the `Book.Fire` chain) are untested.

## Honesty note

`tip_white_spread` is 0.0000 by construction, not by luck: the clock is pinned, so three sessions render the same
frame and this number can only ever say "the pin held". It is not evidence that the look is stable in play.

## State after part 2

    python docs/evidence/flame-jet-opaque-near-2-g2/probe.py           -> exit 1 (tip_white_frac and tip_px_ratio)
    unity test . --mode EditMode --filter FlameJetCoreTests            -> 4 passed, 0 failed

`CoreCool` and the dimmed scraps are committed: they are the half of the owner's 2026-10-19 reversal the core chain
never got, they are guarded by `FlameJetTipCoolsToSoot`, and they make the picture no worse by any number here.
They are **not** proven to remove the loop the owner saw.

# Part 3: the baseline restated on this build's own captures (2026-10-24)

No look change is in this commit. Only the baseline stills and the two numbers derived from them move.

## Why the old baseline was false

Part 1 took `before-1/2/3.png` from the previous round's folder on the strength of `git log` alone. Part 2 showed
by capture that this harness no longer reproduces them. The md5s in this folder settle it:

| File | md5 | What it is |
|---|---|---|
| `jet-t2-1/2/3.png` | `ae11fd4b166c1a7f7d93dd5fe98ca545` | three batch editor sessions of today's build |
| `recapture-unchanged.png` | `ae11fd4b166c1a7f7d93dd5fe98ca545` | **the same picture**, captured from the unchanged tree |
| `before-1/2/3.png` (as they were) | `7e73f1811f22c658283f9b0f55123dcd` | the previous round's stills, copied in |

The after stills and the unchanged-tree capture being one md5 is the proof that 0.0868 / 34 948 px is **the
build's** number, not this round's edit. The old 0.0476 / 25 591 px belongs to a harness this checkout no longer
runs, so every number derived from it — `WHITE_BAR` 0.0238 and `TIP_PX_REF` 25 591 — was measuring nothing.

## The new baseline

`before-1/2/3.png` and their sidecars are now copies of `jet-t2-1/2/3.png` and theirs. The sidecars' `path` field
therefore still reads `jet-t2-N.png`; a captured sidecar is not edited after the fact, so it is said here instead.

    python probe.py --spread
    before-1/2/3.png   x 631..1139 (509 px)   near 24 007 px   last-third 34 948 px
    white 3 032 px  ->  tip_white_frac 0.0868, 0.0868, 0.0868      spread 0.0000
    half the smallest  0.0434   <- what WHITE_BAR must be

    WHITE_BAR  = 0.0434   = 0.5 * 0.0868, half the smallest of the three
    TIP_PX_REF = 34 948   the median of last-third fire px over the same three

Both come straight out of that run; nothing is typed by hand. Halving still puts the bar outside the spread of
three captures of the current build, which is what the owner's words ask for.

The docstring's old claim that the last **sixth** holds 0 white px is deleted: on these stills the white runs the
whole length of the stream and the last tenth measures 0.004. Part 2's per-tenth bins are the record.

## This is option (a) of the open question

`docs/reference/decisions.md`, "Open: waiting on the owner", asks whether a committed bar may be restated when the
build no longer reproduces the stills it was derived from. The question is unanswered; option **(a)** — re-derive
baseline and bar from three captures of today's build — is the stated default, and this part carries it out. If
the owner picks otherwise, this part is what gets reverted.

## State after part 3

    python docs/evidence/flame-jet-opaque-near-2-g2/probe.py --spread  -> exit 0
    python docs/evidence/flame-jet-opaque-near-2-g2/probe.py           -> exit 1

    tip_white_frac_max  0.0868   against the bar 0.0434   <- the only bar still failing
    tip_white_spread    0.0000
    tip_px_ratio        1.000    (was 1.366 against the false reference)
    near_edge_corr_max  +0.0176

That is the honest state before the cut: the tip is no longer accused of having shrunk, and the white stroke is
the one thing left to remove.

# Part 4: the capture is of HEAD, and the white is the HOT core chain's card faces (2026-10-24)

No look change is in this commit. It adds `whitemap.py` (read only) and the measurements below. Twelve captures,
all of this checkout, one still per batch editor session, the same recipe as every other still here.

## The capture does see this checkout's code

One still of HEAD, captured into `%TEMP%/tw-g2-check` (outside the checkout) and compared by md5 with the
committed `jet-t2-1.png`:

    ae11fd4b166c1a7f7d93dd5fe98ca545   jet-t2-1.png (committed)
    ae11fd4b166c1a7f7d93dd5fe98ca545   the HEAD capture of today

**Equal.** So the committed stills are of HEAD, and part 3's restated baseline (0.0868 white, 34 948 last-third px)
is this build's number. Part 1's old baseline stays dead; there is one baseline, part 3's.

## What the white is made of (`whitemap.py`)

    white per tenth (nozzle -> tip): 0.049 0.112 0.115 0.060 0.074 0.097 0.152 0.089 0.100 0.004
    of the white: clipped 0.000 (0 px), cream 0.999 (11 197 px), neither 0.001
    last third: white 3 032 px in 41 pieces
      1 387 px  x  970..1009    0% within 3 px of the contour -> inside the mask
        849 px  x 1035..1094    5% within 3 px of the contour -> inside the mask
        356 px  x 1017..1033    1% within 3 px of the contour -> inside the mask

Two findings, both against the guesses on the table:

- **Nothing is clipped.** Not one white pixel has r, g and b all >= 250, and 99.9% of them are cream (r - b in
  20..45). So this is not additive cards stacking into blow-out; it is the **cream crescent glyph** of the stream
  books being drawn bright.
- **It is not a loop.** Every large piece sits inside the fire mask (0-5% of it within 3 px of the mask's contour),
  so what the owner read as a stroke round the stream is a bright **card face** lying in the orange band, not an
  outline hugging its edge.

## The ablation table

One capture per suspect, the emitter commented out in the working tree only (no production hook, no new static),
`git checkout --` after each. The share is of the last third's 3 032 white px at HEAD.

| # | Suspect (`Flamethrower.cs`) | last-third white | share removed |
|---|---|---|---|
| - | HEAD | 3 032 px | - |
| A | the core chain PAST the hot third (`~841`, `CoreCool`'s band) | 3 741 px | **-23%** (worse) |
| B | the scraps past the head (`~776`, `Book.Core`) | 3 032 px | 0% |
| C | the boil (`~1008`, `Book.Fire`, t 0.55..1.15) | 3 032 px | 0% |
| D | the envelope past the hot third (`~636`, `Book.Jet` links) | 3 075 px | -1% (worse) |
| E | the terminus (`~927`, `Book.Bloom`) | 3 032 px | 0% |
| F | the loose chain (`~985`, `Book.Fire`, glow 1.0 -> 0.22) | 3 183 px | -5% (worse) |
| G | the head bolus (the last envelope link, `Book.Head`) | 3 032 px | 0% |
| H | A + B + C + D + E + F together | 4 974 px | **-64%** (worse) |
| I | the burning fuel pools on the ground (`~1128`, `Pool`/`Pyre`) | 3 032 px | 0% |
| J | the two extra staggered HOT cards (`~882`) | 3 938 px | -30% (worse) |
| K | **the HOT core chain itself** (`u < HotThird`, `Book.JetCore`) | **59 px** | **98%** |

**The emitter is the hot core chain.** Its cards are the near third's opaque white-yellow core
(`Book.JetCore`, thickness x1.8, `glow * 2.3`, plus two staggered passes at 2.2 and 2.1), and they are so wide
that their cream faces reach two thirds of the way down the stream: cut them and 98% of the last third's white
goes with them. Every other emitter in the jet is innocent, and removing any of them makes the stroke **worse**,
because their orange cards are what covers the cream underneath - which is why part 2's `CoreCool` moved nothing.

Suspect K is not a candidate fix as it stands: without the hot chain the fire mask falls from 120 604 px to
100 818 px and the near third loses the opaque body `near_edge_corr` guards. The cut has to bound how far the hot
cards' faces reach, or how bright they are past the hot band - not delete them.

## State after part 4

    python docs/evidence/flame-jet-opaque-near-2-g2/whitemap.py   -> exit 0 (prints the map above)
    python docs/evidence/flame-jet-opaque-near-2-g2/probe.py      -> exit 1, tip_white_frac_max 0.0868 vs 0.0434

Part 3 of the plan (steps 9-11) makes the cut, on suspect K, with `tip_px_ratio` 0.85..1.15 and
`near_edge_corr_max` <= 0.15 as the two things it may not spend.
