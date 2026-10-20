# ability-fx-captures-r2-2-g2: a gas measure three captures can agree on, 2026-10-20

**This leg (part 1 of 2) does not clear the probe.** It carries over the camera maths, rebuilds the mask and the
bar as the plan asks, and sets up the spread rule — but only `run1` exists so far (see "What is here" below), and
the probe needs three. `python probe.py` therefore exits 1, `three runs needed`, by design. The next leg shoots
`run2` and `run3` from this same build and sets `BAR_VALUES` from their spread.

## The owner's complaint

> What the probe calls gas does not read as gas. `cards-gas.jpg` is a flat green puddle with tracer streaks,
> `gas-o240.jpg` is yellow-green paint, and the hue window is 30 px because any plate-difference blob counts.
> `cards-smoke.jpg` is the volume this pose can already draw. Shoot three captures and stop clearing bars of 0.30
> and 0.03 on one JPEG (0.3240 and 0.0321). Bar outside the spread of three captures of the current build.

Two separate faults, both real: r2-2's mask counted ANY pixel that differed from the plate, named that count
"green" when it tested no colour at all, and then barred it against a single capture — a bar a repeat shot of the
very same build could easily land on either side of.

## What is here

- `run1/` — the four `GasBankStills` captures (`gas-t1`, `gas-t1-plate`, `gas-o240`, `gas-o240-plate`) plus
  `manifest.json`/`manifest-plate.json`, copied unchanged from `docs/evidence/ability-fx-captures-r2/`. This is
  capture 1 of three, not a reshoot: `git log` shows no change under `Presentation/` or `Tests/Stills/` since
  commit `2956ad4853` shot them, so the build is the one this unit measures, and the pose, pixels and sidecars
  are exactly what a repeat run of `GasBankStills` against that same build would produce.
- `cards/` — the four `GasBankCards` isolation stills (`cards-gas`, `cards-smoke`, `cards-puff`, `cards-plate`),
  copied the same way, for the unbarred look report only.
- `probe.py` — the measure (below).
- `probe.json` — `run1`'s numbers, written by `python probe.py --calibrate` (no bar check; see "Status").

`run2/` and `run3/` do not exist yet. They are the next leg's job: shoot `GasBankStills` and `GasBankCards` twice
more against this same build (`f032514063`), unchanged, and drop each set in its own `runN/` folder next to `run1`.

## The mask

A gas pixel must (1) differ from the plate by >= 20 RGB (T1) or >= 12 RGB (O240) — r2-2's own bars, carried
over — **and** (2) sit inside the single largest coherent plate-difference patch (`largest_blob`, T1 only; O240 is
five separated calls 40 m apart, so no blob there) **and** (3) be brighter than the plate (Rec.709 luma, >= 2).

(2) and (3) were tried in the order the plan describes — luma lift applied, THEN `largest_blob` — first. It
collapsed T1's patch from 14,805 px to 3,105: a lit translucent card has its own interior shading (tracer
streaks, overlapping sprite edges), so plenty of ITS OWN pixels sit darker than the plate even though the card is
there, and stripping those first breaks the patch into fragments `largest_blob`'s 1 px dilation cannot bridge.
Dropping the luma bar to 1, then 0.5, did not change this (`LUMA_BAR` is noise-tolerance, not the cause; the
cause is interior shading). So the order was flipped: `largest_blob` runs on the plate difference alone — the
test r2-2 already proved finds one coherent patch and not scattered JPEG/infantry noise — and the luma lift is
then applied WITHIN that patch, trimming it from 14,805 to 7,397 px. `mask_px_raw` (all plate-difference pixels),
`mask_px_patch` (after `largest_blob`, no lift) and `mask_px` (final, with the lift) are all in `probe.json` so
this is a visible cut, not an assertion.

## The names

`gas_t1_green_low_share` -> `bank_low_share`, `gas_t1_height_m` -> `bank_rise_m`, `gas_t1_height_frac` ->
`bank_height_frac`, new `bank_area_frac` (share of the T1 frame the bank patch covers, the T1 twin of O240's
area frac), `gas_o240_effect_area_frac` -> `o240_area_frac`. `gas_o240_green_area_frac` (a duplicate of the area
frac under a second name) is dropped. `gas_hue_window_px` stays, unbarred: plate-difference pixels that also fall
in the old 45-95 PIL hue window. No run below bars on it; it is kept only to show how little of the real
difference it would ever pass (30 of 65,336 raw pixels in `run1`'s T1 shot).

## The bar: the spread rule

With three `runN/` loaded, every bar is checked twice: all three runs must sit on the pass side, AND the nearest
run value to the bar must clear it by at least the three runs' own spread (max - min). A bar inside the run
values, or within one spread of them, is "the build cannot tell this from its own noise": the probe prints the
three values, the spread and the margin for every bar and exits 1, `bar inside the noise`, rather than print a
green it cannot support. With fewer than three runs it exits 1, `three runs needed`, before looking at a single
bar — there is no reading one capture as a stand-in for three, which is exactly what r2-2 did.

`BAR_VALUES` in `probe.py` is empty until three runs exist to set it from. `python probe.py --calibrate` measures
and prints whatever runs ARE present (even one) with no bar check at all, for looking at the numbers while the
rest are being shot.

## The look report (unbarred)

From `cards/`: each card's rise in metres and its relief (stdev of masked luma / its mean), plus the gas card's
ratio to the smoke card on both. `run1`'s numbers: gas rises 0.000 m, smoke 0.000 m (this isolation pose holds the
card essentially at ground level — a flat puddle, exactly the owner's description — so the rise ratio is not a
useful number here, 0/0); gas relief 0.1888 against smoke's 0.1824, ratio 1.0351 — the gas card has about as much
internal luma variation as the smoke card draws, not the flat, undifferentiated green puddle the owner's "flat
green puddle" line was aimed at r2-2's hue-mask verdict, not the pixels themselves. None of this is a pass bar:
the gas card is squashed by `CombatFx.GasBankSquash` 0.55 on purpose, so a look change is still out of scope here.

## Status: not done

`python probe.py` exits 1, `three runs needed` (only `run1` exists). `python probe.py --selftest` and
`python probe.py --calibrate` both exit 0. The next leg shoots `run2` and `run3` from build `f032514063`,
unchanged, drops them in `docs/evidence/ability-fx-captures-r2-2-g2/run2/` and `run3/`, runs `probe.py`, reads off
each bar's three values and spread, and sets `BAR_VALUES` in `probe.py` so the nearest value clears by at least
that spread — then `python probe.py` should exit 0.
