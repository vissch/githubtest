# chlorine-bank-visual-comps — the owner's visuals: patches or a rolling cloud

The owner, on the Decide page, to "The chlorine bank at night: flat green patches, or a new sheet for a rolling
cloud": *"i need visuals to decide, give me visuals"*. This folder is those visuals. **Nothing about the look is
decided here** — it is a comparison for him to judge, not a landed change.

## The two pictures

`comp-t1.jpg`, the zoom-30 night frame shot twice off the same tick, the same pose, the same gas call:

| | `bank_area_frac` | what it is |
|---|---|---|
| (a) flat green patches — today | **0.0324** | the build as it stands (`be5e6444fc`): GasBank's own drawing, squashed flat, anchored on the ground. |
| (b) a rolling cloud — stand-in sheet | **0.3285** | a capture-only switch (`CombatFx.GasBankCloud`) that draws the **SmokeBank sheet** over the same gas field, as a stand-in for a rolling-cloud look. |

(a)'s number is `ability-fx-captures-r2-2-g1`'s own frozen measurement of this build, inside its bar
(0.0325 ± 0.004). (b) gets **no bar**: it is a visual for the owner, and nothing about it is decided, so it is
reported but not judged.

## What the stand-in is, and is not

Option (b) needs art that does not exist yet. Rather than show nothing, this picture is drawn with the
**SmokeBank sheet already in the game** (`FlipbookFx.Sheets[SmokeBank]`) standing in for a rolling-cloud sheet:
its ink fills its whole cell (`Ink` 0.03–0.87) where GasBank's fills only the bottom third (`Ink` 0.00–0.35) —
which is exactly the shape difference the owner is choosing between, flat patches lying on the mud versus a cloud
that fills the air above them.

**A yes to (b) buys a new sheet, not this one.** The real cost: a new 8x4 flipbook sheet drawn to spec and run
through `Tools/firebooks.py` from the owner's art pack, a new `Book`/`Sheets` row in `FlipbookFx` for it, and a
`FlipbookOrdinalTests` case so its ordinal does not collide with an existing book. `CombatFx.GasBankCloud` already
exists as the switch that would turn it on — it is capture-only today (`KnobsTests.
TheGasBankCloudVariantIsOffByDefault` guards it off by default) and draws SmokeBank until a real sheet lands.

## The build stays unchanged

`CombatFx.GasBankCloud` defaults to `false` and the harness turns it back off after every capture
(`GasBankStills.Shoot`, "capture only: never leave it on"). No player ever sees the stand-in; it exists only to
answer the owner's question.

## The measure

`comps.py` reuses `ability-fx-captures-r2-2-g1/probe.py` unchanged — the plate-difference mask, the 40-lift bar
and the camera maths are already proven there, so this folder only packs its own stills and composes the comp.

```bash
python docs/evidence/chlorine-bank-visual-comps/comps.py --selftest
python docs/evidence/chlorine-bank-visual-comps/comps.py --pack <gasdir> --plate <platedir>
python docs/evidence/chlorine-bank-visual-comps/comps.py
```

The gas dir holds `gas-t1.png` (patches) and `gas-t1-cloud.png` (the stand-in), shot by the same
`GasBankStills.GasBankPoolsOnTheGroundAtBothBands` run (the `chlorine-bank-visual-comps` block shoots both off one
gas call); the plate dir holds `gas-t1-plate.png` from the same harness with `TW_GAS_PLATE=1`.

Exit 0 means: both stills and the plate are present; `pose_error_m < 0.50` for both (a still from the wrong pose
is worthless); patches `bank_area_frac` lands inside this build's own frozen measurement; cloud `bank_area_frac`
clears 0.005 (it drew at all); and the two stills differ on at least 1% of the frame, so the comparison is not two
copies of one picture.

## Next, if the owner picks (b)

A new sheet is the owner's art pack plus `Tools/firebooks.py`, a `Book`/`Sheets` row, and
`FlipbookOrdinalTests` — out of scope here, left for a lane of its own once he answers.
