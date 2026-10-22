# ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g1-g2: raise the puff further, and move the height floor off this build's spread

**Part 1 of 2 (this commit): the folder is stood up from `-g1-g2-g1-g1` unchanged, no `CombatFx` edit, no fresh
captures yet.** `python probe.py` clears, exit 0 - the carried `run1`-`run3` are still captures of the current
build, because nothing has been changed yet.

## The owner's complaint

> The height floor 0.08 clears 0.0887, 0.0913 and 0.0967 by 0.0087 against a spread of 0.0080, so one shorter
> capture fails it. Raise the puff until a player sees a standing cloud instead of flat pads, and put the height
> floor further outside this build's spread. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g1`'s tip. Bar outside the spread of three captures of
> the current build.

He is right: the floor's margin (0.0087) is barely over the three captures' own spread (0.0080), so the bar does
not hold the look - a slightly shorter capture of the same build would fail it.

## What is here now, and what part 2 adds

| | state |
|---|---|
| `probe.py`, `probe.json` | copied from `-g1-g2-g1-g1` unchanged; `green_height_floor` still **0.08** |
| `run1`/`run2`/`run3` | **carried** from `-g1-g2-g1-g1`: captures of the current build (`GasPuffStand` 0.42, `step == 1` anchor `Ground - 0.6f`), so every bar still clears |
| `cards1`/`cards2`/`cards3` | **carried**. `GasBankCards.Drawer` draws `cards-gaspuff` at the full `CardWidth`, never at `CardWidth * GasPuffStand`, so a field change moves no card pixel. The three `CARD_BARS` are carried as-is |

**Part 2** raises the field draw (`CombatFx.Banks.cs`, `step == 1`: `GasPuffStand` up and the anchor sunk
together - raising the stand alone trades `green_low_share` away one-for-one), reshoots `run1`-`run3` as three
fresh captures of that build at `TW_GAS_WAIT_FRAMES` 200/215/230, and raises `green_height_floor` to a value the
new spread clears by at least twice itself.

## The capture recipe

Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two
batch passes into the SAME directory at the SAME wait (gas, then the same line again with `TW_GAS_PLATE=1`), one
run at a time (two instances on one project conflict and both exit with STATUS_ACCESS_VIOLATION):

```bash
TW_STILLS_DIR=%TEMP%/tw-gasbank-f1 TW_GAS_WAIT_FRAMES=200 \
python Tools/pipeline/run_detached.py start gas-f1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f1.xml -logFile <out>/stills-f1.log
# then the same line again with TW_GAS_PLATE=1
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g1-g2/probe.py --pack %TEMP%/tw-gasbank-f1 --into run1
```

Runs 2 and 3 are the same at `TW_GAS_WAIT_FRAMES` 215 and 230, into `run2` and `run3`. The card sets are not
reshot here; see `-g1-g2-g1`'s README for their recipe.
