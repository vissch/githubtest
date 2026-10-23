# act-rush-and-drop — the rush and drop under fire, as it is drawn

The sim has moved men in rushes under fire for a while (`SimWorld.Alarm` at `StanceRules.CarefulAlarm` or more:
`RushUpTicks` bent double at `RushSpeed`, then flat in the nearest shell hole for the rest of `RushTicks`;
`Sim/Core/StanceRules.cs`, `Sim/Nav/MovementSystem.cs`). No presentation file read `Alarm`, so the dash was drawn
as an upright jog (`Sprint` -> `Standing`) and the drop as an ordinary slow lie-down. The owner: "the rush and drop
under fire does not read."

## What the pictures are

A stress match, filmed at the game camera at the zoom the game is played at (`CaptureRig.Shot(..., 30f, 21f, 25f,
1600, 900)`), seed 12648430, battlefield seed 1917. The men in frame are really advancing across the open into
machine-gun fire, so `MovementSystem` really gives them the rush beat. Ambient shelling is off, so no stray
shell's own dive can be mistaken for the rush dive.

| File | What it is |
|---|---|
| `after-day.png` | the busiest patch by day (`Biome.Winter`, clock 11:00) |
| `after-night.png` | the same moment at night (`Biome.NightMud`, clock 02:00 — the scene's own look) |
| `after-day-series-drop.png` | four frames of one drop, 1/12 s apart, tiled two by two: the dive into the hole |
| `measured.day.json`, `measured.night.json` | the two numbers below, as the rig measured them |
| `before-day.png`, `before-night.png` | the same two frames with the old drawing (`f972155533^`) |
| `measured.before.day.json`, `measured.before.night.json` | the same two numbers, measured on the old drawing |

## The numbers, measured and not eyeballed

The rig that takes the pictures also counts what they claim, over men who are alarmed, alive and in the open, one
sample a sim tick across 1200 ticks (60 s). Both are asserted, so the rig is a check as well as a camera:

| | Before (`f972155533^`) | After (`90fb1d3039`) | The owner asked for |
|---|---|---|---|
| man-ticks of the rush up drawn in a crouched row | 376 / 499 = 75.4 % | 467 / 499 = **93.6 %** | 70 % or more |
| worst time from the sim's switch to a prone row | **18 ticks** (0.9 s) | **1 tick** (0.05 s) | within 0.5 s (10 ticks) |
| the rig | RED (`-runTests` exit 2) | GREEN | green |

The before pass is the same rig on the same seed, with only
`Presentation/Core/AnimationController.cs` put back to the commit before the change. It fails on the drop: a man
the sim had already dropped stayed upright over his own hole for 18 ticks, held by a stumble (the log's tick-by-tick
trail names it). Its crouched share was already over 70 % because the sim's own `Crouch` ticks were drawn crouched
before; it was the `Sprint` ticks that were drawn upright, and those are the 91 man-ticks the change wins back.

Two honest notes on the pictures. At the zoom the game is played at a man is a few pixels tall, so the before and
after full frames differ in about 1200 of 1.44 million pixels (the men who are mid-rush at that one instant): the
pair shows the same moment, and what the change does is in the counts and in `after-day-series-drop.png`, not in a
single wide frame. And the before pass counted 14 drops where the after pass counts 16 — a drop is counted when the
drawing reaches the prone row, and on the old code two men were put back up by the sim before the drawing ever got
them down.

Day and night are the same match on the same seed, so the two counts are identical by construction; the biome is
only a look.

## How to make them again

```bash
# this checkout's editor closed; WITH a graphics device, not -nographics
Unity.exe -batchmode -runTests -projectPath trench-warfare-3d -testPlatform EditMode \
          -testFilter TW.Tests.RushDropStills -testResults <out.xml> -logFile <out.log>
```

For the before pass, put only `Presentation/Core/AnimationController.cs` back to `f972155533^`, run the same line,
and restore the file afterwards: the run is expected RED, and that red is the before picture.

The rig is `Assets/_Project/Tests/Stills/RushDropStills.cs`. It writes to `%TEMP%/tw-rushdropstills` by default
(`TW_STILLS_DIR` to move it — never inside the checkout, `land.py` sees untracked files); `TW_RUSHDROP_STRESS`,
`TW_RUSHDROP_SETTLE` and `TW_RUSHDROP_FILM` are the men a side, the settle and the counted stretch. The log prints
`rush up crouched: N/M` and `drop to flat: worst K ticks`, and the tick-by-tick trail of the slowest drop, which is
what found both of the holds listed below.

## What the pictures cost in code

`Presentation/Core/AnimationController.cs` now reads `w.Alarm`. An alarmed sprinter wants `Crouch` and takes a
crouched gait (the gait then sets the shown stance); the drop plays `Clip.DiveAway` at 2.0x on `Rung.StanceChange`,
flat in 0.35 s. Two holds the rig found, each of which had kept a man upright over his own hole:

- the dive is gated to once in 20 ticks so a man the sim flickers up and down does not stutter — but he is now put
  flat on the tick either way, instead of falling back to the slow flip hold (that measured 18 ticks);
- a stumble (`Rung.Reaction`, 2.3 s, 1.2 s of it before the rung gate lets anything through) outranked the drop.
  An alarmed man the sim has just dropped breaks the stumble off: he is going down anyway.

The guard in EditMode is `Tests/Show/RushDropTests.cs`.
