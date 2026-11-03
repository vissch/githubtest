# muzzle-flash-scale evidence

Harsh-critic fix (special units group, 2026-10-19): rifle muzzle starbursts read as torso-sized and hide the
shooter. The owner's caps, through the critic: rifle at most 0.4 of the shooter's own drawn height, mg at most 0.6;
the flamer and the AT rifle may be bigger. `probe.py` measures the ratio from pixels, not by eye.

## Capture (held tick, T2, `GreyboxCorridor.unity`, `CaptureRig.Bench`, editor eval; `Tools/tw eval '<code>' <timeout>`)

Per class `<name>` (fx: rifle 31, mg 38, flamer 55.5, at 59), three stills, all before `bench.lineupMen` existed the
empty plate was a different battle (no `scenario=` at all); it is now the same staged lineup minus the men:

```
# idle: the men standing, nobody firing
return TW.Editor.CaptureRig.Bench("stress=0 settle_ticks=60 ticks=40 zoom=16 yaw=21 pitch=25 shot_tick=20 shot_hud=0 quit=0 scenario=lineup fx=<fx> fz=120 knobs=bench.lineupFire=2 shot=<abs>/<name>_idle.png out=<abs>/<name>_idle.json");

# empty: the same staged lineup, men absent (bench.lineupMen=0)
return TW.Editor.CaptureRig.Bench("stress=0 settle_ticks=60 ticks=40 zoom=16 yaw=21 pitch=25 shot_tick=20 shot_hud=0 quit=0 scenario=lineup fx=<fx> fz=120 knobs=bench.lineupFire=2|bench.lineupMen=0 shot=<abs>/<name>_empty.png out=<abs>/<name>_empty.json");

# fire: that class alone firing, 10 held frames, the largest flash kept
return TW.Editor.CaptureRig.Bench("stress=0 settle_ticks=60 ticks=40 zoom=16 yaw=21 pitch=25 shot_tick=20 shot_hud=0 quit=0 scenario=lineup fx=<fx> fz=120 shot_tick=18 shot_frames=10 knobs=bench.lineupOnly=<N> shot=<abs>/<name>_fire.png out=<abs>/<name>_fire.json");
```

`UnityEditor.EditorApplication.isPlaying = false;` between captures (`SimHost` reads the stress preset in `Awake`,
so a second `Bench` call while still in play is refused). All stills resampled to 1280x720 before committing.

## Measure

```
python docs/evidence/muzzle-flash-scale/probe.py            # prints the table, writes probe.json
python docs/evidence/muzzle-flash-scale/probe.py --check    # exits 1 if rifle > 0.4 or mg > 0.6, or a blob is missing
```

`probe.json` is the **current** measurement; `probe-before.json` is a frozen copy of the numbers measured right
after the empty-plate fix (this commit), before any scale change — the fault the critic named, with a number on it.

## Known limitation

The flamer's own fire pool is both the "hot" signal and the ground it stands on, so `man_box` for `flamer` (and to
a lesser extent `at`, which also fires into flame) does not reliably land on the shooter himself — see
`flamer_boxes.png`. Neither class has a cap (the owner: "the flamer and the AT rifle may be bigger"), so this does
not block the rifle/mg fix; a tighter flamer man-box is future work if the flamer ever gets its own cap.
