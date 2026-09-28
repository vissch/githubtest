# Juice board

The juice director owns this file. Its brief is `agents/juice-director.md`. A moment is something a player would
screenshot, or would feel in their hands. Each one is scored 0-10 by the critic from a capture of the moment, never
from a description, and a moment closes at 8. `-` means nobody has captured it yet.

Every moment carries a readability check. If the check fails, the change is reverted no matter how good it looks
(README rule 6).

## Moments

| id | moment | trigger and camera | the look it should have (value, hue, motion, size, duration) | readability check | cost class | refs | score | card |
|---|---|---|---|---|---|---|---|---|
| J01 | shell burst at the standard view | HE barrage, scenario=barrage, T1. Shoot `--scenario barrage --shot-tick 140 --shot-frames 16 --no-hud`: the shells fall in window ticks ~82-200 (warm-up 80, spread 120); tick 300 shows only the smoke after (a0022) | the flash reads for 2-3 frames as a hot white core with an orange rim; the earth column leans with the shell's travel; the smoke holds dark for 4-6 s then tears; the ground around it takes the light for the flash | the men within 10 m of the burst stay countable in the frame after the flash | T1 visible: pay for it inside the same commit | - | - | C20 |
| J02 | a tank cooking off | armour scenario, a hull killed in view, T2 | the fireball LASTS (it does since 1ede261: `TankRenderer.Fireballs` re-adds each tongue for 0.9-1.5 s, `TankRenderer.cs:125-128,259,264-276,946-948`; checked cycle 1, so C21's premise is stale), then flames lick from the hatches and a black column rises; hull plates scatter | the wreck's silhouette stays readable against its own smoke | T2 | - | - | C21 |
| J03 | a rifle volley from a trench | stress battle (scenario none), T1. Captured with `aosa.py bench <label> --player --shot-tick 100 --no-hud`: a held frame inside the window, bit-identical run to run (C33, 2c4f3f3; runs 1/c33w-1..3 show volleys in flight). Before C33 every `shot=` was taken paused before the window, so no shot was ever in it | value/hue: each shot at T1 is a white-hot core (luma > 0.9) 2-3 px wide inside its side's halo 6-9 px wide that still reads as hue after bloom (green: G - R > 0.2; red: R - G > 0.2 at the halo's edge); the flare's core luma > 0.9. motion/timing: a flare holds 2-3 frames at 60 fps and its streak crosses in 6-8 frames (0.12 s); the shots of one 50 ms sim tick are spread across that tick instead of all being born on one frame, so a line's fire runs along it as a flicker, not a 20 Hz strobe (no frame lights more than half of a tick's flares). size: at T1 (1920x1080) a flare is 20-30 px, about a helmet (a man draws 55-95 px there, so a flare of a man's height would cover the next man: corrected cycle 1 by the C22 author, measured on runs 1/c33w-1; the flare already scales with the men's grow factor, CombatFx.cs:485,503), a streak 150-300 px; nothing one shot draws covers the next man in the line | the tracer colour still says whose fire it is (green or red at night), and the men along the firing line stay countable through the flares | T1: cost per shot must stay flat (same draw count, only birth times and sizes move) | - | 5 (cycle 2, blind, runs 2/v1-1; 4 before C22) | C22 done; C42, C43, C44, C45 |
| J04 | a walker mounting a parapet | armour scenario, T2 | the hull rises onto the bank, feet find the sandbags, and the body banks into the turn | the machine's team ring stays visible | T2 | - | - | C26, C27 |
| J05 | a leg blown off a walker | armour scenario, T2 | the leg tumbles at the length it was drawn (today it pops to full length), and the body sags toward the hole | - | T2 | - | - | C28 |
| J06 | the star shell | night, any battle, T1 | a slow falling magnesium-white light; shadows swing as it drifts; it drips burning particles | men in its light gain contrast, and do not blow out | T1 | - | - | - |
| J07 | lightning over the field | night storm, T1 | the freeze frame lands on the bolt; the key light swings to the bolt for the flash | the HUD stays legible through the flash | T1 | - | - | - |
| J08 | a landing craft grounding | coast level, T1 then T2 | the bow wave dies, the ramp drops with a splash, and men pour out into the surf | the men leaving the ramp are countable | T1 | - | - | - |
| J09 | gas rolling into a trench | scenario=vfx, T1 | yellow-green clouds pour over the parapet and pool low; men in it are hidden but their silhouettes show | the trench's owner and its pinned state still read on the HUD | T1 | - | - | - |
| J10 | a creeping barrage walking | a creeping HE barrage (scenario=barrage), T1 then zoom 240; knob fx.recipes=1 | each lift lands as a row of bursts a step further on, and the smoke of the last row still stands as the next lands; at 240 the plumes x FarGrow read as a line of dark stalks walking | at T1 the men under the curtain stay countable between lifts; at 240 which way the curtain walks reads from one still | T1 + far | - | - | VFX pass L04 |
| J11 | the beam's sweep | scenario=beam (PerfBench), T1 and zoom 240; fx.recipes=1 | a pillar of drawn fire (FireLance) pours onto the head of the sweep with a bloom at its foot and a burning trail behind; at 240 it grows (x FarGrow, width, height, x2 brightness) and stands as the tallest thing on the field | at 240 the pillar is a vertical read at least as strong as the old 60 m white column | T1 + far | - | - | VFX pass L10 |
| J12 | a jetpack leap into a trench | any battle with a Jetpack trooper in reach of an enemy trench, T2; fx.recipes=1 | a ring of dust at take-off, a jet of fire from his back and a smoke trail over no man's land, a ring and dust thrown either side as he lands, no earth column | the man stays visible through his own smoke and lands readable in the bay | T2 | - | - | VFX pass L15 |
| J13 | a barrage read from the overview | scenario=barrage at zoom 240 and 600; fx.recipes=1 | a flash dot, a faint ground ring and one dark plume standing ~5 s per shell (x FarGrow): the barrage is a row of dark stalks | where the barrage landed reads from one still at 240 | far | - | - | VFX pass L01, IN-5 |
| J14 | a tank knocked out and burning | armour scenario (a later shot tick: the hulls deploy at 1800 and have not fired by 1940), T2; fx.recipes=1 | knocked out: a flash in the hatch and black smoke pouring from it; burning: drawn Stand fire on the deck and a standing WreckSmoke column that marks the wreck from 240 | the wreck's silhouette stays readable against its own smoke | T2 + far | - | - | VFX pass L21, L22 |

## Reference rounds

One row per image request, written by `aosa.py refimg --for juice`.

| cycle | moment | capture | prompt file | refs | usd | critic delta after |
|---|---|---|---|---|---|---|
