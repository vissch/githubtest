# C79 blind critic: render.shadowDistance 160 against 220 (the default)

Stills: `c79s-1.png` (knob 160) and `c79t-1.png` (knob 220, bit-identical to no knob). Both show the same battle (hash_start
A15CF08338D0B510, hash_end F0DECBBC9088B60A), WinterLine by day, tick 100 on the held clock, T1, no HUD, release player
at f8dd628. The discriminating still is `c79d-1.png` (knob 40): changed 0.04274, most of it bottom-left (the near
cascade) and along the top trench lines.

## Where 160 and 220 differ (measured before the critic)

- 0.625% of pixels changed at stilldiff's threshold (channel > 8). The bbox is the whole frame, but the changes are
  thin one-to-three-pixel lines along shadow edges: sandbag rows, trench walls, tree trunks and the men.
- There are two dense spots. The first is the right trench's men and the soft shadow "fingers" below them (x
  1680-1880, y 560-620): up to 17% of a 120x90 cell. The second is the near left trench wall (x 280-320, y 680-1080).
- The far edge barely moves. The top 120 rows change by 0.34%, all of it on edges, and no shadow is present at 220 and
  absent at 160: only 388 pixels get brighter by more than 20 levels, against 1904 that get darker.
- On the gradient, 160 is not sharper. The mean Sobel magnitude near the changed pixels is 101.1 at 160, 104.0 at 220
  and 102.8 at 40. It is lower at 160 in every crop: p1 69.7 against 72.5, p2 99.1 against 104.4, p3 106.3 against
  109.2, p4 93.0 against 93.6. Mean luminance is equal (140.5 and 140.5).

## Pairs and key (X/Y shuffled per pair; the critic was not told the setting or the key)

| pair | region (full-frame px) | X | Y |
|---|---|---|---|
| full | whole frame | 160 | 220 |
| p1 | x 1540-1920, y 460-700: the right trench, men's shadows | 220 | 160 |
| p2 | x 160-500, y 640-1080: the near left trench and a lamp | 220 | 160 |
| p3 | x 860-1420, y 0-220: the far edge | 160 | 220 |
| p4 | x 800-1300, y 200-480: the dead trees' shadows | 160 | 220 |

The crops were upscaled 2x with nearest-neighbour.

## Critic output (verbatim)

> I compared all five pairs back and forth, spot by spot, and could not see a single difference in any of them.
>
> **full (whole frame)**
> 1. Sharper edges: no visible difference. I compared the tree shadows in the centre, the soldier shadows on the right trench, the lamp-post shadows on the left and the crater area in the lower middle. High confidence at this scale.
> 2. Missing or cut-off shadows: none in either image. The top band (bunker, far trench, wire) has the same faint, soft shadow in both, and neither has a hard cutoff line.
> 3. Other differences: none. Snow flecks, tracer lines and lamp glows are in the same places.
>
> **p1 (right trench, men's shadows)**
> 1. Sharper edges: no visible difference. The long, soft shadow "fingers" cast by the men below the sandbag line match in width, softness and darkness. High confidence.
> 2. Missing or cut-off shadows: none.
> 3. Other differences: none. Both have the same mottled texture inside the shadow area. The shadows are equally soft and blurry in both, but that holds for both renders, not one.
>
> **p2 (near side, left trench and lamp)**
> 1. Sharper edges: no visible difference. I checked the shadows of the sandbags to the left of the trench, the sack's shadow, the lamp post and the soft blobs in the upper left. High confidence.
> 2. Missing or cut-off shadows: none.
> 3. Other differences: none.
>
> **p3 (far edge)**
> 1. Sharper edges: no visible difference. I checked the shadows under the sandbag wall, the wire trestles at top right and the trunk shadows at left. Medium-high confidence, because shadows here are faint in both.
> 2. Missing or cut-off shadows: shadows at the far edge are soft and low-contrast in both, but equally so. Nothing is missing or cut off in either.
> 3. Other differences: none.
>
> **p4 (dead trees)**
> 1. Sharper edges: no visible difference. The long diagonal shadow of the right tree, the curved shadow arc at centre right and the shadows at the trunk bases match. High confidence.
> 2. Missing or cut-off shadows: none.
> 3. Other differences: no banding, shadow acne or speckle in either.
>
> **Overall verdict: no.** I could not see a difference in any pair, even in the crops enlarged 2x, so a player at 1920x1080 would not notice one.

## Unblinded

The critic did not call 160 sharper in any pair, and it found no shadow lost at the far edge in either render. The
gradient measure agrees: 160 is 2.8% softer, not sharper. The shadows at T1 are soft by design, so their edge is set
by filtering and softness, not by shadow-map texel density. Cutting the distance from 220 to 160 raises the linear texel
density by about 1.4x and changes nothing a player sees. It also saves no GPU time (gpu_ms.p95 +0.025 against a
0.129 band). C80's condition ("the critic calls 160 sharper, and no shadow is lost") fails on its first half, so C80
is closed.
