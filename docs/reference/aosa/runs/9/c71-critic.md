# C71 blind critic: J03 shot births per line, runs 9/c71f-1 (32 held frames)

Evidence: `c71f-1.png` (frame 0) and `c71f-1.f1..f31.png`, T1 standard view, night, ShelledForest, no HUD, held clock
1/64 s a frame (0.5 s, ~10 sim ticks of 50 ms), window tick 1900. Checked before judging: tracers visible in every
frame checked (0, 1, 20), consecutive frames differ by 8-13% of pixels (the clock advances). The critic was told only
what the frames show and what to count, not the card or its hypothesis. Its answer, verbatim:

---

I counted 136 new shot births across frames 1–31, over five firing lines. Treat the totals as fairly reliable and the per-frame values as ±1 in most cells, up to about ±3 in the volley frames 26–28.

**Firing lines** (green tracers fly right or up-right; red tracers fly left):
- **LTU – left trench, upper section, green.** The trench runs from about (0,150) to (600,1080). Tracers from this part start at about x 250–440, y 180–520 and fly almost horizontally right, towards the right-hand trench and the top trench.
- **LTL – left trench, lower section, green.** Tracers start just right of the trench edge, on a diagonal from about (520,630) to (760,1010), and fly up-right.
- **BOT – off-screen shooters below the bottom edge, green.** Tracers enter at y=1080 between x 760 and 1300 and fly up-right, towards about (1460,280) and (1720,565).
- **RT – enemy trench, right, red.** Shooters are at about x 1150–1350, y 270–400, around the man with the box-shaped helmet at about (1320,270). They fire left.
- **TOP – enemy trench, top-centre section, red.** Shooters are at about x 1040–1120, y 110–200. They fire left.

No muzzle flashes were visible for LTU, LTL, BOT or RT, so those counts come from tracers only. The only flashes I saw were two in TOP at frame 20, at (1075,196) and (1115,118). The bright light at (1266,292) looks like a flash but is a static lamp: it glows in all 32 frames and dims slowly, like the other lamps at (1000,85) and (36,295). I did not count it.

**Births per frame.** Frame 0 has no previous frame, so its row is the number of distinct tracers visible in it, not births. It is excluded from the first totals row.

| frame | LTU | LTL | BOT | RT | TOP |
|---|---|---|---|---|---|
| 0 (visible) | 14 | 7 | 3 | 14 | 0 |
| 1 | 4 | 0 | 0 | 0 | 0 |
| 2 | 4 | 0 | 0 | 0 | 0 |
| 3 | 2 | 0 | 1 | 2 | 0 |
| 4 | 4 | 0 | 1 | 0 | 0 |
| 5 | 1 | 1 | 3 | 0 | 0 |
| 6 | 1 | 0 | 1 | 2 | 0 |
| 7 | 0 | 4 | 1 | 2 | 0 |
| 8 | 0 | 2 | 0 | 0 | 0 |
| 9 | 2 | 3 | 0 | 0 | 0 |
| 10 | 0 | 4 | 3 | 2 | 0 |
| 11 | 1 | 1 | 0 | 1 | 0 |
| 12 | 0 | 3 | 0 | 1 | 0 |
| 13 | 1 | 0 | 2 | 0 | 0 |
| 14 | 1 | 0 | 0 | 1 | 0 |
| 15 | 2 | 0 | 1 | 3 | 0 |
| 16 | 0 | 0 | 0 | 0 | 0 |
| 17 | 0 | 0 | 1 | 0 | 0 |
| 18 | 1 | 0 | 2 | 1 | 0 |
| 19 | 0 | 0 | 0 | 1 | 0 |
| 20 | 1 | 0 | 1 | 1 | 2 |
| 21 | 0 | 2 | 0 | 1 | 0 |
| 22 | 0 | 2 | 2 | 1 | 0 |
| 23 | 1 | 0 | 0 | 1 | 0 |
| 24 | 0 | 4 | 0 | 0 | 0 |
| 25 | 1 | 0 | 0 | 1 | 0 |
| 26 | 7 | 2 | 0 | 2 | 0 |
| 27 | 7 | 5 | 2 | 3 | 0 |
| 28 | 4 | 1 | 0 | 0 | 0 |
| 29 | 2 | 1 | 0 | 0 | 1 |
| 30 | 2 | 0 | 0 | 0 | 0 |
| 31 | 1 | 0 | 1 | 0 | 0 |
| **Total, frames 1–31** | **50** | **35** | **22** | **26** | **3** (all lines: 136) |
| **Total, frames 0–31** | **64** | **42** | **25** | **40** | **3** (all lines: 174) |

**Confidence**
- **LTU: low to medium.** Shots come as bundles of nearly parallel horizontal tracers 5–20 px apart, and in frames 26–28 volleys of 7 or more overlap. A new tracer stays anchored at its start point and grows for 2–3 frames, and its line can drift 3–5 px while it grows, which makes "grew" and "new" hard to tell apart.
  - Frames 1–6 checked by eye (4, 4, 2, 3–4, 0–1, 1) agree with the table to within 1.
  - An independent count at a vertical line x=500 gives 48 against 50.
  - Volley frames could be off by 2–3.
- **LTL: medium.** Frames 5–12 and 27–28 checked by eye agree well. Some tracers first appear as a faint coloured smear with no bright core. I counted those in the smear frame, so an adjacent-frame shift of ±1 is possible.
- **BOT: medium-high for the total, ±1 frame per birth.** Independent counts at y = 1050, 1062 and 1072 give 22, 21 and 21. The shooters are off-screen, so a birth is the tracer entering the bottom edge.
- **RT: low to medium.** Tracer starts are partly hidden by a tree trunk (x 1255–1290), a reddish stump (x 1105–1165) and the pillbox. Hiding splits one tracer into pieces, which I merged by hand in frames 7, 10, 11, 25 and 26. The lamp sits on the machine-gunner's position. Checks at different vertical lines disagree (18 to 34), so the true total could plausibly be anywhere from about 20 to 30.
- **TOP: high.** Three isolated shots, two of them with visible flashes. A faint yellow wisp near (1055,197) in frames 26–28 is ambiguous, and I did not count it.
- **Frame 0 row: low.** It counts every tracer in flight. That includes a bundle of about 6–7 red tracers at x 600–1000, y 580–800 whose origin never shows a red spawn later in the clip. I assigned them to RT anyway.
- **Overall: medium-low.** The totals are more trustworthy than the individual cells.

**Method.** I used Python (numpy, scipy, OpenCV) plus a lot of visual checking.
1. Separated green and red streak glow by colour, and each streak's bright white centre line (its core) by brightness. Masked out the four static lamps.
2. For each frame, found core pixels that were not present in the previous frame (2 px tolerance) and grouped them into straight-line pieces.
3. Counted a piece as a new shot only if the previous frame had no core on the same line (within 2.5 px) behind it; otherwise it was an existing tracer that grew or moved.
4. Assigned each birth to a line by where it starts. Where a shot first appeared only as a coloured smear, I moved the birth back to that frame.
5. Cross-checked totals by counting tracers crossing fixed lines on screen, and checked many frames by eye with overlays that colour each core as new or already present.

Scratch files are in `C:/Users/THOMAS~1.VIS/AppData/Local/Temp/claude/c--Users-thomas-visscher-magi-Documents-claude/491d7320-f770-4699-b1c1-c7866190f53e/scratchpad/critic/`; nothing in the repo was changed.

---

## C62 from this table (computed by the C71/C62 author, frames 1-31)

| line | births | max in one frame | frame(s) | max single-frame share |
|---|---|---|---|---|
| LTU | 50 | 7 | 26, 27 | 0.14 |
| LTL | 35 | 5 | 27 | 0.14 |
| BOT | 22 | 3 | 5, 10 | 0.14 |
| RT | 26 | 3 | 15, 27 | 0.12 |
| TOP | 3 | 2 | 20 | 0.67 (3 births: not a measurement) |

No line with a meaningful count (>= ~10 births) is above 0.35, so C63 is not built by its own gate. Caveat: the 0.35
target came from cycle 2's 8-frame window (~2.5 ticks); over 32 frames (~10 ticks) the same per-tick strobe is diluted
about fourfold, so this share no longer tests J03's "no frame lights more than half of a tick's flares". Sliding
3-frame (~one tick) windows do show bunching (LTL 4 of 4 on frame 24, LTU 7 of 8 in frames 24-26), but tick
boundaries are unknown and the critic puts births at +-1 frame, so that is not a measurement either. The per-tick share
needs C72's spawn log (frame and tick per shot).
