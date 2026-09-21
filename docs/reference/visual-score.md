# Visual score: environment against the reference

Scored from one render of the **standard view** (25 degree lens, 25 degree pitch, zoom 30, start focus) with about 16
men deployed, compared with `battlefield-northstar.jpeg` and the owner's two "Trench Tactics" targets.
Each line is 0-10: 0 = nothing of the target, 5 = recognisably the same idea, 8 = a viewer would call it the same
style, 10 = indistinguishable in a side-by-side. Scores are one reviewer's judgement; the trend matters, not the digit.

| # | Criterion | What the target shows |
|---|---|---|
| 1 | Terrain form | rolling mounds, raised trench lips, shell holes as real bowls with a thrown-up rim, merged craters |
| 2 | Ground paint | umber / peat / olive palette, dry ridges, wet dark mud, silt basins, ruts and foot tracks |
| 3 | Water | dark muddy puddles with crisp cold sky reflection, soft submerged edges, round organic shapes |
| 4 | Trench kit | irregular plank revetment with pickets, sagging squarish sandbags in a bond, broken duckboards |
| 5 | Props and clutter | splintered trees, fallen timber, knife rests and pickets, dugouts cut into the wall, crates, tools |
| 6 | Line and shading | dark ink outline on every silhouette, flat two-step light, occlusion feel under bags and boards |
| 7 | Atmosphere | pale haze to the horizon, mist in the hollows, sepia midtones with cold highlights |
| 8 | Readability | men, trench lines and order buttons read at a glance; nothing pretty hides the game |

Rules for the loop: one focused change per round, aimed at the lowest line that code can move. A round that lowers
the total, or lowers line 8 at all, is reverted. Presentation code only; nothing under `Assets/_Project/Sim`.

## Log

| Round | Commit | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | Total /80 | Change made this round | Next target |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 0 | 2cc6a77 | 6 | 5 | 6 | 6 | 5 | 6 | 6 | 6 | 46 | baseline after the landscape pass (mounds, crater lips, dark water, mist, grade) | palette still reads ochre; dark dips too heavy |
| 1 | (this commit) | 6 | 6 | 6 | 6 | 5 | 6 | 6 | 6 | 47 | palette pulled from ochre to grey-umber, lighter dips, grade less saturated | scene is dark overall and men are hard to pick out in the shaded trench: lift exposure on men and trench floor (line 8), then clutter (line 5) |
| 2 | (this commit) | 6 | 6 | 6 | 6 | 5 | 6 | 6 | 7 | 48 | men lit a step above the field, trench floor lighter: the garrison now reads at a glance | props and clutter (line 5): loose debris across open ground (branches, planks, shell cases) through BattlefieldComposer / BattlefieldKit |
| 3 | (this commit) | 6 | 6 | 6 | 6 | 6 | 6 | 6 | 7 | 49 | loose litter on open ground (broken branches, boards, spent shell cases near the trenches), hash-placed, no shadows | trench kit (line 4): sandbags still read as a bead of pebbles; make them squarish, sagging, in two bonded courses |
| 4 | (this commit) | 6 | 6 | 6 | 7 | 6 | 6 | 6 | 7 | 50 | sandbags are squarish sagging sacks with pinched ends, a squashed lower course and a running-bond upper course | ground paint (line 2): cart ruts and trampled tracks running along the trenches and from the dugouts, painted into the ground texture |
| 5 | (this commit) | 6 | 7 | 6 | 7 | 6 | 6 | 6 | 7 | 51 | cart ruts with squeezed-up ridges and a beaten path along every trench, broken up by noise | line and shading (line 6): painted contact occlusion, a warm dark band on the ground under the sandbag courses and around trunks, stumps and dugouts |
| 6 | (this commit) | 6 | 7 | 6 | 7 | 6 | 6 | 6 | 7 | 51 | painted contact occlusion under sandbag courses and around solid props; honest result: barely visible from the standard view, no score change, kept because it is free and harmless | atmosphere (line 7): the ground mist does not show in the standard view at all; bring it in closer and higher so far hollows and the second trench line sit in pale mist |
| 7 | (this commit) | 6 | 7 | 6 | 7 | 6 | 6 | 7 | 7 | 52 | ground mist brought closer and higher: the far field and second trench line sit in pale mist with the dead trees standing out of it; foreground stays clear. Watch: the front trench is slightly washed, do not push the mist further | water (line 3): puddles have a hard ink outline; give them a soaked dark margin that fades out, and a thin pale shoreline highlight on the far edge |
| 8 | (this commit) | 7 | 7 | 6 | 8 | 6 | 6 | 7 | 7 | 54 | owner request: trenches are no longer ruled lines. Fire bays set forward and back, joined by traverses (a sim-side generator change, gated: 57/57, 3/3, connectivity over 40 seeds holds); kit, ruts and paint follow the bends | water (line 3), carried over: soaked dark margin that fades out and a thin pale shoreline instead of the hard ink outline |
| 9 | (this commit) | 7 | 7 | 7 | 8 | 6 | 6 | 7 | 7 | 55 | puddles lose the hard ink outline: a soaked dark margin that fades into the mud, slightly glossy close in, and a thin pale shoreline | props (line 5): dead trees are clean stakes; give trunk tops a jagged splintered crown (several uneven spikes) and a split down the trunk |
| 10 | (this commit) | 7 | 7 | 7 | 8 | 6 | 6 | 7 | 7 | 55 | splintered crowns on trunk tops (uneven spikes around a torn hollow); honest result: the tops are about 25 cm across, so it does not read from the standard view; no score change, kept as harmless | loop ended. Best next moves: thicker, broken-off trunks with real silhouettes from the artist (line 5), and a screen-space ink line for creases inside shapes (line 6), which inverted hulls cannot draw |
| 11 | (this commit) | 7 | 7 | 7 | 8 | 6 | 8 | 7 | 7 | 57 | screen-space ink pass (`Shaders/InkLines_URP.shader`, Full Screen Pass feature on TW-Renderer, installed by TW/Look/Install Ink Lines): creases inside shapes from the second difference of depth, 5 depth taps a pixel, no normals prepass; fades out by 220 m | a straight ink line shows along the map edge (top left of the standard view) where the ground meets the land beyond it; then props (line 5) needs real models |
| 12 | (this commit) | 7 | 7 | 7 | 8 | 6 | 8 | 7 | 7 | 57 | artefact fix, no score change: the land beyond the map copied each trench's cross-section outward for 30 m (a sunken road with two ink lines); it now takes the bank height, so the cut closes in one short ramp. The skirt also joins the ground at its half-metre spacing | props (line 5): trees are thin black stakes; thicker, paler, more varied trunks with heavy broken limbs |
