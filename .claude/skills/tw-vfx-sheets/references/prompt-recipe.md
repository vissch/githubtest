# The MiniMax H3 prompt that works (reference-driven)

Pure text-to-video gave a generic look the owner rejected. What works: feed H3 a cut reference clip as `<Video 1>`
plus three of its peak frames as `<Picture 1..3>`, anchor black at the first and last frame
(`MiniMaxH3AddGuide`), and describe a NEW subject in the reference's style.

## Template (batch6 `prompt_for`; batch7 is the same with its own style)
```
<Video 1> is a reference animation of a 2D game explosion effect, and <Picture 1>, <Picture 2> and <Picture 3> are
frames from it. Create a new fire effect animation in exactly the same art style as <Video 1>: {STYLE}. Copy the
drawing style, the flame and smoke shapes and the timing of <Video 1>, but the subject is fire. {COLORS} Pure solid
black background at all times. {TIMELINE} {PLACE} The effect stays fully inside the picture. Stepped animation with 12
drawings per second. The background is pure solid black in every frame, edge to edge: no bars, no borders, no
vignette, no floor, no grey. Static camera, no text, no UI, no weapon, no nozzle, no characters, nothing else.
Audio: silence.
```

**STYLE** (batch4): smooth liquid organic hand-drawn shapes, flat cel-shaded colors with exactly three tones (a pale
bright core, a saturated mid tone and a very dark shadow tone), bulbous rounded lobes with hollow round holes inside
them, long curved C-shaped swooshes and streaks, large masses of very dark smoke, a few tiny white spark dots, no
outlines, no gradients, no glow, no photorealism.

**COLORS** e.g. fire: "pale yellow core, bright orange mid tone, dark red-brown shadow". Colour does not survive into
the game (firebooks keeps only value), so choose colours that give good VALUE separation, and choose the reference
by the SHAPE you want.

**TIMELINE** by mode:
- loop: already burning in the very first frame and keeps burning steadily for the whole video without ever going out or changing size: {action}.
- ignite: The picture starts completely black and empty, then {action}.
- die: At the start {action}; the picture ends completely black and empty.
- chain: starts completely black, then {action}, until it is black and empty again.
- oneshot: adds "exactly ONE single event ... Never a second event."

**PLACE** by pivot: JET (L) nozzle at the LEFT EDGE, vertical middle, nothing cut off; BASE (B) sits on an invisible
flat surface at the bottom centre, no floor drawn; CENTER (C).

Explosion variant (batch4): "... exactly ONE single burst happens: {KIND} ... The effect is big and fills about two
thirds of the picture ... Slow motion, about three seconds, stepped animation with 12 drawings per second, then holds
on black."

## Lessons (from the rounds that failed)
- Air bursts: add "floats in empty space, no ground line", or H3 draws a floor.
- Jets: "about HALF of the picture long" keeps the head inside the frame.
- Glows: the NOHALO clause ("no dark halo"), or H3 draws a glow box that keys as a square.
- Smoke must "fade away by the middle height", or it hits the top edge.
- Painted skies cannot be subtracted: the background must be pure black.
- The drawing must never touch the frame: "no edge from the bounding box of the png apparent" (the owner's rule), so edgecheck + wallfix always run.
