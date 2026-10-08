# Idle frogs snap moths off the lamps

Item `idea-idle-frogs-snap-moths-off-the-lamps`, stage game-design, 2026-10-08. Show lane only. Sketch: `shown.jpg` (the stage's band `shown`).
All paths below are under `trench-warfare-3d/Assets/_Project/` unless they start with `docs/`.

## 1. The rule
A frog who is standing about with nothing to do, close to a lit lamp, now and then flicks his tongue out, takes one moth out of the air and swallows it. A new moth drifts back to the lamp a few seconds later. You only see it with the camera down among the men. It changes nothing in the fight: no silver, no health, no morale, and the frog drops it the moment anything else asks for his body.

## 2. What the player sees and does
1. Standard view: nothing. The moths are not drawn there (`Shaders/Motes_URP.shader` collapses every card when `_TWClose` is 0) and no snap is ever started.
2. He zooms in on a quiet stretch. Four moths circle each lamp (`Presentation/Terrain/SmallLife.cs`, 4 motes of kind 0 per `NightLights.LampPoints` entry).
3. A frog near a lamp has been idle a while. Where he would have played a soldier's fidget he instead turns his head to the lamp, the tongue goes out and back in a blink, one moth is gone, he gulps.
4. A few seconds later a moth is back on that lamp. The next frog, or the same one a minute later, takes another.
5. The player does nothing: no button, no order, no HUD. If he gives an order or a shot comes in, the frog is already doing that.

## 3. Numbers (all placeholders; the motion and effects stages tune them by eye, nothing here is for the balance sweep)
- **Who**: the figure drawn as a frog. Today that is archetype 25 only (`VATRenderer.FigureOfArchetype`: 25 -> figure "Frog"; every other man on foot is the soldier). placeholder
- **When**: at the moment the existing standing fidget fires: `ReadyIdle`, no target, suppression under 10, idle for 240 + seed % 360 ticks, i.e. 12 to 30 s at the 0.05 s tick (`Presentation/Core/AnimationController.cs`, rung 9). The snap takes that fidget's turn 1 time in 2 when a lamp qualifies. Reasoning: reuses a timer already judged; a frog by a lamp snaps about once a minute, rare enough to be a find. placeholder
- **Reach**: lamp within 2.5 m on the ground. Reasoning: lamps hang 1.25 to 1.55 m up and the moths ring them at 0.35 to 0.85 m (`NightLights.cs` Build, `SmallLife.cs`), so 2.5 m keeps the tongue under about three metres and inside the lamp's 8.5 to 10 m pool of light. placeholder
- **Camera**: only while `SceneHooks.CloseUp` > 0 and the lamp is within 32 m of the camera, where the shader starts fading motes (gone by 45 m). placeholder (32 m is the shader's own number)
- **Clip**: 0.9 s one-shot: turn and open 0.25 s, tongue out and back 0.15 s, gulp 0.5 s. Reasoning: the shortest clip in the table is `FireSnap` at 0.27 s and the fidgets run 1.8 to 5.9 s; this must be over before he is bored of it. placeholder
- **Moth returns** after 8 s; one snap per lamp at a time, so a lamp never shows fewer than 3 of its 4. placeholder

## 4. Edge cases
1. **In a trench**: the garrison kneels and runs `TrenchRoutine`, not the standing fidget. Default: the snap may take the kneeling fidget's turn (routine 5) under the same reach test; trench lamps hang 1.35 m over the floor.
2. **Camera pulls out mid-snap**: the moths vanish with `_TWClose`; the body clip plays out, the tongue is hidden with the motes. No new snap starts at the standard view.
3. **He dies or is hit mid-snap**: idle is the lowest rung of the ladder (death, trench edge, reaction, action, stance, turn, locomotion, fire, idle), so the higher rung takes the body that tick. The tongue must be dropped the same tick, never left hanging off a corpse.
4. **3,000 units**: the lamp test runs only at the fidget moment (once per 12 to 30 s per idle frog), only in close-up, against at most a few dozen lamp points (caps in `NightLights.cs`: 12 lanterns, 12 prop lamps, 14 trench lamps, 5 fires, 6 torches). No per-frame scan over units.
5. **Replay and determinism**: nothing in the sim reads or writes it; the sim hash and the replay are untouched. The pick uses the controller's seed-and-tick hash like the fidgets do, never `Time` or `Random`. Which moth is taken is not replay-stable (moths move on the shader's `_Time`), and that is accepted: show only.
6. **Lamp put out** (`SceneHooks.LampOut`, a rammed or shot post): no snap at a dark lamp. Seen while reading: `SmallLife` builds its motes once and has no reader of `LampOut`, so moths still circle a dead lamp today; out of scope here, noted for effects.
7. **Fires and torches**: `LampPoints` also holds the burning trees and the torches, and they get moths too. Default: the snap works at any `LampPoints` entry; a frog at a burning tree is rare and harmless.
8. **Two frogs, one lamp**: the lamp's one-at-a-time lock (8 s) decides; the second frog plays his ordinary fidget.
9. **Gas**: no snap while the frog has met the gas (`AnimState.Gassed`; `MaskOn` exists): a masked mouth does not open.

## 5. What it needs
- **Sim**: nothing. No rule, no event, no replay version.
- **Show, motion**: one new one-shot clip on the Frog figure (and its kneeling twin if edge case 1 stays), authored for the frog: today the frog's clips are the soldier's poses carried onto its rig (`Editor/VATBaker.cs`, Bake Frog), and no soldier clip has this. A new `Clip` value and a branch in rung 9 of `AnimationController.Decide`.
- **Show, model**: a tongue. A jaw or tongue bone on the frog: not found (no hit for jaw, mouth or tongue in `VATBaker.cs` or the frog's bake path). The flick is about two frames at the bake's 12 to 20 fps (`Editor/InfantryClipTable.cs`), so the tongue is better a separate drawn strip than part of the baked body; the concept decides.
- **Show, effects**: the lamp points and their lit state reachable from the controller (today `NightLights.LampPoints` is public, lit state is private); one moth hidden for 8 s and brought back; the tongue tip meeting the moth, whose position exists only in the vertex shader (`Motes_URP.shader`, kind 0), so either mirror that formula for the one moth or pull the moth to the tip.
- **HUD**: nothing. **Sound**: cut.
- **Seams**: no archetype id, no ability id, no replay version, no file format changes. The Frog figure's VAT bake gains a row; whether the Soldier and Sniper bakes must be redone when `Clip` grows was not verified.

## 6. What it must not break
- **Concepts first** (`docs/reference/goals.md`, hard limits; decision of 2026-10-06): the tongue's look and the clip are not built before the owner has picked from the concept step.
- **Free at the standard view**: `SmallLife.cs` promises its life costs nothing there; the snap keeps that promise (no test, no draw, no clip while `CloseUp` is 0).
- **The frog's far model stays at 300 triangles** (`docs/reference/decisions.md`, 2026-10-06): the far tier is untouched; it plays the clip's fallback row.
- **Same-build determinism** (`goals.md`): the controller's ladder stays hash-driven; nothing from this reaches `SimWorld`.
- **Fighting comes first**: a snap never delays a shot, a dive or a death. It only ever replaces a fidget.

Cut: any stat or buff, sound, a prone snap, the frog machines (Croaker, Hopper, Bullfrog), moths that run out, rats or other prey, a HUD mark.

## 7. Open to the owner
1. **Who snaps?** Only the frog infantryman, the one unit drawn as a frog today (default), or the frog machines too, later, with their own tongue?
2. **Do a lamp's moths run out?** They come back after about 8 s so every lamp stays alive (default), or a bored garrison slowly strips its lamp bare until the fight moves on?
