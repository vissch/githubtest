# UX flow: a deploy card plays its unit as a tiny film

Page: `flow.html`, picture: `shown.jpg`. Drawn on a real capture of the UI Toolkit battle HUD (`hud-capture.jpg`).

**Before.** A card is a still portrait, a name, a cost and its key. Resting the pointer 0.35 s opens one plate above the bar (heading, one line, cost and key).
**During.** The pointer rests on a card; he presses nothing. The rim lights at once; at 0.35 s the portrait becomes a 3 s silent film, looping, inside the card's own portrait window, and today's plate opens with it.
**After.** The pointer leaves: the still portrait is back in the same frame and the plate closes. A click or the card's key deploys as today.

- **First thing seen:** the rim of the card under the pointer, in the same frame (today's hover rim); one beat later the film and the plate together.
- **Where:** in the deploy cards of the bottom bar. No new element, nothing over the field.
- **Inputs, mouse:** enter a card (rim, then film and plate at 0.35 s); rest (film loops); leave or slide to the next card (film stops, still back, next card starts its own beat); left click (deploys, or arms a support card, as today; the film never delays or blocks it).
- **Inputs, keys:** 1 to 0 deploy, F5 F6 F7 and C M V B arm, as today. A key starts no film. The film has no key of its own: it is not a control, but a keyboard-only player never sees it.
- **Tactical pause:** the film plays (the plate already opens on unscaled time).
- **State, empty** (no film for the unit): the card behaves exactly as today. No placeholder, no words.
- **State, loading:** the still stays and the plate opens; the film starts when ready. No spinner, never a black window.
- **State, one / many:** one film, in the card under the pointer; never two at once; a card passed in under 0.35 s shows only its rim.
- **State, refused** (not enough silver, cooling down): the film still plays under the card's existing shade and red cost; the click stays dead as today. Locked card: no film. Match over: no film.
- **State, error:** as empty.
- **Small cards** (armour and support, 72 px against 180 px for infantry): same rule. Whether a unit's job reads at that size is for UI art to test on the capture.
- **Undo:** move the pointer off. Watching spends nothing.
- **Cost in clicks against today:** 0 more. Watching is 0 clicks and 0 keys; deploying stays 1 click or 1 key.

## Not found, not verified
- Films exist on the Drive only for Rifle and Assault ("game-battle", 18 s) and Maw, Pincer, Banner ("game-moves", 16 s), all 960 x 540 wide shots. Not found: a film for MG, Officer, Shield, Engineer, Breaker or any support card; a 3 s cut or a cut framed on the unit.
- Not found: any video played by the game today (no `VideoPlayer` in `Assets/_Project` .cs); a motion or film setting (`GameSettings.cs` has `Interface.Tooltips` only).
- Not verified: that a disabled card (poor, cooling) still receives the pointer-enter that the plate and the film hang on.
- The capture's bar is cut at both screen edges (INFANTRY and BEAM run off); not part of this idea.

## Sources read
- Item file and `TW3D-pipeline/ideas/2026-10-08-a-deploy-card-plays-its-unit-as-a-tiny-film/` (idea.json, 1-card.png); `.claude/skills/tw-ux/SKILL.md`.
- `docs/reference/tasks.md` (Battle HUD, Support fire), `docs/17-ui-art-spec.md` (card_rim, tooltip_plate), `docs/reference/decisions.md` (2026-09-23 bottom-bar positions, 2026-09-27 bar may shrink).
- `UI/HudController.cs` (Wire, Deploy, ToggleArm, CostLine), `UI/HudTooltip.cs`, `UI/HudLayout.cs` (TooltipDelaySeconds 0.35, CardPx 72, InfantryCardPx 180), `UI/HudView.cs` (BindCard states), `UI/HudHotkeys.cs`, `Presentation/Core/KeyMap.cs`, `Presentation/Core/UnitLook.cs` (tips), `UI/Resources/Hud/BattleHud.uss` (.hud-tooltip), `UI/Skin/dustfront.components.uss` (line 146, hover rim).
- `Tools/assetboard/films.py`, `gamefilm.py`; `TW3D-pipeline/assets/film/` (listing, posters, lengths read from the mp4 headers).
- Captures looked at: `docs/reference/critique-round3-hud.png` and `environment-hud.png` (the old five-card HUD, not used); `TW3D-pipeline/task-board/play-2026-10-07/shot-small.jpg` with `capture.json` (hud_toolkit true; used).
- `tw3d-board/lessons.md`: no line on "ux".
