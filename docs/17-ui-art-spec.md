# UI art spec: the Dust Front skin

Generated from `Assets/_Project/UI/Skin/SkinSpec.cs` by `Tools/gen_artspec.py`; edit the table there, not here. `SkinAssetTests` fails if this file stops naming a file the table knows.

## What the skin is

Dust Front's interface: near-black gunmetal plates with very subtle rivets, 1 px light-grey bevel lines, recessed darker windows for numbers, hex bolts, ribbed hoses, `CAUTION` micro-text along plate edges and a faint film grain over the whole screen. Type is condensed stencil caps; body text is bone (#C8C9C6), readouts amber (#E0762A), danger red (#E02B2B), power pale blue (#7FB4E0), totals white. The palette lives in `dustfront.tokens.uss`; the sprites below are painted in those colours. No rounded corners anywhere: the house corner is a 4 px diagonal cut, drawn into the PNG.

## Rules that let a file be swapped without a code change

1. Paint to the exact size in the table. The 9-slice border (left, bottom, right, top, in px) is the part that must not stretch: rivets and bevels go inside it, the centre stays flat.
2. Plates and elements are painted in final colour and are never tinted by the interface.
3. Icons are WHITE glyphs on a transparent background with a 1 px near-black contour (#0A0B0C), a 4 px safe margin and strokes no thinner than 3 px at 64 px. The interface colours them (bone, amber, red, grey) by tint, so one file serves every state. Keep icons uncoloured: white body, near-black contour, grey only in the anti-aliasing; any hue shows up as a tint error in the verifier.
4. Cut corners and all transparency live in the PNG. Alpha is straight (not premultiplied).
5. Drop the file over the placeholder with the same name in the same folder under `Assets/_Project/UI/Skin/`. Do not touch `.meta` files: import settings and borders are applied automatically from the table.
6. Nothing else. The generator sees the hash changed, records the file as yours, and never overwrites it. `TW/UI/Verify Skin` lists what is still a placeholder.

Import settings (applied by `UiSkinImport`, checked by the verifier): Sprite (2D and UI), Single, Full Rect mesh, 100 pixels per unit, sRGB, alpha is transparency, no mipmaps, not readable, clamp, bilinear, uncompressed RGBA32.

## Plates (23 files, `Assets/_Project/UI/Skin/Sprites/`)

| File | Size | Border L,B,R,T | Notes |
|---|---|---|---|
| `plate_normal.png` | 64x64 | 16,16,16,16 | base button/panel plate: cut corners, 2 px dark edge, 1 px light bevel top-left, corner rivets |
| `plate_hover.png` | 64x64 | 16,16,16,16 | plate_normal +8% value, 1 px amber inner line at 40% |
| `plate_pressed.png` | 64x64 | 16,16,16,16 | plate_normal -10% value, bevel inverted |
| `plate_disabled.png` | 64x64 | 16,16,16,16 | plate-800 fill, plate-400 rivets, no light bevel |
| `btn_accent_normal.png` | 64x64 | 16,16,16,16 | amber-lit plate, brass rivets, brass inner line (primary buttons) |
| `btn_accent_hover.png` | 64x64 | 16,16,16,16 | btn_accent_normal, brighter |
| `btn_accent_pressed.png` | 64x64 | 16,16,16,16 | btn_accent_normal, darker, bevel inverted |
| `panel_bg.png` | 128x128 | 24,24,24,24 | deeper plate for menu panels, larger corner bolts, 1 px rust inner line |
| `order_plate_normal.png` | 64x64 | 16,16,16,16 | square trench-order button plate, heavier bevel than plate_normal |
| `order_plate_hover.png` | 64x64 | 16,16,16,16 | order_plate_normal, hover |
| `order_plate_pressed.png` | 64x64 | 16,16,16,16 | order_plate_normal, pressed |
| `order_plate_disabled.png` | 64x64 | 16,16,16,16 | order_plate_normal, disabled |
| `gauge_window.png` | 32x32 | 6,6,6,6 | recessed dark window the amber digits sit in (silver, men, timer, hotkey badge) |
| `badge.png` | 32x32 | 6,6,6,6 | small hard-cornered plate: counts and hotkeys on cards |
| `card_nameplate.png` | 64x20 | 6,0,6,0 | dark strap across a card's foot for the unit name |
| `keycap.png` | 40x40 | 10,10,10,10 | key-rebind cap: plate-500 top face, 3 px darker lip in the bottom rows |
| `row_plate.png` | 64x40 | 12,8,12,8 | settings / debrief row |
| `tab_plate.png` | 64x40 | 12,8,12,8 | list / mission tab |
| `tab_plate_selected.png` | 64x40 | 12,8,12,8 | tab_plate with an amber top edge |
| `tooltip_plate.png` | 48x48 | 12,12,12,12 | plate-800 with a 1 px rust edge |
| `title_plate.png` | 256x96 | 64,24,64,24 | screen heading strap, two bolts per end; stretches horizontally |
| `banner_ribbon.png` | 512x128 | 96,0,96,0 | centre banner: angled torn steel ends, rust lines top and bottom, flat centre |
| `hazard_strip.png` | 64x8 | 8,0,8,0 | CAUTION micro-text strap along plate edges (tiled along the stretch axis) |

## Elements (23 files, `Assets/_Project/UI/Skin/Sprites/`)

| File | Size | Border L,B,R,T | Notes |
|---|---|---|---|
| `card_frame.png` | 80x80 | 14,14,14,14 | card ring, transparent centre: 10 px plate ring, 2 px dark inner edge, 4 px cut corners |
| `card_rim.png` | 80x80 | 14,14,14,14 | WHITE 3 px ring at the frame's inner edge + 2 px glow; tinted for selected / armed / hover |
| `cooldown_mask.png` | 32x32 | - | rgba(8,9,10,.62) with a 1 px 45-degree hatch every 6 px; seamless (tiled along the stretch axis) |
| `divider_v.png` | 8x64 | 0,8,0,8 | vertical rivet strap, rust edges (tiled along the stretch axis) |
| `divider_h.png` | 64x8 | 8,0,8,0 | horizontal rivet strap (tiled along the stretch axis) |
| `minimap_frame.png` | 96x96 | 24,24,24,24 | heavy 24 px square bezel, corner bolts only, transparent centre |
| `bezel_round.png` | 128x128 | - | round radar bezel, transparent centre (scale-to-fit; the Dust Front option) |
| `order_round_normal.png` | 64x64 | - | round riveted order plate (scale-to-fit alternative to order_plate_*) |
| `order_round_hover.png` | 64x64 | - | order_round_normal, hover |
| `order_round_pressed.png` | 64x64 | - | order_round_normal, pressed |
| `order_round_disabled.png` | 64x64 | - | order_round_normal, disabled |
| `slider_track.png` | 32x12 | 6,0,6,0 | dark groove, 1 px light bottom edge |
| `slider_fill.png` | 32x12 | 6,0,6,0 | amber fill with darker 1 px edges |
| `slider_knob.png` | 24x24 | - | hexagonal knob with a centre rivet |
| `slider_knob_hover.png` | 24x24 | - | slider_knob with a brass cap |
| `checkbox_off.png` | 24x24 | - | 24 px plate |
| `checkbox_on.png` | 24x24 | - | checkbox_off with an amber X stamp (two 3 px diagonals) |
| `dim_vignette.png` | 256x256 | 64,64,64,64 | #08090A, alpha .55 centre to .85 edge: the screen dim behind menus |
| `grain_overlay.png` | 256x256 | - | film grain, alpha .06: the whole-screen overlay (tiled along the stretch axis) |
| `scroller_track.png` | 12x32 | 0,6,0,6 | vertical scroll track |
| `scroller_knob.png` | 12x32 | 0,6,0,6 | vertical scroll knob |
| `stamp_victory.png` | 128x128 | - | WHITE rubber-stamp glyph, tinted accent (debrief) |
| `stamp_defeat.png` | 128x128 | - | WHITE rubber-stamp glyph, tinted alarm (debrief) |

## Icons (26 files, `Assets/_Project/UI/Skin/Icons/`)

| File | Size | Border L,B,R,T | Notes |
|---|---|---|---|
| `ico_fallback.png` | 64x64 | - | double chevron left (BattleHud chevrons, mirrored) |
| `ico_overthetop.png` | 64x64 | - | double chevron right |
| `ico_lock_closed.png` | 64x64 | - | padlock, shackle down |
| `ico_lock_open.png` | 64x64 | - | padlock, shackle up |
| `ico_fireatwill.png` | 64x64 | - | crosshair: ring, four ticks, centre dot |
| `ico_holdfire.png` | 64x64 | - | crosshair with a 5 px diagonal bar (the bar is part of the white glyph) |
| `ico_barrage.png` | 64x64 | - | shell over three burst chevrons |
| `ico_gas.png` | 64x64 | - | cloud over three drops |
| `ico_pause.png` | 64x64 | - | two bars |
| `ico_play.png` | 64x64 | - | triangle |
| `ico_speed.png` | 64x64 | - | two triangles |
| `ico_silver.png` | 64x64 | - | hexagonal ingot |
| `ico_men.png` | 64x64 | - | helmet |
| `ico_clock.png` | 64x64 | - | clock face |
| `ico_settings.png` | 64x64 | - | gear |
| `ico_close.png` | 64x64 | - | X |
| `ico_check.png` | 64x64 | - | tick |
| `ico_key.png` | 64x64 | - | key |
| `ico_bullet.png` | 64x64 | - | objective bullet: disc with a ring |
| `ico_missing.png` | 64x64 | - | a question mark: what a card shows when its portrait is absent |
| `ico_arrow_down.png` | 32x32 | - | dropdown arrow |
| `ico_arrow_left.png` | 32x32 | - | left arrow |
| `ico_arrow_right.png` | 32x32 | - | right arrow |
| `ico_rifle.png` | 32x32 | - | weapon badge for the Rifleman card |
| `ico_smg.png` | 32x32 | - | weapon badge for the Assault card |
| `ico_mg.png` | 32x32 | - | weapon badge for the MG card |

## Portraits (14 files, `Assets/_Project/UI/Skin/Portraits/`)

256x256 RGBA PNG, transparent background, no frame (the card draws its own). One per unit archetype plus the two support emblems, keyed by what the unit IS, never by its slot:

- `Rifleman.png`
- `Assault.png`
- `MG.png`
- `Sniper.png`
- `Maw.png`
- `Tusk.png`
- `Pincer.png`
- `Kettle.png`
- `Censer.png`
- `Pavise.png`
- `Banner.png`
- `Redoubt.png`
- `HeBarrage.png`
- `ChlorineGas.png`
- `Cutter.png` is reserved: the model exists but no archetype fields it yet.

Framing, so the set reads as one: machine turned 55 degrees so its nose points to the viewer's front-right (the enemy is screen-right everywhere in the HUD); camera 18 degrees above horizontal with a long lens (22 degree vertical field); the model fills 86% of the frame's larger extent, centred on its bounds. Infantry: a bust from mid-chest, body turned 30 degrees, face toward frame right; the Rifleman, Assault and MG share one figure and differ by a small weapon badge the card adds. Light: warm key upper-left-front, cool rim from behind-right, dim cool fill from below-front. No ground, no shadow disc, no team colour (the card's rim carries the team). Support emblems are painted, not rendered: a shell over a crater burst (HeBarrage), a drum with a drifting cloud (ChlorineGas), in plate colours with an amber or gas-green accent.

`TW/UI/Bake Unit Portraits` renders placeholders from the game's own models with exactly this framing; a painted portrait replaces one by taking its file name.

## Fonts (`Assets/_Project/UI/Skin/Fonts/`)

Three TextCore font assets, referenced by name from the style sheets:

- `DustFrontDisplay.asset`
- `DustFrontLabel.asset`
- `DustFrontMono.asset`

Placeholders are baked from faces the Unity install ships with their licences alongside: Inter SemiBold (OFL 1.1) for Display and Label, Roboto Mono Bold (Apache 2.0) for Mono, so the silver counter has tabular digits. The intended final faces (all OFL, Google Fonts; the owner picks): Saira Stencil One for Display, Big Shoulders Stencil for Label, Barlow Condensed or Saira Condensed where stencil bridges hurt at 12-14 px. To swap: drop the TTF over the placeholder TTF's name and run `TW/UI/Bake Skin Fonts`; the `.asset` names stay, so no sheet changes.

## Sizes at a glance

The panel scales with screen height against 1920x1080, so a 72 px card is 72 px at 1080p and 96 px at 1440p; the art is 1x. A sharper set later is a second folder of 2x files with `-unity-slice-scale: 0.5`, no code change.
