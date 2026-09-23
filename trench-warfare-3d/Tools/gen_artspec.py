"""Generate docs/17-ui-art-spec.md from SkinSpec.cs so the artist's sheet and the code cannot drift.
Usage: python gen_artspec.py <SkinSpec.cs> <out.md>"""
import re, sys, io

src = io.open(sys.argv[1], encoding="utf-8").read()
# P("file", w, h, b, "note")  |  P("file", w, h, l, b, r, t, "note"[, tiled: true])  |  E(...) same  |  G("file", s, "note")
entries = []
for m in re.finditer(r'^\s*([PEG])\("([^"]+)",\s*(.*)\)\s*,?\s*$', src, re.M):
    kind, f, rest = m.group(1), m.group(2), m.group(3)
    parts = [p.strip() for p in re.split(r',(?=(?:[^"]*"[^"]*")*[^"]*$)', rest)]
    nums = [int(p) for p in parts if re.fullmatch(r'-?\d+', p)]
    note = next((p.strip('"') for p in parts if p.startswith('"')), "")
    tiled = any("tiled" in p for p in parts)
    if kind == "G":
        w = h = nums[0]; l = b = r = t = 0; folder = "Icons/"; kname = "glyph"
    else:
        folder = "Sprites/"; kname = "plate" if kind == "P" else "element"
        if len(nums) == 3: w, h, bb = nums; l = b = r = t = bb
        else: w, h, l, b, r, t = nums[:6]
    entries.append((folder + f, w, h, l, b, r, t, kname, tiled, note))

portraits = re.search(r'PortraitNames\s*=\s*\{([^}]*)\}', src, re.S).group(1)
portraits = re.findall(r'"([^"]+)"', portraits)
fonts = re.findall(r'"(Fonts/[^"]+\.asset)"', src)
root = re.search(r'Root\s*=\s*"([^"]+)"', src).group(1)

out = io.StringIO()
w = out.write
w("# UI art spec: the Dust Front skin\n\n")
w("Generated from `Assets/_Project/UI/Skin/SkinSpec.cs` by `Tools/gen_artspec.py`; edit the table there, not here. "
  "`SkinAssetTests` fails if this file stops naming a file the table knows.\n\n")
w("## What the skin is\n\n")
w("Dust Front's interface: near-black gunmetal plates with very subtle rivets, 1 px light-grey bevel lines, recessed darker "
  "windows for numbers, hex bolts, ribbed hoses, `CAUTION` micro-text along plate edges and a faint film grain over the whole "
  "screen. Type is condensed stencil caps; body text is bone (#C8C9C6), readouts amber (#E0762A), danger red (#E02B2B), power "
  "pale blue (#7FB4E0), totals white. The palette lives in `dustfront.tokens.uss`; the sprites below are painted in those colours. "
  "No rounded corners anywhere: the house corner is a 4 px diagonal cut, drawn into the PNG.\n\n")
w("## Rules that let a file be swapped without a code change\n\n")
w("1. Paint to the exact size in the table. The 9-slice border (left, bottom, right, top, in px) is the part that must not "
  "stretch: rivets and bevels go inside it, the centre stays flat.\n")
w("2. Plates and elements are painted in final colour and are never tinted by the interface.\n")
w("3. Icons are WHITE glyphs on a transparent background with a 1 px near-black contour (#0A0B0C), a 4 px safe margin and "
  "strokes no thinner than 3 px at 64 px. The interface colours them (bone, amber, red, grey) by tint, so one file serves "
  "every state. Keep icons uncoloured: white body, near-black contour, grey only in the anti-aliasing; any hue shows up as a tint error in the verifier.\n")
w("4. Cut corners and all transparency live in the PNG. Alpha is straight (not premultiplied).\n")
w("5. Drop the file over the placeholder with the same name in the same folder under `%s`. Do not touch `.meta` files: "
  "import settings and borders are applied automatically from the table.\n" % root)
w("6. Nothing else. The generator sees the hash changed, records the file as yours, and never overwrites it. "
  "`TW/UI/Verify Skin` lists what is still a placeholder.\n\n")
w("Import settings (applied by `UiSkinImport`, checked by the verifier): Sprite (2D and UI), Single, Full Rect mesh, "
  "100 pixels per unit, sRGB, alpha is transparency, no mipmaps, not readable, clamp, bilinear, uncompressed RGBA32.\n\n")

def table(title, kind):
    rows = [e for e in entries if e[7] == kind]
    if not rows: return
    w("## %s (%d files, `%s`)\n\n" % (title, len(rows), root + rows[0][0].split("/")[0] + "/"))
    w("| File | Size | Border L,B,R,T | Notes |\n|---|---|---|---|\n")
    for f, wd, ht, l, b, r, t, k, tiled, note in rows:
        border = "-" if l + b + r + t == 0 else "%d,%d,%d,%d" % (l, b, r, t)
        if tiled: note += " (tiled along the stretch axis)"
        w("| `%s` | %dx%d | %s | %s |\n" % (f.split("/", 1)[1], wd, ht, border, note))
    w("\n")

table("Plates", "plate")
table("Elements", "element")
table("Icons", "glyph")

w("## Portraits (%d files, `%sPortraits/`)\n\n" % (len(portraits), root))
w("256x256 RGBA PNG, transparent background, no frame (the card draws its own). One per unit archetype plus the two support "
  "emblems, keyed by what the unit IS, never by its slot:\n\n")
for p in portraits: w("- `%s.png`\n" % p)
w("- `Cutter.png` is reserved: the model exists but no archetype fields it yet.\n\n")
w("Framing, so the set reads as one: machine turned 55 degrees so its nose points to the viewer's front-right (the enemy is "
  "screen-right everywhere in the HUD); camera 18 degrees above horizontal with a long lens (22 degree vertical field); the "
  "model fills 86% of the frame's larger extent, centred on its bounds. Infantry: a bust from mid-chest, body turned 30 "
  "degrees, face toward frame right; the Rifleman, Assault and MG share one figure and differ by a small weapon badge the "
  "card adds. Light: warm key upper-left-front, cool rim from behind-right, dim cool fill from below-front. No ground, no "
  "shadow disc, no team colour (the card's rim carries the team). Support emblems are painted, not rendered: a shell over a "
  "crater burst (HeBarrage), a drum with a drifting cloud (ChlorineGas), in plate colours with an amber or gas-green accent.\n\n")
w("`TW/UI/Bake Unit Portraits` renders placeholders from the game's own models with exactly this framing; a painted portrait "
  "replaces one by taking its file name.\n\n")

w("## Fonts (`%sFonts/`)\n\n" % root)
w("Three TextCore font assets, referenced by name from the style sheets:\n\n")
for f in fonts: w("- `%s`\n" % f.split("/")[-1])
w("\nPlaceholders are baked from faces the Unity install ships with their licences alongside: Inter SemiBold (OFL 1.1) for "
  "Display and Label, Roboto Mono Bold (Apache 2.0) for Mono, so the silver counter has tabular digits. The intended final "
  "faces (all OFL, Google Fonts; the owner picks): Saira Stencil One for Display, Big Shoulders Stencil for Label, Barlow "
  "Condensed or Saira Condensed where stencil bridges hurt at 12-14 px. To swap: drop the TTF over the placeholder TTF's name "
  "and run `TW/UI/Bake Skin Fonts`; the `.asset` names stay, so no sheet changes.\n\n")

w("## Sizes at a glance\n\n")
w("The panel scales with screen height against 1920x1080, so a 72 px card is 72 px at 1080p and 96 px at 1440p; the art is "
  "1x. A sharper set later is a second folder of 2x files with `-unity-slice-scale: 0.5`, no code change.\n")
io.open(sys.argv[2], "w", encoding="utf-8", newline="\n").write(out.getvalue())
print("wrote", sys.argv[2], len(entries), "entries", len(portraits), "portraits", len(fonts), "fonts")
