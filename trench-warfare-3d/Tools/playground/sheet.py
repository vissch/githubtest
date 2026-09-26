# Contact sheets for a capture round, each still labelled with what is on it (LOD, tris/verts, stage) from its JSON.
#   python Tools/playground/sheet.py TAG [DIR]  -> DIR/TAG_sheet_{v,u,m,b}.jpg and a stats table (DIR: Captures/playground)
import sys, json, glob, os
from PIL import Image, ImageDraw, ImageFont
T = sys.argv[1]; D = sys.argv[2] if len(sys.argv) > 2 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "Captures", "playground")
try: font = ImageFont.truetype("arial.ttf", 18); small = ImageFont.truetype("arial.ttf", 14)
except Exception: font = small = ImageFont.load_default()
def label(name):
    im = Image.open(f"{D}/{name}.png").convert("RGB"); j = json.load(open(f"{D}/{name}.json"))
    d = ImageDraw.Draw(im); W, H = im.size
    for v in j.get("vehicles", []):
        x, y = v["sx"] * W, (1 - v["sy"]) * H
        t = f"LOD{v['lod']} {v['tris']} tris {v['stage']}"
        d.text((x - 90 + 1, y + 1), t, fill=(0, 0, 0), font=font); d.text((x - 90, y), t, fill=(255, 235, 120), font=font)
    us = j.get("units", [])
    for u in (us if len(us) <= 6 else []):
        x, y = u["sx"] * W, (1 - u["sy"]) * H
        t = f"LOD{u['lod']} {u['verts']}v {u['bones']}b"
        d.text((x - 60 + 1, y + 1), t, fill=(0, 0, 0), font=small); d.text((x - 60, y), t, fill=(160, 230, 255), font=small)
    if len(us) > 6:
        for u in us:
            x, y = u["sx"] * W, (1 - u["sy"]) * H
            d.text((x - 8, y - 16), f"{u['lod']}", fill=(160, 230, 255), font=small)
    tag = name.replace(T + "_", "")
    d.rectangle([0, 0, 360, 26], fill=(0, 0, 0)); d.text((6, 3), f"{tag}  luma {j.get('luma_mean')}  fps {j.get('fps')}  cards {j.get('cards')}", fill=(255, 255, 255), font=small)
    return im, j
for group in ("v", "u", "m", "b"):
    names = sorted(os.path.basename(p)[:-4] for p in glob.glob(f"{D}/{T}_{group}[0-9]*.png"))
    if not names: continue
    ims = [label(n)[0].resize((800, 450)) for n in names]
    cols = 2; rows = (len(ims) + 1) // 2
    S = Image.new("RGB", (800 * cols, 450 * rows), (20, 20, 20))
    for i, im in enumerate(ims): S.paste(im, ((i % cols) * 800, (i // cols) * 450))
    S.save(f"{D}/{T}_sheet_{group}.jpg", quality=86)
    print("sheet", f"{D}/{T}_sheet_{group}.jpg", len(ims))
for p in sorted(glob.glob(f"{D}/{T}_*.json")):
    j = json.load(open(p))
    print(os.path.basename(p)[:-5], "luma", j.get("luma_mean"), "p95", j.get("luma_p95"), "blown", j.get("blown_frac"), "fps", j.get("fps"), "cards", j.get("cards"),
          "veh", [(v["lod"], v["stage"], v["loose"]) for v in j.get("vehicles", [])], "units", [(u["lod"], round(u["dist"])) for u in j.get("units", [])][:14])
