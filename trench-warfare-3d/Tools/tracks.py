"""Draw where the men of a test battle walked: python Tools/tracks.py <tracks.csv> <out.png> [scale]

The csv is what SpreadAndEngageTests.Report_TheNumbersOfThreeMarchesAndThreeBattles leaves in the temp folder
(tw-tracks-march.csv, tw-tracks-battle.csv): the nav layers of the field, a line "#", then one line per man every ten
ticks (tick, slot, team, in the open, x, z, standing). The picture is the field from above, its width across and team 0
walking up it: trenches brown, wire grey, blocked ground dark, mud and craters a shade of the ground; team 0's tracks
cyan and team 1's red, brighter where more men passed; a white dot where a man stood still in the open (to shoot).
Read a picture of rows against a picture of a spread field before believing a number (docs/reference/tasks.md,
Movement). Needs PIL.
"""
import sys
from PIL import Image

SURFACE, TRENCH, LINK, BLOCKED, WIRE, MUD, CRATER, BUNKER = 1, 2, 4, 8, 16, 32, 64, 128


def ground_colour(layer):
    if layer & BLOCKED: return (24, 26, 34)
    if layer & LINK: return (150, 120, 70)
    if layer & TRENCH: return (96, 70, 44)
    if layer & WIRE: return (120, 120, 126)
    if layer & MUD: return (58, 52, 44)
    if layer & CRATER: return (52, 56, 50)
    return (66, 70, 58)


def main():
    if len(sys.argv) < 3:
        print(__doc__); return 2
    scale = int(sys.argv[3]) if len(sys.argv) > 3 else 6   # pixels per metre
    text = open(sys.argv[1], encoding="utf-8").read().replace("\r", "")
    head, body = text.split("#\n", 1)
    rows = head.strip().split("\n")
    nav_w, nav_l = (int(v) for v in rows[0].split(","))
    cell = 2.0
    w, h = int(nav_w * cell * scale), int(nav_l * cell * scale)
    img = Image.new("RGB", (w, h))
    px = img.load()
    layers = [[int(v) for v in r.split(",")] for r in rows[1:1 + nav_l]]
    for y in range(h):
        z = min(nav_l - 1, int((h - 1 - y) / scale / cell))
        for x in range(w):
            px[x, y] = ground_colour(layers[z][min(nav_w - 1, int(x / scale / cell))])
    heat = {}
    stood = []
    last = {}
    for line in body.strip().split("\n"):
        if not line: continue
        t, slot, team, open_, x, z, still = line.split(",")
        t, slot, team = int(t), int(slot), int(team)
        x, z = float(x), float(z)
        key = (slot, team)
        if key in last and t - last[key][0] == 10:
            x0, z0 = last[key][1], last[key][2]
            steps = max(1, int(max(abs(x - x0), abs(z - z0)) * scale))
            if steps < 40 * scale:   # a slot's next tenant starts somewhere else
                for k in range(steps + 1):
                    u = k / steps
                    p = (int((x0 + (x - x0) * u) * scale), h - 1 - int((z0 + (z - z0) * u) * scale))
                    heat[(p, team)] = heat.get((p, team), 0) + 1
        last[key] = (t, x, z)
        if still == "1" and open_ == "1": stood.append((int(x * scale), h - 1 - int(z * scale)))
    for ((x, y), team), n in heat.items():
        if not (0 <= x < w and 0 <= y < h): continue
        a = min(1.0, 0.35 + 0.2 * n)
        tint = (70, 220, 235) if team == 0 else (240, 80, 70)
        for dx in (0, 1):
            for dy in (0, 1):
                if x + dx < w and y + dy < h:
                    r, g, b = px[x + dx, y + dy]
                    px[x + dx, y + dy] = (int(r + (tint[0] - r) * a), int(g + (tint[1] - g) * a), int(b + (tint[2] - b) * a))
    for x, y in stood:
        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                if 0 <= x + dx < w and 0 <= y + dy < h: px[x + dx, y + dy] = (255, 255, 255)
    img.save(sys.argv[2])
    print(f"{sys.argv[2]}  {w}x{h}  {len(heat)} track pixels, {len(stood)} standing samples")
    return 0


if __name__ == "__main__":
    sys.exit(main())
