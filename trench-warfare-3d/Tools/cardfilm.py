"""Cut a deploy card's tiny film: 3 s of a unit's film on the Drive baked to one sprite sheet the HUD plays as a
flipbook (no VideoPlayer). Every sheet is the same shape: 10 fps x 3 s = 30 frames of 160 x 160 in a 6 x 5 grid,
960 x 800, written to Assets/_Project/UI/Resources/CardFilms/<Portrait>.png. 160 px is the infantry card
(HudLayout.InfantryCardPx 180) less the 10 px film inset each side.

The films are 960 x 540 wide shots of a whole trench, so a plain centre crop leaves the unit a few pixels wide
(the critic of round 1, 2026-10-09: "lit trench and fire with no readable unit"). Every unit therefore carries a
frame as well as a start second: zoom is the share of the frame's height the square window keeps, and cx/cy are
where its centre sits, both as fractions of the frame. Zoom 1.0 with cx/cy 0.5 is the old centre crop.

usage (from trench-warfare-3d/): python Tools/cardfilm.py --unit Rifleman Assault MG Maw
                                 python Tools/cardfilm.py --unit Maw --start 6     (override the start second)
                                 python Tools/cardfilm.py --unit Maw --zoom 0.6 --center 0.45 0.62   (override the frame)
                                 python Tools/cardfilm.py --list
It needs ffmpeg on PATH or in TW_FFMPEG, and the films in G:/My Drive/TW3D-pipeline/assets/film (TW_FILM_DIR).
It writes no .meta: Unity makes those on import (no mips, compression on).
"""
import argparse, os, pathlib, shutil, subprocess, sys, tempfile
from PIL import Image

FPS = 10
SECONDS = 3
FRAMES = FPS * SECONDS      # 30
SIDE = 160
COLS, ROWS = 6, 5

FILM_DIR = pathlib.Path(os.environ.get("TW_FILM_DIR", "G:/My Drive/TW3D-pipeline/assets/film"))
OUT_DIR = pathlib.Path("Assets/_Project/UI/Resources/CardFilms")

# portrait (the card's unit) -> the film on the Drive, the second the 3 s cut starts at, and the frame it is cut
# on: (zoom, cx, cy). The seconds and frames of the four baked units were picked by eye on a contact sheet of each
# film, on the one rule the card has: the unit must be the biggest readable thing in a 160 px window.
UNITS = {
    "Rifleman": ("Rifle.battle.game-battle", 1.0, (0.58, 0.45, 0.60)),
    "Assault":  ("Assault.battle.game-battle", 6.0, (0.70, 0.50, 0.55)),
    "MG":       ("Machinegunner.battle.game-battle", 6.0, (0.70, 0.50, 0.55)),
    "Maw":      ("Maw.battle.game-moves", 4.0, (0.80, 0.50, 0.55)),
    "Tusk":     ("Tusk.battle.game-moves", 4.0, (0.80, 0.50, 0.55)),
    "Pincer":   ("Pincer.battle.game-moves", 4.0, (0.80, 0.50, 0.55)),
    "Kettle":   ("Kettle.battle.game-moves", 4.0, (0.80, 0.50, 0.55)),
    "Censer":   ("Censer.battle.game-moves", 4.0, (0.80, 0.50, 0.55)),
    "Pavise":   ("Pavise.battle.game-moves", 4.0, (0.80, 0.50, 0.55)),
    "Banner":   ("Banner.battle.game-moves", 4.0, (0.80, 0.50, 0.55)),
    "Redoubt":  ("Redoubt.battle.game-moves", 4.0, (0.80, 0.50, 0.55)),
}


def ffmpeg() -> str:
    exe = os.environ.get("TW_FFMPEG") or shutil.which("ffmpeg")
    if not exe or not pathlib.Path(exe).exists():
        raise SystemExit("cardfilm: no ffmpeg. Put it on PATH or set TW_FFMPEG to the exe.")
    return exe


def square(im: Image.Image, frame=(1.0, 0.5, 0.5)) -> Image.Image:
    """The square window of one film frame: zoom x the frame's height, centred on (cx, cy), kept inside the frame."""
    zoom, cx, cy = frame
    side = int(round(min(im.size) * max(0.1, min(1.0, zoom))))
    left = int(round(im.width * cx - side / 2))
    top = int(round(im.height * cy - side / 2))
    left = max(0, min(im.width - side, left))
    top = max(0, min(im.height - side, top))
    return im.crop((left, top, left + side, top + side)).resize((SIDE, SIDE), Image.LANCZOS)


def bake(unit: str, start: float, tmp: pathlib.Path, frame=None) -> pathlib.Path:
    film, _, default_frame = UNITS[unit]
    frame = frame or default_frame
    src = FILM_DIR / (film + ".mp4")
    if not src.exists():
        raise SystemExit(f"cardfilm: no film {src}")
    shots = tmp / unit
    shots.mkdir(parents=True, exist_ok=True)
    subprocess.run([ffmpeg(), "-v", "error", "-y", "-ss", str(start), "-t", str(SECONDS), "-i", str(src),
                    "-vf", f"fps={FPS}", "-frames:v", str(FRAMES), "-q:v", "2",
                    str(shots / "f%03d.jpg")], check=True)
    jpgs = sorted(shots.glob("*.jpg"))
    if len(jpgs) < FRAMES:
        raise SystemExit(f"cardfilm: {unit}: the film gave {len(jpgs)} frames from {start} s, not {FRAMES}")
    sheet = Image.new("RGB", (COLS * SIDE, ROWS * SIDE), (0, 0, 0))
    for i, jpg in enumerate(jpgs[:FRAMES]):
        with Image.open(jpg) as im:
            sheet.paste(square(im.convert("RGB"), frame), ((i % COLS) * SIDE, (i // COLS) * SIDE))
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    out = OUT_DIR / (unit + ".png")
    sheet.save(out)
    print(f"{unit}: {FRAMES} frames {sheet.width}x{sheet.height} -> {out} "
          f"(from {film} at {start} s, zoom {frame[0]} on {frame[1]},{frame[2]})")
    return out


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--unit", nargs="+", default=[], help="the portraits to bake (" + " ".join(UNITS) + ")")
    ap.add_argument("--start", type=float, help="the start second, instead of the mapping's")
    ap.add_argument("--zoom", type=float, help="the share of the frame's height the window keeps, instead of the mapping's")
    ap.add_argument("--center", type=float, nargs=2, metavar=("CX", "CY"), help="where that window's centre sits, as fractions")
    ap.add_argument("--list", action="store_true", help="print the mapping and stop")
    a = ap.parse_args(argv)
    if a.list or not a.unit:
        for unit, (film, start, frame) in UNITS.items():
            print(f"{unit:9s} {film} at {start} s, zoom {frame[0]} on {frame[1]},{frame[2]}")
        return 0
    bad = [u for u in a.unit if u not in UNITS]
    if bad:
        raise SystemExit("cardfilm: no film mapped for " + ", ".join(bad))
    with tempfile.TemporaryDirectory(prefix="cardfilm-") as tmp:
        for unit in a.unit:
            z, cx, cy = UNITS[unit][2]
            if a.zoom is not None: z = a.zoom
            if a.center is not None: cx, cy = a.center
            bake(unit, a.start if a.start is not None else UNITS[unit][1], pathlib.Path(tmp), (z, cx, cy))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
