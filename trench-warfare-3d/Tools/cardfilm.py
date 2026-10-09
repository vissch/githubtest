"""Cut a deploy card's tiny film: 3 s of a unit's film on the Drive baked to one sprite sheet the HUD plays as a
flipbook (no VideoPlayer). Every sheet is the same shape: 10 fps x 3 s = 30 frames of 160 x 160 in a 6 x 5 grid,
960 x 800, written to Assets/_Project/UI/Resources/CardFilms/<Portrait>.png. 160 px is the infantry card
(HudLayout.InfantryCardPx 180) less the 10 px film inset each side.

The films are 960 x 540 wide shots, so each frame is centre-cropped square before it is resized.

usage (from trench-warfare-3d/): python Tools/cardfilm.py --unit Rifleman Assault MG Maw
                                 python Tools/cardfilm.py --unit Maw --start 6     (override the start second)
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

# portrait (the card's unit) -> the film on the Drive, and the second the 3 s cut starts at
UNITS = {
    "Rifleman": ("Rifle.battle.game-battle", 6.0),
    "Assault":  ("Assault.battle.game-battle", 6.0),
    "MG":       ("Machinegunner.battle.game-battle", 6.0),
    "Maw":      ("Maw.battle.game-moves", 4.0),
    "Tusk":     ("Tusk.battle.game-moves", 4.0),
    "Pincer":   ("Pincer.battle.game-moves", 4.0),
    "Kettle":   ("Kettle.battle.game-moves", 4.0),
    "Censer":   ("Censer.battle.game-moves", 4.0),
    "Pavise":   ("Pavise.battle.game-moves", 4.0),
    "Banner":   ("Banner.battle.game-moves", 4.0),
    "Redoubt":  ("Redoubt.battle.game-moves", 4.0),
}


def ffmpeg() -> str:
    exe = os.environ.get("TW_FFMPEG") or shutil.which("ffmpeg")
    if not exe or not pathlib.Path(exe).exists():
        raise SystemExit("cardfilm: no ffmpeg. Put it on PATH or set TW_FFMPEG to the exe.")
    return exe


def square(im: Image.Image) -> Image.Image:
    side = min(im.size)
    left = (im.width - side) // 2
    top = (im.height - side) // 2
    return im.crop((left, top, left + side, top + side)).resize((SIDE, SIDE), Image.LANCZOS)


def bake(unit: str, start: float, tmp: pathlib.Path) -> pathlib.Path:
    film, _ = UNITS[unit]
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
            sheet.paste(square(im.convert("RGB")), ((i % COLS) * SIDE, (i // COLS) * SIDE))
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    out = OUT_DIR / (unit + ".png")
    sheet.save(out)
    print(f"{unit}: {FRAMES} frames {sheet.width}x{sheet.height} -> {out} (from {film} at {start} s)")
    return out


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--unit", nargs="+", default=[], help="the portraits to bake (" + " ".join(UNITS) + ")")
    ap.add_argument("--start", type=float, help="the start second, instead of the mapping's")
    ap.add_argument("--list", action="store_true", help="print the mapping and stop")
    a = ap.parse_args(argv)
    if a.list or not a.unit:
        for unit, (film, start) in UNITS.items():
            print(f"{unit:9s} {film} at {start} s")
        return 0
    bad = [u for u in a.unit if u not in UNITS]
    if bad:
        raise SystemExit("cardfilm: no film mapped for " + ", ".join(bad))
    with tempfile.TemporaryDirectory(prefix="cardfilm-") as tmp:
        for unit in a.unit:
            bake(unit, a.start if a.start is not None else UNITS[unit][1], pathlib.Path(tmp))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
