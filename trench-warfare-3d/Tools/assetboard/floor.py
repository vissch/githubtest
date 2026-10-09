"""This station's floor, written where the other station's board reads it. Run from trench-warfare-3d/:

    python Tools/assetboard/floor.py              read once, print who is at work, write the file
    python Tools/assetboard/floor.py --watch 20   ... and again every 20 seconds
    python Tools/assetboard/floor.py --dry        read once and print, write nothing

For a station that works but runs no board of its own (the desktop: the relay's legs, the second opinions, Codex and
Grok there). A board's watcher (ops.py --watch) writes the same file itself, so a station runs one or the other.
The file is <Drive>/TW3D-pipeline/floor/<host>.json (TW_FLOOR names another folder); src_floor.py says what is in it.
Reads everything else; one read that fails is said and the next one tried, as ops.py does.
"""
import argparse
import sys
import time
import traceback
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build        # noqa: E402
import src_floor    # noqa: E402
import src_ops      # noqa: E402


def once(dry=False):
    now = time.time()
    trees = src_ops.checkouts(build.REPO)
    rows = src_ops.floor(trees, now, rel=src_ops.relay(build.REPO / 'trench-warfare-3d', now))
    where = None if dry else src_floor.publish(rows, now)
    return rows, where


def main(argv=None):
    ap = argparse.ArgumentParser(description="this station's floor, for the other station's board")
    ap.add_argument('--watch', type=float, default=0, help='seconds between reads; 0 reads once')
    ap.add_argument('--dry', action='store_true', help='print who is at work, write nothing')
    args = ap.parse_args(argv)
    last = None
    while True:
        try:
            rows, where = once(args.dry)
            n = src_floor.count([w for _, w in rows])
            line = f'{n["working"]} at work ({n["sessions"]} sessions, {n["subagents"]} subagents, {n["relay"]} relay legs, {n["codex"]} Codex, {n["grok"]} Grok)'
            if not args.watch:
                for lane, w in rows:
                    print(f'  {w["state"]:8} {w.get("vendor", ""):6} {w["name"][:22]:22} {lane[:40]:40} {(w.get("title") or w.get("what") or "")[:70]}')
            if line != last or not args.watch:
                print(f'{time.strftime("%H:%M:%S")} floor: {line}' + (f', written to {where}' if where else '' if args.dry else ', NOT written (no folder to write in)'), flush=True)
                last = line
        except Exception:       # noqa: BLE001
            print(f'{time.strftime("%H:%M:%S")} floor: this read failed, the next one is tried', flush=True)
            traceback.print_exc()
        if not args.watch:
            return 0
        time.sleep(max(5, args.watch))


if __name__ == '__main__':
    sys.exit(main())
