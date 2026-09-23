#!/usr/bin/env python3
"""Is the Unity project free? Ask the lock, not the task list.

WHY THIS EXISTS. On 2026-09-23 three sessions sharing this project lost six editor slots between them to the same
confusion, and each of us patched it privately with a different wrong heuristic. The confusion is that these are
four different statements and only the last one matters:

    "no Unity in the task list"          - says nothing; the lock outlives the process
    "Temp/UnityLockfile does not exist"  - true when free, but it also lingers after a bad exit
    "no CAPITAL-U Unity.exe is running"  - not a distinction at all (see below)
    "nothing holds the lock"             - the actual question

WHAT WE EACH GOT WRONG, so nobody re-derives it:

  * The lockfile OUTLIVES an editor that exits badly. A stale one made the project look busy for 30 minutes.
  * A batch `unity test` run holds the lock for its WHOLE run, so a gate is just as blocking as an open editor.
    Two sessions both "checked the editor was closed" and collided with each other's test runs.
  * A dying editor sits in the task list at ~44 KB, still called Unity.exe, indistinguishable from a real one if
    you only count rows. It cost a run.
  * "Capital U is the editor, lowercase is the CLI" is FALSE, and two of us adopted it. Windows process names are
    case-insensitive; tasklist prints whatever case the image happens to carry. Measured on this machine while one
    batch run held the lock: five processes answered to Unity.exe, at 0 MB, 32 MB, 34 MB, 43 MB and 1720 MB, and
    only the last held anything.

THE TEST THAT CANNOT BE WRONG. Unity guards the project with an exclusive handle on Temp/UnityLockfile - the file
is zero bytes, so it carries no PID and was never meant to be read. Try to open it. If the open is refused,
something live holds the project; if it succeeds, nothing does, whatever the task list says. This is the same
handle Unity itself contends on, so it cannot disagree with Unity.

Measured: a READ-ONLY open is refused too, so the probe never asks for write access and cannot take the lock away
from an editor that is in the middle of starting up.

Everything this prints about WHICH process holds it is best-effort diagnostics for a human, and never affects the
verdict. The verdict is the handle.

USAGE
    python Tools/editor_lock.py status          # human-readable; exit 0 free, 1 held, 2 unknown
    python Tools/editor_lock.py guard           # silent unless held; for `guard && do-the-thing`
    python Tools/editor_lock.py wait --timeout 1800
    python Tools/editor_lock.py claim claude-0a --minutes 15 --why "cycle 9 gate"
    python Tools/editor_lock.py release claude-0a

    from editor_lock import is_free, wait_until_free        # if you would rather import it

The claim commands are ADVISORY and exist only so two sessions waiting on the same slot do not both pounce the
instant it frees. They do not lock anything and they cannot stop anyone. They expire on their own, so a session
that dies holding one cannot block the others.

Windows only, which matches the project (owner decision 2026-09-20: Windows x64).
"""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
from pathlib import Path

LOCKFILE = Path("Temp") / "UnityLockfile"

FREE, HELD, UNKNOWN = "free", "held", "unknown"


def project_root(start: Path | None = None) -> Path:
    """The trench-warfare-3d directory, found from here or from the caller's cwd."""
    here = (start or Path.cwd()).resolve()
    for d in (here, *here.parents):
        if (d / "Assets" / "_Project").is_dir():
            return d
    # running from Tools/ inside the project is the common case
    mine = Path(__file__).resolve().parent.parent
    if (mine / "Assets" / "_Project").is_dir():
        return mine
    raise SystemExit("editor_lock: cannot find the Unity project (no Assets/_Project above %s)" % here)


def probe(root: Path | None = None) -> dict:
    """
    Ask the lock itself. Returns {state, detail} where state is free/held/unknown.

    The open is read-only and immediately closed: it is a question, not a claim.
    """
    root = root or project_root()
    path = root / LOCKFILE
    try:
        with open(path, "rb"):
            pass
        return {"state": FREE, "detail": "the lockfile is present but nothing holds it (stale, from a bad exit)"}
    except FileNotFoundError:
        return {"state": FREE, "detail": "no lockfile"}
    except PermissionError:
        return {"state": HELD, "detail": "a live editor or batch run holds the project"}
    except OSError as e:
        # Conservative on purpose: an unreadable lock is not evidence that the project is free, and writing into
        # Assets/ under someone else is the expensive direction to be wrong in.
        return {"state": UNKNOWN, "detail": "could not probe the lock: %s" % e}


def is_free(root: Path | None = None) -> bool:
    return probe(root)["state"] == FREE


def holders() -> list[dict]:
    """
    Best-effort: which Unity processes exist and which look like a batch run. DIAGNOSTICS ONLY - this never
    decides anything. A tiny working set means a process that is dying or is a helper, not an editor, but that is
    a hint for a human reading the output, not a rule.
    """
    ps = (
        "Get-CimInstance Win32_Process -Filter \"Name='Unity.exe'\" | "
        "ForEach-Object { [pscustomobject]@{ pid=$_.ProcessId; mb=[int]($_.WorkingSetSize/1MB); "
        "batch=[bool]($_.CommandLine -match '-runTests|-batchmode') } } | ConvertTo-Json -Compress"
    )
    try:
        out = subprocess.run(
            ["powershell", "-NoProfile", "-Command", ps],
            capture_output=True, text=True, timeout=20,
        ).stdout.strip()
        if not out:
            return []
        data = json.loads(out)
        return data if isinstance(data, list) else [data]
    except Exception:
        return []          # diagnostics are allowed to fail; the verdict is not


# ---- the advisory slot -------------------------------------------------------------------------------------------
# A note on a shared pinboard, not a lock. It stops two sessions that are both waiting from pouncing together; it
# does not and must not stop anybody from working.

def slot_path() -> Path:
    base = os.environ.get("LOCALAPPDATA") or os.environ.get("TEMP") or "."
    return Path(base) / "TrenchWarfare" / "editor-slot.json"


def read_slot() -> dict | None:
    try:
        s = json.loads(slot_path().read_text(encoding="utf-8"))
    except Exception:
        return None
    if not isinstance(s, dict) or time.time() > s.get("until", 0):
        return None            # expired claims are no claim at all
    return s


def claim(who: str, minutes: int, why: str) -> dict:
    p = slot_path()
    p.parent.mkdir(parents=True, exist_ok=True)
    s = {"who": who, "why": why, "since": time.time(), "until": time.time() + minutes * 60}
    p.write_text(json.dumps(s), encoding="utf-8")
    return s


def release(who: str) -> bool:
    s = read_slot()
    if s and s.get("who") != who:
        return False           # never drop someone else's claim
    try:
        slot_path().unlink()
    except FileNotFoundError:
        pass
    return True


def wait_until_free(timeout: float = 1800, every: float = 10, root: Path | None = None) -> bool:
    """Block until the lock is free. Returns False on timeout rather than raising."""
    root = root or project_root()
    deadline = time.time() + timeout
    while True:
        if probe(root)["state"] == FREE:
            return True
        if time.time() >= deadline:
            return False
        time.sleep(every)


#: A Unity above this is doing real work; below it the process is a helper, or one that is dying and has not left
#: the task list yet. Only ever used to LABEL the diagnostics - never to decide whether the project is free.
BIG_MB = 200


def likely_holder(procs: list[dict]) -> dict | None:
    """The biggest Unity, if any is big enough to be the one holding the project. Advisory."""
    big = [p for p in procs if p.get("mb", 0) >= BIG_MB]
    return max(big, key=lambda p: p.get("mb", 0)) if big else None


def describe(root: Path) -> str:
    r = probe(root)
    lines = ["%s: %s" % (r["state"].upper(), r["detail"])]
    procs = holders()
    if procs:
        lines.append("  Unity.exe processes (diagnostics only, not the verdict):")
        for p in sorted(procs, key=lambda x: -x.get("mb", 0)):
            if p.get("mb", 0) < BIG_MB:
                what = ""
            elif p.get("batch"):
                what = "  [batch run - ends on its own]"
            else:
                what = "  [interactive editor - someone is working in it]"
            lines.append("    pid %-7s %5s MB%s" % (p.get("pid"), p.get("mb"), what))
        # The two mean very different waits, and it is worth saying so rather than leaving it to be inferred from
        # the absence of a marker. A batch run is over in minutes. An interactive editor may be open for an hour,
        # and it may be the OWNER playing the game - in which case nobody should be queueing behind it at all.
        h = likely_holder(procs)
        if r["state"] == HELD and h is not None and not h.get("batch"):
            lines.append("  -> an interactive editor, not a test run: waiting may not end. Ask whose it is.")
    s = read_slot()
    if s:
        mins = max(0, int((s["until"] - time.time()) / 60))
        lines.append("  slot claimed by %s for ~%d more min: %s" % (s.get("who"), mins, s.get("why") or "-"))
    return "\n".join(lines)


def main(argv: list[str]) -> int:
    ap = argparse.ArgumentParser(description="Is the Unity project free? Ask the lock, not the task list.")
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("status")
    sub.add_parser("guard")
    w = sub.add_parser("wait"); w.add_argument("--timeout", type=float, default=1800); w.add_argument("--every", type=float, default=10)
    c = sub.add_parser("claim"); c.add_argument("who"); c.add_argument("--minutes", type=int, default=15); c.add_argument("--why", default="")
    r = sub.add_parser("release"); r.add_argument("who")
    a = ap.parse_args(argv)
    root = project_root()

    if a.cmd == "status":
        print(describe(root))
        return {FREE: 0, HELD: 1, UNKNOWN: 2}[probe(root)["state"]]

    if a.cmd == "guard":
        st = probe(root)
        if st["state"] == FREE:
            return 0
        print("editor_lock: NOT free - %s" % st["detail"], file=sys.stderr)
        print(describe(root), file=sys.stderr)
        return 1 if st["state"] == HELD else 2

    if a.cmd == "wait":
        # Say up front if the thing being waited on will not end by itself, so a script does not sit out its whole
        # timeout in silence behind someone who is working - or behind the owner playing the game.
        if probe(root)["state"] == HELD:
            h = likely_holder(holders())
            if h is not None and not h.get("batch"):
                print("editor_lock: waiting on an INTERACTIVE editor (pid %s, %s MB), which may not end on its own."
                      % (h.get("pid"), h.get("mb")), file=sys.stderr)
        if wait_until_free(a.timeout, a.every, root):
            print("free")
            return 0
        print("editor_lock: still held after %gs" % a.timeout, file=sys.stderr)
        print(describe(root), file=sys.stderr)
        return 1

    if a.cmd == "claim":
        s = read_slot()
        if s and s.get("who") != a.who:
            mins = max(0, int((s["until"] - time.time()) / 60))
            print("editor_lock: %s already claims the slot for ~%d more min (%s)" % (s.get("who"), mins, s.get("why") or "-"),
                  file=sys.stderr)
            return 1
        claim(a.who, a.minutes, a.why)
        print("claimed by %s for %d min" % (a.who, a.minutes))
        return 0

    if a.cmd == "release":
        if release(a.who):
            print("released")
            return 0
        s = read_slot() or {}
        print("editor_lock: the slot is %s's, not yours - not releasing it" % s.get("who"), file=sys.stderr)
        return 1

    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
