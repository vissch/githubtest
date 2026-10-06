#!/usr/bin/env python3
"""What the owner has answered on the asset board's Decide page and no session has taken up, for the master's
screen (day.py). It shows that answers wait, which, and what he picked. It decides nothing.

A decision that waits on the owner is put to him as a short report with options; he clicks one on the page, or
answers in his own words. Two folders hold that, written by Tools/assetboard on the asset board's lane (this lane
does not have that code, and reads the folders as they are):

  <decisions folder>/<id>/brief.json   {"id", "title", "state": "open" | "answered", "options": [{"key", "text"}]}
  <notes folder>/<name>.md             a head between two --- lines (about: brief:<id>, when, state: open | done),
                                       then the text: a click is "A: the option's text", anything else his words
An open brief with an open note about it is an answer nobody has taken up: taking it up closes both.

Whether an answer is his yes to queue work is the asset board lane's rule and is written there only (its
`briefs.py waiting` says of each answer queue, nothing or ask). A second copy here would be the same rule in two
lanes that land apart, and read from a station that sees the Drive later than the one he clicked on.

  folders()                     (decisions folder, notes folder): TW_BRIEFS and TW_NOTES, else the Drive's
  read(decisions, notes)        (rows, why). rows: one {"id", "title", "picked", "when"} a brief, the oldest answer
                                first; picked is the option's letter, or "your own words". why: what could not be
                                read, else "".
Reads only. Never raises: a Drive that is not mounted is a line on the screen, not a failed turn.
Stdlib only. ASCII only.
"""
import json, os, re
from pathlib import Path

DRIVE = "G:/My Drive/TW3D-pipeline"
HEAD = re.compile(r"---\n(.*?)\n---\n?(.*)\Z", re.S)
ID = re.compile(r"[\w.-]+\Z")                    # a brief's id is a folder name: nothing with a slash is looked up
CLICK = re.compile(r"([A-D]): ")
OWN = "your own words"


def folders():
    return (Path(os.environ.get("TW_BRIEFS") or DRIVE + "/decisions"),
            Path(os.environ.get("TW_NOTES") or DRIVE + "/notes"))


def plain(text):
    """One line the screen can print: day.py is ASCII only, and a title is the owner's free text."""
    return " ".join(str(text).split()).encode("ascii", "replace").decode("ascii")


def note(path):
    """A note's head fields and its text, or None for a file that is not a note. The files are written on Windows
    and their lines may end either way: read as text, both come in as one."""
    try:
        raw = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return None
    m = HEAD.match(raw)
    if not m:
        return None
    n = {"id": path.stem, "state": "open"}
    for row in m.group(1).split("\n"):
        k, _, v = row.partition(": ")
        n[k.strip()] = v.strip()
    n["text"] = re.split(r"^## Answer \(", m.group(2), flags=re.M)[0].strip()
    return n


def read(decisions, notes):
    decisions, notes = Path(decisions), Path(notes)
    try:
        for d in (decisions, notes):
            if not d.is_dir():
                return [], "%s is not there" % plain(d)
        his = {}
        for f in sorted(notes.glob("*.md")):
            n = note(f)
            about = (n or {}).get("about", "")
            if n and n.get("state") != "done" and about.startswith("brief:") and ID.match(about[6:]):
                his.setdefault(about[6:], []).append(n)
        rows = []
        for bid, mine in his.items():
            try:
                b = json.loads((decisions / bid / "brief.json").read_text(encoding="utf-8"))
            except (OSError, ValueError):
                continue                         # a note about a brief that is gone, or that does not read
            if not isinstance(b, dict) or b.get("state") == "answered":
                continue                         # closed: a late word about it waits on nobody
            last = sorted(mine, key=lambda n: (n.get("when", ""), n["id"]))[-1]      # his last word is his answer
            keys = [o.get("key") for o in b.get("options") or [] if isinstance(o, dict)]
            m = CLICK.match(last["text"].split("\n")[0])
            rows.append({"id": bid, "title": plain(b.get("title") or bid), "when": last.get("when", ""),
                         "picked": m.group(1) if m and m.group(1) in keys else OWN})
        return sorted(rows, key=lambda r: (r["when"], r["id"])), ""
    except Exception as e:                       # whatever a synced folder does, the screen goes on
        return [], plain("%s: %s" % (type(e).__name__, e))
