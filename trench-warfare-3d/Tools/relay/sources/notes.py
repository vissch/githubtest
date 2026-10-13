"""The handoff note: what one leg leaves for the next leg on the same unit, when the unit is not done yet. One place
for every source that hands one on, so a note is read, scored, shown and kept the same way whoever queued the work.

  <board>/relay/notes/<key>.md    the last leg's note. key is the source's name for the unit that stays the same from
                                  one try to the next: a queued unit's id, a board job's item and stage
  ASK                             the card line that tells a leg to write one
  read(ctx, key)                  {"note": the text or "", "predictions": [(command, held)]}, scored in the checkout
  card(unit)                      the card lines that show the note and its scored predictions, and say that its
                                  Decisions (when it has any) are closed; [] with no note
  keep(ctx, key, desk, lim)       a good note goes to the board; a bad or a missing one takes the old one away
  drop(ctx, key)                  the unit is done: its note is gone
The shape of a note and its checks are papers.py's (NOTE_SECTIONS, check_note, run_predictions).
Stdlib only. ASCII only.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import papers   # noqa: E402

ASK = ("- If you cannot finish, write note.md in your leg folder for the next leg (sections: Goal, Done, "
       "In flight, Next, Predictions, Dead ends, and Decisions when anything is decided). A prediction is `command` "
       "-> exit N, with a look-only command. Decisions: one line per choice already made that the next leg must not "
       "reopen, with where it was decided (the id of an answered decision, a decisions.md row, the owner's note "
       "id, or `this leg, because ...`).")
CLOSED = ("The note's Decisions are closed: work by them, do not reopen them, and never put one before the owner "
          "again. One you find wrong: say so in your report, and still go by it.")


def path(ctx, key):
    return Path(ctx["board"]) / "relay" / "notes" / (key + ".md")


def read(ctx, key):
    """Once the checkout is on the lane: the last leg's note, and each of its predictions scored there."""
    note = path(ctx, key)
    text = note.read_text(encoding="utf-8") if note.exists() else ""
    return {"note": text, "predictions": papers.run_predictions(text, ctx["work"]) if text else []}


def card(unit):
    if not unit.get("note"):
        return []
    lines = ["", "# Note from the last leg on this unit", unit["note"].strip()]
    lines += ["", "Its predictions, checked just now by script:"]
    lines += ["- %s: %s" % ("held" if hit else "DID NOT HOLD", cmd) for cmd, hit in unit.get("predictions", [])]
    if papers.decisions(unit["note"]):
        lines += ["", CLOSED]
    return lines


def keep(ctx, key, desk, lim):
    """A good note goes to the board for the next run; a bad one is dropped, so a false note is never handed on."""
    f = Path(desk) / "note.md"
    dst = path(ctx, key)
    if f.exists() and not papers.check_note(f.read_text(encoding="utf-8"), lim["note_max_bytes"]):
        dst.parent.mkdir(parents=True, exist_ok=True)
        dst.write_text(f.read_text(encoding="utf-8"), encoding="utf-8", newline="\n")
    elif dst.exists():
        dst.unlink()                                 # the old note describes a state that is gone


def drop(ctx, key):
    note = path(ctx, key)
    if note.exists():
        note.unlink()
