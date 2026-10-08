#!/usr/bin/env python3
"""The relay's Claude Code hooks, one script for every event (wired by relay-hooks.json, passed with --settings).

  relay_hook.py session-start <guard>   hand the leg its card as context
  relay_hook.py meter <guard>           after each tool batch: measure the context, say amber once, red every batch
  relay_hook.py pre-tool <guard>        refuse what a leg may never do, what a read-only phase may not do, and at
                                        red everything outside the close-out list; log every call it saw
  relay_hook.py pre-compact <guard>     record the trip and block the compaction (the runner then stops the run)

<guard> is the leg's guard folder (leg.json, the meter, the logs). It is in the hook's command line only, never in
the leg's environment, so the leg is not told where its rules are; every tool is refused a path inside it, inside
the relay's own code, and on the board outside its evidence. pre-tool fails closed: rules it cannot read, or any
error, refuse the call. The other events log the error and let the leg go on. This is the second line of defence;
the first is outside the model (prepush.py, land.py and pipeline.py, the runner's audit). The meter reads the
transcript one batch late, so red sits well under the compaction size. Stdlib only. ASCII only.
"""
import json, os, sys, time
from pathlib import Path

AMBER_TEXT = ("RELAY: context is at %d tokens (amber). Finish the step you are on and start nothing new. If you have "
              "uncommitted changes, start the edit gate now: python \"%s\" leg gate start")
RED_TEXT = ("RELAY: context is at %d tokens (red). Close the leg now. Only these still work: git status/diff/log, "
            "writing note.md in your leg folder, and python \"%s\" leg gate wait | leg finish | leg done. "
            "leg finish commits and pushes for you when the gate is green for these exact files.")
WRITE_TOOLS = ("Write", "Edit", "NotebookEdit", "MultiEdit")
READ_ONLY_TOOLS = ("Read", "Grep", "Glob", "Bash", "PowerShell", "Skill", "TodoWrite", "ToolSearch", "WebFetch",
                   "WebSearch", "Agent", "Task", "TaskCreate", "TaskUpdate", "TaskList", "TaskGet") + WRITE_TOOLS
RED_TOOLS = ("Read", "Grep", "Glob", "Bash", "PowerShell") + WRITE_TOOLS
WAKE_TOOLS = ("ScheduleWakeup", "CronCreate", "Monitor")     # each promises a later turn, and a leg has none
NO_WAKE = ("nobody wakes a leg: when your turn ends the leg is over, so nothing runs in the background and no "
           "notice will come. Run it in the foreground (one call may last ten minutes). A longer job: `leg gate "
           "start`, `leg play`, or run_detached.py start, then wait for it with one call")
USAGE = ("input_tokens", "cache_read_input_tokens", "cache_creation_input_tokens", "output_tokens")
BLIND_BATCHES = 3
HERE = Path(__file__).resolve().parent


def read_json(p, default=None):
    try:
        return json.loads(Path(p).read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return default


def write_json(p, obj):
    tmp = Path("%s.%d.tmp" % (p, os.getpid()))
    tmp.write_text(json.dumps(obj, indent=2, sort_keys=True) + "\n", encoding="utf-8", newline="\n")
    for i in range(5):                           # on Windows a reader can hold the target for a moment
        try:
            os.replace(tmp, p)
            return
        except PermissionError:
            time.sleep(0.05 * (i + 1))
    os.replace(tmp, p)


def append(d, name, obj):
    """One line onto a log, under a lock on the file's first byte. Tool calls of one batch run their hooks at the
    same moment, and an append on Windows is a seek and a write: without the lock one of two lines is lost, and
    the runner's audit then counts a call the guard never saw."""
    obj = dict(obj, t=time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()))
    line = (json.dumps(obj, sort_keys=True) + "\n").encode("utf-8")
    fd = os.open(str(Path(d) / name), os.O_RDWR | os.O_CREAT | getattr(os, "O_BINARY", 0))
    try:
        if os.name == "nt":
            import msvcrt
            os.lseek(fd, 0, os.SEEK_SET)
            msvcrt.locking(fd, msvcrt.LK_LOCK, 1)        # waits, a second at a time, ten times, then raises
        else:
            import fcntl
            fcntl.flock(fd, fcntl.LOCK_EX)
        try:
            os.lseek(fd, 0, os.SEEK_END)
            os.write(fd, line)
        finally:
            if os.name == "nt":
                os.lseek(fd, 0, os.SEEK_SET)
                msvcrt.locking(fd, msvcrt.LK_UNLCK, 1)
    finally:
        os.close(fd)


def relay_py():
    return str(HERE / "relay.py").replace("\\", "/")


# ---------- the meter ----------

def context_tokens(transcript, offset=0, last=0):
    """(tokens, new offset, readable): the size of the newest main-chain model call at or after offset, else last.
    Input and output both count: the reply is in the context of the next call. Reads only whole lines; a line is
    parsed only when it can be a model call, so a tool result of many megabytes costs one substring test."""
    try:
        size = os.path.getsize(transcript)
        if offset > size:
            offset = 0
        with open(transcript, "rb") as f:
            f.seek(offset)
            data = f.read()
    except OSError:
        return last, offset, False
    end = data.rfind(b"\n") + 1
    for line in data[:end].splitlines():
        if b'"usage"' not in line or b"assistant" not in line or len(line) > 4_000_000:
            continue
        try:
            d = json.loads(line)
            if d.get("type") != "assistant" or d.get("isSidechain"):
                continue
            u = (d.get("message") or {}).get("usage") or {}
            n = sum(int(u.get(k) or 0) for k in USAGE)
        except (ValueError, TypeError, AttributeError):
            continue
        if n:
            last = n
    return last, offset + end, True


def meter(d, leg, inp):
    if inp.get("agent_id"):                      # a subagent's batch: its context is not the leg's
        return None
    m = read_json(d / "meter.json", {}) or {}
    tokens, off, readable = context_tokens(inp.get("transcript_path") or "", m.get("offset", 0), m.get("tokens", 0))
    blind = 0 if readable else m.get("blind", 0) + 1
    if not readable:
        append(d, "hook-errors.log", {"event": "meter", "error": "cannot read the transcript",
                                      "path": inp.get("transcript_path")})
    was = m.get("level", "green")
    lv = "red" if tokens >= leg["red_tokens"] or was == "red" else \
        "amber" if tokens >= leg["amber_tokens"] or m.get("measured_amber") else "green"   # a measured level stays
    measured_amber = lv in ("amber", "red") and tokens >= leg["amber_tokens"] or bool(m.get("measured_amber"))
    if lv == "green" and blind >= BLIND_BATCHES:
        lv = "amber"                             # cannot see: be careful, but only while blind
    said = m.get("said_amber", False)
    text = None
    if lv == "red":
        text = RED_TEXT % (tokens, relay_py())
    elif lv == "amber" and not said:
        text, said = AMBER_TEXT % (tokens, relay_py()), True
    if lv != was:
        append(d, "trips.jsonl", {"level": lv, "tokens": tokens})
    write_json(d / "meter.json", {"offset": off, "tokens": tokens, "level": lv, "said_amber": said, "blind": blind,
                                  "measured_amber": measured_amber})
    append(d, "meter.jsonl", {"tokens": tokens, "level": lv})
    return text


# ---------- what a leg may run ----------

def inside(path, folder, C):
    a, b = C.norm_path(path), C.norm_path(folder)
    return a == b or a.startswith(b + "/")


def may_write(path, leg, C):
    """In a read-only phase and at red: only the phase's output file and note.md, in the desk folder."""
    desk = leg.get("desk") or ""
    return bool(path) and C.norm_path(path) in [C.norm_path(Path(desk) / n) for n in (leg.get("output"), "note.md") if n]


def off_limits(path, d, leg, C):
    """Why no tool may write this path: the runner's guard, the relay's own code, the board outside evidence."""
    run_root = Path(d).parents[1]                # <home>/runs/<run>
    if inside(path, run_root, C) and not inside(path, leg.get("desk") or run_root / "none", C):
        return "that folder is the runner's: write in your leg folder (%s)" % leg.get("desk")
    if inside(path, HERE, C) or inside(path, HERE.parent / "pipeline", C):
        return "a leg does not edit the relay or the pipeline tools"
    board = leg.get("board")
    if board and inside(path, board, C) and not inside(path, Path(board) / "evidence", C):
        return "on the board a leg writes only under evidence/: results, claims and the queue are the runner's"
    if "/.git/" in C.norm_path(path) + "/":
        return "a leg does not write inside a .git folder: use git commands"
    return None


def names_runner_folder(cmd, d, leg, C):
    """True when a command names the runner's folder: in full, or as a .. path out of the leg's own desk."""
    run_root, desk = Path(d).parents[1], leg.get("desk") or ""
    flat = cmd.replace("\\", "/").lower()
    if C.norm_path(run_root) in flat and C.norm_path(desk) not in flat:
        return True
    for w in flat.replace('"', " ").replace("'", " ").replace("=", " ").replace(">", " ").replace("<", " ").split():
        if ".." in w and desk:
            if inside(C.norm_path(Path(desk) / w), run_root / "legs", C):
                return True
    return False


def level(d):
    """The meter's level; a meter file that exists but cannot be read counts as red (fail closed)."""
    p = d / "meter.json"
    if not p.exists():
        return "green"
    m = read_json(p)
    return m.get("level", "green") if isinstance(m, dict) else "red"


def refusal(d, leg, inp, C):
    """Why this tool call is refused, or None to let Claude Code's own permission rules decide. The same rules hold
    for the leg and for any subagent it started."""
    tool, tin = inp.get("tool_name", ""), inp.get("tool_input") or {}
    cmd = tin.get("command") if isinstance(tin.get("command"), str) else ""      # any tool that runs a command
    path = tin.get("file_path") or tin.get("notebook_path") or ""
    if tool in (leg.get("deny_tools") or ()):
        return "the %s tool is not part of a %s leg" % (tool, leg.get("phase"))
    if tool in WAKE_TOOLS or tin.get("run_in_background"):
        return NO_WAKE
    if tool in WRITE_TOOLS:
        why = off_limits(path, d, leg, C)
        if why:
            return why
    if cmd:
        if names_runner_folder(cmd, d, leg, C):
            return "that folder is the runner's: no command may name it"
        if C.skips_guard(cmd):
            return "git's push guard is the runner's: a leg does not touch or skip it"
        why = C.never(cmd, leg)
        if why:
            return why
    red = level(d) == "red"
    if leg.get("mode") == "read_only" or red:
        what = "context is at red: only the close-out works" if red else "this phase only reads"
        if tool not in (RED_TOOLS if red else READ_ONLY_TOOLS):
            return "%s; the %s tool is not part of it" % (what, tool)
        if tool in WRITE_TOOLS and not may_write(path, leg, C):
            return "%s; you may write only %s in your leg folder" % (what, leg.get("output") or "note.md")
        if cmd and not (C.red_ok(cmd, relay_py()) if red else
                        C.read_only_ok(cmd, relay_py())):
            return ("%s: git status/diff/log, note.md, and python \"%s\" leg finish | leg done" % (what, relay_py())
                    if red else "%s; this command could change something" % what)
    return None


# ---------- events ----------

def out(event, **fields):
    print(json.dumps({"hookSpecificOutput": dict(fields, hookEventName=event)}))


def deny(d, inp, why):
    try:
        append(d, "denials.jsonl", {"tool": inp.get("tool_name"), "why": why, "agent": bool(inp.get("agent_id")),
                                    "input": json.dumps(inp.get("tool_input"))[:300]})
    except OSError:
        pass
    out("PreToolUse", permissionDecision="deny", permissionDecisionReason="relay: " + why)


def main(argv):
    if len(argv) < 2:
        return 0                                 # no guard folder named: not a relay leg
    ev, d, inp = argv[0], Path(argv[1]), {}
    try:
        inp = json.loads(sys.stdin.read() or "{}")
        leg = read_json(d / "leg.json")
        if ev == "pre-tool":
            append(d, "calls.jsonl", {"id": inp.get("tool_use_id"), "tool": inp.get("tool_name"),
                                      "agent": bool(inp.get("agent_id"))})
            if not isinstance(leg, dict) or "mode" not in leg:
                deny(d, inp, "the leg's rules cannot be read, so nothing may run")
                return 0
            sys.path.insert(0, str(HERE))
            import cmdrules as C                 # inside the try: a broken rules file refuses, it does not pass
            why = refusal(d, leg, inp, C)
            if why:
                deny(d, inp, why)
            return 0
        if leg is None:
            return 0
        if ev == "session-start":
            write_json(d / "session.json", {"session_id": inp.get("session_id"),
                                            "transcript_path": inp.get("transcript_path")})
            card = (d / "card.md")
            if card.exists():
                out("SessionStart", additionalContext=card.read_text(encoding="utf-8"))
        elif ev == "meter":
            text = meter(d, leg, inp)
            if text:
                out("PostToolBatch", additionalContext=text)
        elif ev == "pre-compact":
            write_json(d / "compact.json", {"trigger": inp.get("trigger"), "session_id": inp.get("session_id")})
            append(d, "trips.jsonl", {"level": "compact", "trigger": inp.get("trigger")})
            print("relay: compaction is blocked in a leg; close the leg instead", file=sys.stderr)
            return 2
    except BaseException as e:                   # noqa: BLE001
        try:
            append(d, "hook-errors.log", {"event": ev, "error": repr(e)})
        except OSError:
            pass
        if ev == "pre-tool":                     # a guard that broke must not wave the call through
            deny(d, inp, "the guard failed (%s), so the call is refused" % type(e).__name__)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
