#!/usr/bin/env python3
"""OpenAI's Codex (codex.exe, `codex exec`), one read-only turn. Read from `codex exec --help` of 0.150.0-alpha
(2026-10-09); the executable comes with the Codex app, in a folder named by a hash that moves with every update.

The rail is its own sandbox, `-s read-only`: a write in and outside the working folder was refused in every probe
of 2026-10-09 (proof: `relay.py proof second codex`). What that sandbox lets a command do on Windows over ssh is
thin: with the user's setting ("elevated") no process starts at all, with "unelevated" cmd.exe runs and PowerShell
does not. So second.py puts every text file in the prompt and every picture on the command line, and a run needs
no command to do its job. The user's config is left out: it names MCP servers (Unity, After Effects) that a blind
critic must not start. A run prints tokens and no cost: second.py prices them from vendors.json.
Stdlib only. ASCII only.
"""
import os, shutil
from pathlib import Path

from providers import flag, lines, stub

ENV = "TW_RELAY_CODEX"
SANDBOX = "read-only"
OPEN = ("--dangerously-bypass-approvals-and-sandbox", "--approve-for-me", "--add-dir")
WROTE = ("file_change", "patch", "apply_patch", "mcp_tool_call")


def find():
    exe = stub(ENV)
    if exe:
        return exe
    exe = shutil.which("codex")
    if not exe:
        bins = Path(os.environ.get("LOCALAPPDATA", str(Path.home()))) / "OpenAI" / "Codex" / "bin"
        found = sorted(bins.glob("*/codex.exe"), key=lambda p: p.stat().st_mtime) if bins.is_dir() else []
        exe = str(found[-1]) if found else None
    if not exe:
        raise SystemExit("second: codex is not installed here")
    return [exe]


def argv(job):
    a = find() + ["exec", "--json", "-s", SANDBOX, "--skip-git-repo-check", "--ignore-user-config", "--ephemeral"]
    if os.name == "nt":
        a += ["-c", 'windows.sandbox="unelevated"']
    if job.get("model"):
        a += ["-m", job["model"]]
    for img in job.get("images") or []:
        a += ["-i", img]
    return a + ["-C", job["cwd"], "-o", job["last"], "-"]   # -i takes several values: the lone "-" must not follow it


def argv_open(job):
    """The same turn with the rail off, for `second.py proof --open` only: its folder is writable."""
    a = argv(job)
    a[a.index("-s") + 1] = "workspace-write"
    return a


def env(job):
    return {}


def stdin(job):
    return job["prompt"]


def pictures(names):
    return "The pictures are attached to this message, in this order: %s." % ", ".join(names)


def read_only(a):
    if any(w in a for w in OPEN) or flag(a, "-s") != SANDBOX:
        return "codex would not run in its %s sandbox" % SANDBOX
    return None


def read(out, job):
    recs = lines(out)
    thread = next((e.get("thread_id") for e in recs if e.get("type") == "thread.started"), None)
    done = [e for e in recs if e.get("type") == "turn.completed"]
    bad = next((e for e in recs if e.get("type") in ("turn.failed", "error")), None)
    calls, said = [], ""
    for e in recs:
        item = e.get("item") or {}
        if e.get("type") != "item.completed":
            continue
        if item.get("type") == "agent_message":
            said = item.get("text") or said
        elif item.get("type") == "command_execution":
            calls.append({"tool": "command", "ran": item.get("status") == "completed",
                          "what": str(item.get("command") or "")[:300]})
        elif item.get("type") not in ("reasoning", "todo_list"):
            calls.append({"tool": item.get("type"), "ran": item.get("status") not in ("failed", "declined"),
                          "what": str(item.get("query") or item.get("tool") or "")[:300]})
    try:
        last = Path(job["last"]).read_text(encoding="utf-8", errors="replace")
    except OSError:
        last = ""
    use = {}
    for e in done:                                           # one per turn; a one-shot has one
        for k, v in (e.get("usage") or {}).items():
            use[k] = use.get(k, 0) + (v if isinstance(v, int) else 0)
    ok = bool(done) and not bad
    why = "" if ok else "codex ended with %s" % ((bad or {}).get("type") or "no completed turn")
    return {"report": last.strip() or said, "session_id": thread, "ran_model": job.get("model") or "(codex default)",
            "tokens_in": use.get("input_tokens"), "tokens_out": use.get("output_tokens"),
            "cache_read": use.get("cached_input_tokens"), "turns": len(done), "cost_usd": None, "cost_from": "",
            "ok": ok, "why": why, "calls": calls, "said_mode": None, "said_tools": None}


def distrust(rec):
    did = [c["tool"] for c in rec.get("calls") or [] if c["ran"] and c["tool"] in WROTE]
    return ["it ran %s" % ", ".join(sorted(set(did)))] if did else []
