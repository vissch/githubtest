#!/usr/bin/env python3
"""xAI's Grok Build (grok.exe), one read-only turn. Read from `grok --help` and ~/.grok/docs of 1.0.46 (2026-10-09).

Its OS sandbox is Landlock and Seatbelt: on Windows `--sandbox read-only` is asked for and not enforced ("logs a
warning and continues"). So the rail here is the tool list: the run is given the three reading tools and nothing
that writes or runs a command, in the mode that refuses whatever is not pre-approved, with MCP calls denied by rule.
The run says its own tool list and mode in its first record, and distrust() holds it to them.
It opens a picture with read_file (seen 2026-10-09: "red circle"). Its exit code is 0 also when the turn failed:
the result record is the verdict. Stdlib only. ASCII only.
"""
import os, shutil
from pathlib import Path

from providers import flag, lines, stub

ENV = "TW_RELAY_GROK"
READ_TOOLS = ("read_file", "list_dir", "grep")
META_TOOLS = ("search_tool", "use_tool")           # the CLI keeps these two whatever --tools says; MCP is denied by rule
MODE = "dontAsk"
OPEN = ("--always-approve", "--yolo")
# his Claude and Cursor MCP servers and the cross-session memory stay out of a blind critic
QUIET = {"GROK_CLAUDE_MCPS_ENABLED": "0", "GROK_CURSOR_MCPS_ENABLED": "0", "GROK_MEMORY": "0"}


def find():
    exe = stub(ENV)
    if exe:
        return exe
    exe = shutil.which("grok") or str(Path.home() / ".grok" / "bin" / ("grok.exe" if os.name == "nt" else "grok"))
    if not Path(exe).is_file():
        raise SystemExit("second: grok is not installed here (%s)" % exe)
    return [exe]


def argv(job):
    a = find() + ["--prompt-file", job["prompt"], "--output-format", "streaming-messages-json", "--cwd", job["cwd"],
                  "--permission-mode", MODE, "--tools", ",".join(READ_TOOLS), "--disallowed-tools", "Agent",
                  "--deny", "Bash", "--deny", "Edit", "--deny", "MCPTool(*)", "--deny", "WebFetch",
                  "--no-subagents", "--no-plan", "--disable-web-search", "--max-turns", str(job["max_turns"])]
    if job.get("model"):
        a += ["-m", job["model"]]
    return a


def argv_open(job):
    """The same turn with the rail off, for `second.py proof --open` only: every tool, every call approved."""
    return find() + ["--prompt-file", job["prompt"], "--output-format", "streaming-messages-json", "--cwd", job["cwd"],
                     "--always-approve", "--no-subagents", "--no-plan", "--disable-web-search",
                     "--max-turns", str(job["max_turns"])] + (["-m", job["model"]] if job.get("model") else [])


def env(job):
    return dict(QUIET)


def stdin(job):
    return None


def pictures(names):
    return "Open each picture with your file reading tool before you judge: %s." % ", ".join(names)


def read_only(a):
    if any(w in a for w in OPEN) or flag(a, "--permission-mode") != MODE:
        return "grok would not run in %s mode" % MODE
    tools = [t for t in (flag(a, "--tools") or "").split(",") if t]
    if not tools or any(t not in READ_TOOLS for t in tools):
        return "grok would be given tools beyond %s" % ", ".join(READ_TOOLS)
    return None


def read(out, job):
    recs = lines(out)
    init = next((e for e in recs if e.get("type") == "system" and e.get("subtype") == "init"), {})
    res = next((e for e in reversed(recs) if e.get("type") == "result"), {})
    failed = set()
    for e in recs:
        content = (e.get("message") or {}).get("content") if e.get("type") == "user" else None
        for c in content if isinstance(content, list) else []:
            if isinstance(c, dict) and c.get("type") == "tool_result" and c.get("is_error"):
                failed.add(c.get("tool_use_id"))
    calls = []
    for e in recs:
        for c in ((e.get("message") or {}).get("content") or []) if e.get("type") == "assistant" else []:
            if isinstance(c, dict) and c.get("type") == "tool_use":
                inp = c.get("input") or {}
                calls.append({"tool": c.get("name"), "ran": c.get("id") not in failed,
                              "what": str(inp.get("target_file") or inp.get("target_directory") or inp.get("command")
                                          or inp.get("query") or inp.get("pattern") or "")[:300]})
    use = res.get("usage") or {}
    ok = bool(res) and res.get("subtype") == "success" and not res.get("is_error")
    return {"report": res.get("result") or "", "session_id": init.get("session_id") or res.get("session_id"),
            "ran_model": init.get("model"), "tokens_in": use.get("input_tokens"), "tokens_out": use.get("output_tokens"),
            "cache_read": use.get("cache_read_input_tokens"), "turns": res.get("num_turns"),
            "cost_usd": res.get("total_cost_usd"), "cost_from": "vendor", "ok": ok,
            "why": "" if ok else "grok ended as %s" % (res.get("subtype") or "nothing: no result record"),
            "calls": calls, "said_mode": init.get("permissionMode"), "said_tools": init.get("tools")}


def mcp_name(tool):
    """`server__tool`: a tool of an MCP server. The owner's plugins bring some (flights, hotels), and a run lists
    them in its first record when their servers are up by then (one run in four on 2026-10-09). They are no rail's
    hole by being listed: the deny rule and the mode refuse the call, and a call that ran all the same shows in the
    run's own output and makes it untrusted below. `second.py proof` does not try one."""
    return "__" in (tool or "")


def distrust(rec):
    out = []
    if rec.get("said_mode") != MODE:
        out.append("it ran in %s mode, not %s" % (rec.get("said_mode"), MODE))
    tools = rec.get("said_tools")
    extra = [t for t in tools or [] if t not in READ_TOOLS + META_TOOLS and not mcp_name(t)]
    if tools is None or extra:
        out.append("its tool list was %s" % ("not said" if tools is None else "wider: " + ", ".join(extra[:4])))
    did = [c["tool"] for c in rec.get("calls") or [] if c["ran"] and c["tool"] not in READ_TOOLS + ("search_tool",)]
    if did:
        out.append("it ran %s" % ", ".join(sorted(set(did))))
    return out
