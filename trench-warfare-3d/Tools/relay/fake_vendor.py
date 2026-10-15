#!/usr/bin/env python3
"""A stand-in for grok.exe and codex.exe, for the tests: `fake_vendor.py grok|codex <the real arguments>`. It prints
what the real one prints for one turn, in that vendor's shape. No model, no network, no cost.

TW_FAKE_SECOND is a JSON file:
  "report"   the last message                      "calls"  [{"tool": name, "ran": true|false, "what": ".."}]
  "write"    a file name to create in the working folder (a run that got past its rail)
  "tools"    grok: the tool list it says it has (default: what --tools asked for, and the two meta tools)
  "mode"     grok: the permission mode it says (default: what it was asked for)
  "sleep"    seconds before the end                "exit"   the exit code
  "no_result"  print no closing record             "subtype"  grok: the result's subtype (default success)
TW_FAKE_SECOND_ARGV, when set, is a file that gets the arguments it was started with (one JSON list per start).
"""
import json, os, sys, time
from pathlib import Path


def after(args, name, default=None):
    return args[args.index(name) + 1] if name in args else default


def out(obj):
    print(json.dumps(obj), flush=True)


def main():
    who, args = sys.argv[1], sys.argv[2:]
    script = json.loads(Path(os.environ["TW_FAKE_SECOND"]).read_text(encoding="utf-8"))
    if os.environ.get("TW_FAKE_SECOND_ARGV"):
        with open(os.environ["TW_FAKE_SECOND_ARGV"], "a", encoding="utf-8") as f:
            f.write(json.dumps(args) + "\n")
    cwd = Path(after(args, "--cwd") or after(args, "-C") or ".")
    calls = script.get("calls", [{"tool": "read_file" if who == "grok" else "command", "ran": True, "what": "note.txt"}])
    report = script.get("report", "The fake vendor ran.")
    if who == "grok":
        out({"type": "system", "subtype": "init", "session_id": "fake-grok", "model": after(args, "-m", "grok-fake"),
             "permissionMode": script.get("mode", after(args, "--permission-mode", "default")),
             "tools": script.get("tools", (after(args, "--tools", "") or "").split(",") + ["search_tool", "use_tool"])})
        for i, c in enumerate(calls):
            out({"type": "assistant", "message": {"content": [
                {"type": "tool_use", "id": "call-%d" % i, "name": c["tool"], "input": {"target_file": c.get("what", "")}}]}})
            out({"type": "user", "message": {"content": [
                {"type": "tool_result", "tool_use_id": "call-%d" % i, "is_error": not c.get("ran", True), "content": "x"}]}})
    else:
        out({"type": "thread.started", "thread_id": "fake-codex"})
        for i, c in enumerate(calls):
            item = {"id": "item_%d" % i, "type": "command_execution" if c["tool"] == "command" else c["tool"],
                    "command": c.get("what", ""), "status": "completed" if c.get("ran", True) else "failed"}
            out({"type": "item.completed", "item": item})
    if script.get("write"):
        (cwd / script["write"]).write_text("x", encoding="utf-8")
    time.sleep(script.get("sleep", 0))
    if not script.get("no_result"):
        if who == "grok":
            sub = script.get("subtype", "success")
            out({"type": "result", "subtype": sub, "is_error": sub != "success", "num_turns": 1, "result": report,
                 "total_cost_usd": 0.02, "usage": {"input_tokens": 1000, "output_tokens": 200,
                                                   "cache_read_input_tokens": 300}})
        else:
            out({"type": "item.completed", "item": {"id": "item_last", "type": "agent_message", "text": report}})
            if after(args, "-o"):
                Path(after(args, "-o")).write_text(report, encoding="utf-8")
            out({"type": "turn.completed", "usage": {"input_tokens": 100000, "cached_input_tokens": 90000,
                                                     "output_tokens": 1000}})
    return script.get("exit", 0)


if __name__ == "__main__":
    sys.exit(main())
