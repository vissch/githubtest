#!/usr/bin/env python3
"""A stand-in for the claude executable, for the tests: it takes the real arguments, runs the leg's real hooks (from
the --settings file) as Claude Code would, does what a script file tells it for the leg's phase, and prints the same
closing records. No model, no network, no cost.

TW_FAKE_SCRIPT is a JSON file: {"<phase>": [action, ...]}; a phase may also be keyed "<phase>#<leg number>".
Top-level keys: "mode" (the permission mode it reports), "no_hooks" (skip the hooks), "no_result" (print no result).
Actions:  {"write": "<name in the desk folder>", "text": ".."}   {"file": "<path in the checkout>", "text": ".."}
          {"abs": "<any path>", "text": ".."}   {"git": ["add", "-A"]}   {"tool": "Bash", "input": {..}}  (asks the pre-tool hook; refused = not done)
          {"tokens": n}  (a model call of that size, then the meter hook)   {"sleep": seconds}
          {"compact": true}   {"report": ".."}   {"exit": n}
"""
import json, os, subprocess, sys, time
from pathlib import Path


def hook(cmds, event, inp):
    """Run the hook command for one event; returns its hookSpecificOutput or None."""
    for entry in cmds.get(event, []):
        for h in entry.get("hooks", []):
            r = subprocess.run(h["command"], input=json.dumps(inp).encode(), capture_output=True, shell=True)
            if r.returncode == 2:
                return {"blocked": True}
            if r.stdout.strip():
                return json.loads(r.stdout)["hookSpecificOutput"]
    return None


def main():
    args = sys.argv[1:]
    d = Path(args[args.index("--settings") + 1]).parent
    leg = json.loads((d / "leg.json").read_text(encoding="utf-8"))
    desk = Path(leg["desk"])
    script = json.loads(Path(os.environ["TW_FAKE_SCRIPT"]).read_text(encoding="utf-8"))
    actions = script.get("%s#%d" % (leg["phase"], leg["leg"]), script.get(leg["phase"], []))
    cmds = {} if script.get("no_hooks") else json.loads(Path(args[args.index("--settings") + 1]).read_text(encoding="utf-8"))["hooks"]
    transcript = desk / "fake-transcript.jsonl"
    transcript.write_text("", encoding="utf-8")
    base = {"session_id": "fake", "transcript_path": str(transcript)}
    print(json.dumps({"type": "system", "subtype": "init", "model": leg["model"],
                      "permissionMode": script.get("mode", "auto")}), flush=True)
    hook(cmds, "SessionStart", dict(base, source="startup"))
    report, code, refused = "RESULT: done. The fake leg ran.", 0, []
    for a in actions:
        if "write" in a:
            (desk / a["write"]).write_text(a["text"], encoding="utf-8", newline="\n")
        elif "file" in a:
            p = Path.cwd() / a["file"]
            p.parent.mkdir(parents=True, exist_ok=True)
            p.write_text(a["text"], encoding="utf-8", newline="\n")
        elif "abs" in a:
            Path(a["abs"]).parent.mkdir(parents=True, exist_ok=True)
            Path(a["abs"]).write_text(a["text"], encoding="utf-8", newline="\n")
        elif "git" in a:
            subprocess.run(["git"] + a["git"], check=True, capture_output=True)
        elif "tool" in a:
            o = hook(cmds, "PreToolUse", dict(base, tool_name=a["tool"], tool_input=a["input"]))
            if o and o.get("permissionDecision") == "deny":
                refused.append(a["tool"])
        elif "tokens" in a:
            with open(transcript, "a", encoding="utf-8", newline="\n") as f:
                f.write(json.dumps({"type": "assistant", "message": {"usage": {"input_tokens": a["tokens"]}}}) + "\n")
            hook(cmds, "PostToolBatch", base)
        elif "sleep" in a:
            time.sleep(a["sleep"])
        elif "compact" in a:
            hook(cmds, "PreCompact", dict(base, trigger="auto"))
            time.sleep(30)
        elif "report" in a:
            report = a["report"]
        elif "exit" in a:
            code = a["exit"]
    if not script.get("no_result"):
        print(json.dumps({"type": "result", "subtype": script.get("subtype", "success" if code == 0 else "error"), "num_turns": len(actions),
                          "total_cost_usd": 0, "result": report, "refused": refused, "permission_denials": []}), flush=True)
    return code


if __name__ == "__main__":
    sys.exit(main())
