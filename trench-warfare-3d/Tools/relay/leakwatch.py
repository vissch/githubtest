#!/usr/bin/env python3
"""The watch for wasted tokens, as a script: it costs none. Reads what legs left in the relay's home (leg.json, the
session's stream out.jsonl) and says which legs wasted, by rule, and how the phases did.

  python Tools/relay/leakwatch.py [<run> ...] [--since <run>] [--home <relay home>] [--write] [--json]

  no run named    every run in the home (with --since: that run and the later ones)
  --write         also keeps flags.json beside each run it read (runs/<run>/flags.json), for the retrospective
What it measures per leg: answers, tool calls, calls an answer, the context of the first turn, `leg done` calls that
were refused, tool calls after plan.md was first written, and how often one file was named.
The rules and their thresholds are leakwatch.json. A rule names what a leg did, never why: the reader (the
retrospective leg, or a person) looks at the calls around it.
Stdlib only. ASCII only.
"""
import argparse, json, re, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import legdir   # noqa: E402

PATHISH = re.compile(r"[\w./\\-]+\.(?:cs|py|md|json|shader|uss|uxml|asmdef|txt|png)\b")


def rules(folder=None):
    raw = json.loads((Path(folder or HERE) / "leakwatch.json").read_text(encoding="utf-8"))
    for k in ("dear_plan_usd", "dear_plan_answers", "size_fight_refusals", "no_batching_calls_per_answer",
              "no_batching_min_answers", "same_file_touches", "heavy_start_tokens", "late_calls"):
        if not isinstance(raw.get(k), (int, float)):
            raise SystemExit("relay: leakwatch.json %s must be a number" % k)
    return raw


def measure(d):
    """What one leg did, from its folder: {} when it has no record or no stream."""
    d = Path(d)
    try:
        leg = json.loads((d / legdir.LEG).read_text(encoding="utf-8"))
        fh = open(d / legdir.OUT, encoding="utf-8", errors="replace")
    except (OSError, ValueError):
        return {}
    answers, calls, first_ctx, refused, done_calls, first_write, files, check_ids = set(), 0, None, 0, 0, None, {}, set()
    with fh:
        for line in fh:
            if '"type":"assistant"' not in line.replace(" ", "")[:200] and '"type":"user"' not in line.replace(" ", "")[:200]:
                continue
            try:
                e = json.loads(line)
            except ValueError:
                continue
            if e.get("parent_tool_use_id"):
                continue
            msg = e.get("message") or {}
            content = msg.get("content") if isinstance(msg.get("content"), list) else []
            if e.get("type") == "assistant":
                answers.add(msg.get("id"))
                u = msg.get("usage") or {}
                if first_ctx is None and u:
                    first_ctx = sum(u.get(k) or 0 for k in ("input_tokens", "cache_read_input_tokens",
                                                            "cache_creation_input_tokens"))
                for b in content:
                    if not isinstance(b, dict) or b.get("type") != "tool_use":
                        continue
                    calls += 1
                    inp = b.get("input") or {}
                    target, cmd = str(inp.get("file_path") or ""), str(inp.get("command") or "")
                    if b.get("name") in ("Write", "Edit") and target.replace("\\", "/").endswith("/plan.md"):
                        first_write = first_write or calls
                    if "relay.py" in cmd and re.search(r"\bleg\s+done\b", cmd):
                        done_calls += 1
                        check_ids.add(b.get("id"))
                    for f in set([Path(target).name] if target else [Path(m).name for m in PATHISH.findall(cmd)]):
                        if f and f != "relay.py":
                            files[f] = files.get(f, 0) + 1
            else:
                for b in content:
                    if isinstance(b, dict) and b.get("type") == "tool_result" and b.get("tool_use_id") in check_ids:
                        body = b.get("content")
                        text = body if isinstance(body, str) else " ".join(
                            str(x.get("text", "")) for x in body or [] if isinstance(x, dict))
                        refused += "NOT DONE" in text
    top = max(files.items(), key=lambda kv: kv[1]) if files else ("", 0)
    n = len(answers)
    return {"run": leg.get("run"), "leg": leg.get("leg"), "unit": leg.get("unit"), "phase": leg.get("phase"),
            "role": leg.get("role"), "model": leg.get("ran_model") or leg.get("model"), "cost_usd": leg.get("cost_usd"),
            "said": leg.get("said"), "answers": n, "calls": calls,
            "calls_per_answer": round(calls / n, 2) if n else None, "first_ctx": first_ctx,
            "done_calls": done_calls, "done_refused": refused,
            "late_calls": calls - first_write if first_write else None, "top_file": top[0], "top_file_touches": top[1]}


def flags(m, r):
    """The rules one leg broke: [{flag, what}]."""
    out = []
    cost = m.get("cost_usd") or 0
    if m.get("phase") == "plan" and (cost > r["dear_plan_usd"] or m["answers"] > r["dear_plan_answers"]):
        out.append(("dear-plan", "$%.2f, %d answers" % (cost, m["answers"])))
    if m["done_refused"] >= r["size_fight_refusals"]:
        out.append(("size-fight", "leg done refused %d times" % m["done_refused"]))
    if m.get("phase") == "plan" and (m.get("late_calls") or 0) > r["late_calls"]:
        out.append(("late-calls", "%d tool calls after plan.md was first written" % m["late_calls"]))
    if m["answers"] > r["no_batching_min_answers"] and (m["calls_per_answer"] or 0) < r["no_batching_calls_per_answer"]:
        out.append(("no-batching", "%.2f tool calls an answer over %d answers" % (m["calls_per_answer"], m["answers"])))
    if m["top_file_touches"] >= r["same_file_touches"]:
        out.append(("same-file", "%s named in %d calls" % (m["top_file"], m["top_file_touches"])))
    if (m.get("first_ctx") or 0) > r["heavy_start_tokens"]:
        out.append(("heavy-start", "%d tokens at the first turn" % m["first_ctx"]))
    return [{"flag": f, "what": w, "run": m["run"], "leg": m["leg"], "unit": m["unit"], "phase": m["phase"],
             "cost_usd": m.get("cost_usd")} for f, w in out]


def read_runs(home, names=None, since=None):
    root = Path(home) / "runs"
    runs = sorted(p.name for p in root.iterdir() if (p / "legs").is_dir()) if root.is_dir() else []
    if names:
        runs = [r for r in runs if r in names]
    if since:
        runs = [r for r in runs if r >= since]
    return {run: [m for m in (measure(d) for d in sorted((root / run / "legs").iterdir())) if m] for run in runs}


def write_flags(home, got, r):
    """Keep flags.json beside each run read (runs/<run>/flags.json): its flags and every leg's figures. Returns the
    flags."""
    found = []
    for run, ms in got.items():
        mine = [f for m in ms for f in flags(m, r)]
        (Path(home) / "runs" / run / "flags.json").write_text(json.dumps({"run": run, "flags": mine, "legs": ms}, indent=1),
                                                              encoding="utf-8")
        found += mine
    return found


def med(xs):
    xs = sorted(x for x in xs if x is not None)
    return xs[len(xs) // 2] if xs else None


def summary(legs):
    """Per phase: how many legs, and the middle leg's figures. The read-out's table."""
    out = {}
    for ph in sorted({m["phase"] for m in legs if m.get("phase")}):
        g = [m for m in legs if m["phase"] == ph]
        cost = [m["cost_usd"] for m in g if isinstance(m.get("cost_usd"), (int, float))]
        out[ph] = {"legs": len(g), "mean_usd": round(sum(cost) / len(cost), 2) if cost else None,
                   "answers": med(m["answers"] for m in g), "calls_per_answer": med(m["calls_per_answer"] for m in g),
                   "first_ctx": med(m["first_ctx"] for m in g),
                   "done_refused_mean": round(sum(m["done_refused"] for m in g) / len(g), 2),
                   "late_calls": med(m["late_calls"] for m in g)}
    return out


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("runs", nargs="*")
    p.add_argument("--since")
    p.add_argument("--home")
    p.add_argument("--write", action="store_true")
    p.add_argument("--json", action="store_true")
    a = p.parse_args(argv)
    home, r = Path(a.home or legdir.home()), rules()
    got = read_runs(home, a.runs, a.since)
    legs = [m for ms in got.values() for m in ms]
    found = [f for m in legs for f in flags(m, r)]
    if a.write:
        write_flags(home, got, r)
    if a.json:
        print(json.dumps({"summary": summary(legs), "flags": found}, indent=1))
        return 0
    print("%d runs, %d legs, %d flags" % (len(got), len(legs), len(found)))
    for ph, s in summary(legs).items():
        print("  %-8s %3d legs  mean $%s  answers %s  calls/answer %s  first turn %s tokens  leg done refused %.2f a leg"
              "  calls after first plan write %s" % (ph, s["legs"], s["mean_usd"], s["answers"], s["calls_per_answer"],
                                                     s["first_ctx"], s["done_refused_mean"], s["late_calls"]))
    kinds = {}
    for f in found:
        kinds.setdefault(f["flag"], []).append(f)
    for k, fs in sorted(kinds.items(), key=lambda kv: -len(kv[1])):
        print("  %-12s %3d legs, $%.0f in them" % (k, len(fs), sum(f.get("cost_usd") or 0 for f in fs)))
        for f in sorted(fs, key=lambda f: -(f.get("cost_usd") or 0))[:3]:
            print("      %s-%02d %s (%s): %s" % (f["run"], f["leg"], f["unit"], f["phase"], f["what"]))
    return 0


if __name__ == "__main__":
    sys.exit(main())
