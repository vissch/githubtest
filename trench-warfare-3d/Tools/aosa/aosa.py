#!/usr/bin/env python3
"""aosa.py - the command-line tool of the AOSA optimisation loop. The contract is docs/reference/aosa/README.md.

Commands (run from trench-warfare-3d/, e.g. `python Tools/aosa/aosa.py status`):
  status                      editor lock + slot, player build freshness, budget left, WIP/ready, suggested mode P/E/B
  bench LABEL [...]           run PerfBench (player or editor), repeats, A/B interleaving with --against; --dry-run
  compare A B [--metric M]    the cmp.py table plus the verdict for acceptance rules 1-4 (delta = A - B; B = baseline)
  attempt add JSON|@FILE      validate one attempt, assign its id, append it to attempts.jsonl
  learn                       derive priors.json (classes, files, noise bands) and knobs.json (knob sensitivities)
  pick [--n 3]                rank the ready cards of BACKLOG.md
  age [--moved C1,C2]         age every open card; reset idle on the cards that moved a metric
  budget                      today's image spend per share (creates budget.json with defaults if missing)
  refimg PNG --for juice|tier --prompt-file P [--n 2] [--dry-run]   reference images from fal (budget-gated)
  retro                       the 10-cycle summary the retrospective starts from
  ledger add --mode P|E|B --cards ... --summary "..."              append a LEDGER.md row

Environment: AOSA_DOCS (docs dir, default <repo>/docs/reference/aosa), TW_BUILDS (player builds dir, default the
main clone's trench-warfare-3d/Builds), TW_PROJECT (the Unity project the editor has open, default the main clone's
trench-warfare-3d), AOSA_FALKIT (path of falkit.py), AOSA_OFFLINE=1 (never touch the network, even for pricing).
Stdlib only. ASCII only.
"""
import argparse, datetime, glob, importlib.util, json, os, re, statistics, subprocess, sys, time
from pathlib import Path

sys.dont_write_bytecode = True  # importing editor_lock/falkit by path must not leave __pycache__ in the tree
HERE = Path(__file__).resolve()
REPO = HERE.parents[3]
TOOLS = HERE.parents[1]
MAIN_PROJECT = Path("C:/Users/thomas.visscher_magi/Documents/GitHub/githubtest/trench-warfare-3d")
FALKIT = "C:/Users/thomas.visscher_magi/Documents/claude/emtd-dragon-circle/falkit.py"
ENDPOINT = "fal-ai/nano-banana-pro/edit"

CLASSES = ["cull", "cap", "cache", "batch", "shader", "budget-sweep", "instrument", "juice", "art", "sim-proposal"]
VERDICTS = ["landed", "reverted", "void", "measure-only"]
RULE3 = ["main_ms", "gpu_ms", "draw_calls", "setpass", "gc_bytes", "frame_budget_draws", "frame_budget_vertices"]
FLOOR = ["window.alive_end", "vat_vertices.p50", "vat_shadows_on.p50"]
HIGHER_BETTER = {"window.fps_mean"}
GLOBAL_PRIOR = {"hit_rate": 0.3, "cost": 1.0}
SIZE_COST = {"XS": 1, "S": 1, "M": 2, "L": 3, "XL": 5}
# What one unit of each metric is worth, so deltas in ms, draw calls, bytes and critic points can be ranked against
# each other: a delta is divided by its scale (roughly the budget it is spent against, docs/05). A critic score is
# out of 10. Unknown metrics count at face value. The retrospective may retune these; they are priors, not truths.
METRIC_SCALE = [("main_ms", 3.0), ("gpu_ms", 13.0), ("cpu_frame_ms", 16.6), ("worst_frame", 16.6), ("per_tick", 3.6),
                ("draw_calls", 300.0), ("frame_budget_draws", 300.0), ("setpass", 300.0),
                ("frame_budget_vertices", 1.5e6), ("vat_vertices", 1.5e6), ("vat_shadows_on", 1.0),
                ("gc_bytes", 65536.0), ("texture_mb", 256.0), ("memory", 256.0),
                ("score", 10.0), ("critic", 10.0), ("moment", 10.0), ("env", 10.0), ("rig", 10.0)]


def metric_scale(metric):
    m = (metric or "").strip().lower()
    for key, scale in METRIC_SCALE:
        if key in m:
            return scale
    return 1.0


def card_metric(c):
    return (c.get("metric") or "").split(":")[0]
LEDGER_COLS = ["cycle", "utc", "mode", "cards", "attempts", "result", "gate", "commit", "usd", "note"]
STD_BENCH = [("stress", "1500"), ("settle_ticks", "1800"), ("ticks", "400"), ("warm", "120"), ("ff", "8"), ("vsync", "0")]
BUDGET_DEFAULT = {"daily_cap_usd": 3.0, "shares": {"juice": 0.6667, "tier": 0.3333}, "price_fallback_usd": 0.35,
                  "entries": []}


def docs():
    return Path(os.environ.get("AOSA_DOCS") or REPO / "docs" / "reference" / "aosa")


def builds_dir():
    # The loop measures players built from THIS tree (the lander builds with -projectPath <this project>, and
    # BuildWindows writes Builds/ beside it). A player from the main clone is other code: never the loop's baseline.
    return Path(os.environ.get("TW_BUILDS") or HERE.parents[2] / "Builds")


def project_dir():
    # The loop's editor opens THIS tree's project (lane/show/aosa), never the main clone's: the main clone's editor
    # belongs to other sessions. Its lock is this project's Temp/UnityLockfile.
    return Path(os.environ.get("TW_PROJECT") or HERE.parents[2])


def die(msg, code=2):
    print("aosa: " + msg, file=sys.stderr)
    sys.exit(code)


def utcnow():
    return datetime.datetime.now(datetime.timezone.utc)


def dumps(obj):
    return json.dumps(obj, sort_keys=True, indent=1) + "\n"


def rnd(x):
    return None if x is None else round(float(x), 6)


def num(s):
    """First number in a string ('-0.3', '+2', '-30%', 'n/a' -> None)."""
    m = re.search(r"[-+]?\d+(?:\.\d+)?", str(s or ""))
    return float(m.group(0)) if m else None


def mad(xs):
    m = statistics.median(xs)
    return statistics.median([abs(x - m) for x in xs])


def load_module(name, path):
    spec = importlib.util.spec_from_file_location(name, str(path))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


# ---- markdown tables (BACKLOG.md, LEDGER.md) ------------------------------------------------------------------------

PIPE = re.compile(r"(?<!\\)\|")


def split_row(line):
    """Raw pieces of a table row, outer empties included, so '|'.join(pieces) gives the line back."""
    return PIPE.split(line.rstrip("\n"))


def read_table(path, first_col):
    """(lines, header, rows) where rows are (line_index, [cells]) of the first table whose header starts with first_col."""
    if not path.exists():
        return [], None, []
    lines = path.read_text(encoding="utf-8").splitlines(True)
    header, rows = None, []
    for i, line in enumerate(lines):
        s = line.strip()
        if not s.startswith("|"):
            if header is not None and rows:
                break
            continue
        cells = [c.strip() for c in split_row(s)[1:-1]]
        if header is None:
            if cells and cells[0].lower() == first_col:
                header = [c.lower() for c in cells]
            continue
        if all(set(c) <= set("-: ") for c in cells):
            continue
        rows.append((i, cells))
    return lines, header, rows


def col(header, name):
    for i, h in enumerate(header):
        if h == name or h.startswith(name):
            return i
    return None


def load_cards():
    lines, header, rows = read_table(docs() / "BACKLOG.md", "id")
    if header is None:
        return []
    names = {"id": "id", "class": "class", "tier": "tier", "metric": "metric", "predicted": "predicted",
             "evidence": "evidence", "size": "size", "owner": "owner", "age": "age", "idle": "idle", "status": "status"}
    cards = []
    for i, cells in rows:
        c = {"_line": i}
        for key, name in names.items():
            j = col(header, name)
            c[key] = cells[j] if j is not None and j < len(cells) else ""
        cards.append(c)
    return cards


def ledger_rows():
    _, header, rows = read_table(docs() / "LEDGER.md", "cycle")
    out = []
    for _, cells in rows:
        r = dict(zip(header, cells))
        try:
            r["cycle"] = int(r.get("cycle", ""))
        except ValueError:
            continue
        out.append(r)
    return out


def current_cycle():
    rows = ledger_rows()
    return rows[-1]["cycle"] + 1 if rows else 0


def load_attempts():
    p = docs() / "attempts.jsonl"
    if not p.exists():
        return []
    out = []
    for n, line in enumerate(p.read_text(encoding="utf-8").splitlines(), 1):
        if line.strip():
            try:
                out.append(json.loads(line))
            except ValueError:
                print("aosa: attempts.jsonl line %d is not JSON, skipped" % n, file=sys.stderr)
    return out


def load_json(path, default=None):
    try:
        return json.loads(Path(path).read_text(encoding="utf-8"))
    except Exception:
        return default


# ---- budget ---------------------------------------------------------------------------------------------------------

def load_budget():
    p = docs() / "budget.json"
    b = load_json(p)
    if b is None:
        b = json.loads(json.dumps(BUDGET_DEFAULT))
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(dumps(b), encoding="utf-8")
    for k, v in BUDGET_DEFAULT.items():
        b.setdefault(k, v)
    return b


def budget_today(b):
    """{share: (cap, spent, left)} for today (UTC)."""
    today = utcnow().strftime("%Y-%m-%d")
    out = {}
    for share, frac in sorted(b["shares"].items()):
        cap = b["daily_cap_usd"] * frac
        spent = sum(float(e.get("usd") or 0) for e in b["entries"]
                    if e.get("share") == share and str(e.get("utc", "")).startswith(today))
        out[share] = (cap, spent, cap - spent)
    return out


def cmd_budget(a):
    b = load_budget()
    print("budget %s UTC: cap $%.2f/day, fallback $%.2f/image" % (utcnow().strftime("%Y-%m-%d"), b["daily_cap_usd"],
                                                                   b["price_fallback_usd"]))
    for share, (cap, spent, left) in budget_today(b).items():
        print("  %-6s spent $%.2f of $%.2f, left $%.2f" % (share, spent, cap, left))
    print("  all-time entries %d, $%.2f" % (len(b["entries"]), sum(float(e.get("usd") or 0) for e in b["entries"])))
    return 0


# ---- status ---------------------------------------------------------------------------------------------------------

def git(*args):
    r = subprocess.run(["git", "-C", str(REPO)] + list(args), capture_output=True, text=True)
    return r.returncode, r.stdout.strip()


def build_freshness(name):
    info = load_json(builds_dir() / name / "build-info.json")
    if not info:
        return None, "%-11s missing (%s)" % (name, builds_dir() / name)
    sha = info.get("git_sha", "")
    rc, _ = git("merge-base", "--is-ancestor", sha, "HEAD")
    if rc == 0:
        _, n = git("rev-list", "--count", sha + "..HEAD")
        n = int(n or 0)
        state = "fresh" if n == 0 else "STALE (%d commits behind HEAD)" % n
        stale = n > 0
    else:
        state, stale = "STALE (not an ancestor of HEAD)", True
    dirty = info.get("dirty_files") or 0
    return stale, "%-11s %s built %s %s%s" % (name, sha[:7], info.get("built_utc", "?"), state,
                                              ", %d dirty files" % dirty if dirty else "")


def editor_state():
    """(free_for_aosa, text). free_for_aosa: the lock is free and no one else claims the slot."""
    try:
        el = load_module("editor_lock", TOOLS / "editor_lock.py")
        pr = el.probe(project_dir())
        slot = el.read_slot()
    except BaseException as e:
        return False, "unknown (editor_lock failed: %s)" % e, None
    text = "%s - %s (%s)" % (pr["state"].upper(), pr["detail"], project_dir())
    # The shared slot queues the MAIN clone's editor. The loop opens only its own project, so the slot is shown for
    # information and never blocks it; only this project's lockfile does.
    if slot:
        mins = max(0, int((slot.get("until", 0) - time.time()) / 60))
        text += "; main clone's slot: %s for ~%d min (does not concern this tree)" % (slot.get("who"), mins)
    return pr["state"] == "free", text, slot


def cmd_status(a):
    _, head = git("rev-parse", "--short", "HEAD")
    print("AOSA status %s  HEAD %s  cycle %d" % (utcnow().strftime("%Y-%m-%dT%H:%MZ"), head or "?", current_cycle()))
    free, etext, _ = editor_state()
    print("editor : " + etext)
    try:
        procs = load_module("editor_lock", TOOLS / "editor_lock.py").holders()
        big = [p for p in procs if (p.get("mb") or 0) >= 300]
        print("machine: %d Unity process(es), %d large (%s)%s" % (
            len(procs), len(big), ", ".join("pid %s %s MB%s" % (p.get("pid"), p.get("mb"), " batch" if p.get("batch") else "")
                                           for p in big) or "-",
            "; benches will be noisy, rely on interleaving and the band" if big else ""))
    except Exception as e:
        print("machine: unknown (%s)" % e)
    stale_any = False
    for i, name in enumerate(["WinBench", "WinBenchDev"]):
        stale, text = build_freshness(name)
        stale_any = stale_any or stale is None or stale
        print(("build  : " if i == 0 else "         ") + text)
    try:
        print("budget : " + ", ".join("%s $%.2f left of $%.2f" % (share, left, cap)
                                      for share, (cap, spent, left) in budget_today(load_budget()).items()))
    except Exception as e:
        print("budget : unreadable (%s)" % e)
    cards = load_cards()
    count = lambda s: sum(1 for c in cards if c["status"] == s)
    print("backlog: WIP %d/3, ready %d, blocked:editor %d, cards %d" % (count("wip"), count("ready"),
                                                                         count("blocked:editor"), len(cards)))
    if free and stale_any:
        mode, why = "B", "the editor is free and a player build is older than HEAD"
    elif free:
        mode, why = "E", "this tree's project is free: open the loop's own editor on it (never claim the shared slot)"
    else:
        mode, why = "P", "the editor is held or claimed" + ("; rebuild (B) once it frees" if stale_any else "")
    print("mode   : %s - %s" % (mode, why))
    return 0


# ---- reports --------------------------------------------------------------------------------------------------------

def canon(metric):
    return metric[7:] if metric.startswith("series.") else metric


def get_metric(report, metric):
    m = canon(metric)
    parts = m.split(".")
    if parts[0] in ("window", "run", "config", "scenario", "machine", "build_info"):
        node = report
    elif parts[0] == "per_tick":
        return (report.get("per_tick_ms") or {}).get(".".join(parts[1:]))
    else:
        node, parts = report.get("series") or {}, [".".join(parts[:-1]), parts[-1]]
    for p in parts:
        if not isinstance(node, dict) or p not in node:
            return None
        node = node[p]
    return node if isinstance(node, (int, float)) and not isinstance(node, bool) else None


def cycle_key(d):
    return (1, int(d.name)) if d.name.isdigit() else (0, d.name)


def resolve_reports(spec):
    """A file path, a glob, or a label (the newest runs/<cycle>/ holding <label>-N.json or <label>.json)."""
    if os.path.isfile(spec):
        return [Path(spec)]
    if any(ch in spec for ch in "*?["):
        return sorted(Path(p) for p in glob.glob(spec))
    runs = docs() / "runs"
    dirs = sorted([d for d in runs.glob("*") if d.is_dir()], key=cycle_key, reverse=True) if runs.exists() else []
    for d in dirs:
        hits = [p for p in d.glob(glob.escape(spec) + "*.json")
                if re.fullmatch(re.escape(spec) + r"(-\d+)?\.json", p.name)]
        if hits:
            return sorted(hits, key=lambda p: [int(t) if t.isdigit() else t for t in re.split(r"(\d+)", p.name)])
    return []


def load_reports(spec):
    out = []
    for p in resolve_reports(spec):
        r = load_json(p)
        if isinstance(r, dict) and r.get("schema") == "tw-perf/1":
            r["_path"] = str(p)
            out.append(r)
    return out


def cmp_table(runs):
    """The cmp.py table, ported (cmp.py runs at import time, so it cannot be imported)."""
    lines = []
    labels = [r["run"]["label"] for r in runs]
    lines.append("%-26s " % "" + " ".join("%24s" % l[:24] for l in labels))

    def row(name, get):
        vals = []
        for r in runs:
            try:
                vals.append(get(r))
            except Exception:
                vals.append(None)
        lines.append("%-26s " % name + " ".join("%24s" % ("-" if v is None else v) for v in vals))
    row("hash_start", lambda r: r["window"]["hash_start"])
    row("hash_end", lambda r: r["window"]["hash_end"])
    row("build", lambda r: r["run"]["build"])
    row("ticks", lambda r: "%d..%d" % (r["window"]["tick_start"], r["window"]["tick_end"]))
    row("alive start/end", lambda r: "%d/%d" % (r["window"]["alive_start"], r["window"]["alive_end"]))
    row("fps mean", lambda r: "%.1f" % r["window"]["fps_mean"])
    row("gc collections", lambda r: r["window"]["gc_collections"])
    row("hitches >33ms (cap 64)", lambda r: len(r["window"]["hitches_over_33ms"]))
    for k in ["cpu_frame_ms", "main_ms", "main_ms_tick_frames", "main_ms_idle_frames", "render_ms", "gpu_ms",
              "draw_calls", "setpass", "gc_bytes", "gc_count", "vat_vertices", "vat_shadows_on",
              "frame_budget_draws", "frame_budget_vertices"]:
        if any(k in r.get("series", {}) for r in runs):
            row(k + " p50/p95/p99", lambda r, k=k: "%s/%s/%s" % tuple(("%.4g" % r["series"][k][q]) for q in ("p50", "p95", "p99")))
    ticks = [r.get("per_tick_ms") or {} for r in runs]
    keys = sorted({k for t in ticks for k in t}, key=lambda k: -max(t.get(k, 0) for t in ticks))
    lines.append("per tick ms (all worlds):")
    for k in keys:
        if max(t.get(k, 0) for t in ticks) >= 0.05:
            row("  " + k.replace("TW.Sim.Sys.", "Sys."), lambda r, k=k: "%.3f" % r["per_tick_ms"].get(k, float("nan")))
    return lines


def band_for(metric, build, a_vals, b_vals, priors):
    pb = ((priors or {}).get("bands") or {}).get(canon(metric), {}).get(build)
    if pb and pb.get("n", 0) >= 3 and pb.get("band") is not None:
        return pb["band"], "priors (n=%d)" % pb["n"]
    if len(a_vals) >= 3 and len(b_vals) >= 3:
        devs = [x - statistics.median(a_vals) for x in a_vals] + [x - statistics.median(b_vals) for x in b_vals]
        return 3 * statistics.median([abs(d) for d in devs]), "3*MAD of repeats"
    spread = max(max(v) - min(v) for v in (a_vals, b_vals))
    return spread, "min-max spread" if max(len(a_vals), len(b_vals)) > 1 else "none (1 sample each)"


def verdict(A, B, target="main_ms.p95", higher_better=None, priors=None):
    """Rules 1-4 over two groups of tw-perf/1 reports. A is the candidate, B the baseline; delta = median A - median B."""
    for grp in (A, B):
        if not grp:
            return {"verdict": "refused", "why": "no reports"}
    builds = {r["run"].get("build") for r in A + B}
    gpus = {(r.get("machine") or {}).get("gpu") for r in A + B}
    if len(builds) > 1:
        return {"verdict": "refused", "why": "build types differ: %s" % sorted(map(str, builds))}
    if len(gpus) > 1:
        return {"verdict": "refused", "why": "machine.gpu differs: %s" % sorted(map(str, gpus))}
    build = builds.pop()
    hb = HIGHER_BETTER if higher_better is None else set(higher_better)
    rules, failed = {}, []

    hs = {r["window"].get("hash_start") for r in A + B}
    he = [r["window"].get("hash_end") for r in A + B]
    same = len(hs) == 1
    why = "hash_start %s" % ("equal" if same else "differs: " + " vs ".join(sorted(map(str, hs))))
    if all(h is not None for h in he):
        same = same and len(set(he)) == 1
        why += "; hash_end " + ("equal" if len(set(he)) == 1 else "differs: " + " vs ".join(sorted(set(he))))
        het = {r["window"].get("hash_end_tick") for r in A + B}
        if len(het) > 1:  # PerfBench: compare hash_end only between runs whose hash_end_tick is the same
            same = False
            why += " (taken at different ticks %s: rerun)" % sorted(map(str, het))
    else:
        why += "; hash_end not in every report"
    rules["1"] = {"ok": same, "why": ("same battle: " if same else "different battle: ") + why}

    rows = {}

    def measure(metric):
        av = [v for v in (get_metric(r, metric) for r in A) if v is not None]
        bv = [v for v in (get_metric(r, metric) for r in B) if v is not None]
        if not av or not bv:
            return None
        band, src = band_for(metric, build, av, bv, priors)
        d = statistics.median(av) - statistics.median(bv)
        row = {"metric": canon(metric), "a": rnd(statistics.median(av)), "b": rnd(statistics.median(bv)),
               "delta": rnd(d), "band": rnd(band), "band_source": src, "n_a": len(av), "n_b": len(bv)}
        rows[canon(metric)] = row
        return row

    n_ok = len(A) >= 3 and len(B) >= 3
    rules["2"] = {"ok": n_ok, "why": "%d vs %d repeats%s" % (len(A), len(B), "" if n_ok else " (need >= 3 each)")}

    t = measure(target)
    sign = 1 if canon(target) in hb else -1
    if t is None:
        rules["3"] = {"ok": False, "why": "target %s missing" % canon(target)}
    else:
        t["improved"] = sign * t["delta"] > t["band"]
        regress = []
        for k in RULE3:
            m = k + ".p95"
            if m == canon(target):
                continue
            r = measure(m)
            if r is not None:
                r["regressed"] = r["delta"] > r["band"]
                if r["regressed"]:
                    regress.append(m)
        ok = t["improved"] and not regress
        rules["3"] = {"ok": ok, "why": "target %s delta %+.4g vs band %.4g (%s)%s" % (
            canon(target), t["delta"], t["band"], "improved" if t["improved"] else "not beyond the band",
            "; regressed: " + ", ".join(regress) if regress else "; no other metric rose beyond its band"),
            "regressed": regress}
    fell = []
    for m in FLOOR:
        r = measure(m)
        if r is not None:
            r["fell"] = -r["delta"] > r["band"]
            if r["fell"]:
                fell.append(m)
    rules["4"] = {"ok": not fell, "why": "fell: " + ", ".join(fell) if fell else "alive_end, vat_vertices, vat_shadows_on hold"}
    failed = [int(k) for k in sorted(rules) if not rules[k]["ok"]]
    return {"verdict": "pass" if not failed else "fail", "failed": failed, "rules": rules, "build": build,
            "gpu": gpus.pop(), "target": rows.get(canon(target)), "metrics": [rows[k] for k in sorted(rows)],
            "a": {"n": len(A), "files": [r["_path"] for r in A]}, "b": {"n": len(B), "files": [r["_path"] for r in B]}}


def cmd_compare(a):
    A, B = load_reports(a.A), load_reports(a.B)
    if not A or not B:
        die("no tw-perf/1 reports for %s" % (a.A if not A else a.B))
    priors = load_json(docs() / "priors.json", {})
    hb = HIGHER_BETTER | ({canon(a.metric)} if a.higher_better else set())
    v = verdict(A, B, a.metric, hb, priors)
    if a.json:
        print(json.dumps(v, sort_keys=True, indent=1))
        return 0 if v["verdict"] == "pass" else (2 if v["verdict"] == "refused" else 1)
    runs = A + B if len(A) + len(B) <= 6 else [A[0], B[0]]
    print("\n".join(cmp_table(runs)))
    if v["verdict"] == "refused":
        print("VERDICT: REFUSED - " + v["why"])
        return 2
    print("\nA = %s (%d)   B = %s (%d, baseline)   build %s   delta = A - B" % (a.A, len(A), a.B, len(B), v["build"]))
    print("%-28s %12s %12s %12s %10s  %s" % ("metric", "A median", "B median", "delta", "band", "band from"))
    for r in v["metrics"]:
        flag = " <- target" if v["target"] is r else (" REGRESSED" if r.get("regressed") else (" FELL" if r.get("fell") else ""))
        print("%-28s %12.5g %12.5g %+12.4g %10.4g  %s%s" % (r["metric"], r["a"], r["b"], r["delta"], r["band"], r["band_source"], flag))
    for k in sorted(v["rules"]):
        print("rule %s %s: %s" % (k, "ok  " if v["rules"][k]["ok"] else "FAIL", v["rules"][k]["why"]))
    print("VERDICT: %s" % ("PASS (rules 1-4 hold)" if v["verdict"] == "pass" else
                          "FAIL rule " + ",".join(map(str, v["failed"]))))
    return 0 if v["verdict"] == "pass" else 1


# ---- bench ----------------------------------------------------------------------------------------------------------

def bench_string(spec, label, out, shot):
    pairs = list(STD_BENCH) + ([("quality", "5")] if spec["mode"] == "player" else []) + [("canary", "0")]
    if spec.get("scenario"):
        pairs.append(("scenario", spec["scenario"]))
    if spec.get("knobs"):
        pairs.append(("knobs", "|".join(k.strip() for k in spec["knobs"].split(",") if k.strip())))
    for tok in (spec.get("extra") or "").split():
        k = tok.split("=", 1)[0]
        pairs = [p for p in pairs if p[0] != k] if "=" in tok else pairs
        pairs.append((k, tok.split("=", 1)[1]) if "=" in tok else (tok, None))
    pairs += [("label", label), ("out", out), ("shot", shot)]
    return " ".join(k if v is None else "%s=%s" % (k, v) for k, v in pairs)


def next_index(d, label):
    idx = [int(m.group(1)) for p in d.glob("*.json") for m in [re.fullmatch(re.escape(label) + r"-(\d+)\.json", p.name)] if m]
    return max(idx, default=0) + 1


def find_args(label, cyc_dir):
    p = cyc_dir / (label + ".args.json")
    if p.exists():
        return load_json(p)
    hits = sorted((docs() / "runs").glob("*/" + glob.escape(label) + ".args.json"), key=lambda q: q.stat().st_mtime)
    return load_json(hits[-1]) if hits else None


def unity_exe():
    return str(Path(os.environ.get("LOCALAPPDATA", "")) / "unity" / "bin" / "unity.exe")


def run_one(spec, label, cyc_dir, dry):
    out = (cyc_dir / (label + ".json")).resolve()
    shot, log = out.with_suffix(".png"), out.with_suffix(".log")
    tw = bench_string(spec, label, out.as_posix(), shot.as_posix())
    if spec["mode"] == "player":
        exe = Path(spec["build_dir"]) / "TrenchWarfare.exe"
        cmd = [str(exe), "-screen-fullscreen", "0", "-screen-width", "1920", "-screen-height", "1080",
               "-logFile", str(log), "-twbench", tw]
        if dry:
            print("player: " + subprocess.list2cmdline(cmd) + "   (timeout 1200 s)" + ("" if exe.exists() else "  [exe missing]"))
            return True
        if not exe.exists():
            print("%s: no player at %s" % (label, exe))
            return False
        t0 = time.time()
        try:
            rc = subprocess.run(cmd, timeout=1200).returncode
        except subprocess.TimeoutExpired:
            rc = "timeout"
        print("%s: player exit %s after %d s" % (label, rc, time.time() - t0))
    else:
        idle = [unity_exe(), "command", "--timeout", "10", "eval",
                'return UnityEditor.EditorApplication.isPlaying + " " + UnityEditor.EditorApplication.isCompiling;']
        bench = [unity_exe(), "command", "--timeout", "60", "eval", 'return TW.Editor.CaptureRig.Bench("%s");' % tw]
        if dry:
            print("editor (cwd %s): wait up to 60x3 s until %s -> \"False False\"" % (project_dir(), subprocess.list2cmdline(idle)))
            print("editor: " + subprocess.list2cmdline(bench) + "   then poll %s up to 1200 s" % out)
            return True
        for _ in range(60):
            r = subprocess.run(idle, capture_output=True, text=True, cwd=str(project_dir()))
            if '"result":"False False"' in r.stdout:
                break
            time.sleep(3)
        r = subprocess.run(bench, capture_output=True, text=True, cwd=str(project_dir()))
        print("%s: %s" % (label, (re.findall(r'"result":"[^"]*"', r.stdout) or [r.stdout.strip()[:80]])[0][:60]))
        for _ in range(240):
            if out.exists():
                break
            time.sleep(5)
    if out.exists():
        rep = load_json(out, {})
        print("%s: report written, main_ms.p95 %s, hash_start %s" % (label, get_metric(rep, "main_ms.p95"),
                                                                     (rep.get("window") or {}).get("hash_start")))
        return True
    tail = log.read_text(errors="replace").splitlines()[-20:] if log.exists() else []
    print("%s: NO REPORT%s" % (label, "; log tail:\n  " + "\n  ".join(tail) if tail else ""))
    return False


def cmd_bench(a):
    cyc = a.cycle if a.cycle is not None else current_cycle()
    cyc_dir = docs() / "runs" / str(cyc)
    mode = "editor" if a.editor else "player"
    spec = {"label": a.label, "cycle": cyc, "mode": mode, "dev": a.dev, "scenario": a.scenario, "knobs": a.knobs,
            "extra": a.extra, "build_dir": str(Path(a.build_dir) if a.build_dir else builds_dir() / ("WinBenchDev" if a.dev else "WinBench"))}
    other = None
    if a.against:
        other = find_args(a.against, cyc_dir)
        if not other:
            die("no recorded args for label %s (runs/*/%s.args.json)" % (a.against, a.against))
        if other.get("mode") != mode:
            die("--against %s is a %s bench, this one is %s" % (a.against, other.get("mode"), mode))
    if mode == "editor":
        # An editor bench talks to the editor open on THIS tree's project (the loop's own). If none is open, there is
        # nothing to talk to; the main clone's editor is never used.
        try:
            state = load_module("editor_lock", TOOLS / "editor_lock.py").probe(project_dir())["state"]
        except BaseException:
            state = "unknown"
        if state == "free" and not a.dry_run:
            die("no editor is open on %s; open the loop's own editor first (README mode E)" % project_dir(), 3)
    n = a.repeats or (3 if a.against else 1)
    plan = []
    ia = next_index(cyc_dir, a.label) if cyc_dir.exists() else 1
    ib = next_index(cyc_dir, a.against) if (a.against and cyc_dir.exists()) else 1
    for i in range(n):
        plan.append((spec, "%s-%d" % (a.label, ia + i)))
        if other:
            plan.append((other, "%s-%d" % (a.against, ib + i)))
    print("bench cycle %d, %d runs, %s%s -> %s" % (cyc, len(plan), mode, " (dry run)" if a.dry_run else "", cyc_dir))
    if not a.dry_run:
        cyc_dir.mkdir(parents=True, exist_ok=True)
        (cyc_dir / (a.label + ".args.json")).write_text(dumps(spec), encoding="utf-8")
        if other and not (cyc_dir / (a.against + ".args.json")).exists():
            (cyc_dir / (a.against + ".args.json")).write_text(dumps(other), encoding="utf-8")
    ok = all([run_one(s, lab, cyc_dir, a.dry_run) for s, lab in plan])
    if a.against:
        print("next: python Tools/aosa/aosa.py compare %s %s" % (a.label, a.against))
    return 0 if ok else 1


# ---- attempts / learn -----------------------------------------------------------------------------------------------

def validate_attempt(x):
    errs = []
    for k in ("cycle", "card", "class", "verdict"):
        if k not in x:
            errs.append("missing %s" % k)
    if "cycle" in x and not isinstance(x["cycle"], int):
        errs.append("cycle must be an int")
    if x.get("class") not in CLASSES:
        errs.append("class %r not in %s" % (x.get("class"), CLASSES))
    if x.get("verdict") not in VERDICTS:
        errs.append("verdict %r not in %s" % (x.get("verdict"), VERDICTS))
    rule = x.get("rule")
    if rule is not None and not re.fullmatch(r"[1-9]", str(rule)):
        errs.append("rule %r is not an acceptance-rule number 1-9" % rule)
    if x.get("verdict") == "reverted" and rule is None:
        errs.append("a reverted attempt needs its rule")
    p = x.get("predicted")
    if not x.get("seed") and x.get("verdict") != "void":  # rule 9: seeds and void attempts may lack a prediction
        if not isinstance(p, dict) or not p.get("metric"):
            errs.append("predicted {metric, delta} is required (rule 9)")
        elif not isinstance(p.get("delta"), (int, float)) and x.get("verdict") != "measure-only":
            errs.append("predicted.delta must be a number")
    for k in ("measured", "cost"):
        if x.get(k) is not None and not isinstance(x[k], dict):
            errs.append("%s must be an object" % k)
    if not isinstance(x.get("files", []), list):
        errs.append("files must be a list")
    return errs


def add_attempt(x):
    errs = validate_attempt(x)
    if errs:
        die("attempt rejected: " + "; ".join(errs))
    have = load_attempts()
    if not x.get("id"):
        x["id"] = "a%04d" % (max([int(h["id"][1:]) for h in have if re.fullmatch(r"a\d+", str(h.get("id")))], default=0) + 1)
    if any(h.get("id") == x["id"] for h in have):
        die("attempt id %s already exists" % x["id"])
    for k, v in (("files", []), ("knob", None), ("measured", None), ("rule", None), ("commit", None),
                 ("cost", {"cycles": 1, "editor_min": None, "usd": 0.0}), ("lesson", None), ("owner_verdict", None)):
        x.setdefault(k, v)
    p = docs() / "attempts.jsonl"
    p.parent.mkdir(parents=True, exist_ok=True)
    with open(p, "a", encoding="utf-8") as f:
        f.write(json.dumps(x, sort_keys=False) + "\n")
    return x["id"]


def cmd_attempt(a):
    s = a.json
    if s == "@-":
        s = sys.stdin.read()
    elif s.startswith("@"):
        s = Path(s[1:]).read_text(encoding="utf-8")
    try:
        x = json.loads(s)
    except ValueError as e:
        die("not JSON: %s" % e)
    print("added " + add_attempt(x))
    return 0


def stats(group):
    judged = [x for x in group if x.get("verdict") in ("landed", "reverted")]
    landed = [x for x in judged if x["verdict"] == "landed"]
    # gains are normalised by their metric's scale so a class mixing ms and bytes still has one meaningful median
    gains = [abs(x["measured"]["delta"]) / metric_scale(x["measured"].get("metric")) for x in landed
             if isinstance((x.get("measured") or {}).get("delta"), (int, float))]
    costs = [x["cost"]["cycles"] for x in group if isinstance((x.get("cost") or {}).get("cycles"), (int, float))]
    ratios = []
    for x in group:
        if x.get("seed"):
            continue  # seeds carry no prediction
        p, m = x.get("predicted") or {}, x.get("measured") or {}
        if isinstance(p.get("delta"), (int, float)) and p["delta"] and isinstance(m.get("delta"), (int, float)):
            ratios.append(m["delta"] / p["delta"])
    return {"n": len(group), "n_judged": len(judged), "hit_rate": rnd(len(landed) / len(judged)) if judged else None,
            "median_gain": rnd(statistics.median(gains)) if gains else None,
            "median_cost_cycles": rnd(statistics.median(costs)) if costs else None,
            "calibration": rnd(statistics.median(ratios)) if ratios else None, "n_calibration": len(ratios)}


def all_reports():
    out = []
    for p in sorted((docs() / "runs").glob("*/*.json")):
        r = load_json(p)
        if isinstance(r, dict) and r.get("schema") == "tw-perf/1":
            r["_path"], r["_group"] = str(p), (p.parent.name, re.sub(r"[-_]\d+$", "", p.stem))
            out.append(r)
    return out


def knob_set(r):
    s = ((r.get("config") or {}).get("knobs") or {}).get("set")
    if isinstance(s, str):
        s = dict(t.split("=", 1) for t in re.split(r"[|,]", s) if "=" in t)
    return s if isinstance(s, dict) else {}


def cmd_learn(a):
    att = load_attempts()
    by_class, by_file = {}, {}
    for x in att:
        by_class.setdefault(x.get("class"), []).append(x)
        for f in x.get("files") or []:
            by_file.setdefault(f, []).append(x)
    # Noise bands. A group is the reports of one runs/<cycle>/ dir whose file stems match once a trailing -N or _N is
    # removed (c7-a-1, c7-a-2 ...), with one build type and one hash_start and at least 2 members. Every member's
    # deviation from its group median is pooled per metric and build type; band = 3 * median |deviation|.
    groups = {}
    for r in all_reports():
        groups.setdefault(r["_group"], []).append(r)
    metrics = sorted({"window.alive_end", "window.fps_mean"} |
                     {"%s.%s" % (k, q) for g in groups.values() for r in g for k in r.get("series", {}) for q in ("p50", "p95")})
    devs = {}
    for key, g in sorted(groups.items()):
        if len(g) < 2 or len({r["run"].get("build") for r in g}) > 1 or len({r["window"].get("hash_start") for r in g}) > 1:
            continue
        build = g[0]["run"].get("build")
        for m in metrics:
            vals = [v for v in (get_metric(r, m) for r in g) if v is not None]
            if len(vals) >= 2:
                med = statistics.median(vals)
                d = devs.setdefault(m, {}).setdefault(build, {"devs": [], "groups": set()})
                d["devs"] += [abs(v - med) for v in vals]
                d["groups"].add("/".join(key))
    bands = {m: {b: {"band": rnd(3 * statistics.median(d["devs"])), "n": len(d["devs"]), "groups": sorted(d["groups"])}
                 for b, d in bd.items()} for m, bd in devs.items()}
    priors = {"global": GLOBAL_PRIOR, "n_attempts": len(att),
              "classes": {c: stats(g) for c, g in by_class.items() if c},
              "files": {f: stats(g) for f, g in by_file.items()}, "bands": bands,
              "band_grouping": "runs/<cycle>/<stem>-N.json grouped by (cycle dir, stem); same build and hash_start; "
                               ">= 2 members; band = 3 * median |value - group median| pooled per metric and build"}
    # Knob sensitivities from reports that set exactly one knob.
    knobs = {}
    for r in all_reports():
        s = knob_set(r)
        if len(s) != 1:
            continue
        (k, v), = s.items()
        v = num(v)
        if v is None:
            continue
        kb = knobs.setdefault(k, {}).setdefault(r["run"].get("build", "?"), {"n": 0, "points": {}, "sensitivity": {}})
        kb["n"] += 1
        for m in ("main_ms.p95", "gpu_ms.p95", "draw_calls.p95", "vat_vertices.p50", "frame_budget_vertices.p95"):
            y = get_metric(r, m)
            if y is not None:
                kb["points"].setdefault(m, []).append([rnd(v), rnd(y)])
    for k in knobs.values():
        for kb in k.values():
            for m, pts in kb["points"].items():
                pts.sort()
                xs, ys = [p[0] for p in pts], [p[1] for p in pts]
                mx, my = sum(xs) / len(xs), sum(ys) / len(ys)
                sxx = sum((x - mx) ** 2 for x in xs)
                kb["sensitivity"][m] = rnd(sum((x - mx) * (y - my) for x, y in zip(xs, ys)) / sxx) if sxx else None
    (docs() / "priors.json").write_text(dumps(priors), encoding="utf-8")
    (docs() / "knobs.json").write_text(dumps(knobs), encoding="utf-8")
    print("priors.json: %d attempts, %d classes, %d files, %d banded metrics; knobs.json: %d knobs" % (
        len(att), len(priors["classes"]), len(priors["files"]), len(bands), len(knobs)))
    for c, s in sorted(priors["classes"].items()):
        print("  %-13s n %2d judged %2d hit %-5s gain %-8s cost %-4s calib %s" % (
            c, s["n"], s["n_judged"], s["hit_rate"], s["median_gain"], s["median_cost_cycles"], s["calibration"]))
    return 0


# ---- pick / age -----------------------------------------------------------------------------------------------------

def int0(s):
    try:
        return int(str(s).strip())
    except ValueError:
        return 0


def cmd_pick(a):
    cards = load_cards()
    classes = (load_json(docs() / "priors.json", {}) or {}).get("classes", {})
    wip = sum(1 for c in cards if c["status"] == "wip")
    ok_status = {"ready"} | ({"blocked:editor"} if a.mode == "E" else set())
    cand = [c for c in cards if c["status"] in ok_status and c["owner"] in ("loop", "")]
    preds = [abs(num(c["predicted"])) / metric_scale(card_metric(c)) for c in cand if num(c["predicted"]) is not None]
    typical = statistics.median(preds) if preds else 1.0
    for c in cand:
        pr = classes.get(c["class"]) or {}
        hit = pr.get("hit_rate") if pr.get("hit_rate") is not None else GLOBAL_PRIOR["hit_rate"]
        size = c["size"].strip().upper()
        cost = SIZE_COST.get(size) or num(size) or pr.get("median_cost_cycles") or GLOBAL_PRIOR["cost"]
        p = num(c["predicted"])
        c["_est"] = p is None
        c["_pred"] = abs(p) / metric_scale(card_metric(c)) if p is not None else typical
        c["_hit"], c["_cost"] = hit, cost
        c["_score"] = hit * c["_pred"] / max(cost, 1)
        cal = pr.get("calibration")
        c["_bad_cal"] = cal is not None and not (0.5 <= cal <= 2) and c["class"] != "instrument"
        idle = int0(c["idle"])
        c["_note"] = ("SPLIT (idle %d)" % idle if idle >= 3 else "measure next" if idle >= 2 else "") + \
                     (" demoted: calibration %.2f" % cal if c["_bad_cal"] else "") + (" est. delta" if c["_est"] else "")
    inst = [c["_score"] for c in cand if c["class"] == "instrument"]
    floor = min(inst) if inst else None
    for c in cand:
        if c["_bad_cal"] and floor is not None and c["_score"] >= floor:
            c["_score"] = floor * 0.999  # below every instrument card: a model that mispredicts needs measuring
    cand.sort(key=lambda c: (-c["_score"], c["id"]))
    picked, juice = [], 0
    for c in cand:
        if len(picked) >= a.n:
            break
        if c["class"] == "juice":
            if juice:
                continue
            juice += 1
        picked.append(c["id"])
    print("pick: %d candidates, WIP %d/3%s; score = hit_rate x |predicted|/scale(metric) / max(cost,1)" % (
        len(cand), wip, " (WIP cap reached)" if wip >= 3 else ""))
    print("%-4s %-5s %-12s %-6s %8s %5s %5s %8s  %s" % ("pick", "id", "class", "tier", "|pred|", "hit", "cost", "score", "note"))
    for c in cand:
        print("%-4s %-5s %-12s %-6s %8.4g %5.2f %5.3g %8.4g  %s" % (
            "*" if c["id"] in picked else "", c["id"], c["class"], c["tier"][:6], c["_pred"], c["_hit"], c["_cost"],
            c["_score"], c["_note"].strip()))
    print("picked: " + (", ".join(picked) or "-"))
    return 0


def cmd_age(a):
    path = docs() / "BACKLOG.md"
    lines, header, rows = read_table(path, "id")
    if header is None:
        die("no card table in %s" % path, 1)
    moved = {m.strip() for m in (a.moved or "").split(",") if m.strip()}
    ia, ii, ist, iid = col(header, "age"), col(header, "idle"), col(header, "status"), col(header, "id")
    seen = set()
    for li, cells in rows:
        if cells[ist].startswith("done"):
            continue
        pieces = split_row(lines[li])  # pieces[1 + j] is cell j, raw
        seen.add(cells[iid])
        pieces[1 + ia] = " %d " % (int0(cells[ia]) + 1)
        pieces[1 + ii] = " %d " % (0 if cells[iid] in moved else int0(cells[ii]) + 1)
        lines[li] = "|".join(pieces) + ("\n" if lines[li].endswith("\n") else "")
    path.write_text("".join(lines), encoding="utf-8")
    for m in sorted(moved - seen):
        print("aosa: warning: --moved %s is not an open card" % m, file=sys.stderr)
    print("aged %d cards; idle reset on %s" % (len(seen), ", ".join(sorted(moved & seen)) or "-"))
    return 0


# ---- refimg ---------------------------------------------------------------------------------------------------------

def unit_price(fk, fallback):
    if os.environ.get("AOSA_OFFLINE") or fk is None:
        return fallback, "fallback (offline)"
    try:
        res = fk.pricing([ENDPOINT])
        for p in res.get("prices", []) if isinstance(res, dict) else []:
            if p.get("endpoint_id") == ENDPOINT and isinstance(p.get("unit_price"), (int, float)):
                return float(p["unit_price"]), "fal pricing (per %s)" % p.get("unit", "?")
    except BaseException as e:
        if isinstance(e, KeyboardInterrupt):
            raise
        return fallback, "fallback (pricing failed: %s)" % str(e)[:80]
    return fallback, "fallback (endpoint not in pricing)"


def cmd_refimg(a):
    cap = Path(a.capture)
    if not cap.exists():
        die("no capture at %s" % cap)
    prompt = Path(a.prompt_file).read_text(encoding="utf-8").strip()
    b = load_budget()
    share = a.share
    if share not in b["shares"]:
        die("share %s not in budget.json" % share)
    fk = None
    if not os.environ.get("AOSA_OFFLINE"):
        try:
            fk = load_module("falkit", os.environ.get("AOSA_FALKIT") or FALKIT)
        except BaseException as e:
            print("aosa: falkit not loadable (%s)" % e, file=sys.stderr)
            if not a.dry_run:
                die("cannot reach fal without falkit", 1)
    price, src = unit_price(fk, b["price_fallback_usd"])
    est = price * a.n
    capd, spent, left = budget_today(b)[share]
    print("refimg %s share: $%.2f left of $%.2f today; estimate %d x $%.3f = $%.2f (%s)" % (share, left, capd, a.n, price, est, src))
    if left < est:
        print("aosa: refused - the %s share has $%.2f left today, this needs $%.2f" % (share, left, est), file=sys.stderr)
        return 4
    cyc = a.cycle if a.cycle is not None else current_cycle()
    refs = docs() / "runs" / str(cyc) / "refs"
    i0 = max([int(m.group(1)) for p in refs.glob("*.png") for m in [re.fullmatch(re.escape(cap.stem) + r"-(\d+)\.png", p.name)] if m], default=0) + 1
    dests = [refs / ("%s-%d.png" % (cap.stem, i0 + i)) for i in range(a.n)]
    side = refs / ("%s-%d.json" % (cap.stem, i0))
    payload = {"prompt": prompt, "image_urls": ["<upload of %s>" % cap.as_posix()], "num_images": a.n}
    if a.dry_run:
        print("dry run: would upload %s, submit %s %s" % (cap, ENDPOINT, json.dumps(payload)))
        print("dry run: would poll status every 5 s (timeout %d s), download to:" % a.timeout)
        for d in dests:
            print("  " + str(d))
        print("dry run: sidecar %s; charge <= $%.2f to %s; log a class %s attempt" % (side, est, share, "juice" if share == "juice" else "art"))
        return 0
    payload["image_urls"] = [fk.upload(str(cap))]
    rid = fk.submit(ENDPOINT, payload)["request_id"]
    t0, st = time.time(), {}
    while time.time() - t0 < a.timeout:
        st = fk.status(ENDPOINT, rid)
        if st.get("status") in ("COMPLETED", "FAILED", "ERROR", "CANCELLED"):
            break
        time.sleep(5)
    if st.get("status") != "COMPLETED":
        die("fal request %s ended %s after %d s" % (rid, st.get("status"), time.time() - t0), 1)
    res = fk.result(ENDPOINT, rid)
    urls = [im.get("url") for im in res.get("images", []) if im.get("url")]
    paths = [str(fk.download(u, dests[i] if i < len(dests) else refs / ("%s-%d.png" % (cap.stem, i0 + i)))) for i, u in enumerate(urls)]
    cost = price * len(urls)
    side.write_text(dumps({"endpoint": ENDPOINT, "request_id": rid, "prompt": prompt, "capture": str(cap), "share": share,
                           "unit_price": price, "price_source": src, "cost_usd": rnd(cost), "images": paths, "result": res}), encoding="utf-8")
    aid = add_attempt({"cycle": cyc, "card": a.card, "class": "juice" if share == "juice" else "art", "files": [],
                       "predicted": {"metric": "critic.reference", "delta": None}, "verdict": "measure-only",
                       "measured": {"metric": "refimg", "delta": None, "runs": [str(side)]},
                       "cost": {"cycles": 0, "editor_min": 0, "usd": rnd(cost)}, "lesson": None})
    b["entries"].append({"utc": utcnow().strftime("%Y-%m-%dT%H:%M:%SZ"), "share": share, "usd": rnd(cost),
                         "what": "%s x%d %s" % (ENDPOINT, len(urls), cap.name), "attempt": aid, "cycle": cyc})
    (docs() / "budget.json").write_text(dumps(b), encoding="utf-8")
    print("charged $%.2f to %s (attempt %s)" % (cost, share, aid))
    for p in paths + [str(side)]:
        print(p)
    return 0


# ---- retro / ledger -------------------------------------------------------------------------------------------------

def cmd_retro(a):
    rows, att = ledger_rows(), [x for x in load_attempts() if isinstance(x.get("cycle"), int) and x["cycle"] >= 0]
    cycles = sorted({r["cycle"] for r in rows} | {x["cycle"] for x in att})[-10:]
    sel = [x for x in att if x["cycle"] in cycles]
    print("retro over cycles %s (%d ledger rows, %d attempts)" % (
        "%d..%d" % (cycles[0], cycles[-1]) if cycles else "-", sum(1 for r in rows if r["cycle"] in cycles), len(sel)))
    count = lambda key: sorted(((k, sum(1 for x in sel if x.get(key) == k)) for k in {x.get(key) for x in sel}), key=lambda t: -t[1])
    print("by verdict: " + (", ".join("%s %d" % t for t in count("verdict")) or "-"))
    print("by class (landed/judged):")
    for c, _ in count("class"):
        g = [x for x in sel if x.get("class") == c]
        s = stats(g)
        print("  %-13s %d attempts, hit %s, calibration %s" % (c, s["n"], s["hit_rate"], s["calibration"]))
    rules = sorted(((r, sum(1 for x in sel if str(x.get("rule")) == r)) for r in {str(x.get("rule")) for x in sel if x.get("rule") is not None}),
                   key=lambda t: -t[1])
    print("revert rules: " + (", ".join("rule %s x%d%s" % (r, n, " (twice+: propose a rule or instrument)" if n >= 2 else "") for r, n in rules) or "-"))
    stuck = [c for c in load_cards() if int0(c["idle"]) >= 3 and not c["status"].startswith(("done", "parked"))]
    print("cards idle >= 3: " + (", ".join("%s (idle %s, %s)" % (c["id"], c["idle"], c["status"]) for c in stuck) or "-"))
    usd = sum(num(r.get("usd")) or 0 for r in rows if r["cycle"] in cycles)
    b = load_json(docs() / "budget.json", {}) or {}
    print("spend: ledger $%.2f over these cycles; budget.json all-time $%.2f" % (usd, sum(float(e.get("usd") or 0) for e in b.get("entries", []))))
    out = []
    for x in sel:
        p, m = x.get("predicted") or {}, x.get("measured") or {}
        if isinstance(p.get("delta"), (int, float)) and p["delta"] and isinstance(m.get("delta"), (int, float)):
            r = m["delta"] / p["delta"]
            if not 0.5 <= r <= 2:
                out.append("%s %s %.2f" % (x.get("id"), x.get("class"), r))
    print("calibration outliers (measured/predicted outside [0.5, 2]): " + (", ".join(out) or "-"))
    return 0


def cmd_ledger(a):
    path = docs() / "LEDGER.md"
    cyc = a.cycle if a.cycle is not None else current_cycle()
    att = [x for x in load_attempts() if x.get("cycle") == cyc]
    res = a.result or " ".join("%s%d" % (v[0].upper(), sum(1 for x in att if x.get("verdict") == v)) for v in VERDICTS)
    usd = a.usd
    if usd is None:
        usd = sum(float(e.get("usd") or 0) for e in (load_json(docs() / "budget.json", {}) or {}).get("entries", []) if e.get("cycle") == cyc)
    cells = [str(cyc), utcnow().strftime("%Y-%m-%dT%H:%MZ"), a.mode, a.cards or "-", ",".join(x.get("id", "?") for x in att) or "-",
             res, a.gate or "-", a.commit or "-", "%.2f" % usd, a.summary.replace("|", "\\|")]
    row = "| " + " | ".join(cells) + " |\n"
    lines, header, rows = read_table(path, "cycle")
    if header is None:
        text = "".join(lines)
        if not text:
            text = "# AOSA ledger\n\nOne row per cycle (README.md, step 6).\n"
        text = text.rstrip("\n") + "\n\n| " + " | ".join(LEDGER_COLS) + " |\n|" + "---|" * len(LEDGER_COLS) + "\n" + row
    else:
        hi = next(i for i, l in enumerate(lines) if l.strip().startswith("|")
                  and split_row(l.strip())[1].strip().lower() == "cycle")
        at = (rows[-1][0] if rows else hi + 1) + 1
        if not lines[at - 1].endswith("\n"):
            lines[at - 1] += "\n"
        lines.insert(at, row)
        text = "".join(lines)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    print("ledger: cycle %d added" % cyc)
    return 0


# ---- main -----------------------------------------------------------------------------------------------------------

def main(argv=None):
    ap = argparse.ArgumentParser(prog="aosa.py", description=__doc__.split("\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("status")
    b = sub.add_parser("bench")
    b.add_argument("label")
    g = b.add_mutually_exclusive_group()
    g.add_argument("--player", action="store_true")
    g.add_argument("--editor", action="store_true")
    b.add_argument("--dev", action="store_true", help="use the development player (WinBenchDev)")
    b.add_argument("--scenario")
    b.add_argument("--knobs", help="k=v,k2=v2")
    b.add_argument("--repeats", type=int)
    b.add_argument("--against", help="interleave with a prior label's recorded args: A1 B1 A2 B2 ...")
    b.add_argument("--extra", default="")
    b.add_argument("--cycle", type=int)
    b.add_argument("--build-dir")
    b.add_argument("--dry-run", action="store_true")
    c = sub.add_parser("compare")
    c.add_argument("A", help="candidate: label, file or glob")
    c.add_argument("B", help="baseline: label, file or glob")
    c.add_argument("--metric", default="main_ms.p95")
    c.add_argument("--higher-better", action="store_true")
    c.add_argument("--json", action="store_true")
    at = sub.add_parser("attempt")
    at.add_argument("op", choices=["add"])
    at.add_argument("json", help="JSON text, @file, or @- for stdin")
    sub.add_parser("learn")
    p = sub.add_parser("pick")
    p.add_argument("--n", type=int, default=3)
    p.add_argument("--mode", choices=["P", "E", "B"], default="P", help="E also considers blocked:editor cards")
    ag = sub.add_parser("age")
    ag.add_argument("--moved", default="")
    sub.add_parser("budget")
    r = sub.add_parser("refimg")
    r.add_argument("capture")
    r.add_argument("--for", dest="share", required=True, choices=["juice", "tier"])
    r.add_argument("--prompt-file", required=True)
    r.add_argument("--n", type=int, default=2)
    r.add_argument("--card")
    r.add_argument("--cycle", type=int)
    r.add_argument("--timeout", type=int, default=600)
    r.add_argument("--dry-run", action="store_true")
    sub.add_parser("retro")
    lg = sub.add_parser("ledger")
    lg.add_argument("op", choices=["add"])
    lg.add_argument("--mode", required=True, choices=["P", "E", "B"])
    lg.add_argument("--cards", default="")
    lg.add_argument("--summary", required=True)
    lg.add_argument("--gate")
    lg.add_argument("--commit")
    lg.add_argument("--usd", type=float)
    lg.add_argument("--result")
    lg.add_argument("--cycle", type=int)
    a = ap.parse_args(argv)
    fn = {"status": cmd_status, "bench": cmd_bench, "compare": cmd_compare, "attempt": cmd_attempt, "learn": cmd_learn,
          "pick": cmd_pick, "age": cmd_age, "budget": cmd_budget, "refimg": cmd_refimg, "retro": cmd_retro,
          "ledger": cmd_ledger}[a.cmd]
    if a.cmd == "status":
        try:
            return cmd_status(a)
        except Exception as e:  # status always exits 0
            print("status: partial, %s" % e)
            return 0
    return fn(a)


if __name__ == "__main__":
    sys.exit(main())
