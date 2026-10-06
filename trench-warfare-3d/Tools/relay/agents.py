#!/usr/bin/env python3
"""What the agents cost that sessions spawned outside the relay, so the day's budget counts them too.

The day's cap is for every agent spawned to work on the project (the owner, 2026-10-07). A relay leg's cost is in its
leg record. An agent a session spawns (a subagent, a workflow's agents) leaves no such record: only Claude Code's own
log of it on that machine, <logs>/<project>/<session>/subagents/agent-*.jsonl, with the tokens of every answer.

  count(day, ...)          what such agents used that local day on this machine: {day, usd, agents, models, unpriced}
  record(counted, ...)     that as the file the board keeps: relay/<station>/agents/<day>.json
  book(board, station)     count today here and write the record on the board (ledger.py adds it to the day)
  lines(counted, ...)      a few lines for a person
  hand_in(home, rec), take_in(board, home)    a booking that arrives while a run is going waits in the relay's
                           home, and the run takes it up before its next unit: nothing but the run writes the
                           board while a leg works, or the leg would be blamed for it
An agent counts when its folder or its first prompt names the project (agents.json "names"). The agents of a relay
leg do not: their cost is in the leg's own (the cost Claude prints for a session covers the agents it spawned). A
leg's session is known by its leg folder here, and by the folder it runs in (agents.json "leg_folders").
Tokens become dollars at the list prices in agents.json (per million tokens; a cache write for five minutes costs
1.25 times the input price, for an hour 2 times). A model the file does not name is priced as its dearest one, and
the lines say so. The figure is never a measurement: every line says "about". The owner's own talk with a session is
not counted, only what the session spawned.
Stdlib only. ASCII only.
"""
import datetime, json, os, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
import pipeline as P                    # noqa: E402
import ledger, legdir                   # noqa: E402

INBOX = "agents-inbox"
LOGS = "TW_AGENT_LOGS"                  # Claude Code's projects folder, when it is not the usual one
KINDS = ("input", "output", "cache_read", "cache_write_5m", "cache_write_1h")


def settings(folder=None):
    """agents.json, checked: names, leg_folders, write factors and a price per model."""
    p = Path(folder or HERE) / "agents.json"
    try:
        raw = json.loads(p.read_text(encoding="utf-8"))
    except (OSError, ValueError) as e:
        raise SystemExit("relay: cannot read %s: %s" % (p, e))
    prices = raw.get("usd_per_million_tokens")
    if not isinstance(prices, dict) or not prices or not all(
            isinstance(v, dict) and all(isinstance(v.get(k), (int, float)) for k in ("input", "output", "cache_read"))
            for v in prices.values()):
        raise SystemExit("relay: agents.json usd_per_million_tokens needs input, output and cache_read per model")
    for k in ("names", "leg_folders"):
        if not isinstance(raw.get(k), list) or not all(isinstance(x, str) and x for x in raw[k]):
            raise SystemExit("relay: agents.json %s must be a list of words" % k)
    for k in ("cache_write_5m_factor", "cache_write_1h_factor"):
        if not isinstance(raw.get(k), (int, float)):
            raise SystemExit("relay: agents.json %s must be a number" % k)
    return raw


def logs():
    """Where Claude Code keeps its session logs on this machine."""
    return Path(os.environ.get(LOGS) or Path(os.environ.get("CLAUDE_CONFIG_DIR") or Path.home() / ".claude") / "projects")


def leg_sessions(home=None):
    """The session ids of the relay legs this machine ran: an agent of theirs is not counted again."""
    out = set()
    for f in Path(home or legdir.home()).glob("runs/*/legs/*/" + legdir.SESSION):
        try:
            out.add(str(P.read_json(f).get("session_id")))
        except (OSError, ValueError, AttributeError):
            pass
    return out


def price(model, st):
    """(the prices of the model, True when the file names it). The longest name the model's id starts with wins;
    a model the file does not name is priced as the dearest one in it."""
    table = st["usd_per_million_tokens"]
    hit = max((m for m in table if str(model).startswith(m)), key=len, default=None)
    if hit:
        return table[hit], True
    return max(table.values(), key=lambda v: v["output"]), False


def tokens(usage):
    """The token counts of one answer by kind. A write to the cache says how long it is kept when the log holds
    that; one that does not is counted as kept five minutes."""
    def n(x):
        return x if isinstance(x, int) and not isinstance(x, bool) and x > 0 else 0
    made = usage.get("cache_creation") if isinstance(usage.get("cache_creation"), dict) else {}
    w5, w1 = n(made.get("ephemeral_5m_input_tokens")), n(made.get("ephemeral_1h_input_tokens"))
    if not (w5 or w1):
        w5 = n(usage.get("cache_creation_input_tokens"))
    return {"input": n(usage.get("input_tokens")), "output": n(usage.get("output_tokens")),
            "cache_read": n(usage.get("cache_read_input_tokens")), "cache_write_5m": w5, "cache_write_1h": w1}


def cost(tok, model, st):
    p, _ = price(model, st)
    per = {"input": p["input"], "output": p["output"], "cache_read": p["cache_read"],
           "cache_write_5m": p["input"] * st["cache_write_5m_factor"],
           "cache_write_1h": p["input"] * st["cache_write_1h_factor"]}
    return sum(tok[k] * per[k] for k in KINDS) / 1e6


def _local_day(stamp):
    """The local day of a log stamp (2026-10-07T12:25:07.924Z), or None."""
    try:
        t = datetime.datetime.strptime(str(stamp)[:19], "%Y-%m-%dT%H:%M:%S").replace(tzinfo=datetime.timezone.utc)
    except ValueError:
        return None
    return t.astimezone().strftime(ledger.DAY)


def _text(content):
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return " ".join(c.get("text", "") for c in content if isinstance(c, dict) and isinstance(c.get("text"), str))
    return ""


def read_agent(f, day):
    """One agent's log: {cwd, prompt, answers: {answer id: (model, tokens)}} for the answers given that day. One
    answer is logged once per block of it, each time with its tokens: it is counted once, at its largest count."""
    cwd, prompt, answers = "", None, {}
    try:
        with open(f, encoding="utf-8", errors="replace") as fh:
            for n, line in enumerate(fh):
                try:
                    rec = json.loads(line)
                except ValueError:
                    continue
                if not isinstance(rec, dict):
                    continue
                cwd = cwd or str(rec.get("cwd") or "")
                msg = rec.get("message") if isinstance(rec.get("message"), dict) else {}
                if prompt is None and msg.get("role") == "user":
                    prompt = _text(msg.get("content"))[:8000]
                use = msg.get("usage")
                if not isinstance(use, dict) or _local_day(rec.get("timestamp")) != day:
                    continue
                tok = tokens(use)
                key = str(msg.get("id") or rec.get("uuid") or n)
                if key not in answers or sum(tok.values()) > sum(answers[key][1].values()):
                    answers[key] = (str(msg.get("model") or "?"), tok)
    except OSError:
        pass
    return {"cwd": cwd, "prompt": prompt or "", "answers": answers}


def count(day=None, root=None, home=None, st=None):
    """What the agents sessions spawned outside the relay used that local day on this machine.
    {day, usd, agents, models: {model: {usd, tokens by kind}}, unpriced: [models priced as the dearest]}"""
    day, st, root = day or ledger.today(), st or settings(), Path(root or logs())
    start = datetime.datetime.strptime(day, ledger.DAY).timestamp()
    legs = leg_sessions(home)
    out = {"day": day, "usd": 0.0, "agents": 0, "models": {}, "unpriced": []}
    for f in sorted(root.glob("*/*/subagents/agent-*.jsonl")) if root.is_dir() else []:
        try:
            if f.stat().st_mtime < start or f.parent.parent.name in legs:      # not touched that day, or a leg's own
                continue
        except OSError:
            continue
        a = read_agent(f, day)
        where = (a["cwd"] + " " + a["prompt"]).lower().replace("\\", "/")
        if not a["answers"] or not any(n.lower() in where for n in st["names"]):
            continue
        if any(n.lower().replace("\\", "/") in a["cwd"].lower().replace("\\", "/") for n in st["leg_folders"]):
            continue
        out["agents"] += 1
        for model, tok in a["answers"].values():
            if not sum(tok.values()):
                continue
            m = out["models"].setdefault(model, dict({k: 0 for k in KINDS}, usd=0.0))
            for k in KINDS:
                m[k] += tok[k]
            m["usd"] += cost(tok, model, st)
            if not price(model, st)[1] and model not in out["unpriced"]:
                out["unpriced"].append(model)
    out["usd"] = sum(m["usd"] for m in out["models"].values())
    return out


def record(counted, station, by=""):
    """What the board keeps of a count: the day's total of one station, which a newer count of the day replaces."""
    return {"day": counted["day"], "station": station, "usd": round(counted["usd"], 2), "agents": counted["agents"],
            "models": {m: round(v["usd"], 2) for m, v in sorted(counted["models"].items())},
            "unpriced": sorted(counted["unpriced"]), "at": P.now(), "by": by}


def check(rec):
    """Why a record somebody hands in cannot be booked, or None."""
    if not isinstance(rec, dict):
        return "it is not a record"
    try:
        datetime.datetime.strptime(str(rec.get("day")), ledger.DAY)
    except ValueError:
        return "it names no day"
    if not isinstance(rec.get("station"), str) or not rec["station"].replace("-", "").replace("_", "").isalnum():
        return "it names no station"
    for k in ("usd", "agents"):
        if not isinstance(rec.get(k), (int, float)) or isinstance(rec.get(k), bool) or rec[k] < 0:
            return "it holds no %s" % k
    return None


def path(board, rec):
    return Path(board) / "relay" / rec["station"] / "agents" / (rec["day"] + ".json")


def keep(board, rec):
    """Write a record on the board. Returns (the file, what it held before or None)."""
    why = check(rec)
    if why:
        raise SystemExit("relay: this is no booking of agents: %s" % why)
    p = path(board, rec)
    try:
        before = P.read_json(p) if p.exists() else None
    except (OSError, ValueError):
        before = None
    P.write_json(p, rec)
    return p, before


def book(board, station, by="", day=None, root=None, home=None, st=None):
    """Count this machine's agents of the day and keep the record on the board. Returns (record, before). A day
    with no such agent and no earlier booking writes nothing."""
    rec = record(count(day, root, home, st), station, by)
    if not rec["agents"] and not path(board, rec).exists():
        return rec, None
    return rec, keep(board, rec)[1]


def hand_in(home, rec):
    """Keep a booking for the run that is going on this machine. Returns the file."""
    why = check(rec)
    if why:
        raise SystemExit("relay: this is no booking of agents: %s" % why)
    p = Path(home) / INBOX / ("%s-%s.json" % (rec["station"], rec["day"]))
    P.write_json(p, rec)
    return p


def take_in(board, home):
    """Write the bookings that wait in the relay's home on the board. Returns the records taken; a file that is
    no booking is dropped."""
    taken = []
    for f in sorted((Path(home) / INBOX).glob("*.json")) if (Path(home) / INBOX).is_dir() else []:
        try:
            rec = P.read_json(f)
        except (OSError, ValueError):
            rec = None
        if check(rec) is None:
            keep(board, rec)
            taken.append(rec)
        f.unlink()
    return taken


def lines(counted, per_usd=None, booked=None):
    """A few lines for a person. per_usd: percent points of the week a dollar stands for (ledger.rate), to say the
    figure the way the day is read. booked: the record the board holds for this station and day, or None."""
    n = counted["agents"]
    pct = " (about %.1f%% of the week)" % (counted["usd"] * per_usd) if per_usd is not None and n else ""
    out = ["Agents outside the relay, %s, this machine: %d agent%s, about $%.2f at list prices%s."
           % (counted["day"], n, "" if n == 1 else "s", counted["usd"], pct)]
    for model, v in sorted(counted["models"].items()):
        out.append("  %-28s about $%7.2f  %s" % (model, v["usd"], "  ".join(
            "%s %d" % (k.replace("cache_", ""), v[k]) for k in KINDS if v[k])))
    if counted["unpriced"]:
        out.append("  agents.json names no price for %s: priced as its dearest model." % ", ".join(sorted(counted["unpriced"])))
    if booked is None:
        out.append("Not booked for the day yet: `relay.py agents book` adds it to the day's budget." if n else
                   "Nothing to book.")
    else:
        out.append("Booked for the day: $%.2f, %d agent%s (%s)." % (booked.get("usd", 0), booked.get("agents", 0),
                                                                  "" if booked.get("agents") == 1 else "s",
                                                                  booked.get("at", "?")))
    return out
