#!/usr/bin/env python3
"""Relay settings: limits.json (thresholds and caps), phases.json (model, effort and mode per phase) and style.json
(how a leg talks to the owner). Each is checked on load, so a bad edit stops the run before a leg starts.
Stdlib only. ASCII only.
"""
import json, re
from pathlib import Path

HERE = Path(__file__).resolve().parent
TUNABLE = ("amber_tokens", "red_tokens", "run_hours", "leg_minutes", "leg_budget_usd", "day_budget_usd",
           "day_budget_pct")
FIXED = ("autocompact_tokens", "no_progress_units", "stuck_lanes", "max_plan_parts", "retro_every_legs", "note_max_bytes",
         "plan_max_bytes", "prompt_max_bytes", "evidence_max_kb", "evidence_min_px", "quiet_seconds",
         "done_when_seconds", "gate_seconds", "play_seconds", "critic_target", "critic_rounds", "critic_max_bytes",
         "retro_max_bytes", "usual_leg_usd", "price_legs", "queue_priority", "queue_priority_max",
         "day_queue_rows", "day_line_chars", "usage_max_age_seconds", "week_usd", "pace_from_hour",
         "pace_to_hour", "share_finish", "share_fix", "share_critique", "ideas_in_flight",
         "second_minutes", "second_per_day")
SECOND_VENDORS = ("grok", "codex")      # providers.NAMES: who may give a second opinion (test_relay holds them equal)
SHARES = ("finish", "fix", "critique")      # the kinds of work the day is split over (sources.KINDS): share_<kind>


def shares(lim, named=None):
    """{kind: percent of the day's tokens} from the limits, or from `named` when that is a whole split: every kind
    a number from 0 to 100 and 100 together. A split that is not whole is not used: the limits' own stands."""
    if isinstance(named, dict):
        vals = [named.get(k) for k in SHARES]
        if all(isinstance(v, (int, float)) and not isinstance(v, bool) and 0 <= v <= 100 for v in vals) \
                and abs(sum(vals) - 100) < 0.01:
            return dict(zip(SHARES, (float(v) for v in vals)))
    return {"finish": float(lim.get("share_finish") or 0), "fix": float(lim.get("share_fix") or 0),
            "critique": float(lim.get("share_critique") or 0)}
RETRO_TUNES = ("amber_tokens", "red_tokens", "leg_minutes")    # what a retrospective may move, inside the bounds
MODES = ("read_only", "work")
EFFORTS = ("low", "medium", "high", "xhigh", "max")


def _read(name, folder=None):
    p = Path(folder or HERE) / name
    try:
        return json.loads(p.read_text(encoding="utf-8"))
    except (OSError, ValueError) as e:
        raise SystemExit("relay: cannot read %s: %s" % (p, e))


def limits(folder=None, overrides=None):
    """Flat dict of numbers. overrides (the retrospective's tuning, or a CLI flag) are clamped to each bound."""
    raw = _read("limits.json", folder)
    out = {}
    for k in TUNABLE:
        e = raw.get(k)
        if not isinstance(e, dict) or not all(isinstance(e.get(x), (int, float)) for x in ("value", "min", "max")):
            raise SystemExit("relay: limits.json %s needs value, min and max" % k)
        v = (overrides or {}).get(k, e["value"])
        out[k] = min(max(v, e["min"]), e["max"])
    for k in FIXED:
        if not isinstance(raw.get(k), (int, float)):
            raise SystemExit("relay: limits.json %s must be a number" % k)
        out[k] = raw[k]
    if not out["amber_tokens"] < out["red_tokens"] < out["autocompact_tokens"]:
        raise SystemExit("relay: limits.json needs amber_tokens < red_tokens < autocompact_tokens")
    if not 0 <= out["pace_from_hour"] <= 24 or not 0 <= out["pace_to_hour"] <= 24:
        raise SystemExit("relay: limits.json pace_from_hour and pace_to_hour are hours of the day, 0 to 24")
    return out


def second_critic(folder=None):
    """The vendor that scores each critic round a second time, recording only; "" when nobody does (second.py)."""
    v = _read("limits.json", folder).get("second_critic", "")
    if v not in ("",) + SECOND_VENDORS:
        raise SystemExit("relay: limits.json second_critic is \"\" or one of %s" % ", ".join(SECOND_VENDORS))
    return v


def phases(folder=None):
    raw = _read("phases.json", folder)
    for name, p in raw.items():
        if p.get("mode") not in MODES or p.get("effort") not in EFFORTS or not p.get("model"):
            raise SystemExit("relay: phases.json %s needs model, effort (%s) and mode (%s)"
                             % (name, "|".join(EFFORTS), "|".join(MODES)))
        if not isinstance(p.get("deny_tools", []), list):
            raise SystemExit("relay: phases.json %s: deny_tools must be a list of tool names" % name)
        tools = p.get("tools", [])
        if not isinstance(tools, list) or not all(isinstance(t, str) and t.isalnum() for t in tools):
            raise SystemExit("relay: phases.json %s: tools must be a list of tool names" % name)
        if tools and not {"Bash", "Read", "Write"} <= set(tools):
            raise SystemExit("relay: phases.json %s: tools must hold Bash, Read and Write, or a leg cannot work" % name)
        if not isinstance(p.get("skills", True), bool):
            raise SystemExit("relay: phases.json %s: skills is true or false" % name)
        if p["model"] not in LEG_MODELS or (p["model"] == EASY_MODEL and p["mode"] != "read_only"):
            raise SystemExit("relay: phases.json %s: model is %s, and %s only for a phase that only reads"
                             % (name, "|".join(LEG_MODELS), EASY_MODEL))
        by = p.get("by_grade", {})
        if not isinstance(by, dict) or not all(g in GRADES and m in ROUTE_MODELS for g, m in by.items()):
            raise SystemExit("relay: phases.json %s: by_grade names a model (%s) for easy, medium or hard"
                             % (name, "|".join(ROUTE_MODELS)))
        if p["mode"] == "read_only" and not p.get("output"):
            raise SystemExit("relay: phases.json %s is read_only, so it needs an output file" % name)
    for need in ("plan", "execute"):
        if need not in raw:
            raise SystemExit("relay: phases.json has no %s phase" % need)
    return raw


ROUTE_MODELS = ("opus", "sonnet")     # the models a leg that does work may run on
EASY_MODEL = "haiku"                  # reading and sorting only: a phase that only reads may run on it
LEG_MODELS = ROUTE_MODELS + (EASY_MODEL,)
GRADES = ("easy", "medium", "hard")   # papers.plan_grade: what a plan leg says of its unit


def graded(ph, role, unit, grade, folder=None):
    """The model an execute leg runs on for a plan of this grade, or None for the phase's own. phases.json
    execute.by_grade is the table; a route in routes.json for this role and unit wins over it (the owner's word),
    except for hard work: that runs on the phase's own model whatever the route says (the owner, 2026-10-12:
    "opus for the difficult items"; rv-04-flow-goals was graded hard and ran on the review-fix route's Sonnet)."""
    if "model" in route(role, "execute", folder, unit):
        return ph["execute"]["model"] if grade == "hard" else None
    return (ph.get("execute", {}).get("by_grade") or {}).get(grade)


def routes(folder=None):
    """routes.json, checked: the legs that do not run on their phase's model and effort. A list of
    {role, phase, model and/or effort, why, and optionally units: [ids]}; a role and phase may stand in it once.
    With units, the route holds for those units only: a trial names its units, the others of the role are its
    control. The role must be one of Tools/pipeline/roles.json: a misspelt one would never apply, unsaid."""
    raw = _read("routes.json", folder)
    rows, seen, known = raw.get("routes"), set(), phases(folder)
    roles = _read("roles.json", HERE.parent / "pipeline")
    if not isinstance(rows, list):
        raise SystemExit("relay: routes.json needs a list named routes")
    for r in rows:
        if not isinstance(r, dict) or not isinstance(r.get("role"), str) or not r["role"] or r.get("phase") not in known:
            raise SystemExit("relay: routes.json: every route names a role and a phase of phases.json (%s)" % r)
        if r["role"] not in roles:
            raise SystemExit("relay: routes.json: no role named %s in Tools/pipeline/roles.json" % r["role"])
        if "units" in r and (not isinstance(r["units"], list) or not r["units"]
                             or not all(isinstance(u, str) and u for u in r["units"])):
            raise SystemExit("relay: routes.json %s/%s: units is a list of unit ids, or is left out" % (r["role"], r["phase"]))
        if (r["role"], r["phase"]) in seen:
            raise SystemExit("relay: routes.json names %s/%s twice" % (r["role"], r["phase"]))
        seen.add((r["role"], r["phase"]))
        if "model" not in r and "effort" not in r:
            raise SystemExit("relay: routes.json: the route %s/%s changes nothing" % (r["role"], r["phase"]))
        easy_ok = (EASY_MODEL,) if known[r["phase"]]["mode"] == "read_only" else ()
        if r.get("model", ROUTE_MODELS[0]) not in ROUTE_MODELS + easy_ok or r.get("effort", EFFORTS[0]) not in EFFORTS:
            raise SystemExit("relay: routes.json %s/%s: model is %s, effort is %s"
                             % (r["role"], r["phase"], "|".join(ROUTE_MODELS), "|".join(EFFORTS)))
    return rows


def route(role, phase, folder=None, unit=None):
    """What routes.json changes for a leg of this role and phase: {} or {model and/or effort}. unit: the unit's id;
    a route that names its units holds for no other."""
    for r in routes(folder):
        if r["role"] == role and r["phase"] == phase and ("units" not in r or unit in r["units"]):
            return {k: r[k] for k in ("model", "effort") if k in r}
    return {}


def routed(ph, unit, folder=None):
    """phases.json as it holds for this unit: each phase with its route applied. The budget prices a leg by it."""
    return {name: dict(p, **route(unit.get("role"), name, folder, unit.get("id"))) for name, p in ph.items()}


def style(folder=None):
    raw = _read("style.json", folder)
    for k in ("reader", "talk", "report"):
        if k not in raw:
            raise SystemExit("relay: style.json has no %s" % k)
    if not isinstance(raw["report"].get("max_words"), int) or not raw["report"].get("shape"):
        raise SystemExit("relay: style.json report needs max_words and shape")
    return raw


def style_text(st):
    """The style rules as the first block of every role prompt: plain lines, not JSON."""
    t, r = st["talk"], st["report"]
    lines = ["# How you write", "Reader: %s" % st["reader"]]
    if t.get("answer_first"):
        lines.append("- Give the answer first.")
    lines += ["- Words: %s." % t["words"], "- Sentences: %s." % t["sentences"]]
    if t.get("explain_terms"):
        lines.append("- Terms: %s." % t["explain_terms"])
    if t.get("no"):
        lines.append("- Never: %s." % ", ".join(t["no"]))
    for k in ("tables", "details"):
        if st.get(k):
            lines.append("- %s: %s." % (k.capitalize(), st[k]))
    lines.append("Your last message is the report, at most %d words, in exactly this shape:" % r["max_words"])
    lines += ["  " + s for s in r["shape"]]
    q = st.get("question_to_owner")
    if q:
        lines.append("A question for the owner: at most %d words, one decision, %s."
                     % (q["max_words"], q.get("give_options", "with options")))
    return "\n".join(lines) + "\n"


# A NEEDS YOU line that asks nothing of the owner: nothing, none or n/a, also with words after a mark ("None - the
# lane is pushed."); no, only alone or as "no action (needed)", for "No window can open here" does ask; a dash.
NOTHING = re.compile(r"^(?:(?:nothing|none|n/?a)(?:\s+(?:is\s+)?(?:needed|required))?\s*(?:[.,;:(\u2013\u2014-].*)?"
                     r"|no(?:\s+action(?:\s+(?:is\s+)?(?:needed|required))?)?[.!]?|[\u2013\u2014-]+)$", re.I | re.S)


def needs_you(text, st):
    """What a leg's report asks of the owner: the rest of its NEEDS YOU line and the lines under it, up to the
    next heading of the report's shape (style.json). '' when the report has no such line or it asks nothing.
    The next heading is one written as the shape writes it and marked as the NEEDS YOU line is (bold, a bullet, a
    number): "- Next: pick A or B" under a plain NEEDS YOU is part of what is asked."""
    names = "|".join(re.escape(s.split(":")[0].strip()) for s in st["report"]["shape"])
    start = re.compile(r"^([\W\d]*?)NEEDS YOU\b[^\w:]*(?::|$)", re.I)
    head = re.compile(r"^([\W\d]*?)(?:%s)\b[^\w:]*(?::|$)" % names)
    mark = lambda m: re.sub(r"[\d\s]", "", m.group(1))
    out = marked = None
    for line in (text or "").splitlines():
        if out is None:
            m = start.match(line)
            if m:
                out, marked = [line[m.end():]], mark(m)
        elif head.match(line) and mark(head.match(line)) == marked:
            break
        else:
            out.append(line)
    said = "\n".join(out or []).strip().strip("*_` \t").strip()
    return "" if not said or NOTHING.match(said) else said


def report_problems(text, st):
    """Why a leg's final report breaks the style, as a list of short strings (empty = fine)."""
    out = []
    words = len(text.split())
    if words > st["report"]["max_words"]:
        out.append("report is %d words, the limit is %d" % (words, st["report"]["max_words"]))
    if not text.lstrip().upper().startswith("RESULT:"):
        out.append("report does not start with RESULT:")
    return out
