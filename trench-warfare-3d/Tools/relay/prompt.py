#!/usr/bin/env python3
"""Builds what a leg is told, from files: nothing about a role or a phase is written in code.

  system.md  appended to the system prompt: the style rules (style.json), roles/_common.md, roles/_phase_<phase>.md,
             and roles/<role>.md when the role has one
  card.md    handed over at session start: the unit, where to work, and the plan for an execute leg
Stdlib only. ASCII only.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import config   # noqa: E402

ROLES = Path(__file__).resolve().parent / "roles"


def system_text(role, phase, st, max_bytes, roles_dir=None):
    roles_dir = Path(roles_dir or ROLES)
    parts = [config.style_text(st)]
    for name in ("_common.md", "_phase_%s.md" % phase, "%s.md" % role):
        p = roles_dir / name
        if p.exists():
            parts.append(p.read_text(encoding="utf-8").strip() + "\n")
        elif name.startswith("_"):
            raise SystemExit("relay: roles/%s is missing" % name)
    text = "\n".join(parts)
    if len(text.encode("utf-8")) > max_bytes:
        raise SystemExit("relay: the %s/%s prompt is %d bytes, the limit is %d: shorten the role files"
                         % (role, phase, len(text.encode("utf-8")), max_bytes))
    return text


def card_text(leg, body, plan=None):
    desk = leg["desk"].replace("\\", "/")
    lines = ["# Leg card", "- Leg: %s-%02d (%s phase)" % (leg["run"], leg["leg"], leg["phase"]),
             "- Unit: %s (%s)" % (leg["unit"], leg["source"]), "- Role: %s" % leg["role"],
             "- Work in: %s" % leg["worktree"], "- Lane: %s" % leg["lane"],
             "- Your leg folder (the only place for plans and notes): %s" % desk]
    if leg.get("output"):
        lines.append("- Write your result to: %s/%s" % (desk, leg["output"]))
    tool = str(Path(__file__).resolve().parent / "relay.py").replace("\\", "/")
    if leg.get("phase") in ("plan", "critic", "retro"):
        lines.append("- Before you end, check %s the way the runner will: python \"%s\" leg done"
                     % (leg.get("output"), tool))
    if leg.get("mode") == "work":
        lines += ["- The edit gate, detached: python \"%s\" leg gate start, then: leg gate wait" % tool,
                  "- Close out: python \"%s\" leg finish -m \"<message>\" (commits and pushes when the gate is green "
                  "for exactly these files), then: leg done" % tool]
    lines += ["", body.strip(), ""]
    if plan:
        lines += ["# The plan to follow", plan.strip(), ""]
    return "\n".join(lines)
