#!/usr/bin/env python3
"""Script checks for the two things a leg writes for the next one: the plan (plan leg -> execute leg) and the handoff
note (lane work only). A paper that fails is not handed on. Stdlib only. ASCII only.

  plan.md   ## Goal  ## Steps  ## Files  ## Checks  ## Done when  ## Risks      (roles/_phase_plan.md)
  note.md   ## Goal  ## Done  ## In flight  ## Next  ## Predictions  ## Dead ends
  retro.md  ## What happened  ## Tuning  ## Proposals                             (roles/_phase_retro.md)
  critic.md   first line "VERDICT: <stage> ROUND <n>: <score>/100 ..", and three numbered fixes under
              "TOP-3 MANDATED FIXES" (roles/_phase_critic.md; the shape is the tw-critic skill's)
A prediction is one line:  `<command>` -> exit <n>   or   `<command>` -> contains "<text>"
and the command must only look (cmdrules.look_only), so the runner can run it before the next leg and score the note
without a model.
"""
import re, subprocess
from pathlib import Path

import cmdrules

PLAN_SECTIONS = ("Goal", "Steps", "Files", "Checks", "Done when", "Risks")
NOTE_SECTIONS = ("Goal", "Done", "In flight", "Next", "Predictions", "Dead ends")
RETRO_SECTIONS = ("What happened", "Tuning", "Proposals")
MAY_BE_EMPTY = ("Risks", "Dead ends", "In flight", "Tuning", "Proposals")
TUNE = re.compile(r"^\s*[-*]\s*`?([a-z_]+)`?\s*[:=]\s*([0-9][0-9_.]*)")
LEG_BREAK = "--- leg break ---"
PREDICTION = re.compile(r'^\s*[-*]?\s*`([^`]+)`\s*->\s*(exit\s+(\d+)|contains\s+"([^"]+)")\s*$')
TICKED = re.compile(r"`([^`\n]+)`")
PATHLIKE = re.compile(r"^[\w./\\-]*[/\\][\w./\\-]+$|^[\w.-]+\.(cs|py|md|json|ps1|sh|txt|asmdef|shader|uss|uxml|csv|meta|unity|prefab|asset|mat)$")


def sections(text):
    """{heading: body} for the '## ' headings, in order."""
    out, cur = {}, None
    for line in text.splitlines():
        m = re.match(r"^##\s+(.+?)\s*$", line)
        if m:
            cur = m.group(1)
            out[cur] = []
        elif cur is not None:
            out[cur].append(line)
    return {k: "\n".join(v).strip() for k, v in out.items()}


def _shape(text, need, max_bytes, what):
    out = []
    size = len(text.encode("utf-8"))
    if size > max_bytes:
        out.append("%s is %d bytes, the limit is %d" % (what, size, max_bytes))
    sec = sections(text)
    for s in need:
        if s not in sec:
            out.append("%s has no '## %s' section" % (what, s))
        elif s not in MAY_BE_EMPTY and len(sec[s].split()) < (1 if s == "Files" else 3):
            out.append("%s: '## %s' says nothing (under 3 words)" % (what, s))
    return out, sec


def plan_steps(text):
    """The Steps section cut at the leg breaks: one execute leg per part."""
    steps = sections(text).get("Steps", "")
    return [p.strip() for p in steps.split(LEG_BREAK) if p.strip()] or [steps]


def plan_for_part(text, parts, i):
    """The plan one execute leg gets: everything, but with only its own part under Steps."""
    if len(parts) == 1:
        return text
    out, skipping = [], False
    for line in text.splitlines():
        if re.match(r"^##\s+Steps\s*$", line):
            out += ["## Steps (part %d of %d: do only these)" % (i, len(parts)), parts[i - 1]]
            skipping = True
        elif re.match(r"^##\s+", line):
            skipping = False
            out.append(line)
        elif not skipping:
            out.append(line)
    return "\n".join(out) + "\n"


def named_file_exists(repo, tok):
    """A file a plan names is there: from the repo root or the Unity project folder, or, for a bare file name,
    anywhere among the tracked files. A placeholder (`.../x`, `BOARD/x`, `<leg>/x`) is not a repo path: not checked."""
    tok = tok.replace("\\", "/")
    first = tok.split("/")[0]
    if tok.startswith(("...", "<", "$", "~")) or "/" in tok and first.isupper() or re.match(r"^[A-Za-z]:/", tok):
        return True
    if any((Path(repo) / base / tok).exists() for base in ("", "trench-warfare-3d")):
        return True
    if "/" in tok:
        return False
    r = subprocess.run(["git", "ls-files", "--", "*/" + tok, tok], cwd=str(repo), capture_output=True)
    return r.returncode == 0 and bool(r.stdout.strip())


def check_plan(text, max_bytes, repo=None, max_parts=4):
    """Problems with a plan, as short strings (empty = hand it to the execute leg)."""
    out, sec = _shape(text, PLAN_SECTIONS, max_bytes, "plan.md")
    steps, files, checks = sec.get("Steps", ""), sec.get("Files", ""), sec.get("Checks", "")
    if steps and not re.search(r"^\s*1[.)]", steps, re.M):
        out.append("plan.md: '## Steps' is not a numbered list")
    if steps and not TICKED.search(steps):
        out.append("plan.md: no step names a file or a command in backticks")
    if checks and not TICKED.search(checks):
        out.append("plan.md: '## Checks' holds no command in backticks")
    if len(plan_steps(text)) > max_parts:
        out.append("plan.md cuts the work into %d legs, the limit is %d: the unit is too big" % (len(plan_steps(text)), max_parts))
    if files and not TICKED.search(files):
        out.append("plan.md: '## Files' names no file in backticks")
    for line in files.splitlines():
        for tok in TICKED.findall(line):
            tok = tok.strip()
            if PATHLIKE.match(tok) and repo and "(new)" not in line and not named_file_exists(repo, tok):
                out.append("plan.md names %s, which does not exist (mark a file to create with (new))" % tok)
    return out


CRITIC_VERDICT = re.compile(r"^\W*VERDICT\b.*?(\d{1,3})\s*/\s*100", re.I)
CRITIC_FIXES = re.compile(r"TOP-3 MANDATED FIXES", re.I)
CRITIC_HEADING = re.compile(r"^\W*[A-Z][A-Z0-9 -]{3,}:")
LIST_ITEM = re.compile(r"^\s*(?:\d+[.)]|[-*])\s+(\S.*)$")


def critic_score(text):
    """The score out of 100 from the first line that says anything, or None."""
    first = next((l for l in text.splitlines() if l.strip()), "")
    m = CRITIC_VERDICT.match(first)
    return int(m.group(1)) if m and int(m.group(1)) <= 100 else None


def critic_fixes(text):
    """The list items under TOP-3 MANDATED FIXES, up to the next heading in capitals."""
    out, on = [], False
    for line in text.splitlines():
        if CRITIC_FIXES.search(line):
            on = True
        elif on and CRITIC_HEADING.match(line):
            break
        elif on and LIST_ITEM.match(line):
            out.append(LIST_ITEM.match(line).group(1).strip())
    return out


def check_critic(text, max_bytes):
    """Problems with a critic's paper, as short strings (empty = the score counts)."""
    out, size = [], len(text.encode("utf-8"))
    if size > max_bytes:
        out.append("critic.md is %d bytes, the limit is %d" % (size, max_bytes))
    if critic_score(text) is None:
        out.append("critic.md does not start with 'VERDICT: <stage> ROUND <n>: <score>/100 ..' (a score of 0 to 100)")
    if len(critic_fixes(text)) < 3:
        out.append("critic.md lists %d fixes under 'TOP-3 MANDATED FIXES': three are needed, as a numbered list"
                   % len(critic_fixes(text)))
    return out


def plan_for_fixes(plan, fixes, round_no, score, target):
    """The plan a fix round's execute leg gets: the unit's plan with the critic's three fixes as its only steps."""
    steps = ["%d. %s" % (i, f) for i, f in enumerate(fixes[:3], 1)]
    steps.append("%d. Make the unit's evidence again, so every file it asks for is new, and close out as the plan says."
                 % (len(steps) + 1))
    steps.append("A fix you judge wrong: do not do it, and say why in your report. Do the others.")
    out, skipping = [], False
    for line in plan.splitlines():
        if re.match(r"^##\s+Steps\s*$", line):
            out += ["## Steps (fix round: a critic scored the work %d/100 in round %d, the target is %d; do only these)"
                    % (score, round_no, target)] + steps
            skipping = True
        elif re.match(r"^##\s+", line):
            skipping = False
            out.append(line)
        elif not skipping:
            out.append(line)
    return "\n".join(out) + "\n"


def check_retro(text, max_bytes):
    """Problems with a retrospective, as short strings (empty = its tuning and proposals are taken)."""
    return _shape(text, RETRO_SECTIONS, max_bytes, "retro.md")[0]


def retro_tuning(text, allowed):
    """{key: number} from the '## Tuning' lines `- <key>: <number> - why`, for the keys a retrospective may move."""
    out = {}
    for line in sections(text).get("Tuning", "").splitlines():
        m = TUNE.match(line)
        if m and m.group(1) in allowed:
            try:
                v = float(m.group(2).replace("_", ""))
                out[m.group(1)] = int(v) if v == int(v) else v
            except ValueError:
                pass
    return out


def predictions(text):
    out = []
    for line in sections(text).get("Predictions", "").splitlines():
        m = PREDICTION.match(line)
        if m:
            out.append({"cmd": m.group(1), "exit": int(m.group(3)) if m.group(3) else None, "contains": m.group(4)})
    return out


def check_note(text, max_bytes):
    out, sec = _shape(text, NOTE_SECTIONS, max_bytes, "note.md")
    pred = predictions(text)
    lines = [l for l in sec.get("Predictions", "").splitlines() if l.strip()]
    if not 1 <= len(pred) <= 3 or len(pred) != len(lines):
        out.append("note.md needs 1 to 3 predictions, each as `command` -> exit N  or  -> contains \"text\"")
    for p in pred:
        if not cmdrules.look_only(p["cmd"]):
            out.append("note.md prediction runs '%s', which is not a look-only command" % p["cmd"])
    return out


def run_predictions(text, cwd, timeout=60):
    """Run each prediction; [(command, hit)] - the one measure of whether a note told the truth."""
    out = []
    for p in predictions(text):
        hit = False
        if cmdrules.look_only(p["cmd"]):
            try:
                r = subprocess.run(cmdrules.argv(p["cmd"]), cwd=str(cwd), capture_output=True, timeout=timeout)
                got = (r.stdout + r.stderr).decode("utf-8", "replace")
                hit = r.returncode == p["exit"] if p["exit"] is not None else p["contains"] in got
            except (OSError, subprocess.TimeoutExpired):
                pass
        out.append((p["cmd"], hit))
    return out
