"""The brief of a role: the skill a leg is handed for the role its unit names. One table for every source,
Tools/pipeline/roles.json (role -> skill, or null for a role that is known and has no brief), which pipeline.py
also reads to refuse a stage with a role nobody knows."""
import re, shutil, sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "pipeline"))
import pipeline as P   # noqa: E402

# the skill that is the brief for a role (the table in .claude/skills/pipeline/SKILL.md)
ROLE_SKILLS = {role: skill for role, skill in P.roles().items() if skill}


def skills_root():
    """The skills live in this repo. A leg works in another checkout, on another lane, which may not have them.
    parents: sources, relay, Tools, trench-warfare-3d, repo root."""
    return Path(__file__).resolve().parents[4] / ".claude" / "skills"


def brief_name(role):
    name = ROLE_SKILLS.get(role)
    if name and (skills_root() / name / "SKILL.md").is_file():
        return name
    return None


def place_brief(role, desk):
    """Copy the role's skill into the leg's desk, plus any sibling skill its text links to with ../, so those
    links still resolve. A leg can read its desk. The work checkout is not given the skill. None when this role
    has no brief on this machine."""
    name = brief_name(role)
    if not name:
        return None
    root = skills_root()
    text = (root / name / "SKILL.md").read_text(encoding="utf-8")
    names = {name}
    for sib in re.findall(r"\.\./([A-Za-z0-9_-]+)/", text):
        if (root / sib).is_dir():
            names.add(sib)
    dest = Path(desk) / "brief"
    for n in sorted(names):
        shutil.copytree(root / n, dest / n)
    return "brief/%s/SKILL.md" % name


def card_line(role):
    """What the leg card says about the brief, or None for a role that has none."""
    name = brief_name(role)
    if not name:
        return None
    return ("- Read `brief/%s/SKILL.md` in your leg folder and follow it: it is the brief for the role `%s`. "
            "The work checkout does not have that skill, so it is copied into the leg folder. Do not load it by name."
            % (name, role))
