"""Source: jobs on the two-station board (docs/reference/stations.md). The runner claims and completes; the leg only
does the stage. A master stage (review, gate, land) is the owner's and is never taken."""
import json, os, sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "pipeline"))
import pipeline as P   # noqa: E402

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import config, gitio   # noqa: E402
from sources import verdict   # noqa: E402

NAME = "pipeline"
SKIP_ROLES = ("master",)
# the skill that is the brief for a stage role (the table in .claude/skills/pipeline/SKILL.md)
ROLE_SKILLS = {"balance-simulator": "tw-balance-sim", "env-simulator": "tw-env-sim",
               "character-simulator": "tw-character-sim", "vehicle-simulator": "tw-vehicle-sim",
               "destruction-vfx-simulator": "tw-destruction-vfx", "optimizer": "tw-optimizer",
               "bug-catcher": "tw-bug-catcher", "hard-critic": "tw-critic"}


def jpeg_size(data):
    """(width, height) from a JPEG's frame header, or None when the bytes are not a JPEG picture."""
    if data[:2] != b"\xff\xd8":
        return None
    pos = 2
    while pos + 9 <= len(data) and data[pos] == 0xFF:
        marker, size = data[pos + 1], int.from_bytes(data[pos + 2:pos + 4], "big")
        if 0xC0 <= marker <= 0xCF and marker not in (0xC4, 0xC8, 0xCC):
            return int.from_bytes(data[pos + 7:pos + 9], "big"), int.from_bytes(data[pos + 5:pos + 7], "big")
        pos += 2 + size
    return None


def next(ctx, only=None):
    board = P.Board(ctx["board"])
    for item in board.items().values():
        stages = {s["id"]: s for s in item["stages"]}
        for sid, info in P.evaluate(item, board).items():
            st = stages[sid]
            if only and (item["id"], sid) != only:
                continue
            if (info["station"] == ctx["station"] and info["state"] in ("READY", "STALE", "RECHECK")
                    and st.get("role") not in SKIP_ROLES and info["job"] not in ctx["skip"]):
                return {"id": info["job"], "source": NAME, "role": st.get("role") or "pipeline",
                        "lane": st.get("lane") or item["lane"], "item": item["id"], "stage": sid,
                        "todo": "RECHECK" if info["state"] == "RECHECK" else "REGENERATE",
                        "bands": st.get("bands", []), "stage_json": st, "title": item.get("title", "")}
    return None


def refresh(unit, ctx):
    """A job's id is a hash of its inputs at the lane ref, so it changes when the runner first creates the lane:
    read the id again once the checkout is on the lane, and claim that one."""
    return next(ctx, only=(unit["item"], unit["stage"]))


def already_done(unit, ctx):
    return False


def evidence_dir(unit):
    return "evidence/%s/%s" % (unit["item"], unit["stage"])


def body(unit):
    kb = config.limits()["evidence_max_kb"]
    return "\n".join([
        "# Pipeline job %s (%s)" % (unit["id"], unit["todo"]),
        "Item: %s" % unit["title"],
        "RECHECK means: rerun only this stage's checks on the outputs it already has. REGENERATE means: the full stage.",
        "The stage, as the board defines it:", "```json", json.dumps(unit["stage_json"], indent=1), "```",
        ("- Load the skill `%s` and follow it: it is the brief for the role `%s`."
         % (ROLE_SKILLS[unit["role"]], unit["role"])) if unit["role"] in ROLE_SKILLS else
        "- Load the skill for the role `%s` and follow it. There is no .claude/agents folder: the skill is the brief."
        % unit["role"],
        "- This checkout is the relay's pipeline worktree: pipeline git work is allowed here.",
        "- Do NOT claim, complete or release the job: the runner does that after it has checked your result.",
        "- Evidence: one fresh JPG per band (%s), at most %d KB each, named <band>.jpg, in the board folder %s/."
        % (", ".join(unit["bands"]) or "none asked", kb, evidence_dir(unit)),
        "- Commit your outputs on the lane with the edit gate green, and push the lane."])


def verify(unit, ctx):
    out, kb, px = [], config.limits()["evidence_max_kb"], config.limits()["evidence_min_px"]
    folder = Path(ctx["board"]) / evidence_dir(unit)
    for band in unit["bands"]:
        f = folder / (band + ".jpg")
        if not f.is_file():
            out.append("no evidence for band %s (%s)" % (band, f.name))
            continue
        size, dims = f.stat().st_size, jpeg_size(f.read_bytes())
        if not dims:
            out.append("evidence %s is not a JPEG picture" % f.name)
        elif min(dims) < px:
            out.append("evidence %s is %dx%d pixels: too small to show anything" % ((f.name,) + dims))
        elif size > kb * 1024:
            out.append("evidence %s is %d KB, the limit is %d" % (f.name, size // 1024, kb))
        elif f.stat().st_mtime < ctx.get("since", 0):
            out.append("evidence %s is older than this unit: it was not made by these legs" % f.name)
    if gitio.branch(ctx["work"]) != unit["lane"]:
        out.append("the work checkout is on %s, not %s" % (gitio.branch(ctx["work"]), unit["lane"]))
    elif not gitio.dirty(ctx["work"]) and not gitio.pushed(ctx["work"], unit["lane"]):
        out.append("the lane is not pushed")
    return out


def claim(unit):
    os.environ["TW_WORKER_PID"] = str(os.getpid())      # the runner outlives every leg
    P.main(["claim", unit["id"]])


def release():
    P.main(["release"])


def keep_note(unit, ctx, desk, lim):
    pass


def finish(unit, ctx, problems, outcome):
    """Write the board result. The leg never does: a result must rest on the script's checks."""
    v = verdict(problems, outcome)
    args = ["complete", unit["id"], "--verdict", v, "--note", ("; ".join(problems) or "checked by relay")[:200]]
    if v == "PASS" and unit["bands"]:
        args += ["--evidence"] + ["%s=%s/%s.jpg" % (b, evidence_dir(unit), b) for b in unit["bands"]]
    P.main(args)
    return v
