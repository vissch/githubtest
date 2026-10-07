"""Source: jobs on the two-station board (docs/reference/stations.md). The runner claims and completes; the leg only
does the stage. A master stage (review, gate, land) is the owner's and is never taken."""
import json, os, re, shutil, sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "pipeline"))
import pipeline as P   # noqa: E402

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import config, gitio   # noqa: E402
from sources import briefs, verdict   # noqa: E402
from sources.briefs import ROLE_SKILLS, brief_name, place_brief   # noqa: E402,F401  (the runner asks a source)

NAME = "pipeline"
SKIP_ROLES = ("master",)


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
        briefs.card_line(unit["role"]) or
        "- Follow the stage notes. There is no brief on this machine for the role `%s`." % unit["role"],
        "- This checkout is the relay's pipeline worktree: pipeline git work is allowed here.",
        "- Do NOT claim, complete or release the job: the runner does that after it has checked your result.",
        "- Evidence: one fresh JPG per band (%s), at most %d KB each, named <band>.jpg, in the board folder %s/."
        % (", ".join(unit["bands"]) or "none asked", kb, evidence_dir(unit)),
        "- Beside each still the game rendered, its capture sidecar as <band>.json, and one frames.txt in that folder: "
        "a line of facts per image that starts `<band>.jpg:` (view and band, moment, clock held or running, what is "
        "in frame). A still the rig did not shoot as it stands (a contact sheet, a diff, a concept sheet, a mock-up) "
        "has no sidecar: its frames.txt line says `no sidecar`. The runner fails a job without these, and the critic "
        "sees only that folder.",
        "- Commit your outputs on the lane with the edit gate green, and push the lane."])


def sidecar_problem(folder, band, since):
    """What is wrong with the facts beside a still, or None. The critic scores a capture by its sidecar and reads
    the frame's limits in frames.txt; a still the rig did not shoot as it stands says `no sidecar` on its line."""
    try:
        lines = (folder / "frames.txt").read_text(encoding="utf-8").splitlines()
    except (OSError, ValueError):
        return "no readable frames.txt beside the evidence (one line of facts per image, starting <band>.jpg:)"
    mine = [l for l in lines if l.startswith(band + ".jpg:")]
    if not mine:
        return "frames.txt has no line that starts %s.jpg:" % band
    f = folder / (band + ".json")
    if not f.is_file():
        return None if "no sidecar" in mine[0].lower() else (
            "evidence %s.jpg has no sidecar %s.json, and its frames.txt line does not say `no sidecar`" % (band, band))
    try:
        if not isinstance(json.loads(f.read_text(encoding="utf-8")), dict):
            raise ValueError
    except (OSError, ValueError):
        return "sidecar %s is not a JSON object" % f.name
    if f.stat().st_mtime < since:
        return "sidecar %s is older than this unit: it was not written with this still" % f.name
    return None


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
        elif sidecar_problem(folder, band, ctx.get("since", 0)):
            out.append(sidecar_problem(folder, band, ctx.get("since", 0)))
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


CRITIC_ROUND = re.compile(r"^critic-r\d+\.md$")
RUBRIC = Path(__file__).resolve().parents[4] / ".claude" / "skills" / "tw-critic" / "SKILL.md"


def rubric():
    """The tw-critic skill without its front matter: a critic leg works outside the repo, so it cannot load it."""
    try:
        text = RUBRIC.read_text(encoding="utf-8")
    except OSError:
        return "(the tw-critic skill is missing: score coverage, look, budget, repeatability and cost, 100 in all)"
    return re.sub(r"\A---\n.*?\n---\n", "", text, flags=re.S).strip()


def critic(unit, ctx, round_no):
    """A blind critic gets the stage's evidence and the stage as the board defines it, never the producer's story."""
    folder = Path(ctx["board"]) / evidence_dir(unit)

    def fill(dst):
        dst.mkdir(parents=True, exist_ok=True)
        for f in sorted(folder.iterdir()) if folder.is_dir() else []:
            if f.is_file() and not CRITIC_ROUND.match(f.name):
                shutil.copy2(f, dst / f.name)
        P.write_json(dst / "stage.json", unit["stage_json"])

    body = "\n".join([
        "# Critic round %d: pipeline job %s" % (round_no, unit["id"]),
        "Item: %s. Stage `%s`, role `%s`. Bands asked: %s."
        % (unit["title"], unit["stage"], unit["role"], ", ".join(unit["bands"]) or "none"),
        "- Your working folder holds the evidence bundle and stage.json (the stage as the board defines it).",
        "- Judge only by those files. You are not told how the work was made.",
        "- Score it out of 100 with the rubric below, for this role. Write critic.md in your leg folder, in the "
        "rubric's output shape: the VERDICT line first, TOP-3 MANDATED FIXES as a numbered list of three.",
        "", "# The rubric (the tw-critic skill)", rubric()])
    return {"body": body, "fill": fill}


def keep_critic(unit, ctx, round_no, text):
    f = Path(ctx["board"]) / evidence_dir(unit) / ("critic-r%d.md" % round_no)
    f.parent.mkdir(parents=True, exist_ok=True)
    f.write_text(text, encoding="utf-8", newline="\n")


def finish(unit, ctx, problems, outcome):
    """Write the board result. The leg never does: a result must rest on the script's checks."""
    v = verdict(problems, outcome)
    note = "; ".join(problems) or "checked by relay"
    if unit.get("critic_note"):
        note += "; " + unit["critic_note"]
    args = ["complete", unit["id"], "--verdict", v, "--note", note[:200]]
    if v == "PASS" and unit["bands"]:
        args += ["--evidence"] + ["%s=%s/%s.jpg" % (b, evidence_dir(unit), b) for b in unit["bands"]]
    P.main(args)
    return v
