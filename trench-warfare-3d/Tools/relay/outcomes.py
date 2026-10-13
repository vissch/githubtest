#!/usr/bin/env python3
"""What became of a unit after it passed, and what a landed unit costs. The ledger says what a leg cost and the board
says a unit passed; nothing said whether the passed work ever reached integration or had to be mended (2026-10-12:
twenty units passed in two nights, none landed). Reads only: the board's records and one checkout's git.

  python Tools/relay/relay.py outcomes [--repo <checkout>] [--into <ref>] [--days 14] [--json]

  read(board, repo, ..)     {"units": [one row per unit with leg records], "tiers": [the sums per tier], ..}
  lines(got)                the same for a person
  state(repo, into, tip, grew)   (landed | passed | empty | unknown, why) for one lane head, from git alone
A unit is every leg record with its id (ledger.py reads them, the boards TW_BOARD_ALSO names too); a retrospective
is no unit. A row holds:
  usd, legs, unpriced       its legs' cost added up (ledger.known_cost), and how many legs named no cost: those
                            count as nothing here, never at a usual price
  plan, execute             the model of its plan legs and of its execute legs, from the leg records (two models
                            read "sonnet+opus": a cheaper leg failed and the work moved up); "none" with no such
                            leg, "unknown" when a record names no model. The two together are the unit's tier
  outcome                   open       it has not passed: no <board>/relay/done/<id>.json, and for a board job no
                                       PASS as its newest result (<board>/results/<id>--<n>.json)
                            passed     it passed, and its lane has a commit whose patch is not on integration
                            landed     it passed, and its lane is on integration (below)
                            reworked   landed or passed, and a LATER unit of kind fix (sources.kind_of) changed one
                                       of the same files within --days days of the pass; reworked_by names it
                            empty      it passed, and its legs added no commit: nothing to land
                            unknown    it passed, and this checkout cannot say: the lane or its head is not here,
                                       or the records name neither
  landed                    true | false | null (not known), also for a reworked unit
The lane is judged at the head the unit passed at (the done file's head, else its last leg's head_after), else at
origin/<lane>, else <lane>. Landed is: every commit in merge-base(integration, head)..head has a commit with the
same patch on integration (`git cherry` prints only "-" lines). An ancestor test alone would miss every landing:
land.py rebases the lane first, and a combined landing branch cherry-picks, so the hashes on integration are other
ones. A head that is an ancestor of integration is landed too, unless the leg records show the legs added nothing
(empty); when they do not say (records from before the heads were kept), it is unknown, never a guess.
A commit that was changed on its way in (a conflict settled by hand) has another patch and reads as not landed.
The files a unit changed are what its legs added (head_before..head_after of each leg), without docs/: every leg
may write decisions.md, and that is no rework. A unit whose commits are not in this checkout has no files, so
nothing is said to rework it or to be reworked by it (files_known false).
The sums are per tier: units, passed, landed, reworked, what the tier's units cost in all, that cost per passed
unit and per LANDED unit (every dollar the tier spent, also on units that never passed, over the units that got
there: "none landed" when none did), and the rework rate (reworked over passed).
Nothing is fetched: the answer is as new as the checkout's refs, and the first line says how new that is.
Stdlib only. ASCII only.
"""
import argparse, datetime, json, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
import pipeline as P                    # noqa: E402
import gitio, ledger                    # noqa: E402
from sources import kind_of             # noqa: E402

REWORK_DAYS = 14
STAMP = "%Y-%m-%dT%H:%M:%SZ"
NONE, UNKNOWN = "none", "unknown"


def _commit(repo, name):
    """The full hash of a commit this checkout has under that name, or ''."""
    if not name:
        return ""
    return gitio.git(["rev-parse", "-q", "--verify", str(name) + "^{commit}"], repo, check=False)


def integration(repo, named=None):
    """(the ref the lanes are judged against, its hash): the one named, else origin's integration branch, else this
    checkout's own. ('', '') when the checkout has none of them."""
    for ref in ([named] if named else ["origin/" + gitio.INTEGRATION, gitio.INTEGRATION]):
        if _commit(repo, ref):
            return ref, _commit(repo, ref)
    return "", ""


def state(repo, into, tip, grew=None):
    """(landed | passed | empty | unknown, why) for a lane head, from git alone. grew: True when the leg records
    show the unit's legs added a commit, False when they show none was added, None when they do not say."""
    if not _commit(repo, tip):
        return UNKNOWN, "its head is not in this checkout"
    if not gitio.git(["merge-base", into, tip], repo, check=False):
        return UNKNOWN, "its head shares no history with integration"
    own = gitio.git(["rev-list", "--count", "%s..%s" % (into, tip)], repo, check=False)
    if own == "0":                                          # the head is an ancestor of integration
        if grew is None:
            return UNKNOWN, "its head is on integration, and the leg records do not say whether the legs added a commit"
        return ("landed", "its head is on integration") if grew else ("empty", "its legs added no commit")
    marks = [l[:1] for l in gitio.git(["cherry", into, tip], repo, check=False).splitlines() if l.strip()]
    if not marks:                                           # only merges: no patch to look for
        return UNKNOWN, "its %s commits hold no patch to look for (merges)" % own
    ahead = marks.count("+")
    if ahead:
        return "passed", "%d of its %d commits %s not on integration" % (ahead, len(marks), "is" if ahead == 1 else "are")
    return "landed", "every one of its %d commits has its patch on integration" % len(marks)


def _stamp(text):
    try:
        return datetime.datetime.strptime(text, STAMP)
    except (TypeError, ValueError):
        return None


def _models(legs, phase):
    """The model of a unit's legs of one phase, in the order they first ran: "opus", "sonnet+opus", none, unknown."""
    mine = [l.get("model") for l in legs if l.get("phase") == phase]
    if not mine:
        return NONE
    if not all(isinstance(m, str) and m for m in mine):
        return UNKNOWN
    return "+".join(sorted(set(mine), key=mine.index))


def passes(board):
    """{unit id: {lane, head, done_at}} for every unit that passed: a queued unit's done file, and a board job whose
    newest result is a PASS (a later FAIL outranks it, as in pipeline.valid_result). Every board is read."""
    out = {}
    for b in ledger.boards(board):
        newest = {}
        for f in sorted((b / "results").glob("*.json")) if (b / "results").is_dir() else []:
            r = ledger._read(f) or {}
            if isinstance(r.get("job"), str) and isinstance(r.get("attempt"), int) \
                    and r["attempt"] >= newest.get(r["job"], r)["attempt"]:
                newest[r["job"]] = r
        for job, r in newest.items():
            if r.get("verdict") == "PASS":
                out.setdefault(job, {"lane": None, "head": None, "done_at": r.get("finished_at")})
        for f in sorted((b / "relay" / "done").glob("*.json")) if (b / "relay" / "done").is_dir() else []:
            r = ledger._read(f) or {}
            out.setdefault(f.stem, {"lane": r.get("lane"), "head": r.get("head"), "done_at": r.get("done_at")})
    return out


def units(board, home=None):
    """{unit id: its leg records, oldest first}, as ledger.py reads them. A retrospective is no unit."""
    out = {}
    for f in ledger._files(board):
        rec = ledger._read(f)
        if rec and rec.get("unit") and rec.get("source") != "retro" and rec.get("phase") != "retro":
            out.setdefault(str(rec["unit"]), []).append(rec)
    for legs in out.values():
        legs.sort(key=lambda l: str(l.get("started_at") or l.get("finished_at") or ""))
    return out


def _files_of(repo, legs):
    """(the files the legs added commits to, without docs/; True when git could say for every leg that moved)."""
    files, known = set(), True
    for l in legs:
        a, b = l.get("head_before"), l.get("head_after")
        if not a or not b or a == b:
            continue
        if not _commit(repo, a) or not _commit(repo, b) \
                or gitio.git_raw(["merge-base", "--is-ancestor", a, b], repo).returncode:
            known = False                                   # not here, or the lane was rebased under the leg
            continue
        files.update(gitio.code_changed(repo, a, b))
    return files, known


def _grew(legs):
    """True when the records show a leg added a commit, False when they show none did, None when they do not say."""
    said = [(l.get("head_before"), l.get("head_after")) for l in legs]
    if any(l.get("commits") for l in legs) or any(a and b and a != b for a, b in said):
        return True
    return False if said and all(a and b for a, b in said) else None


def read(board, repo=None, into=None, days=REWORK_DAYS, home=None):
    """Every unit with leg records, what it cost, its tier and its outcome, and the sums per tier."""
    repo = Path(repo or HERE)
    ref, sha = integration(repo, into)
    done, rows, seen, judged = passes(board), [], {}, {}
    for uid, legs in sorted(units(board, home).items()):
        cost = [ledger.known_cost(l, home) for l in legs]
        last, d = legs[-1], done.get(uid)
        row = {"unit": uid, "source": last.get("source"), "role": last.get("role"),
               "kind": kind_of(last.get("source"), last.get("role"), last.get("kind")),
               "lane": (d or {}).get("lane") or next((l["lane"] for l in reversed(legs) if l.get("lane")), None),
               "legs": len(legs), "usd": round(sum(c for c in cost if c is not None), 4), "unpriced": cost.count(None),
               "plan": _models(legs, "plan"), "execute": _models(legs, "execute"),
               "started_at": legs[0].get("started_at"), "passed_at": (d or {}).get("done_at") if d else None,
               "outcome": "open", "landed": None, "why": "it has not passed", "reworked_by": None, "shared": []}
        row["tier"] = "plan %s, execute %s" % (row["plan"], row["execute"])
        files, known = _files_of(repo, legs) if sha else (set(), False)
        row["files_known"] = bool(known and files)
        seen[uid] = files
        if d:
            head = d.get("head") or next((l["head_after"] for l in reversed(legs) if l.get("head_after")), None)
            tips = [t for t in (head, row["lane"] and "origin/" + row["lane"], row["lane"]) if t]
            tip = next((t for t in tips if _commit(repo, t)), None)
            if not sha:
                row["outcome"], row["why"] = UNKNOWN, "this checkout has no integration branch"
            elif not tips:
                row["outcome"], row["why"] = UNKNOWN, "the records name no lane and no head for it"
            elif not tip:
                row["outcome"], row["why"] = UNKNOWN, "neither its head nor its lane is in this checkout"
            else:
                key = (_commit(repo, tip), _grew(legs))     # units that passed at one head are judged once
                if key not in judged:
                    judged[key] = state(repo, sha, tip, key[1])
                row["outcome"], row["why"] = judged[key]
                if tip != head:
                    row["why"] += " (judged at %s: %s)" % (tip, "the head it passed at is not in this checkout"
                                                          if head else "the records hold no head for it")
            row["landed"] = {"landed": True, "passed": False, "empty": False}.get(row["outcome"])
        rows.append(row)
    span = datetime.timedelta(days=days)
    fixes = sorted((r for r in rows if r["kind"] == "fix" and seen[r["unit"]] and _stamp(r["started_at"])),
                   key=lambda r: r["started_at"])
    for row in rows:
        at = _stamp(row["passed_at"])
        if row["outcome"] not in ("landed", "passed") or not at or not seen[row["unit"]]:
            continue
        for fix in fixes:
            both = seen[row["unit"]] & seen[fix["unit"]]
            if fix["unit"] != row["unit"] and both and at < _stamp(fix["started_at"]) <= at + span:
                row.update(outcome="reworked", reworked_by=fix["unit"], shared=sorted(both)[:5],
                           why="%s changed %s%s after it passed" % (fix["unit"], sorted(both)[0],
                                                                    " and %d more" % (len(both) - 1) if len(both) > 1 else ""))
                break
    return {"board": str(board), "repo": str(repo), "into": ref, "into_sha": sha,
            "into_date": gitio.git(["log", "-1", "--format=%cI", sha], repo, check=False) if sha else "",
            "rework_days": days, "units": rows, "tiers": tiers(rows),
            "passed_without_legs": len([u for u in done if u not in seen])}



def _per(usd, n):
    return round(usd / n, 4) if n else None


def tiers(rows):
    """The sums per tier, the tier with the most units first: [{tier, plan, execute, units, passed, landed,
    reworked, open, empty, unknown, usd, unpriced, usd_per_passed, usd_per_landed, rework_rate}]. A figure with
    nothing to divide by is None: no unit passed, or none landed."""
    out = {}
    for r in rows:
        t = out.setdefault(r["tier"], {"tier": r["tier"], "plan": r["plan"], "execute": r["execute"], "units": 0,
                                       "passed": 0, "landed": 0, "reworked": 0, "open": 0, "empty": 0, "unknown": 0,
                                       "usd": 0.0, "unpriced": 0})
        t["units"] += 1
        t["usd"] += r["usd"]
        t["unpriced"] += r["unpriced"]
        t["passed"] += r["outcome"] != "open"
        t["landed"] += r["landed"] is True
        for k in ("reworked", "open", "empty", "unknown"):
            t[k] += r["outcome"] == k
    for t in out.values():
        t.update(usd=round(t["usd"], 4), usd_per_passed=_per(t["usd"], t["passed"]),
                 usd_per_landed=_per(t["usd"], t["landed"]),
                 rework_rate=round(t["reworked"] / t["passed"], 4) if t["passed"] else None)
    return sorted(out.values(), key=lambda t: (-t["units"], t["tier"]))


def _n(n, word):
    return "%d %s%s" % (n, word, "" if n == 1 else "s")


def lines(got):
    """What `relay.py outcomes` prints: a line per unit, oldest first, then a line per tier."""
    rows = sorted(got["units"], key=lambda r: str(r["started_at"] or ""))
    if not got["into_sha"]:
        head = "no integration branch in %s: nothing can be said about a landing" % got["repo"]
    else:
        head = "judged in %s against %s at %s (%s)" % (Path(got["repo"]).name, got["into"], got["into_sha"][:10],
                                                     got["into_date"][:10])
    out = ["Outcomes of %s with leg records on %s, %s. Nothing was fetched." % (_n(len(rows), "unit"), got["board"], head)]
    wide = min(44, max([len(r["unit"]) for r in rows] or [4]))
    for r in rows:
        cost = "$%8.2f%s" % (r["usd"], "*" if r["unpriced"] else " ")
        out.append("  %-*s %-8s %s %-22s %s%s" % (wide, r["unit"][:wide], r["outcome"], cost,
                                                 "%s/%s" % (r["plan"], r["execute"]), r["why"],
                                                 " (it had landed)" if r["outcome"] == "reworked" and r["landed"] else ""))
    if any(r["unpriced"] for r in rows):
        out.append("  * %s named no cost and count as nothing." % _n(sum(r["unpriced"] for r in rows), "leg"))
    if got["passed_without_legs"]:
        out.append("  %s passed with no leg record here (no leg was needed, or its records are elsewhere): not counted."
                   % _n(got["passed_without_legs"], "unit"))
    out.append("By tier (the model of the plan legs / of the execute legs); a cost per unit is everything the tier "
               "spent over the units that got there:")
    for t in got["tiers"]:
        per = "$%.2f per passed unit" % t["usd_per_passed"] if t["passed"] else "none passed"
        land = "$%.2f per landed unit" % t["usd_per_landed"] if t["landed"] else "none landed"
        rework = "rework %d of %d (%d%%)" % (t["reworked"], t["passed"], round(100 * t["rework_rate"])) \
            if t["passed"] else "no rework to count"
        rest = ", ".join(["%d %s" % (t[k], k) for k in ("empty", "unknown") if t[k]]
                         + (["%s with no cost named" % _n(t["unpriced"], "leg")] if t["unpriced"] else []))
        out.append("  %-22s %s, %d passed, %d landed, $%.2f spent: %s, %s, %s%s"
                   % ("%s/%s" % (t["plan"], t["execute"]), _n(t["units"], "unit"), t["passed"], t["landed"], t["usd"],
                      per, land, rework, " (%s)" % rest if rest else ""))
    return out


def main(argv=None):
    p = argparse.ArgumentParser(prog="relay.py outcomes", description=__doc__.splitlines()[0])
    p.add_argument("--repo", help="the checkout whose git says what landed (default: the one this file is in)")
    p.add_argument("--into", help="the ref a landing reaches (default: origin's integration branch, else the local one)")
    p.add_argument("--days", type=int, default=REWORK_DAYS, help="how long after a pass a fix counts as rework")
    p.add_argument("--json", action="store_true", help="the rows and the sums as JSON, for a page to read")
    a = p.parse_args(argv)
    got = read(P.board_dir(), a.repo, a.into, max(0, a.days))
    if a.json:
        print(json.dumps(got, indent=1, sort_keys=True))
        return 0
    for line in lines(got):
        print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main())
