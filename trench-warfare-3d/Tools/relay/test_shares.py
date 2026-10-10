#!/usr/bin/env python3
"""Tests of how the day's tokens are split over kinds of work, and which board job goes first. Run from
trench-warfare-3d/: python Tools/relay/test_shares.py

The board is a folder made here; its jobs' states are worked out by the pipeline's own evaluate. Nothing is started.
The order is tested against the day it was written for (2026-10-10): fifteen ideas in flight, none landed, and the
runner's next pick a concept that had failed, by file name ahead of a motion one step from its end."""
import json
import os
import sys
import tempfile
import time
import types
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
import pipeline as P                                  # noqa: E402
import config, ledger, runner, sources               # noqa: E402
from sources import lane as lane_src                  # noqa: E402
from sources import pipeline as pipe_src              # noqa: E402

results = []
ROLE = "destruction-vfx-simulator"


def case(name, ok, detail=""):
    results.append(bool(ok))
    print(("ok    " if ok else "FAIL  ") + name + ("" if ok else "\n      " + str(detail)[:700]))


def item(board, iid, stages):
    """An item whose stages follow one another; `stages` is [(id, role)]."""
    st = [dict(id=s, role=r, station="desktop", inputs=[], outputs=[], **({"after": [stages[i - 1][0]]} if i else {}))
          for i, (s, r) in enumerate(stages)]
    P.write_json(board / "items" / (iid + ".json"), dict(id=iid, title=iid, lane="lane/show/pipe-" + iid, stages=st))


def result(board, iid, sid, verdict):
    """The stage's result as the runner would write it, for the job the state machine names now."""
    b = P.Board(board)
    info = P.evaluate(b.items()[iid], b)[sid]
    n = len(list((board / "results").glob("%s--%s--*.json" % (iid, sid)))) + 1
    P.write_json(board / "results" / ("%s--%s--%d.json" % (iid, sid, n)),
                 dict(job=info["job"], attempt=n, verdict=verdict, upstream_rev=info["upstream_rev"], note="x"))


def unit(board, uid, role, priority=None, kind=None):
    u = dict(id=uid, lane="lane/show/" + uid, role=role, goal="g", done_when=["python", "-c", "pass"])
    u.update({k: v for k, v in (("priority", priority), ("kind", kind)) if v is not None})
    P.write_json(board / "relay" / "queue" / (uid + ".json"), u)


def leg(board, n, source, role, usd, kind=None):
    rec = dict(run="20261010-103442-1", leg=n, unit="u%d" % n, source=source, role=role, phase="execute",
               cost_usd=usd, started_at=time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()))
    if kind:
        rec["kind"] = kind
    P.write_json(board / "relay" / "desktop" / "legs" / ("20261010-103442-1-%02d.json" % n), rec)


def main():
    board = Path(tempfile.mkdtemp(prefix="tw-shares-test-"))
    for d in ("items", "results", "feedback", "claims", "relay/queue", "relay/done", "relay/desktop/legs"):
        (board / d).mkdir(parents=True)
    os.environ.update(TW_BOARD=str(board), TW_STATION="desktop")
    os.environ.pop("TW_BOARD_ALSO", None)
    three = [("concept", ROLE), ("build", ROLE), ("motion", ROLE)]
    item(board, "aaa-new", three)                       # nothing of it worked yet
    item(board, "bbb-near", three)                      # one step from its end
    item(board, "ccc-mid", three)                       # two steps from its end
    item(board, "ddd-failed", three)                    # its first step failed
    for iid, done in (("bbb-near", ("concept", "build")), ("ccc-mid", ("concept",))):
        for sid in done:
            result(board, iid, sid, "PASS")
    result(board, "ddd-failed", "concept", "FAIL")
    ctx = {"board": board, "work": board, "station": "desktop", "skip": set(), "since": 0}
    real = config.limits

    def with_cap(n):
        config.limits = lambda *a, **k: dict(real(*a, **k), ideas_in_flight=n)
    with_cap(3)
    got = [(u["item"], u["stage"]) for u in pipe_src.candidates(ctx)]
    case("board jobs: the one nearest its item's end goes first, a job that just failed goes last; the old order was the file's name",
         got == [("bbb-near", "motion"), ("ccc-mid", "build"), ("ddd-failed", "concept")] and pipe_src.next(ctx)["item"] == "bbb-near", got)
    b = P.Board(board)
    case("in flight: an item with a stage worked and not every stage done; three are, so with a cap of three no new item is started",
         pipe_src.in_flight([P.evaluate(i, b) for i in b.items().values()]) == 3 and ("aaa-new", "concept") not in got, got)
    with_cap(4)
    got4 = [(u["item"], u["stage"]) for u in pipe_src.candidates(ctx)]
    with_cap(0)
    got0 = [(u["item"], u["stage"]) for u in pipe_src.candidates(ctx)]
    case("in flight: with room under the cap, or no cap, the new item's first step takes its place by steps left",
         got4 == got0 == [("bbb-near", "motion"), ("ccc-mid", "build"), ("aaa-new", "concept"), ("ddd-failed", "concept")], (got4, got0))
    with_cap(3)
    again = pipe_src.refresh(dict(item="aaa-new", stage="concept"), ctx)
    case("a job the runner already holds is found again whatever the cap says (refresh)", again and again["item"] == "aaa-new", again)
    case("a board job is finishing work: asked for another kind there is none", pipe_src.next(ctx, kind="fix") is None and pipe_src.next(ctx, kind="finish")["item"] == "bbb-near")

    # ---- the kinds, and the split ----
    case("kinds: the critique source is critique, the review's fixes and a found task that says so are fix, everything else is finishing",
         [sources.kind_of(*a) for a in (("critique", "critic"), ("lane", "review-fix"), ("lane", "lane", "fix"), ("lane", "lane"), ("pipeline", ROLE), ("lane", ROLE, "nonsense"))]
         == ["critique", "fix", "fix", "finish", "finish", "finish"])
    S = dict(finish=60, fix=25, critique=15)
    case("split: with nothing spent the largest share goes first; then the kind furthest under its share; a kind with no share is not in the order",
         sources.order(S, {}) == ["finish", "fix", "critique"] and sources.order(S, dict(finish=9.0, fix=1.0)) == ["critique", "fix", "finish"]
         and sources.order(S, dict(finish=5.0, fix=4.0, critique=1.0)) == ["critique", "finish", "fix"] and sources.order(dict(S, critique=0), dict(fix=9.0)) == ["finish", "fix"])
    unit(board, "rv-1", "review-fix")
    unit(board, "x-1", "lane")
    unit(board, "found-1", ROLE, kind="fix")
    names = ["pipeline", "lane"]
    first = sources.next_unit(names, dict(ctx, shares=S, spent_kinds={}))
    under = sources.next_unit(names, dict(ctx, shares=S, spent_kinds=dict(finish=9.0, fix=1.0)))
    case("the next unit is of the kind furthest under its share: finishing first on a fresh day, a fix when finishing has had its part (and a found task counts as a fix)",
         first["item"] == "bbb-near" and first["kind"] == "finish" and under["id"] == "found-1" and under["kind"] == "fix", (first, under))
    lends = sources.next_unit(["lane"], dict(ctx, shares=S, spent_kinds=dict(fix=9.0), skip={"x-1"}))
    case("a kind with nothing to do lends its room: finishing is owed, has no unit, and the fix runs",
         lends and lends["id"] == "found-1", lends)
    plain = sources.next_unit(names, ctx)
    plain_lane = lane_src.next(ctx)
    case("with no split named the runner picks as it did: the first source with a unit, the queue by priority then name",
         plain["source"] == "pipeline" and "kind" not in plain and plain_lane["id"] == "found-1", (plain, plain_lane))
    import relay as relay_cmd
    uf = board / "u.json"
    P.write_json(uf, dict(id="found-2", lane="lane/show/found-2", role="lane", goal="g", done_when=["python", "-c", "pass"], kind="fix", priority=40))
    kept = relay_cmd.unit_from(str(uf))
    P.write_json(uf, dict(id="found-3", lane="lane/show/found-3", role="lane", goal="g", done_when=["python", "-c", "pass"], colour="red"))
    P.write_json(board / "relay" / "queue" / "bad-kind.json", dict(id="bad-kind", lane="lane/show/x", role="lane", goal="g", done_when=["python"], kind="nonsense"))
    said = []
    for call in (lambda: relay_cmd.unit_from(str(uf)), lambda: lane_src.load(board / "relay" / "queue" / "bad-kind.json")):
        try:
            call()
        except SystemExit as e:
            said.append(str(e))
    (board / "relay" / "queue" / "bad-kind.json").unlink()
    case("a unit file may name its kind and its priority, and they are kept; another word in it, or a kind that is none of the three, is refused",
         kept["kind"] == "fix" and kept["priority"] == 40 and len(said) == 2 and "colour" in said[0] and "finish, fix or critique" in said[1], (kept, said))
    case("nothing of any kind is no unit", sources.next_unit(["lane"], dict(ctx, shares=S, spent_kinds={}, skip={"x-1", "rv-1", "found-1"})) is None)

    # ---- what the day spent on each kind ----
    leg(board, 1, "pipeline", ROLE, 6.0)
    leg(board, 2, "lane", "review-fix", 2.5)
    leg(board, 3, "lane", ROLE, 1.5, kind="fix")
    leg(board, 4, "critique", "critic", 1.0)
    leg(board, 5, "retro", "retro", 0.5)
    kinds = ledger.spent(board)["kinds"]
    case("the ledger adds the day up by kind from the leg records (old records too, by their source and role); a retrospective is nobody's share",
         kinds == dict(finish=6.0, fix=4.0, critique=1.0) and ledger.spent(board, "2020-01-01")["kinds"] == {}, kinds)
    case("with that day the critique is owed most (9 percent spent of a 15 percent share), then finishing (55 of 60), and the fixes have had more than theirs (36 of 25)",
         sources.order(S, kinds) == ["critique", "finish", "fix"], sources.order(S, kinds))

    # ---- between units the runner reads the day's figure and the split again ----
    home = board / "home"
    home.mkdir()
    lim = real()
    me = types.SimpleNamespace(home=home, base_pct=11, lim=dict(lim, day_budget_pct=11), ctx=dict(ctx), board=board)
    runner.Run.allot(me)
    plain_day = me.lim["day_budget_pct"] == 11 and me.ctx["shares"] == dict(finish=60.0, fix=25.0, critique=15.0) and me.ctx["spent_kinds"] == kinds
    P.write_json(home / "day.json", dict(day=time.strftime("%Y-%m-%d"), pct=21))
    P.write_json(home / "shares.json", dict(finish=40, fix=30, critique=30))
    runner.Run.allot(me)
    raised = me.lim["day_budget_pct"] == 21 and me.ctx["shares"]["critique"] == 30.0
    P.write_json(home / "day.json", dict(day="2026-10-09", pct=21))
    P.write_json(home / "shares.json", dict(finish=90, fix=30, critique=30))
    runner.Run.allot(me)
    case("allot: the run's own figure and the limits' split; a day.json that names today and a whole shares.json are taken at the next unit; "
         "yesterday's figure and a split that is not 100 are not, and the run's own stand again",
         plain_day and raised and me.lim["day_budget_pct"] == 11 and me.ctx["shares"]["finish"] == 60.0, (plain_day, raised, me.lim["day_budget_pct"], me.ctx["shares"]))
    P.write_json(home / "day.json", dict(day=time.strftime("%Y-%m-%d"), pct=500))
    runner.Run.allot(me)
    case("allot: a figure outside the limits' bounds is clamped to them", me.lim["day_budget_pct"] == 100, me.lim["day_budget_pct"])
    config.limits = real
    print("%d of %d cases behaved" % (sum(results), len(results)))
    return 0 if all(results) else 1


if __name__ == "__main__":
    sys.exit(main())
