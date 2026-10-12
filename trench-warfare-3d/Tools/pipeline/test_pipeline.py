#!/usr/bin/env python3
"""Tests for pipeline.py on throwaway git repos: python Tools/pipeline/test_pipeline.py. Stdlib only."""
import json, os, subprocess, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import pipeline as P
import run_detached as R


def sh(cwd, *args):
    subprocess.run(["git"] + list(args), cwd=cwd, check=True, capture_output=True)


ITEM = {
    "id": "crate", "lane": "lane/show/pipe-crate",
    "stages": [
        {"id": "balance", "station": "laptop", "inputs": ["units.json#/crate"], "outputs": ["notes/balance.md"]},
        {"id": "sim", "station": "desktop", "after": ["balance"], "inputs": ["mesh/crate.txt", "units.json#/crate/hp"],
         "outputs": ["captures/"], "bands": ["t1", "t2"]},
        {"id": "destroy", "station": "desktop", "after": ["sim"], "inputs": ["mesh/crate.txt"], "outputs": ["chunks/"]},
    ],
}


class PipelineTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        t = Path(self.tmp.name)
        self.repo, self.board = t / "repo", t / "board"
        self.repo.mkdir()
        sh(self.repo, "init", "-q", "-b", "lane/show/pipe-crate")
        sh(self.repo, "config", "user.email", "t@t"); sh(self.repo, "config", "user.name", "t")
        self.units = {"crate": {"hp": 100, "cost": 5}, "tank": {"hp": 900}}
        (self.repo / "mesh").mkdir()
        (self.repo / "mesh" / "crate.txt").write_text("v1")
        self.commit()
        for d in ("items", "results", "feedback", "claims", "evidence"):
            (self.board / d).mkdir(parents=True)
        (self.board / "items" / "crate.json").write_text(json.dumps(ITEM))
        for band in ("t1", "t2"):
            (self.board / "evidence" / (band + ".jpg")).write_text("x")
        self.old = P.REPO
        P.REPO = self.repo
        os.environ["TW_BOARD"] = str(self.board)
        os.environ["TW_WORKER_PID"] = str(os.getpid())

    def tearDown(self):
        P.REPO = self.old
        for k in ("TW_BOARD", "TW_STATION", "TW_WORKER_PID"):
            os.environ.pop(k, None)
        self.tmp.cleanup()

    def commit(self):
        (self.repo / "units.json").write_text(json.dumps(self.units))
        sh(self.repo, "add", "-A"); sh(self.repo, "commit", "-q", "-m", "c", "--allow-empty")

    def states(self):
        b = P.Board()
        return {k: v["state"] for k, v in P.evaluate(b.items()["crate"], b).items()}

    def job(self, stage):
        b = P.Board()
        return P.evaluate(b.items()["crate"], b)[stage]["job"]

    def run_stage(self, stage, st, verdict="PASS", bands=()):
        os.environ["TW_STATION"] = st
        j = self.job(stage)
        P.main(["claim", j])
        P.main(["complete", j, "--verdict", verdict, "--evidence"] + ["%s=evidence/%s.jpg" % (b, b) for b in bands])
        return j

    def all_done(self):
        self.run_stage("balance", "laptop")
        self.run_stage("sim", "desktop", bands=("t1", "t2"))
        self.run_stage("destroy", "desktop")
        self.assertEqual(self.states(), {"balance": "DONE", "sim": "DONE", "destroy": "DONE"})

    def test_fresh_item_is_ready_then_blocked(self):
        self.assertEqual(self.states(), {"balance": "READY", "sim": "BLOCKED", "destroy": "BLOCKED"})

    def test_whole_chain_runs(self):
        self.all_done()

    def test_consumed_change_regenerates_downstream(self):
        self.all_done()
        self.units["crate"]["hp"] = 120      # read by balance (whole object) and by sim (field)
        self.commit()
        self.assertEqual(self.states(), {"balance": "STALE", "sim": "BLOCKED", "destroy": "BLOCKED"})
        self.run_stage("balance", "laptop")
        s = self.states()
        self.assertEqual(s["sim"], "STALE")          # its own field changed: regenerate
        self.run_stage("sim", "desktop", bands=("t1", "t2"))
        self.assertEqual(self.states()["destroy"], "RECHECK")   # only upstream results changed

    def test_unconsumed_change_rechecks_only(self):
        self.all_done()
        self.units["crate"]["cost"] = 7      # balance reads it, sim reads only hp
        self.commit()
        self.run_stage("balance", "laptop")
        self.assertEqual(self.states()["sim"], "RECHECK")

    def test_unrelated_change_leaves_everything_done(self):
        self.all_done()
        self.units["tank"]["hp"] = 1000
        (self.repo / "other.txt").write_text("x")
        self.commit()
        self.assertEqual(set(self.states().values()), {"DONE"})

    def test_own_output_never_stales_its_stage(self):
        bad = json.loads(json.dumps(ITEM))
        bad["stages"][2]["inputs"].append("chunks/a.fbx")
        (self.board / "items" / "crate.json").write_text(json.dumps(bad))
        with self.assertRaises(SystemExit) as e:
            self.states()
        self.assertIn("stale itself", str(e.exception))

    def test_pass_without_band_evidence_is_refused(self):
        self.run_stage("balance", "laptop")
        os.environ["TW_STATION"] = "desktop"
        j = self.job("sim")
        P.main(["claim", j])
        with self.assertRaises(SystemExit) as e:
            P.main(["complete", j, "--verdict", "PASS", "--evidence", "t1=evidence/t1.jpg"])
        self.assertIn("t2", str(e.exception))

    def test_wrong_station_cannot_claim(self):
        os.environ["TW_STATION"] = "desktop"
        with self.assertRaises(SystemExit):
            P.main(["claim", self.job("balance")])

    def test_second_worker_on_a_station_is_refused(self):
        self.run_stage("balance", "laptop")
        os.environ["TW_STATION"] = "desktop"
        P.main(["claim", self.job("sim")])
        with self.assertRaises(SystemExit) as e:
            P.main(["claim", self.job("sim")])
        self.assertIn("busy", str(e.exception))

    def test_dead_worker_is_taken_over(self):
        self.run_stage("balance", "laptop")
        os.environ["TW_STATION"] = "desktop"
        P.main(["claim", self.job("sim")])
        c = json.loads((self.board / "claims" / "desktop.json").read_text())
        c["pid_start"] = -1                  # a pid reused by another process has a different start time
        (self.board / "claims" / "desktop.json").write_text(json.dumps(c))
        P.main(["claim", self.job("sim")])
        self.assertNotEqual(json.loads((self.board / "claims" / "desktop.json").read_text())["token"], c["token"])

    def test_later_fail_outranks_earlier_pass(self):
        self.run_stage("balance", "laptop")
        os.environ["TW_STATION"] = "laptop"
        b = P.Board()
        # re-run the same job and fail it: the latest attempt decides
        (self.board / "results" / (self.job("balance") + "--2.json")).write_text(json.dumps(dict(
            json.loads((self.board / "results" / (self.job("balance") + "--1.json")).read_text()),
            attempt=2, verdict="FAIL")))
        self.assertEqual(self.states()["balance"], "READY")

    def test_claim_needs_a_worker_pid_that_outlives_one_command(self):
        # [O2] an agent's per-command shell exits at once, so a claim owned by it reads as dead and the job never
        # shows IN_PROGRESS: a claim without TW_WORKER_PID is refused, with a live one the stage is IN_PROGRESS.
        os.environ["TW_STATION"] = "laptop"
        j = self.job("balance")
        os.environ.pop("TW_WORKER_PID")
        with self.assertRaises(SystemExit, msg="[O2] claim without TW_WORKER_PID was accepted"):
            P.main(["claim", j])
        self.assertFalse((self.board / "claims" / "laptop.json").exists(),
                         "[O2] a refused claim still wrote a claim file")
        os.environ["TW_WORKER_PID"] = str(os.getpid())
        P.main(["claim", j])
        self.assertEqual(self.states()["balance"], "IN_PROGRESS", "[O2] the job never shows IN_PROGRESS")


    def test_feedback_regenerates_and_closing_does_not(self):
        self.all_done()
        os.environ["TW_STATION"] = "laptop"
        P.main(["feedback", "crate", "sim", "the crate floats at far zoom", "--check", "gap < 1 px at t2"])
        self.assertEqual(self.states()["sim"], "STALE")
        self.run_stage("sim", "desktop", bands=("t1", "t2"))
        self.assertEqual(self.states()["sim"], "DONE")
        fid = next((self.board / "feedback").glob("FR-*.json")).stem
        P.main(["close-feedback", fid])
        self.assertEqual(self.states()["sim"], "DONE")


class RunDetachedTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        os.environ["TW_RUNS"] = self.tmp.name

    def tearDown(self):
        os.environ.pop("TW_RUNS", None)
        self.tmp.cleanup()

    def write(self, name, rec):
        d = Path(self.tmp.name) / name
        d.mkdir(parents=True, exist_ok=True)
        rj = d / "run.json"
        P.write_json(rj, dict(rec, name=name, log=str(d / "out.log")))
        (d / "out.log").write_text("x")
        return rj

    def test_stop_keeps_a_finished_verdict_and_refuses_an_unknown_name(self):
        # [O3] stop on a finished run rewrote DONE exit=0 as STOPPED exit=None (the verdict is lost) and an
        # unknown name was a traceback. No pids in the record: the pre-fix stop kills every matching recorded pid.
        rj = self.write("fin", dict(state="DONE", exit_code=0, timeout_s=60, cwd=os.getcwd(), cmd=["x"]))
        R.main(["stop", "fin"])
        rec = P.read_json(rj)
        self.assertEqual((rec["state"], rec["exit_code"]), ("DONE", 0),
                         "[O3] stop erased the verdict of a finished run")
        with self.assertRaises(SystemExit, msg="[O3] stop on an unknown name is not a clean refusal"):
            R.main(["stop", "nosuch"])

    def test_start_refuses_while_a_recorded_child_still_lives(self):
        # [O4] "wrapper gone, child still running" reads CRASHED; a second start overwrote the record, so the old
        # child had no timeout and could not be stopped by name. cmd_start never kills, so our own pid is safe here.
        me = os.getpid()
        rj = self.write("crash", dict(state="RUNNING", wrapper_pid=1, wrapper_start="x", child_pid=me,
                                      child_start=P.proc_start(me), heartbeat=R.now(), log_size=1,
                                      timeout_s=60, cwd=os.getcwd(), cmd=["python", "-c", "pass"]))
        self.assertEqual(R.state(P.read_json(rj))[0], "CRASHED", "[O4] the record is not the CRASHED case")
        with self.assertRaises(SystemExit, msg="[O4] start overwrote a record whose child still runs"):
            R.main(["start", "crash", "--timeout", "60", "--", "python", "-c", "pass"])
        self.assertEqual(P.read_json(rj).get("child_pid"), me, "[O4] the running child was forgotten")


if __name__ == "__main__":
    unittest.main(verbosity=1)
