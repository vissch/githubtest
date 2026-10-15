#!/usr/bin/env python3
"""Tests for pipeline.py on throwaway git repos: python Tools/pipeline/test_pipeline.py. Stdlib only."""
import io, json, os, subprocess, sys, tempfile, time, unittest
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
        # Run by a relay leg, pipeline.py would refuse every claim here (the leg's mark, and the marker in the
        # checkout it works in). These claims are on a throwaway board: work from the temp folder, without the mark.
        self.leg_mark, self.cwd = os.environ.pop("TW_RELAY", None), os.getcwd()
        os.chdir(t)

    def tearDown(self):
        os.chdir(self.cwd)
        if self.leg_mark is not None:
            os.environ["TW_RELAY"] = self.leg_mark
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

    def test_a_stage_with_a_role_nobody_knows_is_refused(self):
        def with_role(role):
            item = json.loads(json.dumps(ITEM))
            item["stages"][1]["role"] = role
            (self.board / "items" / "crate.json").write_text(json.dumps(item))
        with_role("charcter-simulator")                      # a slip of the hand: its worker would get no brief
        with self.assertRaises(SystemExit) as e:
            self.states()
        self.assertIn("charcter-simulator", str(e.exception))
        self.assertIn("roles.json", str(e.exception))
        for role in ("character-simulator", "character", "critic", "lowpoly", "sim", "master"):
            with_role(role)                                  # the long names, and the short ones the board holds
            self.assertEqual(self.states()["balance"], "READY")
        self.assertEqual(sorted(k for k, v in P.roles().items() if v == "tw-character-sim"),
                         ["character", "character-simulator"])

    def test_the_roles_an_accepted_ideas_route_names_are_known_and_have_a_brief(self):
        """Tools/assetboard/idearoute.py writes an item from an idea the owner accepted, and its first stages are
        these four: a role missing here is an item the board refuses, a role with no skill a leg with no brief."""
        briefs = {"game-designer": "tw-game-design", "concept-artist": "tw-concept-art", "ux": "tw-ux",
                  "ui-artist": "tw-ui-art"}
        self.assertEqual({r: P.roles().get(r) for r in briefs}, briefs)
        for role in briefs:
            item = json.loads(json.dumps(ITEM))
            item["stages"][1]["role"] = role
            (self.board / "items" / "crate.json").write_text(json.dumps(item))
            self.assertEqual(self.states()["balance"], "READY", role)

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

    def test_claim_with_no_pid_start_is_not_immortal(self):
        # [O7] a claim stored with pid_start=None (the worker died between worker_pid()'s check and take_lock's
        # own proc_start call) used to read as alive forever, since a dead pid's proc_start is also None.
        self.run_stage("balance", "laptop")
        os.environ["TW_STATION"] = "desktop"
        P.main(["claim", self.job("sim")])
        c = json.loads((self.board / "claims" / "desktop.json").read_text())
        c["pid"] = 999999
        c["pid_start"] = None
        (self.board / "claims" / "desktop.json").write_text(json.dumps(c))
        self.assertFalse(P.claim_alive(c), "[O7] a claim with pid_start=None read as alive forever")
        P.main(["claim", self.job("sim")])
        self.assertNotEqual(json.loads((self.board / "claims" / "desktop.json").read_text())["token"], c["token"])

    def test_later_fail_outranks_earlier_pass(self):
        # [O15] the latest attempt decides; a FAIL used to map back to READY, so `next` handed the same job out
        # forever with nothing changed. It is FAILED now: `next` skips it, `claim` still takes it by hand.
        self.run_stage("balance", "laptop")
        os.environ["TW_STATION"] = "laptop"
        self.fail_last("balance")
        self.assertEqual(self.states()["balance"], "FAILED", "[O15] a failed last attempt did not read as FAILED")
        out = io.StringIO()
        real = sys.stdout
        sys.stdout = out
        try:
            P.main(["next"])
        finally:
            sys.stdout = real
        self.assertIn("nothing for laptop", out.getvalue(),
                      "[O15] next handed the failed job back: " + out.getvalue().strip())
        P.main(["claim", self.job("balance")])   # a retry by hand is still possible

    def fail_last(self, stage):
        """Add a later FAIL attempt for this stage's current job."""
        j = self.job(stage)
        (self.board / "results" / (j + "--2.json")).write_text(json.dumps(dict(
            json.loads((self.board / "results" / (j + "--1.json")).read_text()), attempt=2, verdict="FAIL")))

    def test_o15_an_item_whose_id_is_not_its_file_name_is_refused(self):
        # [O15] results are globbed by item["id"], so an id that differs from the file stem reads another item's
        # results (and an id with a slash or ".." would be joined into a path).
        bad = json.loads(json.dumps(ITEM))
        bad["id"] = "other"
        (self.board / "items" / "crate.json").write_text(json.dumps(bad))
        with self.assertRaises(SystemExit, msg="[O15] an id/file-name mismatch was accepted") as e:
            self.states()
        self.assertIn("file name", str(e.exception))
        # [O15.1] a caller that lints a definition before it is written passes a label, not a file name
        # (idearoute checks a route by its kind): the id rule still applies, the file-name rule cannot.
        import pipeline as P
        P.lint_item(json.loads(json.dumps(ITEM)), "mechanic")
        with self.assertRaises(SystemExit, msg="[O15.1] a bad id was accepted when linted by label"):
            P.lint_item(dict(json.loads(json.dumps(ITEM)), id="../escape"), "mechanic")
        bad["id"] = "../escape"
        (self.board / "items" / "crate.json").write_text(json.dumps(bad))
        with self.assertRaises(SystemExit, msg="[O15] an item id that escapes the board was accepted") as e:
            self.states()
        self.assertIn("A-Za-z0-9", str(e.exception))

    def test_o15_two_stages_with_one_id_are_refused(self):
        # [O15] both stages would glob the same results/<item>--<stage>--*.json, so one stage's verdict decided
        # the other's state.
        bad = json.loads(json.dumps(ITEM))
        bad["stages"][2]["id"] = "sim"
        (self.board / "items" / "crate.json").write_text(json.dumps(bad))
        with self.assertRaises(SystemExit, msg="[O15] a duplicate stage id was accepted") as e:
            self.states()
        self.assertIn("two stages", str(e.exception))

    def test_o15_evidence_outside_the_board_is_refused(self):
        # [O15] the evidence path was only checked for existence, so "../outside.jpg" recorded a file off the board.
        outside = self.board.parent / "outside.jpg"
        outside.write_text("x")
        self.run_stage("balance", "laptop")
        os.environ["TW_STATION"] = "desktop"
        j = self.job("sim")
        P.main(["claim", j])
        with self.assertRaises(SystemExit, msg="[O15] evidence outside the board was accepted") as e:
            P.main(["complete", j, "--verdict", "FAIL", "--evidence", "t1=../outside.jpg"])
        self.assertIn("inside the board", str(e.exception))

    def test_o15_a_stale_claim_lock_does_not_block_forever(self):
        # [O15] the .lock was taken after the busy check and never aged out: a session killed while holding it
        # blocked every later claim on that station for good.
        os.environ["TW_STATION"] = "laptop"
        lock = self.board / "claims" / "laptop.json.lock"
        lock.parent.mkdir(parents=True, exist_ok=True)
        lock.write_text("")
        with self.assertRaises(SystemExit, msg="[O15] a fresh lock did not hold off a second claimer") as e:
            P.main(["claim", self.job("balance")])
        self.assertIn("claiming", str(e.exception))
        os.utime(lock, (time.time() - 600, time.time() - 600))
        P.main(["claim", self.job("balance")])
        self.assertTrue((self.board / "claims" / "laptop.json").exists(),
                        "[O15] a 600 s old lock still blocked the claim")
        self.assertFalse(lock.exists(), "[O15] the stale lock was left behind")

    def test_o15_release_of_a_live_claim_needs_the_token(self):
        # [O15] release deleted whatever claim file it found, so any session could drop a live worker's claim.
        os.environ["TW_STATION"] = "laptop"
        P.main(["claim", self.job("balance")])
        c = json.loads((self.board / "claims" / "laptop.json").read_text())
        c["pid"], c["pid_start"] = os.getppid(), P.proc_start(os.getppid())
        (self.board / "claims" / "laptop.json").write_text(json.dumps(c))
        with self.assertRaises(SystemExit, msg="[O15] release dropped another live worker's claim") as e:
            P.main(["release"])
        self.assertIn("--token", str(e.exception))
        P.main(["release", "--token", c["token"]])
        self.assertFalse((self.board / "claims" / "laptop.json").exists(), "[O15] --token did not release")

    def test_a_band_on_a_done_stage_stays_done_inputs_or_lane_still_go_stale(self):
        """Bands, station, outputs and role are how a stage is run, not what it reads."""
        self.all_done()
        item = json.loads((self.board / "items" / "crate.json").read_text())
        sim = item["stages"][1]
        sim["bands"] = ["t1", "t2", "t3"]
        sim["station"] = "laptop"
        sim["outputs"] = ["captures/", "notes/sim.md"]
        sim["role"] = "critic"
        (self.board / "items" / "crate.json").write_text(json.dumps(item))
        self.assertEqual(self.states()["sim"], "DONE")
        kept = json.loads(json.dumps(item))
        sim["inputs"] = list(sim["inputs"]) + ["other.txt"]
        (self.board / "items" / "crate.json").write_text(json.dumps(item))
        self.assertEqual(self.states()["sim"], "STALE")
        kept["stages"][1]["lane"] = "lane/show/pipe-crate-other"
        (self.board / "items" / "crate.json").write_text(json.dumps(kept))
        self.assertEqual(self.states()["sim"], "STALE")
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

    def test_commit_headroom_floor_defaults_to_10(self):
        """A forgotten --min-headroom-gb must still refuse a heavy start. 0 is the way out."""
        import run_detached as rd

        class Args(object):
            pass

        def args(name, floor):
            a = Args()
            a.name, a.min_headroom_gb, a.timeout, a.cwd = name, floor, 30, None
            a.cmd = [sys.executable, "-c", "pass"]
            return a

        seen = {}

        def spy(a):
            seen["a"] = a
            raise SystemExit("spy-stop")

        old_start = rd.cmd_start
        rd.cmd_start = spy
        try:
            with self.assertRaises(SystemExit) as caught:
                rd.main(["start", "headroom-probe", "--timeout", "30", "--", sys.executable, "-c", "pass"])
            self.assertEqual(str(caught.exception), "spy-stop")
        finally:
            rd.cmd_start = old_start
        self.assertEqual(seen["a"].min_headroom_gb, 10)

        old_hr, old_root = rd.headroom_gb, rd.runs_root
        floor = seen["a"].min_headroom_gb
        rd.headroom_gb = lambda: floor - 7
        rd.runs_root = lambda: Path(self.tmp.name) / "runs"
        try:
            with self.assertRaises(SystemExit) as caught:
                rd.cmd_start(args("low", floor))
            self.assertIn("refused", str(caught.exception))

            def past(*_a, **_k):
                raise RuntimeError("past-headroom")

            old_popen = rd.subprocess.Popen
            rd.subprocess.Popen = past
            rd.cmd_start = old_start
            try:
                with self.assertRaises(RuntimeError) as boom:
                    rd.main(["start", "way-out", "--timeout", "30", "--min-headroom-gb", "0", "--",
                             sys.executable, "-c", "pass"])
                self.assertEqual(str(boom.exception), "past-headroom")
            finally:
                rd.subprocess.Popen = old_popen
        finally:
            rd.cmd_start = old_start
            rd.headroom_gb, rd.runs_root = old_hr, old_root

    def test_start_refuses_while_the_recorded_child_still_runs(self):
        """A dead wrapper with a live child is CRASHED, not a free name. start must exit naming stop."""
        import run_detached as rd

        child = subprocess.Popen(
            [sys.executable, "-c", "import time; time.sleep(120)"],
            stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        gone = subprocess.Popen([sys.executable, "-c", "pass"])
        gone.wait(timeout=30)
        root = Path(self.tmp.name) / "runs"
        name = "child-still-running"
        old_root, old_popen = rd.runs_root, rd.subprocess.Popen

        def no_spawn(*_a, **_k):
            raise RuntimeError("spawned")

        rd.runs_root = lambda: root
        rd.subprocess.Popen = no_spawn
        try:
            child_start = rd.proc_start(child.pid)
            self.assertIsNotNone(child_start)
            d = root / name
            d.mkdir(parents=True)
            rd.write_json(d / "run.json", dict(
                name=name, cmd=[sys.executable, "-c", "pass"], cwd=self.tmp.name, timeout_s=30,
                log=str(d / "out.log"), state="RUNNING",
                wrapper_pid=gone.pid, wrapper_start=1,
                child_pid=child.pid, child_start=child_start,
                heartbeat=rd.now(), log_size=0))
            st, detail = rd.state(rd.read_json(d / "run.json"))
            self.assertEqual(st, "CRASHED", detail)
            self.assertIn("stop", detail)
            try:
                rd.main(["start", name, "--timeout", "30", "--min-headroom-gb", "0", "--",
                         sys.executable, "-c", "pass"])
            except SystemExit as caught:
                self.assertIn("stop", str(caught))
            except RuntimeError as spawned:
                self.fail("start spawned a second job instead of naming stop: %s" % spawned)
            else:
                self.fail("start returned while the recorded child was still running")
            self.assertEqual(rd.read_json(d / "run.json")["child_pid"], child.pid)
        finally:
            rd.runs_root, rd.subprocess.Popen = old_root, old_popen
            if child.poll() is None:
                child.kill()
                child.wait(timeout=10)


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

    def test_start_raises_when_wrapper_never_records_a_child(self):
        # [O5] a wrapper that never writes child_pid (e.g. the spawned exe does not exist) fell through the poll
        # loop straight to "started" and exit 0, hiding the failure.
        with self.assertRaises(SystemExit, msg="[O5] start did not raise when the wrapper never started"):
            R.main(["start", "nospawn", "--timeout", "5", "--", "tw-no-such-executable-xyz"])

    def test_wrap_survives_a_failed_status_write(self):
        # [O6] one failed mid-tick write_json (e.g. a Windows sharing violation) used to raise out of cmd_wrap and
        # kill the wrapper, so the job ran on with no timeout enforcement and no exit code ever written.
        rj = self.write("flaky", dict(state="STARTING", timeout_s=60, cwd=os.getcwd(),
                                      cmd=[sys.executable, "-c", "import time; time.sleep(0.5)"]))
        real_write_json = R.write_json
        calls = {"n": 0}

        def flaky_write_json(path, rec):
            calls["n"] += 1
            if calls["n"] == 2:
                raise OSError("simulated sharing violation")
            return real_write_json(path, rec)

        real_beat = R.BEAT_S
        R.write_json = flaky_write_json
        R.BEAT_S = 0.05
        try:
            a = type("A", (), {"name": "flaky"})()
            R.cmd_wrap(a)   # must not raise despite the simulated OSError
        finally:
            R.write_json = real_write_json
            R.BEAT_S = real_beat
        rec = P.read_json(rj)
        self.assertEqual(rec["state"], "DONE", "[O6] a failed status write left the wrapper without a verdict")
        self.assertIn("exit_code", rec)

    def test_a_bad_run_name_is_refused(self):
        # [O16] the name was joined into a path unchecked, so "../escape" read and wrote outside TW_RUNS.
        with self.assertRaises(SystemExit, msg="[O16] a run name that escapes TW_RUNS was accepted") as cm:
            R.main(["status", "../escape"])
        self.assertIn("bad run name", str(cm.exception), "[O16] the refusal does not name the name rule")

    def test_wrap_starts_a_fresh_log(self):
        # [O16] the log was opened "ab", so a second run of the same name left the old run's output in front of the
        # new one and a reader took stale text for fresh output.
        rj = self.write("relog", dict(state="STARTING", timeout_s=60, cwd=os.getcwd(),
                                      cmd=[sys.executable, "-c", "print('new')"]))
        Path(P.read_json(rj)["log"]).write_text("old\n")
        R.cmd_wrap(type("A", (), {"name": "relog"})())
        text = Path(P.read_json(rj)["log"]).read_text()
        self.assertNotIn("old", text, "[O16] the log still holds the previous run's output")
        self.assertIn("new", text, "[O16] the new run's output is missing")

    def test_the_timeout_is_not_wall_clock(self):
        # [O16] the timeout was measured with time.time(): an NTP step or a DST change could postpone a kill for
        # hours or fire one at once. Here the wall clock is frozen; a monotonic clock still times out.
        rj = self.write("mono", dict(state="STARTING", timeout_s=0.3, cwd=os.getcwd(),
                                     cmd=[sys.executable, "-c", "import time; time.sleep(3)"]))
        frozen = time.time()
        shim = type("T", (), {"time": staticmethod(lambda: frozen), "monotonic": staticmethod(time.monotonic),
                              "sleep": staticmethod(time.sleep)})
        real_time, real_beat = R.time, R.BEAT_S
        R.time, R.BEAT_S = shim, 0.05
        try:
            R.cmd_wrap(type("A", (), {"name": "mono"})())
        finally:
            R.time, R.BEAT_S = real_time, real_beat
        self.assertEqual(P.read_json(rj)["state"], "TIMEOUT",
                         "[O16] a frozen wall clock stopped the timeout from firing")

    def test_a_run_records_its_job_name(self):
        # [O16] grandchildren survived stop and timeout: nothing held the child's descendants. The wrapper now puts
        # the child in a named Windows Job, and the record names it so another process can kill the whole tree.
        rj = self.write("jobbed", dict(state="STARTING", timeout_s=60, cwd=os.getcwd(),
                                       cmd=[sys.executable, "-c", "pass"]))
        R.cmd_wrap(type("A", (), {"name": "jobbed"})())
        rec = P.read_json(rj)
        if os.name == "nt":
            self.assertEqual(rec.get("job"), "tw-run-jobbed", "[O16] the run does not record its Job object")
        else:
            self.assertIn("job", rec, "[O16] the record has no job field")


if __name__ == "__main__":
    unittest.main(verbosity=1)
