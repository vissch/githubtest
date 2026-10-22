#!/usr/bin/env python3
"""Tests for aosa.py on temporary fixture data. Run from trench-warfare-3d/: python Tools/aosa/test_aosa.py

Every test points AOSA_DOCS, TW_BUILDS, TW_PROJECT and LOCALAPPDATA at a temp dir and sets AOSA_OFFLINE, so the real
docs, the editor slot, the builds and the network are never touched.
"""
import contextlib, datetime, importlib.util, io, json, os, shutil, subprocess, sys, tempfile, time, unittest
sys.dont_write_bytecode = True
import pathlib
from pathlib import Path


def git_bash():
    """Git's own bash. A bare "bash" on Windows can be WSL's launcher (WindowsApps), first on PATH on the desktop:
    it cannot open a C: path, so the shell cases exited 127 there instead of testing the scripts."""
    git = shutil.which("git")
    if git:
        g = Path(git).resolve()
        for cand in (g.parent / "bash.exe", g.parent.parent / "bin" / "bash.exe", g.parent.parent / "usr" / "bin" / "bash.exe"):
            if cand.exists():
                return str(cand)
    return shutil.which("bash")

NL = chr(10)
ESC = chr(92)
HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("aosa", str(HERE / "aosa.py"))
aosa = importlib.util.module_from_spec(spec)
spec.loader.exec_module(aosa)

BACKLOG = """# AOSA backlog

Some preamble that must survive `age`.

| id | class | tier | metric: now -> target | predicted delta | evidence | size | owner | age | idle | status |
|---|---|---|---|---|---|---|---|---|---|---|
| C01 | instrument | T1 | noise band | n/a | none yet \\| escaped pipe | S | loop | 0 | 0 | ready |
| C02 | cull | T1 | gpu_ms: ? -> -0.3 | -0.3 | bounds | S | loop | 1 | 2 | ready |
| C03 | cache | T1 | main_ms: ? -> -0.2 | -0.2 | RemoveAt(0) | M | loop | 4 | 3 | ready |
| C04 | juice | T2 | critic | +3 | cook-off | S | loop | 0 | 0 | ready |
| C05 | juice | T1 | critic | +2 | rifle | S | loop | 0 | 0 | ready |
| C06 | art | T1 | coast | +2 | owner call | L | owner | 0 | 0 | parked:ASK A04 |
| C07 | shader | T1 | gpu_ms | -0.1 | ink | S | loop | 2 | 1 | done:a0003 |
| C08 | batch | T1 | setpass | -60 | kit | L | loop | 0 | 0 | wip |

Trailing text after the table.
"""


def report(label, build="development", hs="AAAA", he=None, main=8.0, gpu=5.0, draws=330, alive=1681, vat=1.5e6,
           knobs=None, gpu_name="RTX 4070", warn=None, sha=None, exit_code=None):
    s = lambda v: {"p50": v * 0.6, "p95": v, "p99": v * 1.2, "mean": v * 0.7, "max": v * 2, "sum": v * 100, "n": 100}
    w = {"tick_start": 1800, "tick_end": 2200, "hash_start": hs, "alive_start": 2185, "alive_end": alive,
         "fps_mean": 1000.0 / main, "gc_collections": 1, "hitches_over_33ms": []}
    if he:
        w["hash_end"] = he
    r = {"schema": "tw-perf/1", "run": {"label": label, "build": build, "context": "player"},
         "machine": {"gpu": gpu_name}, "config": {"quality": "Ultra"}, "window": w,
         "series": {"main_ms": s(main), "gpu_ms": s(gpu), "draw_calls": s(draws), "setpass": s(236), "gc_bytes": s(1800),
                    "vat_vertices": {**s(vat), "p50": vat}, "vat_shadows_on": s(0)},
         "per_tick_ms": {"TW.Sim.Step": 1.3}}
    if knobs is not None:
        r["config"]["knobs"] = {"read": {}, "set": knobs}
    if warn is not None:
        r["warnings"] = list(warn)
    if sha is not None:
        r["build_info"] = {"git_sha": sha}
    if exit_code is not None:
        r["run"]["exit"] = exit_code
    return r


class AosaTest(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="aosa-test-"))
        self.docs = self.tmp / "docs"
        (self.docs / "runs").mkdir(parents=True)
        self.env = {k: os.environ.get(k) for k in ("AOSA_DOCS", "TW_BUILDS", "TW_PROJECT", "LOCALAPPDATA", "AOSA_OFFLINE")}
        os.environ.update({"AOSA_DOCS": str(self.docs), "TW_BUILDS": str(self.tmp / "Builds"),
                           "TW_PROJECT": str(self.tmp / "proj"), "LOCALAPPDATA": str(self.tmp / "lad"), "AOSA_OFFLINE": "1"})

    def tearDown(self):
        for k, v in self.env.items():
            if v is None:
                os.environ.pop(k, None)
            else:
                os.environ[k] = v
        shutil.rmtree(self.tmp, ignore_errors=True)

    def run_cli(self, *argv):
        out, err = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            try:
                code = aosa.main(list(argv))
            except SystemExit as e:
                code = e.code
        return code, out.getvalue() + err.getvalue()

    def put(self, rel, obj):
        p = self.docs / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps(obj) if not isinstance(obj, str) else obj, encoding="utf-8")
        return p

    def ab(self, cyc="3", a_kw=None, b_kw=None, n=3):
        noise = [0.0, 0.05, -0.04, 0.02]
        for i in range(n):
            self.put("runs/%s/cand-%d.json" % (cyc, i + 1), report("cand-%d" % (i + 1), main=7.0 + noise[i], **(a_kw or {})))
            self.put("runs/%s/base-%d.json" % (cyc, i + 1), report("base-%d" % (i + 1), main=8.0 + noise[i + 1], **(b_kw or {})))

    # ---- compare -----------------------------------------------------------------------------------------------
    def test_compare_pass(self):
        self.ab()
        code, out = self.run_cli("compare", "cand", "base", "--json")
        v = json.loads(out)
        self.assertEqual(code, 0, out)
        self.assertEqual(v["verdict"], "pass")
        self.assertTrue(v["target"]["improved"])
        self.assertEqual(v["target"]["band_source"], "max(3*MAD, spread) of repeats")
        self.assertAlmostEqual(v["target"]["delta"], 7.0 - 8.02, places=6)
        code, out = self.run_cli("compare", "cand", "base")
        self.assertIn("VERDICT: PASS", out)
        self.assertIn("hash_start", out)

    def test_compare_shows_hud_parts_engine_markers_and_hitch_carriers(self):
        # C64, C66, C69: the new data reaches the compare table; a report without it (the release player, older runs)
        # prints exactly the sections it printed before
        self.ab()
        code, out = self.run_cli("compare", "cand", "base")
        for absent in ("HUD parts", "engine markers", "hitch carriers", "hitches with a carrier"):
            self.assertNotIn(absent, out)
        s = lambda v: {"p50": v, "p95": v * 2, "p99": v * 3, "mean": v * 1.1, "max": v * 4, "sum": v * 100, "n": 100}
        for p in (self.docs / "runs" / "3").glob("cand-*.json"):
            r = json.loads(p.read_text())
            r["series"].update({"TW.Hud.Cards": s(0.6), "script:PreLateUpdate.ScriptRunBehaviourLateUpdate": s(4.0),
                                "script:GC.Collect": None})
            r["window"]["hitch_records"] = [
                {"frame": 611, "tick": 1886, "ms": 36.1, "marker": "TW.Terrain.Update", "marker_ms": 20.0, "gc_bytes": 512},
                {"frame": 705, "tick": 1910, "ms": 35.4, "marker": None, "marker_ms": None, "gc_bytes": None}]
            r["window"]["hitch_carriers"] = [{"marker": "TW.Terrain.Update", "hitches": 1, "median_share": 0.554}]
            p.write_text(json.dumps(r))
        code, out = self.run_cli("compare", "cand", "base")
        self.assertIn("VERDICT: PASS", out)
        self.assertIn("HUD parts per frame", out)
        self.assertIn("0.600/0.660/1.200", out)
        self.assertIn("engine markers per frame", out)
        self.assertIn("ScriptRunLateUpdate", out)
        self.assertIn("GC.Collect", out)   # a null series still gets its row, as '-'
        self.assertIn("1 of 2", out)
        self.assertIn("1 @ 0.55", out)
        self.assertEqual(aosa.short_marker("Gfx.WaitForPresentOnGfxThread"), "Gfx.WaitForPresent(gfx)")
        self.assertEqual(aosa.short_marker("FixedUpdate.PhysicsFixedUpdate"), "PhysicsFixedUpdate")

    def test_compare_rule1_different_battle(self):
        self.ab(a_kw={"hs": "BBBB"})
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertIn(1, v["failed"])
        self.assertIn("different battle", v["rules"]["1"]["why"])

    def test_compare_rule1_hash_end(self):
        self.ab(a_kw={"he": "E1"}, b_kw={"he": "E2"})
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["failed"], [1])
        shutil.rmtree(self.docs / "runs" / "3")
        self.ab(a_kw={"he": "E1"}, b_kw={"he": "E1"})
        self.assertEqual(json.loads(self.run_cli("compare", "cand", "base", "--json")[1])["failed"], [])
        p = self.docs / "runs" / "3" / "cand-1.json"
        r = json.loads(p.read_text()); r["window"]["hash_end_tick"] = 2201; p.write_text(json.dumps(r))
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["failed"], [1])
        self.assertIn("different ticks", v["rules"]["1"]["why"])

    def test_a6_rule1_hash_end_not_disabled_by_one_missing(self):
        # [A6] one report without hash_end drops itself from rule 1, it does not switch the test off for the others
        self.ab(a_kw={"he": "E1"}, b_kw={"he": "E2"})
        p = self.docs / "runs" / "3" / "base-1.json"
        r = json.loads(p.read_text()); del r["window"]["hash_end"]; p.write_text(json.dumps(r))
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["failed"], [1], "[A6] rule 1 must still fail when hash_end differs in the reports that have it")
        self.assertIn("hash_end differs", v["rules"]["1"]["why"], "[A6] the reason must name the hash_end mismatch")
        self.assertIn("hash_end in 5 of 6 reports", v["rules"]["1"]["why"], "[A6] say how many reports carry hash_end")

    def test_a6_rule3_names_what_it_could_not_check(self):
        # [A6] a rule-3 metric that is absent from the reports is named as unchecked, not claimed to hold
        self.ab()
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["verdict"], "pass", "[A6] an absent rule-3 metric must not fail the compare")
        why = v["rules"]["3"]["why"]
        for m in ("frame_budget_draws.p95", "frame_budget_vertices.p95"):
            self.assertIn(m, why, "[A6] rule 3 must name %s as unchecked" % m)
            self.assertIn(m, v["rules"]["3"]["unchecked"], "[A6] %s belongs in rule 3's unchecked list" % m)
        self.assertIn("except unchecked:", why, "[A6] the text may not claim no other metric rose without saying what it skipped")

    def test_compare_rule2_and_spread(self):
        self.ab(n=2)
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertIn(2, v["failed"])
        self.assertEqual(v["target"]["band_source"], "min-max spread")

    def test_compare_rule3_regression_and_priors_band(self):
        self.ab(a_kw={"gpu": 6.0})
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["failed"], [3])
        self.assertIn("gpu_ms.p95", v["rules"]["3"]["regressed"])
        # a wide prior band swallows both the gain and the regression
        self.put("priors.json", {"bands": {"main_ms.p95": {"development": {"band": 2.0, "n": 9}},
                                           "gpu_ms.p95": {"development": {"band": 2.0, "n": 9}}}})
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["target"]["band_source"], "priors (n=9)")
        self.assertFalse(v["target"]["improved"])
        self.assertEqual(v["rules"]["3"]["regressed"], [])

    def test_compare_rule4_floor(self):
        self.ab(a_kw={"vat": 1.2e6})
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["failed"], [4])

    def test_compare_refuses_across_builds_and_gpus(self):
        self.ab(a_kw={"build": "release"})
        code, out = self.run_cli("compare", "cand", "base")
        self.assertEqual(code, 2)
        self.assertIn("REFUSED", out)
        shutil.rmtree(self.docs / "runs" / "3")
        self.ab(a_kw={"gpu_name": "other"})
        self.assertEqual(json.loads(self.run_cli("compare", "cand", "base", "--json")[1])["verdict"], "refused")

    def test_compare_label_takes_newest_cycle(self):
        self.ab(cyc="2", a_kw={"hs": "OLD"})
        self.ab(cyc="10")
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["verdict"], "pass")
        self.assertTrue(all("/10/" in f.replace("\\", "/") for f in v["a"]["files"]))

    # ---- attempts / learn ----------------------------------------------------------------------------------------
    def test_attempt_add_and_validate(self):
        ok = {"cycle": 1, "card": "C02", "class": "cull", "files": ["A.cs"], "predicted": {"metric": "gpu_ms.p95", "delta": -0.4},
              "measured": {"metric": "gpu_ms.p95", "delta": -0.2}, "verdict": "landed"}
        self.assertEqual(self.run_cli("attempt", "add", json.dumps(ok))[0], 0)
        f = self.tmp / "a.json"
        f.write_text(json.dumps(dict(ok, verdict="reverted", rule=3)))
        self.assertEqual(self.run_cli("attempt", "add", "@" + str(f))[0], 0)
        lines = [json.loads(l) for l in (self.docs / "attempts.jsonl").read_text().splitlines()]
        self.assertEqual([l["id"] for l in lines], ["a0001", "a0002"])
        self.assertIn("owner_verdict", lines[0])
        for bad in [dict(ok, verdict="maybe"), dict(ok, **{"class": "magic"}), dict(ok, predicted=None),
                    dict(ok, verdict="reverted"), dict(ok, rule=12)]:
            code, out = self.run_cli("attempt", "add", json.dumps(bad))
            self.assertEqual(code, 2, bad)
            self.assertIn("rejected", out)
        seed = dict(ok, id="s001", cycle=-1, predicted=None, seed="docs/05", rule="8", verdict="reverted")
        self.assertEqual(self.run_cli("attempt", "add", json.dumps(seed))[0], 0)
        self.assertEqual(self.run_cli("attempt", "add", json.dumps(seed))[0], 2)  # duplicate id

    def test_learn(self):
        att = [
            {"id": "s001", "cycle": -1, "card": None, "class": "cull", "files": ["X.cs"], "predicted": None,
             "measured": {"metric": "gpu_ms.p95", "delta": -1.0}, "verdict": "landed", "cost": {"cycles": 1}, "seed": "d"},
            {"id": "a0001", "cycle": 1, "card": "C02", "class": "cull", "files": ["X.cs", "Y.cs"],
             "predicted": {"metric": "gpu_ms.p95", "delta": -0.4}, "measured": {"metric": "gpu_ms.p95", "delta": -0.2},
             "verdict": "landed", "cost": {"cycles": 2}},
            {"id": "a0002", "cycle": 1, "card": "C02", "class": "cull", "files": ["X.cs"],
             "predicted": {"metric": "gpu_ms.p95", "delta": -0.1}, "measured": {"metric": "gpu_ms.p95", "delta": 0.05},
             "verdict": "reverted", "rule": 3, "cost": {"cycles": 1}},
            {"id": "a0003", "cycle": 2, "card": "C09", "class": "cache", "files": [], "predicted": None, "verdict": "void"},
        ]
        self.put("attempts.jsonl", "".join(json.dumps(x) + "\n" for x in att))
        self.ab()
        for i, v in enumerate([90, 130, 170]):
            self.put("runs/4/sweep-%d.json" % (i + 1), report("sweep", main=5.0 + 0.01 * v, knobs={"vat.lodDistance": v}))
        self.put("runs/4/two-1.json", report("two", knobs={"a": 1, "b": 2}))
        self.assertEqual(self.run_cli("learn")[0], 0)
        first = (self.docs / "priors.json").read_text(), (self.docs / "knobs.json").read_text()
        self.run_cli("learn")
        self.assertEqual(first, ((self.docs / "priors.json").read_text(), (self.docs / "knobs.json").read_text()))
        pr = json.loads(first[0])
        cull = pr["classes"]["cull"]
        self.assertEqual((cull["n"], cull["n_judged"]), (3, 3))
        self.assertAlmostEqual(cull["hit_rate"], 2 / 3, places=4)
        self.assertAlmostEqual(cull["median_gain"], 0.6 / 13.0, places=3)  # |-1.0| and |-0.2| over the gpu_ms scale 13, seed included
        self.assertEqual(cull["n_calibration"], 2)            # the seed has no prediction
        self.assertAlmostEqual(cull["calibration"], (0.5 + -0.5) / 2)
        self.assertEqual(pr["files"]["Y.cs"]["n"], 1)
        self.assertIsNone(pr["classes"]["cache"]["hit_rate"])
        b = pr["bands"]["main_ms.p95"]["development"]
        self.assertIn("3/cand", b["groups"])
        self.assertGreater(b["n"], 3)
        kn = json.loads(first[1])
        self.assertEqual(list(kn), ["vat.lodDistance"])
        k = kn["vat.lodDistance"]["development"]
        self.assertEqual(k["n"], 3)
        self.assertAlmostEqual(k["sensitivity"]["main_ms.p95"], 0.01, places=6)

    # ---- backlog -------------------------------------------------------------------------------------------------
    def test_pick(self):
        self.put("BACKLOG.md", BACKLOG)
        self.put("priors.json", {"classes": {"cache": {"hit_rate": 0.9, "calibration": 3.0}, "juice": {"hit_rate": 0.5}}})
        code, out = self.run_cli("pick", "--n", "3")
        self.assertEqual(code, 0)
        rows = [l.split() for l in out.splitlines() if l.startswith(("*", "  ", "    "))]
        ids = [r[1] if r[0] == "*" else r[0] for r in rows]
        picked = out.splitlines()[-1]
        self.assertNotIn("C06", out)          # owner card
        self.assertNotIn("C07 ", out)         # done
        self.assertNotIn("C08 ", out)         # wip
        self.assertLess(ids.index("C01"), ids.index("C03"))   # cache is mis-calibrated: demoted below instrument
        self.assertEqual(sum(1 for c in ("C04", "C05") if c in picked), 1)  # one juice card at most
        self.assertIn("SPLIT", [l for l in out.splitlines() if "C03" in l][0])
        self.assertIn("measure next", [l for l in out.splitlines() if "C02" in l][0])

    def test_age(self):
        self.put("BACKLOG.md", BACKLOG)
        self.assertEqual(self.run_cli("age", "--moved", "C02")[0], 0)
        text = (self.docs / "BACKLOG.md").read_text()
        self.assertIn("Some preamble that must survive `age`.", text)
        self.assertIn("Trailing text after the table.", text)
        self.assertIn("none yet \\| escaped pipe", text)
        cards = {c["id"]: c for c in aosa.load_cards()}
        self.assertEqual((cards["C01"]["age"], cards["C01"]["idle"]), ("1", "1"))
        self.assertEqual((cards["C02"]["age"], cards["C02"]["idle"]), ("2", "0"))
        self.assertEqual((cards["C03"]["age"], cards["C03"]["idle"]), ("5", "4"))
        self.assertEqual((cards["C07"]["age"], cards["C07"]["idle"]), ("2", "1"))   # done: untouched
        self.assertEqual(len(text.splitlines()), len(BACKLOG.splitlines()))

    # ---- budget / refimg -----------------------------------------------------------------------------------------
    def test_budget(self):
        code, out = self.run_cli("budget")
        self.assertEqual(code, 0)
        b = json.loads((self.docs / "budget.json").read_text())
        self.assertEqual(b["daily_cap_usd"], 3.0)
        self.assertEqual(b["shares"], {"juice": 0.6667, "tier": 0.3333})
        now = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
        b["entries"] = [{"utc": now, "share": "juice", "usd": 0.7, "what": "x", "attempt": "a0001"},
                        {"utc": "2020-01-01T00:00:00Z", "share": "juice", "usd": 9, "what": "old", "attempt": None}]
        self.put("budget.json", b)
        out = self.run_cli("budget")[1]
        self.assertIn("juice  spent $0.70 of $2.00, left $1.30", out)

    def test_refimg_dry_run_and_refusal(self):
        cap = self.tmp / "shot.png"
        cap.write_bytes(b"\x89PNG fake")
        pf = self.tmp / "prompt.txt"
        pf.write_text("make the burst land harder")
        self.put("LEDGER.md", "| cycle | utc | mode | cards | attempts | result | gate | commit | usd | note |\n|---|---|---|---|---|---|---|---|---|---|\n| 6 | x | P | - | - | - | - | - | 0 | n |\n")
        code, out = self.run_cli("refimg", str(cap), "--for", "juice", "--prompt-file", str(pf), "--dry-run")
        self.assertEqual(code, 0, out)
        self.assertIn("fal-ai/nano-banana-pro/edit", out)
        self.assertIn("shot-1.png", out)
        self.assertIn(os.path.join("runs", "7", "refs"), out)
        b = json.loads((self.docs / "budget.json").read_text())
        self.assertEqual(b["entries"], [])
        self.assertFalse((self.docs / "attempts.jsonl").exists())
        now = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
        b["entries"] = [{"utc": now, "share": "tier", "usd": 0.5, "what": "x", "attempt": None}]
        self.put("budget.json", b)
        code, out = self.run_cli("refimg", str(cap), "--for", "tier", "--prompt-file", str(pf), "--dry-run")
        self.assertEqual(code, 4, out)      # 1.00 - 0.50 left < 2 x 0.35
        self.assertEqual(self.run_cli("refimg", str(cap), "--for", "tier", "--prompt-file", str(pf), "--n", "1", "--dry-run")[0], 0)

    # [A11] a corrupt budget.json must not be silently overwritten with defaults by a read-only command
    def test_a11_unreadable_budget_is_not_reset(self):
        p = self.put("budget.json", "{\"daily_cap_usd\": 3.0,")
        before = p.read_bytes()
        code, out = self.run_cli("budget")
        self.assertEqual(code, 1, "[A11] status must refuse, not reset the cap and every entry")
        self.assertIn("not readable JSON", out)
        self.assertEqual(p.read_bytes(), before, "[A11] the unreadable file must be left untouched")

    # [A12] the fal charge must be written at submit time, not only after everything succeeds
    def test_a12_charge_is_written_at_submit(self):
        falkit = self.tmp / "falkit.py"
        falkit.write_text(
            "def pricing(endpoints):\n"
            "    return {'prices': []}\n"
            "def upload(path):\n"
            "    return 'https://example/u.png'\n"
            "def submit(endpoint, payload):\n"
            "    return {'request_id': 'req-1'}\n"
            "def status(endpoint, rid):\n"
            "    raise AssertionError('status must not be reached with --timeout 0')\n"
            "def result(endpoint, rid):\n"
            "    raise AssertionError('result must not be reached with --timeout 0')\n"
            "def download(url, dest):\n"
            "    raise AssertionError('download must not be reached with --timeout 0')\n",
            encoding="utf-8")
        cap = self.tmp / "shot.png"
        cap.write_bytes(b"\x89PNG fake")
        pf = self.tmp / "prompt.txt"
        pf.write_text("make the burst land harder")
        os.environ["AOSA_FALKIT"] = str(falkit)
        os.environ.pop("AOSA_OFFLINE", None)
        try:
            code, out = self.run_cli("refimg", str(cap), "--for", "juice", "--prompt-file", str(pf),
                                      "--n", "2", "--timeout", "0")
        finally:
            os.environ["AOSA_OFFLINE"] = "1"
        self.assertEqual(code, 1, "[A12] a request that never completes must still fail the command")
        b = json.loads((self.docs / "budget.json").read_text())
        self.assertEqual(len(b["entries"]), 1, "[A12] exactly one entry, written at submit")
        e = b["entries"][0]
        self.assertAlmostEqual(e["usd"], 0.70, places=6)   # 0.35 fallback x n=2
        self.assertEqual(e["request_id"], "req-1")

    # ---- bench ---------------------------------------------------------------------------------------------------
    def test_bench_dry_run_player(self):
        self.put("LEDGER.md", "# l\n\n| cycle | utc | mode | cards | attempts | result | gate | commit | usd | note |\n|---|---|---|---|---|---|---|---|---|---|\n| 4 | x | P | - | - | - | - | - | 0 | n |\n")
        code, out = self.run_cli("bench", "lod90", "--player", "--knobs", "vat.lodDistance=90,vat.x=2", "--scenario", "barrage",
                                 "--extra", "ticks=200 foo", "--dry-run")
        self.assertEqual(code, 0, out)
        self.assertIn("bench cycle 5", out)
        self.assertIn("-screen-width 1920 -screen-height 1080 -logFile", out)
        self.assertIn("stress=1500 settle_ticks=1800 warm=120 ff=8 vsync=0 quality=5 canary=0 scenario=barrage "
                      "knobs=vat.lodDistance=90|vat.x=2 ticks=200 foo label=lod90-1 out=", out)
        self.assertIn("runs/5/lod90-1.json shot=", out)
        self.assertIn("lod90-1.png", out)
        self.assertIn("WinBench", out)
        self.assertFalse((self.docs / "runs" / "5").exists())     # a dry run writes nothing

    def test_bench_dry_run_against_interleaves(self):
        self.put("runs/2/base.args.json", {"label": "base", "mode": "player", "dev": True, "scenario": None, "knobs": None,
                                           "extra": "", "build_dir": str(self.tmp / "Builds" / "WinBenchDev"), "cycle": 2})
        code, out = self.run_cli("bench", "cand", "--against", "base", "--dev", "--dry-run", "--cycle", "3")
        self.assertEqual(code, 0, out)
        labels = [l.split("label=")[1].split()[0] for l in out.splitlines() if "label=" in l]
        self.assertEqual(labels, ["cand-1", "base-1", "cand-2", "base-2", "cand-3", "base-3"])
        self.assertIn("compare cand base", out)
        self.assertEqual(self.run_cli("bench", "cand", "--editor", "--against", "base", "--dry-run")[0], 2)

    # [H11] the slot is per checkout (editor_lock.slot_path digests the project path), so a claim on THIS tree's
    # project is this tree's own slot: status must report its holder, not call it the main clone's and dismiss it.
    def test_h11_a_claim_on_this_trees_slot_is_reported_not_called_the_main_clones(self):
        el = aosa.load_module("editor_lock", aosa.TOOLS / "editor_lock.py")
        project = pathlib.Path(os.environ["TW_PROJECT"])
        slot = el.slot_path(project)
        slot.parent.mkdir(parents=True, exist_ok=True)
        slot.write_text(json.dumps({"who": "claude-7", "why": "captures", "since": time.time(),
                                    "until": time.time() + 600}), encoding="utf-8")
        self.assertIsNotNone(el.read_slot(project), "[H11] the fixture claim must be where editor_lock looks")

        free, text, read = aosa.editor_state()
        self.assertEqual((read or {}).get("who"), "claude-7", "[H11] status did not read this tree's own slot")
        self.assertIn("claude-7", text, "[H11] status does not name the holder of this tree's slot")
        self.assertNotIn("main clone", text,
                         "[H11] status still calls this tree's own slot the main clone's")
        self.assertNotIn("does not concern this tree", text,
                         "[H11] status still dismisses a claim on this tree's own slot")

        # the old half: the slot is advisory, so it never blocks the loop's own editor
        code, out = self.run_cli("bench", "e", "--editor", "--dry-run")
        self.assertEqual(code, 0, out)
        self.assertIn('TW.Editor.CaptureRig.Bench(', out)
        self.assertIn("canary=0 label=e-1", out)
        self.assertNotIn("quality=5", out)

    def test_compare_header_names_the_sides(self):
        # [A1] the first argument is the candidate and the second the baseline; the verdict inverts if they are
        # swapped, so the table says which side is which (three docs told the agent to call it the wrong way round)
        self.ab()
        code, out = self.run_cli("compare", "cand", "base")
        self.assertEqual(code, 0, out)
        self.assertIn("candidate cand vs baseline base", out)
        self.assertLess(out.index("candidate cand vs baseline base"), out.index("VERDICT"))

    # ---- snapshot ----------------------------------------------------------------------------------------------
    def live_player(self, kind="WinBench"):
        d = Path(os.environ["TW_BUILDS"]) / kind
        d.mkdir(parents=True, exist_ok=True)
        (d / "TrenchWarfare.exe").write_bytes(b"x" * 64)
        (d / "data.bin").write_bytes(b"y" * 32)
        return d

    def test_snapshot_removes_a_broken_copy_and_fails(self):
        # [A13] an interrupted copytree used to be kept as a valid snapshot and the land went on building over the
        # live players: a copy that does not match the source is removed and the exit code is non-zero
        import shutil as sh
        self.live_player("WinBench")
        self.live_player("WinBenchDev")
        real = sh.copytree

        def short(src, dst, *a, **kw):
            Path(dst).mkdir(parents=True)
            (Path(dst) / "TrenchWarfare.exe").write_bytes(b"x" * 8)      # half a file, as a kill mid-copy leaves
            return dst

        sh.copytree = short
        try:
            code, out = self.run_cli("snapshot", "--label", "abc1234")
        finally:
            sh.copytree = real
        self.assertNotEqual(code, 0, out)
        self.assertIn("COPY DIFFERS, removed WinBench@abc1234", out)
        self.assertFalse((Path(os.environ["TW_BUILDS"]) / "WinBench@abc1234").exists(), out)

    def test_snapshot_no_player_is_not_a_failure(self):
        # [A13] a first build has no live player yet: a printed note, exit 0, so it never blocks a land
        code, out = self.run_cli("snapshot", "--label", "abc1234")
        self.assertEqual(code, 0, out)
        self.assertIn("nothing to copy", out)

    def test_snapshot_verifies_a_good_copy(self):
        # [A13] the good path still copies and verifies both kinds
        self.live_player("WinBench")
        self.live_player("WinBenchDev")
        code, out = self.run_cli("snapshot", "--label", "abc1234")
        self.assertEqual(code, 0, out)
        self.assertEqual(out.count("verified"), 2, out)
        self.assertTrue((Path(os.environ["TW_BUILDS"]) / "WinBenchDev@abc1234" / "TrenchWarfare.exe").exists())

    # ---- ledger / retro / status ---------------------------------------------------------------------------------
    def test_ledger_retro_status(self):
        self.assertEqual(aosa.current_cycle(), 0)
        self.put("attempts.jsonl", json.dumps({"id": "a0001", "cycle": 0, "card": "C02", "class": "cull", "verdict": "reverted",
                                               "rule": 3, "predicted": {"metric": "m", "delta": -1}, "measured": {"metric": "m", "delta": -5}}) + "\n")
        self.assertEqual(self.run_cli("ledger", "add", "--mode", "P", "--cards", "C02", "--summary", "first | cycle")[0], 0)
        self.assertEqual(self.run_cli("ledger", "add", "--mode", "E", "--summary", "second", "--usd", "0.7")[0], 0)
        rows = aosa.ledger_rows()
        self.assertEqual([r["cycle"] for r in rows], [0, 1])
        self.assertEqual(rows[0]["attempts"], "a0001")
        self.assertEqual(rows[0]["result"], "L0 R1 V0 M0")
        self.assertEqual(aosa.current_cycle(), 2)
        self.put("BACKLOG.md", BACKLOG)
        code, out = self.run_cli("retro")
        self.assertEqual(code, 0)
        self.assertIn("reverted 1", out)
        self.assertIn("C03", out)
        self.assertIn("a0001 cull 5.00", out)
        self.assertIn("$0.70", out)
        head = subprocess.run(["git", "-C", str(aosa.REPO), "rev-parse", "HEAD"], capture_output=True, text=True).stdout.strip()
        self.put("../Builds/WinBench/build-info.json", {"git_sha": head, "dirty_files": 0, "built_utc": "now"})
        code, out = self.run_cli("status")
        self.assertEqual(code, 0)
        self.assertIn("WinBench    %s built now fresh" % head[:7], out)
        self.assertIn("WinBenchDev missing", out)
        self.assertIn("mode   : B", out)      # the temp project is free and one build is missing
        self.assertIn("WIP 1/3, ready 5", out)


    # [A5] a report that does not parse is not a sample: run_one's tail must refuse it, not read exit as 0
    def test_a5_unreadable_report_refused(self):
        bad = self.docs / "runs" / "bad.json"
        bad.write_text("{not json", encoding="utf-8")
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            ok = aosa.report_ok(bad, "bad")
        self.assertFalse(ok, "[A5] an unreadable report must not count as a sample")
        self.assertIn("REPORT UNREADABLE", buf.getvalue(), "[A5] the unreadable report must be named")
        good = self.docs / "runs" / "good.json"
        good.write_text(json.dumps(report("good", he="BBBB")), encoding="utf-8")
        with contextlib.redirect_stdout(io.StringIO()):
            self.assertTrue(aosa.report_ok(good, "good"), "[A5] a readable tw-perf/1 report is still a sample")

    # [A9] a bench value holding a space would make PerfBench write the report elsewhere: refuse it
    def test_a9_bench_string_refuses_space(self):
        spec = {"mode": "editor"}
        with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
            with self.assertRaises(SystemExit) as cm:
                aosa.bench_string(spec, "lab", "C:/My Runs/x.json", "C:/My Runs/x.png")
        self.assertEqual(cm.exception.code, 2, "[A9] a space in out= must refuse the run")
        with contextlib.redirect_stdout(io.StringIO()):
            s = aosa.bench_string(spec, "lab", "C:/runs/x.json", "C:/runs/x.png")
        self.assertIn("out=C:/runs/x.json", s, "[A9] a clean path still builds the -twbench string")

    # [A9] the shells guard the same split: a label with a space must exit 1 before anything runs
    def test_a9_shells_refuse_space(self):
        bash = git_bash()
        if not bash:
            self.skipTest("no bash on PATH")
        for name in ("bench.sh", "player_bench.sh"):
            env = dict(os.environ, TW_PROJECT=str(self.tmp / "proj"))
            r = subprocess.run([bash, str(HERE / name), "lab el", ""], capture_output=True, text=True,
                               timeout=60, env=env)
            self.assertEqual(r.returncode, 1, "[A9] %s must refuse a label with a space: %s" % (name, r.stdout))
            self.assertIn("space", r.stdout, "[A9] %s must say why it refused" % name)

    # [A16] a player run that writes no report must exit non-zero and name the path it looked at
    def test_a16_player_bench_fails_without_a_report(self):
        bash = git_bash()
        if not bash:
            self.skipTest("no bash on PATH")
        bdir = self.tmp / "Builds" / "WinBench"
        bdir.mkdir(parents=True)
        exe = bdir / "TrenchWarfare.exe"
        exe.write_text("#!/bin/sh" + NL + "exit 3" + NL, encoding="utf-8")
        exe.chmod(0o755)
        env = dict(os.environ, TW_PROJECT=str(self.tmp / "proj"), AOSA_DOCS=str(self.docs))
        r = subprocess.run([bash, str(HERE / "player_bench.sh"), "lab", "", str(bdir)],
                           capture_output=True, text=True, timeout=300, env=env)
        out = r.stdout + r.stderr
        self.assertEqual(r.returncode, 1, "[A16] a run without a report must exit 1: %s" % out)
        self.assertIn("NO REPORT", out, "[A16] the run must say NO REPORT: %s" % out)
        self.assertIn(str(self.docs).replace(ESC, "/"), out.replace(ESC, "/"),
                      "[A16] NO REPORT must name the path it looked at, under AOSA_DOCS: %s" % out)

    # [A16] the stale perf-pass copies are gone and the new hand-run folder is git-ignored
    def test_a16_stale_copies_are_gone(self):
        self.assertFalse((HERE / "cmp.py").exists(),
                         "[A16] cmp.py is a third copy of the compare table: Tools/perfcmp.py is the maintained one")
        self.assertFalse((HERE / "gpu_probe.cs").exists(),
                         "[A16] gpu_probe.cs is referenced by nothing; the perf-pass copy stays")
        ign = (HERE / ".." / ".." / ".." / "docs" / "reference" / "aosa" / "runs" / ".gitignore").resolve()
        self.assertIn("manual/", ign.read_text(encoding="utf-8"),
                      "[A16] runs/manual, where the shells now write, must be git-ignored")

    # [A7] a run that exited non-zero is no sample: compare must refuse it, not average it in
    def test_a7_failed_run_is_not_a_sample(self):
        self.ab()
        f = self.docs / "runs" / "3" / "cand-2.json"
        r = json.loads(f.read_text()); r["run"]["exit"] = 2; r["run"]["why"] = "unknown ground"
        f.write_text(json.dumps(r), encoding="utf-8")
        code, out = self.run_cli("compare", "cand", "base")
        self.assertEqual(code, 2, "[A7] a failed run must refuse the compare: %s" % out)
        self.assertIn("REFUSED", out, "[A7] the compare must be refused")
        self.assertIn("cand-2", out, "[A7] the refusal must name the failed report")
        self.assertIn("exit 2", out, "[A7] the refusal must name the run's exit code")

    # [A7] a PerfBench warning that says the run is not comparable refuses it; the focus warning never does
    def test_a7_refuses_a_warning_that_says_so(self):
        self.ab(a_kw={"warn": ["custom focus (fx/fz): not the standard view, do not compare with standard runs"]})
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["verdict"], "refused", "[A7] an fx/fz run must not be compared with standard runs")
        shutil.rmtree(self.docs / "runs" / "3")
        self.ab(a_kw={"warn": ["the window lost focus during the run"]})
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["verdict"], "pass", "[A7] the focus warning is by design and must still compare")

    # [A7] one side may not pool two builds: runs from before and after a rebuild are not one baseline
    def test_a7_refuses_two_shas_on_one_side(self):
        self.ab(b_kw={"sha": "aaa"})
        f = self.docs / "runs" / "3" / "base-2.json"
        r = json.loads(f.read_text()); r["build_info"]["git_sha"] = "bbb"
        f.write_text(json.dumps(r), encoding="utf-8")
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["verdict"], "refused", "[A7] a baseline of two builds must be refused")
        self.assertIn("pools two builds", v["why"], "[A7] the refusal must say the side pools two builds")
        shutil.rmtree(self.docs / "runs" / "3")
        self.ab(a_kw={"sha": "aaa"}, b_kw={"sha": "aaa"})
        v = json.loads(self.run_cli("compare", "cand", "base", "--json")[1])
        self.assertEqual(v["verdict"], "pass", "[A7] one sha on both sides is a knob sweep and still compares")

    # [A7] a failed run is no noise sample either: its group must leave no band behind
    def test_a7_failed_run_leaves_no_noise_band(self):
        self.ab()
        for i, main in enumerate([9.0, 9.1]):
            self.put("runs/5/dud-%d.json" % (i + 1), report("dud-%d" % (i + 1), main=main))
        f = self.docs / "runs" / "5" / "dud-2.json"
        r = json.loads(f.read_text()); r["run"]["exit"] = 2; r["run"]["why"] = "timeout"
        f.write_text(json.dumps(r), encoding="utf-8")
        self.assertEqual(self.run_cli("learn")[0], 0)
        b = json.loads((self.docs / "priors.json").read_text())["bands"]["main_ms.p95"]["development"]
        self.assertIn("3/cand", b["groups"], "[A7] a clean group still feeds the band")
        self.assertNotIn("5/dud", b["groups"], "[A7] a group holding a failed run must leave no noise band")

    # [A14] current_cycle() must be the max of the rows + 1: an out-of-order row must not reuse a cycle number
    def test_a14_cycle_is_the_max_not_the_last(self):
        hdr = "| " + " | ".join(aosa.LEDGER_COLS) + " |"
        rows = ["| %d | 2026-10-01T00:00Z | P | - | - | - | - | - | 0.00 | s |" % c for c in (0, 1, 5, 2)]
        self.put("LEDGER.md", "# AOSA ledger" + NL * 2 + hdr + NL + "|" + "---|" * len(aosa.LEDGER_COLS) + NL
                 + NL.join(rows) + NL)
        self.assertEqual(aosa.current_cycle(), 6, "[A14] the next cycle is max + 1, never last + 1")
        code, out = self.run_cli("ledger", "add", "--mode", "P", "--cards", "C1", "--summary", "s")
        self.assertEqual(code, 0, out)
        self.assertIn("cycle 6", out, "[A14] ledger add must write the max + 1 cycle")

    # [A14] every cell escapes a pipe, not only the summary: one stray | shifts every column the readers index
    def test_a14_every_cell_escapes_a_pipe(self):
        self.assertEqual(self.run_cli("ledger", "add", "--mode", "P", "--cards", "C1|C2",
                                      "--summary", "s", "--gate", "a|b")[0], 0)
        _, header, rows = aosa.read_table(self.docs / "LEDGER.md", "cycle")
        self.assertEqual(len(rows), 1, "[A14] exactly one row was added")
        cells = rows[-1][1]
        self.assertEqual(len(cells), len(header), "[A14] a cell holding a pipe must not add a column")
        self.assertEqual(cells[aosa.col(header, "cards")], "C1" + ESC + "|C2",
                         "[A14] the cards cell must read back with its pipe escaped")
        self.assertEqual(cells[aosa.col(header, "gate")], "a" + ESC + "|b",
                         "[A14] the gate cell must read back with its pipe escaped")

    # [A14] a failed ledger write leaves the old LEDGER.md whole and no .tmp behind
    def test_a14_a_failed_ledger_write_keeps_the_old_file(self):
        hdr = "| " + " | ".join(aosa.LEDGER_COLS) + " |"
        seed = ("# AOSA ledger" + NL * 2 + hdr + NL + "|" + "---|" * len(aosa.LEDGER_COLS) + NL
                + "| 0 | 2026-10-01T00:00Z | P | - | - | - | - | - | 0.00 | seeded |" + NL)
        self.put("LEDGER.md", seed)
        real = aosa.os.replace
        aosa.os.replace = lambda *a, **k: (_ for _ in ()).throw(OSError("disk full"))
        try:
            with self.assertRaises(OSError, msg="[A14] a failed write must not be swallowed"):
                aosa.cmd_ledger(aosa.argparse.Namespace(
                    cmd="ledger", sub="add", cycle=None, mode="P", cards="C1", result=None,
                    gate=None, commit=None, usd=0.0, summary="second"))
        finally:
            aosa.os.replace = real
        self.assertEqual((self.docs / "LEDGER.md").read_text(encoding="utf-8"), seed,
                         "[A14] the old ledger must survive a failed write whole")
        self.assertEqual(list(self.docs.glob("*.tmp")), [],
                         "[A14] a failed write must leave no .tmp file behind")

    # ---- prune (A17) ---------------------------------------------------------------------------------------------
    def prune_tree(self):
        """A ledger at cycle 3, three WinBench snapshots (oldest deletable) and one old sequence frame."""
        hdr = "| " + " | ".join(aosa.LEDGER_COLS) + " |"
        self.put("LEDGER.md", "# AOSA ledger" + NL * 2 + hdr + NL + "|" + "---|" * len(aosa.LEDGER_COLS) + NL
                 + "| 2 | 2026-10-01T00:00Z | P | - | - | - | - | - | 0.00 | seeded |" + NL)
        self.assertEqual(aosa.current_cycle(), 3)
        snaps = []
        for i, label in enumerate(("old", "mid", "new")):
            d = Path(os.environ["TW_BUILDS"]) / ("WinBench@" + label)
            d.mkdir(parents=True)
            (d / "TrenchWarfare.exe").write_bytes(b"x" * 64)
            os.utime(d, (1700000000 + i * 100, 1700000000 + i * 100))
            snaps.append(d)
        frame = self.docs / "runs" / "0" / "l.f1.png"
        frame.parent.mkdir(parents=True, exist_ok=True)
        frame.write_bytes(b"p" * 16)
        return snaps, frame

    # A17 gap: prune --apply had no test at all; this one covers the good path, which was already right, so it has
    # no [A17] tag (it is green before the fix too)
    def test_prune_apply_deletes_the_plan(self):
        (old, mid, new), frame = self.prune_tree()
        code, out = self.run_cli("prune", "--apply")
        self.assertEqual(code, 0, out)
        self.assertFalse(old.exists(), "the snapshot past the newest two must be deleted")
        self.assertFalse(frame.exists(), "the old cycle's sequence frame must be deleted")
        self.assertTrue(mid.exists() and new.exists(), "the newest two snapshots must stay")
        self.assertIn("deleted 2 items", out, "prune reports what it deleted")

    # [A17] the planned count used to be printed as deleted and the exit stayed 0 even when a delete raised
    def test_a17_prune_counts_only_what_it_deleted(self):
        (old, mid, new), frame = self.prune_tree()
        real = shutil.rmtree

        def refuse(path, *a, **kw):
            if Path(path) == old:
                raise OSError("in use by another process")
            return real(path, *a, **kw)

        shutil.rmtree = refuse
        try:
            code, out = self.run_cli("prune", "--apply")
        finally:
            shutil.rmtree = real
        self.assertEqual(code, 1, "[A17] a prune that could not delete everything must not exit 0:" + NL + out)
        self.assertIn("could not delete", out, "[A17] the item that stayed must be named")
        self.assertIn(old.name, out, "[A17] the item that stayed must be named")
        self.assertIn("deleted 1 items", out, "[A17] only what really went may be counted as deleted")
        self.assertTrue(old.exists())
        self.assertFalse(frame.exists())

    # A17 gap: run_one was only ever tested with --dry-run; these two cover the real player path (already correct)
    def test_run_one_without_a_report_fails(self):
        d = self.live_player()
        spec = {"label": "l", "cycle": 3, "mode": "player", "dev": False, "scenario": None, "knobs": None,
                "shot_tick": None, "no_hud": False, "shot_frames": 1, "extra": "", "build_dir": str(d)}
        cyc = self.docs / "runs" / "3"
        cyc.mkdir(parents=True, exist_ok=True)
        real = aosa.run_player_quietly
        aosa.run_player_quietly = lambda cmd, timeout: 0
        try:
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                ok = aosa.run_one(spec, "l-1", cyc, False)
        finally:
            aosa.run_player_quietly = real
        self.assertFalse(ok)
        self.assertIn("NO REPORT", out.getvalue())

    def test_run_one_with_a_report_passes(self):
        d = self.live_player()
        spec = {"label": "l", "cycle": 3, "mode": "player", "dev": False, "scenario": None, "knobs": None,
                "shot_tick": None, "no_hud": False, "shot_frames": 1, "extra": "", "build_dir": str(d)}
        cyc = self.docs / "runs" / "3"
        cyc.mkdir(parents=True, exist_ok=True)
        real = aosa.run_player_quietly

        def write(cmd, timeout):
            (cyc / "l-1.json").write_text(json.dumps(report("l-1")), encoding="utf-8")
            return 0

        aosa.run_player_quietly = write
        try:
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                ok = aosa.run_one(spec, "l-1", cyc, False)
        finally:
            aosa.run_player_quietly = real
        self.assertTrue(ok, out.getvalue())
        self.assertIn("report written", out.getvalue())


if __name__ == "__main__":
    unittest.main(verbosity=1)
