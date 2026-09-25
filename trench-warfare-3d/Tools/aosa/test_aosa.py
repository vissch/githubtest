#!/usr/bin/env python3
"""Tests for aosa.py on temporary fixture data. Run from trench-warfare-3d/: python Tools/aosa/test_aosa.py

Every test points AOSA_DOCS, TW_BUILDS, TW_PROJECT and LOCALAPPDATA at a temp dir and sets AOSA_OFFLINE, so the real
docs, the editor slot, the builds and the network are never touched.
"""
import contextlib, datetime, importlib.util, io, json, os, shutil, subprocess, sys, tempfile, time, unittest
sys.dont_write_bytecode = True
from pathlib import Path

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
           knobs=None, gpu_name="RTX 4070"):
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
        self.assertEqual(v["target"]["band_source"], "3*MAD of repeats")
        self.assertAlmostEqual(v["target"]["delta"], 7.0 - 8.02, places=6)
        code, out = self.run_cli("compare", "cand", "base")
        self.assertIn("VERDICT: PASS", out)
        self.assertIn("hash_start", out)

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

    def test_bench_dry_run_editor_ignores_shared_slot(self):
        slot = self.tmp / "lad" / "TrenchWarfare" / "editor-slot.json"
        slot.parent.mkdir(parents=True)
        slot.write_text(json.dumps({"who": "claude-7", "why": "", "since": time.time(), "until": time.time() + 600}))
        # the shared slot queues the MAIN clone's editor: someone else holding it never blocks the loop's own editor
        code, out = self.run_cli("bench", "e", "--editor", "--dry-run")
        self.assertEqual(code, 0, out)
        self.assertIn('TW.Editor.CaptureRig.Bench(', out)
        self.assertIn("canary=0 label=e-1", out)
        self.assertNotIn("quality=5", out)

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


if __name__ == "__main__":
    unittest.main(verbosity=1)
