#!/usr/bin/env python3
"""Tests for sweep.py with no editor: python Tools/test_sweep.py. Stdlib only.

The spec's grids and its refusals, the numbers (percentile, how the seeds moved, in or out of a band), and a run
played by a stand-in for Unity: what is written, what a resume skips, and the exit codes.
"""
import contextlib, io, json, os, sys, tempfile, unittest, warnings
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import sweep as S

SPEC = {
    "scenario": "match", "seeds": [1, 2, 3],
    "base": [{"config": "StartingSilver", "set": 300}],
    "variants": [
        {"name": "mg", "patches": [{"unit": "Machinegunner", "field": "Weapon.Damage", "mul": [0.8, 0.9]}]},
        {"name": "bold", "patches": [{"script_b": "Odds", "set": 1.5}, {"script_b": "Defends", "set": True}]},
    ],
    "targets": {"win_a": [0.4, 0.6]},
}


class Args:
    def __init__(self, spec, **kw):
        self.spec, self.seeds, self.minutes, self.chunk, self.resume = spec, None, None, 2, None
        self.__dict__.update(kw)


class SpecTest(unittest.TestCase):
    def test_a_grid_becomes_one_variant_a_value_and_now_comes_first(self):
        header, variants = S.compile_spec(SPEC)
        self.assertEqual([v["name"] for v in variants], ["now", "mg@0.8", "mg@0.9", "bold"])
        self.assertEqual(header["swapSeats"], True)
        base = {"on": "config", "unit": "", "field": "StartingSilver", "op": "set", "value": "300"}
        self.assertEqual(variants[0]["patches"], [base], "the baseline carries the base patches and nothing else")
        self.assertEqual(variants[1]["patches"], [base, {"on": "unit", "unit": "Machinegunner", "field": "Weapon.Damage", "op": "mul", "value": "0.8"}])
        self.assertEqual(variants[3]["patches"][2]["value"], "true", "a switch is written as the test reads it")

    def test_a_harness_patch_is_written_like_a_script_s(self):
        spec = {"variants": [{"name": "walk", "patches": [{"harness": "Sea", "set": False}]}]}
        self.assertEqual(S.compile_spec(spec)[1][1]["patches"], [{"on": "harness", "unit": "", "field": "Sea", "op": "set", "value": "false"}])

    def test_two_grids_in_one_variant_are_every_pair(self):
        spec = {"variants": [{"name": "g", "patches": [{"unit": "Rifle", "field": "Weapon.Damage", "mul": [1, 2]},
                                                       {"unit": "Rifle", "field": "Weapon.RangeMax", "set": [90, 110, 130]}]}]}
        names = [v["name"] for v in S.compile_spec(spec)[1]]
        self.assertEqual(len(names), 1 + 6)
        self.assertIn("g@2@110", names)

    def test_every_value_is_a_string_and_every_field_is_there(self):
        header, variants = S.compile_spec(dict(SPEC, scenario="ladder", ladder=[{"attackers": 30, "defenders": 10}]))
        self.assertEqual(header["ladder"], [{"attackers": 30, "defenders": 10, "support": "None", "gunners": 0, "attackGuns": 0, "gunsCover": False}])
        self.assertFalse(header["swapSeats"], "nobody swaps seats on the ladder")
        for v in variants:
            for p in v["patches"]:
                self.assertEqual(set(p), {"on", "unit", "field", "op", "value"})
                self.assertTrue(all(isinstance(x, str) for x in p.values()))

    def test_what_it_refuses(self):
        bad = [
            {"scenario": "siege"},
            {"turns": 3},
            {"variants": [{"name": "now", "patches": [{"config": "FactionA", "set": "Brass"}]}]},
            {"variants": [{"name": "a b", "patches": [{"config": "FactionA", "set": "Brass"}]}]},
            {"variants": [{"name": "empty", "patches": []}]},
            {"variants": [{"name": "x", "patches": [{"config": "FactionA"}]}]},
            {"variants": [{"name": "x", "patches": [{"config": "FactionA", "set": 1, "mul": 2}]}]},
            {"variants": [{"name": "x", "patches": [{"unit": "Rifle", "set": 1}]}]},
            {"variants": [{"name": "x", "patches": [{"unit": "Rifle", "field": "Hp", "config": "Seed", "set": 1}]}]},
            {"variants": [{"name": "x", "patches": [{"config": "FactionA", "set": []}]}]},
            {"variants": [{"name": "x", "patches": [{"config": "FactionA", "set": {"a": 1}}]}]},
            {"variants": [{"name": "x", "patches": [{"config": "FactionA", "set": 1, "why": "?"}]}]},
            {"variants": [{"name": "x", "patches": [{"config": "A", "set": 1}]}, {"name": "x", "patches": [{"config": "A", "set": 2}]}]},
            {"base": [{"config": "FactionA", "set": ["Iron", "Brass"]}]},
            {"scenario": "ladder"},
            {"policy": "Defend", "swap_seats": True},
            {"targets": {"win_a": [0.6, 0.4]}},
            {"ladder": [{"attackers": 30, "defenders": 10, "smoke": True}]},
        ]
        for spec in bad:
            with self.assertRaises(S.SpecError, msg=json.dumps(spec)):
                S.compile_spec(spec)
        self.assertFalse(S.compile_spec({"policy": "Defend"})[0]["swapSeats"], "a policy that is not the script never swaps, unasked")


class NumbersTest(unittest.TestCase):
    def test_percentile_is_linear_between_the_ranks(self):
        self.assertEqual(S.percentile([4, 1, 3, 2], 0.5), 2.5)
        self.assertEqual(S.percentile([10], 0.9), 10)
        self.assertAlmostEqual(S.percentile([0, 10], 0.1), 1.0)
        self.assertNotEqual(S.percentile([], 0.5), S.percentile([], 0.5), "nothing to rank: not a number")

    def test_how_the_seeds_moved(self):
        self.assertEqual(S.agreement([0, 0, 0]), "same")
        self.assertEqual(S.agreement([1, 2, 0.1]), "all up")
        self.assertEqual(S.agreement([-1, -2]), "all down")
        self.assertEqual(S.agreement([1, -1, 0]), "mixed")
        self.assertEqual(S.agreement([1, 0]), "mixed", "one seed that did not move is not agreement")
        self.assertEqual(S.agreement([]), "no seed has both")

    def test_a_number_only_some_seeds_have_is_compared_on_those(self):
        data = {"now": {"seed1": {"t": 10.0, "w": 1.0}, "seed2": {"w": 0.0}, "mean": {"t": 10.0, "w": 0.5}},
                "v": {"seed1": {"t": 14.0, "w": 1.0}, "seed2": {"t": 30.0, "w": 1.0}, "mean": {"t": 22.0, "w": 1.0}}}
        rows = {r["metric"]: r for r in S.compare(data, ["now", "v"], {"w": [0.4, 0.6]})}
        self.assertEqual(rows["t"]["n"], 1)
        self.assertEqual(rows["t"]["variants"]["v"]["seeds"], "all up", "only seed1 has both")
        self.assertEqual(rows["w"]["variants"]["v"]["seeds"], "mixed")
        self.assertTrue(rows["w"]["now_in"])
        self.assertFalse(rows["w"]["variants"]["v"]["in"])
        self.assertIsNone(rows["t"]["band"])


def stand_in(win_by_variant, refuse=()):
    """Plays a chunk as Unity would: one report per variant, none for a variant the test refuses."""
    calls = []

    def runner(compiled, out, seeds, index):
        part = json.load(open(compiled))
        calls.append([v["name"] for v in part["variants"]])
        for v in part["variants"]:
            if v["name"] in refuse:
                return (0, 1)
            win = win_by_variant.get(v["name"], 0.5)
            report = {f"seed{s}": {"win_a": win, "end_s": 400.0 + int(s)} for s in seeds.split(",")}
            report["mean"] = {"win_a": win, "end_s": 402.0}
            json.dump(report, open(os.path.join(out, v["name"] + ".json"), "w"))
        return (1, 0)
    runner.calls = calls
    return runner


class RunTest(unittest.TestCase):
    def setUp(self):
        warnings.simplefilter("ignore", ResourceWarning)   # json.load(open(...)), as the other tools read
        self.quiet = contextlib.redirect_stdout(io.StringIO())   # a run prints its report
        self.quiet.__enter__()
        self.tmp = tempfile.TemporaryDirectory()
        self.root = os.path.join(self.tmp.name, "sweeps")
        os.environ["TW_SWEEPS"] = self.root
        self.spec = os.path.join(self.tmp.name, "spec.json")
        json.dump(SPEC, open(self.spec, "w"))

    def tearDown(self):
        self.quiet.__exit__(None, None, None)
        os.environ.pop("TW_SWEEPS", None)
        self.tmp.cleanup()

    def run_folder(self):
        runs = sorted(os.listdir(self.root))
        return os.path.join(self.root, runs[-1])

    def test_a_run_plays_every_variant_in_chunks_and_reports(self):
        runner = stand_in({"now": 0.5, "mg@0.8": 0.7})
        self.assertEqual(S.run(Args(self.spec), runner), 0, "the baseline is inside the band")
        self.assertEqual(runner.calls, [["now", "mg@0.8"], ["mg@0.9", "bold"]])
        out = self.run_folder()
        report = json.load(open(os.path.join(out, "report.json")))
        win = [r for r in report["rows"] if r["metric"] == "win_a"][0]
        self.assertEqual(win["variants"]["mg@0.8"], {"mean": 0.7, "seeds": "all up", "in": False})
        self.assertIn("| mg@0.8 | 0 of 1 |", open(os.path.join(out, "report.md"), encoding="utf-8").read())
        self.assertEqual(json.load(open(os.path.join(out, "run.json")))["seeds"], "1,2,3")

    def test_a_baseline_outside_a_band_exits_2(self):
        self.assertEqual(S.run(Args(self.spec), stand_in({"now": 0.9})), 2)

    def test_a_target_no_run_reported_exits_2(self):
        json.dump(dict(SPEC, targets={"win_z": [0, 1]}), open(self.spec, "w"))
        self.assertEqual(S.run(Args(self.spec), stand_in({})), 2)

    def test_a_refused_variant_stops_the_run_and_a_resume_plays_only_what_is_missing(self):
        first = stand_in({}, refuse=("mg@0.9",))
        self.assertEqual(S.run(Args(self.spec), first), 1, "not every variant reported")
        out = self.run_folder()
        self.assertIn("Not played: mg@0.9, bold", open(os.path.join(out, "report.md"), encoding="utf-8").read())
        second = stand_in({})
        self.assertEqual(S.run(Args(self.spec, resume=out), second), 0)
        self.assertEqual(second.calls, [["mg@0.9", "bold"]], "what was reported is not played again")
        self.assertEqual(S.run(Args(self.spec, resume=out, seeds="1,2"), stand_in({})), 1, "other seeds are another run")

    def test_a_spec_that_does_not_compile_plays_nothing(self):
        json.dump({"scenario": "siege"}, open(self.spec, "w"))
        runner = stand_in({})
        self.assertEqual(S.run(Args(self.spec), runner), 1)
        self.assertEqual(runner.calls, [])
        self.assertFalse(os.path.exists(self.root) and os.listdir(self.root), "no run folder for a spec that is refused")

    def test_prune_keeps_the_newest_and_anything_marked_and_touches_only_sweep_runs(self):
        os.makedirs(self.root)
        for name in ("20260101-0000-a", "20260102-0000-b", "20260103-0000-c", "20260104-0000-d"):
            os.makedirs(os.path.join(self.root, name))
            open(os.path.join(self.root, name, "run.json"), "w").write("{}")
        open(os.path.join(self.root, "20260101-0000-a", "KEEP.txt"), "w").write("the owner is reading it")
        os.makedirs(os.path.join(self.root, "20250101-not-a-run"))
        S.prune(self.root, 2)
        self.assertEqual(sorted(os.listdir(self.root)), ["20250101-not-a-run", "20260101-0000-a", "20260103-0000-c", "20260104-0000-d"])


class StandingSpecsTest(unittest.TestCase):
    def test_every_spec_in_tools_sweeps_compiles(self):
        warnings.simplefilter("ignore", ResourceWarning)
        folder = Path(__file__).resolve().parent / "sweeps"
        specs = sorted(folder.glob("*.json"))
        self.assertTrue(specs, "Tools/sweeps/ holds the standing specs")
        for p in specs:
            header, variants = S.compile_spec(json.load(open(p, encoding="utf-8")))
            self.assertEqual(variants[0]["name"], "now", p.name)


if __name__ == "__main__":
    unittest.main(verbosity=1)
