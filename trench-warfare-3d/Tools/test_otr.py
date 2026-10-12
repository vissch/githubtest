"""Tests for Tools/otr.py's per-run verdict rules (stdlib unittest; no Unity, no dlls).

Run: python Tools/test_otr.py
"""
import io, contextlib, pathlib, sys, unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import otr  # noqa: E402


def scan(text, returncode=0, verbose=False):
    """scan_run with its printing captured: (tally, bad, current, done, printed)."""
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        tally, bad, current, done = otr.scan_run(text, returncode, {}, verbose)
    return tally, bad, current, done, out.getvalue()


class ScanRunVerdicts(unittest.TestCase):
    # [O1] a run that drops a whole module must not read as a pass: no #DONE and no class, #CRASH,
    # LOADER / ENGINE-INSTALL-FAILED, and a non-zero exit after #DONE are all bad.

    def test_scan_run_exists(self):
        self.assertTrue(hasattr(otr, "scan_run"), "[O1] otr.py has no module-level scan_run to judge one run")

    def test_runner_died_before_first_class(self):
        tally, bad, current, done, _ = scan("Unhandled Exception\n  at Foo.Bar\n", returncode=1)
        self.assertEqual(current, "", "[O1] no #CLASS line, so no class ran")
        self.assertFalse(done, "[O1] no #DONE, so the run did not finish")
        self.assertEqual(tally, {}, "[O1] nothing ran, so the run itself records no verdict")

    def test_crash_line_is_bad(self):
        tally, bad, current, done, printed = scan("#CLASS A\n#CRASH A.one\n")
        self.assertTrue(bad, "[O1] a #CRASH line must set bad")
        self.assertIn("#CRASH A.one", printed, "[O1] a #CRASH line is printed")

    def test_loader_line_is_bad_and_printed_without_verbose(self):
        tally, bad, current, done, printed = scan("#CLASS A\nLOADER could not load TW.Sim\n#DONE\n")
        self.assertTrue(bad, "[O1] a LOADER line means the module's types never loaded: bad")
        self.assertIn("LOADER could not load TW.Sim", printed, "[O1] a LOADER line is printed whatever -v says")

    def test_engine_install_failed_is_bad(self):
        tally, bad, current, done, printed = scan("ENGINE-INSTALL-FAILED TypeLoadException: no\n#DONE\n")
        self.assertTrue(bad, "[O1] ENGINE-INSTALL-FAILED means no stand-in engine: bad")
        self.assertIn("ENGINE-INSTALL-FAILED", printed, "[O1] ENGINE-INSTALL-FAILED is printed whatever -v says")

    def test_nonzero_exit_after_done_is_bad(self):
        tally, bad, current, done, printed = scan("#CLASS A\nPASS  A.one\n#DONE\n", returncode=1)
        self.assertTrue(done, "[O1] #DONE was printed")
        self.assertTrue(bad, "[O1] a non-zero exit code after #DONE must set bad")
        self.assertIn("exited 1", printed, "[O1] otr says the runner exited non-zero after #DONE")

    def test_clean_run_is_not_bad(self):
        tally, bad, current, done, _ = scan("#CLASS A\nPASS  A.one\n#DONE\n", returncode=0)
        self.assertFalse(bad, "[O1] a clean run stays good")
        self.assertEqual(tally, {"PASS": 1}, "[O1] a clean run tallies its PASS")


if __name__ == "__main__":
    unittest.main(verbosity=2)
