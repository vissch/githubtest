"""Tests for Tools/otr.py's per-run verdict rules (stdlib unittest; no Unity, no dlls).

Run: python Tools/test_otr.py
"""
import io, contextlib, os, pathlib, shutil, sys, tempfile, time, unittest

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


class StaleDlls(unittest.TestCase):
    # [O10] staleness was "newest existing source vs dll": a deleted .cs, or an asmdef-only change (references,
    # defines), left the old dll reading as fresh and otr gave a verdict on code that is not the tree's. The dll's
    # real file list is the .rsp occ.py compiled it from.

    def setUp(self):
        self.root = pathlib.Path(tempfile.mkdtemp(prefix="otr-stale-"))
        self.addCleanup(shutil.rmtree, self.root, True)
        self.out = self.root / "out"
        self.out.mkdir()
        self.asm = self.root / "Asm"
        self.asm.mkdir()
        self.asmdef = self.asm / "TW.Thing.asmdef"
        self.asmdef.write_text('{"name": "TW.Thing"}', encoding="utf-8")
        self.table = {"TW.Thing": (self.asmdef, {"name": "TW.Thing"})}
        self.dirs = [self.asm]

    def write(self, *names):
        made = []
        for n in names:
            f = self.asm / n
            f.write_text("class X {}", encoding="utf-8")
            made.append(f)
        return made

    def rsp(self, *files):
        body = ['-nologo', '-r:"C:/unity/UnityEngine.dll"'] + [f'"{f}"' for f in files]
        (self.out / "TW.Thing.rsp").write_text(chr(10).join(body), encoding="utf-8")

    def dll(self, age=0.0):
        """The built dll, newest of all: nothing in the tree may look newer than it."""
        d = self.out / "TW.Thing.dll"
        d.write_bytes(b"MZ")
        now = time.time()
        for f in [self.asmdef, *self.asm.rglob("*.cs")]:
            os.utime(f, (now - 100, now - 100))
        os.utime(d, (now - age, now - age))
        return d

    def test_o10_a_deleted_source_is_stale(self):
        a, b = self.write("A.cs", "B.cs")
        self.rsp(a, b)
        self.dll()
        b.unlink()
        why = otr.stale(self.table, self.dirs, self.out, self.root)
        self.assertIsNotNone(why, "[O10] a .cs the dll was built with is gone: not a verdict")
        self.assertIn("B.cs", why, "[O10] the message names the source that is gone")

    def test_o10_a_new_source_not_in_the_rsp_is_stale(self):
        a, = self.write("A.cs")
        self.rsp(a)
        self.write("New.cs")
        self.dll()
        why = otr.stale(self.table, self.dirs, self.out, self.root)
        self.assertIsNotNone(why, "[O10] a .cs on disk that the dll was not built from: not a verdict")
        self.assertIn("New.cs", why, "[O10] the message names the source the dll does not have")

    def test_o10_an_asmdef_newer_than_the_dll_is_stale(self):
        a, = self.write("A.cs")
        self.rsp(a)
        d = self.dll()
        os.utime(self.asmdef, (d.stat().st_mtime + 60, d.stat().st_mtime + 60))
        why = otr.stale(self.table, self.dirs, self.out, self.root)
        self.assertIsNotNone(why, "[O10] an asmdef changed after the build (references, defines): not a verdict")
        self.assertIn("TW.Thing.asmdef", why, "[O10] the message names the asmdef")

    def test_o10_a_matching_tree_is_fresh(self):
        a, b = self.write("A.cs", "B.cs")
        self.rsp(a, b)
        self.dll()
        self.assertIsNone(otr.stale(self.table, self.dirs, self.out, self.root),
                          "[O10] every source in the rsp and on disk, the dll newest: a verdict")

    def test_o10_no_rsp_beside_the_dll_is_stale(self):
        a, = self.write("A.cs")
        self.dll()
        why = otr.stale(self.table, self.dirs, self.out, self.root)
        self.assertIsNotNone(why, "[O10] without the rsp otr cannot tell what the dll holds: not a verdict")
        self.assertIn("TW.Thing.dll", why, "[O10] the message names the dll it cannot vouch for")


if __name__ == "__main__":
    unittest.main(verbosity=2)
