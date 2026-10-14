"""Tests for Tools/otr.py's per-run verdict rules (stdlib unittest; no Unity, no dlls).

Run: python Tools/test_otr.py
"""
import io, contextlib, os, pathlib, shutil, subprocess, sys, tempfile, time, unittest

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

    def test_o13_an_engine_readback_line_is_printed_without_v(self):
        # [O13] an ENGINE line is silent without -v, so a test that used to pass and now reads an unregistered
        # Get*/Has*/get_* call (the stand-in keeps no rendering state) vanished from a plain otr run.
        nl = chr(10)
        text = ("#CLASS X" + nl +
                "ENGINE  X.reads  otr engine: UnityEngine.Material::GetColor reads back rendering state "
                "the stand-in does not keep" + nl +
                "ENGINE  X.layout  UI Toolkit" + nl + "#DONE" + nl)
        tally, bad, current, done, printed = scan(text)
        self.assertIn("X.reads", printed, "[O13] a read-back ENGINE line is printed without -v")
        self.assertNotIn("X.layout", printed, "[O13] every other ENGINE line still needs -v")

    def test_o13_render_cs_still_throws_the_wording_otr_looks_for(self):
        render = pathlib.Path(otr.__file__).resolve().parent / "otr" / "Render.cs"
        self.assertIn(otr.READBACK, render.read_text(encoding="utf-8"),
                      "[O13] Render.cs must still throw otr.READBACK's wording, or the two drift apart")

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

    def test_o10_a_failed_compile_does_not_make_a_deleted_source_read_fresh(self):
        # [O10] occ.py wrote the .rsp before it compiled, so a FAILED compile left an rsp naming today's files
        # beside yesterday's dll: a deleted .cs then read as fresh and otr gave a verdict on code that is gone.
        import occ   # importing otr put Tools/aosa on sys.path
        a, b = self.write("A.cs", "B.cs")
        self.rsp(a, b)
        self.dll()
        b.unlink()
        empty = self.root / "none"
        empty.mkdir()
        saved = (occ.OUT, occ.UNITY, occ.MAIN_LIB, occ.PKG_CACHE, occ.subprocess.run)
        occ.OUT, occ.UNITY, occ.MAIN_LIB, occ.PKG_CACHE = self.out, empty, empty, empty
        occ.subprocess.run = lambda *a, **k: subprocess.CompletedProcess(a[0] if a else [], 1, "A.cs(1,1): error CS0103: no", "")
        try:
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                ok = occ.compile_one("TW.Thing", self.table, {}, self.dirs)
        finally:
            occ.OUT, occ.UNITY, occ.MAIN_LIB, occ.PKG_CACHE, occ.subprocess.run = saved
        self.assertFalse(ok, "[O10] the compile failed, so compile_one reports False")
        why = otr.stale(self.table, self.dirs, self.out, self.root)
        self.assertIsNotNone(why, "[O10] a failed compile must not leave the dll reading as fresh: not a verdict")
        self.assertIn("B.cs", why, "[O10] the rsp still names the source that is gone")

    def test_o10_no_rsp_beside_the_dll_is_stale(self):
        a, = self.write("A.cs")
        self.dll()
        why = otr.stale(self.table, self.dirs, self.out, self.root)
        self.assertIsNotNone(why, "[O10] without the rsp otr cannot tell what the dll holds: not a verdict")
        self.assertIn("TW.Thing.dll", why, "[O10] the message names the dll it cannot vouch for")



class HungRunnerProcess(unittest.TestCase):
    # [O17] otr had no timeout on the runner process: one wedged process (a native call waiting forever) hung the
    # whole run with no verdict. A process over PROC_TIMEOUT_S is CRASH for the class it was in, and the classes
    # after it run in a new process.

    def test_o17_a_hung_runner_process_is_a_crash_not_a_hang(self):
        calls = []

        def runner(cmd, **kw):
            calls.append(cmd)
            self.assertIn("timeout", kw, "[O17] the runner process must be given a timeout")
            if len(calls) == 1:
                raise subprocess.TimeoutExpired(cmd, kw["timeout"], output="#CLASS A\nPASS  A.one\n", stderr="")
            return subprocess.CompletedProcess(cmd, 0, "#CLASS B\nPASS  B.one\n#DONE\n", "")

        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            tally, bad = otr.run_module(pathlib.Path("Runner.exe"), pathlib.Path("TW.Tests.Thing.dll"), "",
                                        {}, {}, False, runner=runner)
        self.assertTrue(bad, "[O17] a hung runner process is not a pass")
        self.assertEqual(tally.get("CRASH"), 1, "[O17] the class the process hung in is tallied CRASH")
        self.assertEqual(tally.get("PASS"), 2, "[O17] the partial output of the hung process is still counted")
        self.assertIn("CRASH  A", out.getvalue(), "[O17] otr names the class the runner hung in")
        self.assertEqual(len(calls), 2, "[O17] the run carries on in a new process after the hung class")
        self.assertEqual(calls[1][-1], "A", "[O17] the new process resumes after the class that hung")


class SweepTemp(unittest.TestCase):
    # [O17] the runner's tw-otr-<pid> scratch folder was never removed when the process crashed, so otr left one
    # tree per crashed class behind for good. sweep_temp clears the old ones and leaves a live run's alone.

    def test_o17_old_temp_folders_go_and_fresh_ones_stay(self):
        root = pathlib.Path(tempfile.mkdtemp(prefix="otr-sweep-"))
        self.addCleanup(shutil.rmtree, root, True)
        old = root / "tw-otr-1"
        (old / "persistent").mkdir(parents=True)
        (old / "persistent" / "save.json").write_text("{}", encoding="utf-8")
        fresh = root / "tw-otr-2"
        fresh.mkdir()
        keep = root / "something-else"
        keep.mkdir()
        then = time.time() - 3 * 86400
        os.utime(old, (then, then))

        gone = otr.sweep_temp(root)
        self.assertEqual(gone, 1, "[O17] sweep_temp reports the one folder it deleted")
        self.assertFalse(old.exists(), "[O17] a tw-otr folder older than a day is deleted, with what is in it")
        self.assertTrue(fresh.exists(), "[O17] a fresh tw-otr folder may be a live run: it stays")
        self.assertTrue(keep.exists(), "[O17] sweep_temp touches nothing but tw-otr-* folders")


if __name__ == "__main__":
    unittest.main(verbosity=2)
