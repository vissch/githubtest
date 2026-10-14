#!/usr/bin/env python3
"""Tests for occ.py. Run from trench-warfare-3d/: python Tools/aosa/test_occ.py"""
import contextlib, importlib.util, io, pathlib, sys, tempfile, unittest
sys.dont_write_bytecode = True
from pathlib import Path

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("occ", str(HERE / "occ.py"))
occ = importlib.util.module_from_spec(spec)
spec.loader.exec_module(occ)


class OccTest(unittest.TestCase):
    def setUp(self):
        self._asmdefs = occ.asmdefs
        self._changed = occ.changed
        occ.asmdefs = lambda: {}

    def tearDown(self):
        occ.asmdefs = self._asmdefs
        occ.changed = self._changed

    def run_main(self, argv):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            code = occ.main(argv)
        return code, out.getvalue()

    # [A10] --changed with nothing changed must be 0, not the usage docstring's 2
    def test_a10_changed_with_nothing_changed(self):
        occ.changed = lambda table: []
        code, out = self.run_main(["--changed"])
        self.assertEqual(code, 0, "[A10] --changed with nothing changed must exit 0")
        self.assertIn("nothing to compile", out, "[A10] must say why it is doing nothing")

    # [A10] a failed git call must not look like "nothing changed"
    def test_a10_git_failure_is_not_silence(self):
        tmp = Path(tempfile.mkdtemp(prefix="occ-test-"))
        orig_proj = occ.PROJ
        occ.PROJ = tmp / "trench-warfare-3d"
        try:
            code, out = self.run_main(["--changed"])
        finally:
            occ.PROJ = orig_proj
        self.assertEqual(code, 2, "[A10] a failed git call must exit 2, not silently compile nothing")
        self.assertIn("occ: git", out, "[A10] a failed git call must be reported, not folded into 'nothing changed'")


    # [A10] an asmdef-only change (references, defines) changes the dll, so --changed must not say
    # "nothing to compile" for it
    def test_a10_an_asmdef_only_change_is_still_a_change(self):
        tmp = Path(tempfile.mkdtemp(prefix="occ-asmdef-"))
        asmdef = tmp / "trench-warfare-3d" / "Assets" / "_Project" / "Thing" / "TW.Thing.asmdef"
        asmdef.parent.mkdir(parents=True)
        asmdef.write_text('{"name": "TW.Thing"}', encoding="utf-8")
        table = {"TW.Thing": (asmdef, {"name": "TW.Thing"})}
        rel = str(asmdef.relative_to(tmp)).replace("\\", "/")

        def fake_run(cmd, **kw):
            out = rel + chr(10) if "diff" in cmd else ""
            return __import__("subprocess").CompletedProcess(cmd, 0, out, "")

        orig_proj, orig_run = occ.PROJ, occ.subprocess.run
        occ.PROJ = tmp / "trench-warfare-3d"
        occ.subprocess.run = fake_run
        occ.asmdefs = lambda: table
        try:
            self.assertEqual(occ.changed(table), ["TW.Thing"],
                             "[A10] a changed .asmdef must put its assembly in --changed")
            code, out = self.run_main(["--changed"])
        finally:
            occ.PROJ, occ.subprocess.run = orig_proj, orig_run
        self.assertNotIn("nothing to compile", out,
                         "[A10] an asmdef-only change must not read as nothing to compile")


if __name__ == "__main__":
    unittest.main(verbosity=1)
