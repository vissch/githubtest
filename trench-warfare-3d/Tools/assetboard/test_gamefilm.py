#!/usr/bin/env python3
"""Tests of gamefilm.py's refusal to film without ffmpeg, the finding
found-gamefilm-refuse-to-film-when-there-is-no-ffm-20261017-3. Run from trench-warfare-3d/:
python Tools/assetboard/test_gamefilm.py

gamefilm.py passed FFMPEG as argv[0] of subprocess.run while the entry point checked only the unity CLI, so on a
machine without ffmpeg `gamefilm.py film` started the editor, rendered every frame, and then died with a bare
TypeError at the encode step, with the editor left in Play.
"""
import contextlib
import io
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import gamefilm  # noqa: E402

ID = 'found-gamefilm-refuse-to-film-when-there-is-no-ffm-20261017-3'


class _LoudSubprocess:
    """A stand-in for the subprocess module: any call means the guard did not stop the editor/ffmpeg call."""

    def run(self, *a, **k):
        raise AssertionError(f'[{ID}] it called subprocess.run: {a!r}')

    def Popen(self, *a, **k):
        raise AssertionError(f'[{ID}] it called subprocess.Popen: {a!r}')


class GamefilmGuards(unittest.TestCase):

    def setUp(self):
        self.main = getattr(gamefilm, 'main', None)
        self.assertTrue(callable(self.main), f'[{ID}] gamefilm.py has no callable entry point (main)')
        self.keep = (gamefilm.FFMPEG, gamefilm.CLI, gamefilm.run, gamefilm.subprocess)
        gamefilm.CLI = Path(gamefilm.__file__)  # certainly there: the CLI check must not be what fires
        gamefilm.run = lambda *a, **k: self.fail(f'[{ID}] it called the editor')
        gamefilm.subprocess = _LoudSubprocess()

    def tearDown(self):
        gamefilm.FFMPEG, gamefilm.CLI, gamefilm.run, gamefilm.subprocess = self.keep

    def test_film_refuses_without_ffmpeg(self):
        gamefilm.FFMPEG = None
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = self.main(['film'])
        self.assertNotEqual(rc, 0, f'[{ID}] film started with no ffmpeg: exit {rc}')
        self.assertIn('ffmpeg', out.getvalue(), f'[{ID}] the refusal does not name ffmpeg: {out.getvalue()!r}')

    def test_battle_refuses_without_ffmpeg(self):
        gamefilm.FFMPEG = None
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = self.main(['battle'])
        self.assertNotEqual(rc, 0, f'[{ID}] battle started with no ffmpeg: exit {rc}')
        self.assertIn('ffmpeg', out.getvalue(), f'[{ID}] the refusal does not name ffmpeg: {out.getvalue()!r}')

    def test_a_path_that_is_not_there_is_no_ffmpeg(self):
        gamefilm.FFMPEG = str(Path(tempfile.gettempdir()) / 'tw-no-such-ffmpeg.exe')
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            rc = self.main(['film'])
        self.assertNotEqual(rc, 0, f'[{ID}] film started with a TW_FFMPEG that points nowhere: exit {rc}')
        self.assertIn('ffmpeg', out.getvalue(), f'[{ID}] the refusal does not name ffmpeg: {out.getvalue()!r}')

    def test_the_guard_leaves_the_other_commands_alone(self):
        gamefilm.FFMPEG = None
        self.assertEqual(gamefilm.missing_tools('up'), '', f'[{ID}] up is wrongly refused without ffmpeg')
        self.assertEqual(gamefilm.missing_tools('down'), '', f'[{ID}] down is wrongly refused without ffmpeg')
        gamefilm.FFMPEG = sys.executable  # a file that exists and is executable
        self.assertEqual(gamefilm.missing_tools('film'), '', f'[{ID}] film is refused with a real ffmpeg present')


if __name__ == '__main__':
    unittest.main()
