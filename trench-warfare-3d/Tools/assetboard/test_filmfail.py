#!/usr/bin/env python3
"""Tests of the finding found-board-board-build-must-survive-a-blender-film-work-20261017-1 (board build must survive
a Blender film worker that dies). Run from trench-warfare-3d/:
python Tools/assetboard/test_filmfail.py

films.attach() waited on each Blender worker and then read its results file with no try: a worker killed mid-write
left a truncated results<k>.json and json.loads raised straight out of attach(), killing the whole board build; a
hung worker raised TimeoutExpired from the same line and was never killed. film_blender.py rewrote results<k>.json
in place after every job, so a kill mid-write truncated it. Both fixes let one bad worker fail its own films alone.
"""
import json
import os
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent

TEMP = Path(tempfile.mkdtemp(prefix='tw-filmfail-'))
os.environ['TW_BLENDER'] = str(TEMP / 'blender.exe')
os.environ['TW_FFMPEG'] = str(TEMP / 'ffmpeg.exe')
(TEMP / 'blender.exe').write_text('stub', encoding='utf-8')

import sys
sys.path.insert(0, str(HERE))
import films      # noqa: E402

films.FFMPEG = 'ffmpeg'
films.thumbs.BLENDER = TEMP / 'blender.exe'

ID = 'found-board-build-must-survive-a-blender-film-work-20261017-1'


def a_job(id_, order):
    return dict(id=id_, kind='turn', sources=[], models=[], title='Turntable', order=order)


class FakeProc:
    def __init__(self, res, text=None, timeout_then_hang=False):
        self.res = res
        self.text = text
        self.timeout_then_hang = timeout_then_hang
        self.waited = 0
        self.killed = False

    def wait(self, timeout=None):
        if self.timeout_then_hang and self.waited == 0:
            self.waited += 1
            import subprocess
            raise subprocess.TimeoutExpired('blender', 14400)
        if self.text is not None:
            self.res.write_text(self.text, encoding='utf-8')
        return 0

    def kill(self):
        self.killed = True


class FilmWorkerDies(unittest.TestCase):
    """[found-board-build-must-survive-a-blender-film-work-20261017-1] a dead or hung Blender worker fails its own
    films alone, instead of killing the whole board build."""

    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix='tw-filmfail-case-', dir=TEMP))
        self.P = self.tmp / 'repo' / 'trench-warfare-3d' / 'Tools'
        self.P.mkdir(parents=True)
        self.stage = self.tmp / 'stage'
        self.out = self.tmp / 'out'
        self.local = self.tmp / 'local'
        for d in (self.stage, self.out, self.local):
            d.mkdir()
        self.meta = dict(warnings=[], sources={}, built='2026-10-17')
        self.assets = {}
        self.code = dict(clips=[])
        self.procs = []
        self.keep = (films.jobs_for, films.subprocess.Popen, films.encode)
        films.jobs_for = lambda P, assets, code, warnings=None: [a_job('A.turn', 0), a_job('B.turn', 1)]
        films.encode = lambda frames, result, mp4, poster: (mp4.parent.mkdir(parents=True, exist_ok=True), mp4.write_bytes(b'mp4'), poster.write_bytes(b'jpg'))

    def tearDown(self):
        films.jobs_for, films.subprocess.Popen, films.encode = self.keep

    def _popen(self, procs_by_res):
        def popen(argv, **kw):
            res = Path(argv[-1])
            return procs_by_res(res)
        films.subprocess.Popen = popen

    def test_truncated_result_fails_its_films_alone(self):
        def make(res):
            if res.name == 'results0.json':
                return FakeProc(res, text='{"A.turn": {"ok": tr')
            return FakeProc(res, text=json.dumps({'B.turn': {'ok': True, 'frames': 48, 'segments': []}}))
        self._popen(make)

        films.attach(self.P, self.assets, self.code, self.stage, self.out, self.local, self.meta)

        warnings = self.meta['warnings']
        self.assertTrue(any('A.turn' in w or 'results0' in w for w in warnings),
                         '[found-board-build-must-survive-a-blender-film-work-20261017-1] no warning named the unreadable worker: %s' % warnings)
        self.assertFalse(any('B.turn' in w for w in warnings),
                          '[found-board-build-must-survive-a-blender-film-work-20261017-1] the healthy worker was blamed too: %s' % warnings)
        self.assertEqual(self.meta['sources']['board films']['made'], 1,
                          '[found-board-build-must-survive-a-blender-film-work-20261017-1] the healthy film was not made: %s' % self.meta['sources']['board films'])

    def test_hung_worker_is_killed(self):
        hung = []

        def make(res):
            if res.name == 'results0.json':
                p = FakeProc(res, timeout_then_hang=True)
                hung.append(p)
                return p
            return FakeProc(res, text=json.dumps({'B.turn': {'ok': True, 'frames': 48, 'segments': []}}))
        self._popen(make)

        films.attach(self.P, self.assets, self.code, self.stage, self.out, self.local, self.meta)

        self.assertTrue(hung and hung[0].killed,
                         '[found-board-build-must-survive-a-blender-film-work-20261017-1] the hung worker was never killed')
        self.assertTrue(any('hung' in w for w in self.meta['warnings']),
                         '[found-board-build-must-survive-a-blender-film-work-20261017-1] no warning named the hung worker: %s' % self.meta['warnings'])


class ResultsAreWrittenWhole(unittest.TestCase):
    """[found-board-build-must-survive-a-blender-film-work-20261017-1] film_blender writes results through a temp
    file and os.replace, so a kill mid-write cannot truncate the last whole file."""

    @classmethod
    def setUpClass(cls):
        from unittest.mock import MagicMock
        cls.keep_mods = {k: sys.modules.get(k) for k in ('bpy', 'mathutils')}
        sys.modules['bpy'] = MagicMock()
        sys.modules['mathutils'] = MagicMock()
        import film_blender
        cls.film_blender = film_blender

    @classmethod
    def tearDownClass(cls):
        for k, v in cls.keep_mods.items():
            if v is None:
                sys.modules.pop(k, None)
            else:
                sys.modules[k] = v

    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix='tw-filmfail-write-', dir=TEMP))
        self.path = str(self.tmp / 'results0.json')

    def test_results_are_written_whole(self):
        first = {'A.turn': {'ok': True, 'frames': 1, 'segments': []}}
        self.film_blender.write_results(self.path, first)
        self.assertEqual(json.loads(Path(self.path).read_text(encoding='utf-8')), first,
                          '[found-board-build-must-survive-a-blender-film-work-20261017-1] write_results did not write the file')

        keep_dump = self.film_blender.json.dump

        def boom(*a, **kw):
            raise RuntimeError('killed mid-write')
        self.film_blender.json.dump = boom
        try:
            with self.assertRaises(RuntimeError):
                self.film_blender.write_results(self.path, {'A.turn': {'ok': False}})
        finally:
            self.film_blender.json.dump = keep_dump

        self.assertEqual(json.loads(Path(self.path).read_text(encoding='utf-8')), first,
                          '[found-board-build-must-survive-a-blender-film-work-20261017-1] a failed write truncated the last whole file')


if __name__ == '__main__':
    unittest.main()
