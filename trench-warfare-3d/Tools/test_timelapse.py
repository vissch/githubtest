#!/usr/bin/env python3
"""Tests for timelapse.py: python Tools/test_timelapse.py. Stdlib only; no Unity, no ffmpeg (run()/subprocess.run
are patched, and `film` is skipped when PIL or ffmpeg is missing from this machine, exercised separately below)."""
import json
import os
import re
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import timelapse as T


LANDINGS = [
    dict(sha='a' * 40, sha12='a' * 12, date='2026-10-01', subject='first'),
    dict(sha='b' * 40, sha12='b' * 12, date='2026-10-02', subject='second'),
    dict(sha='c' * 40, sha12='c' * 12, date='2026-10-03', subject='third'),
]


def write_sidecar(folder, c, **extra):
    png, j = T.frame_paths(folder, c)
    png.write_bytes(b'\x89PNG')
    data = dict(dict(sha=c['sha'], hash_start='AAAA', seed=1, battlefield_seed=2, args=T.BENCH_ARGS), **extra)
    j.write_text(json.dumps(data))
    return png, j


class MissingTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.folder = Path(self.tmp.name)
        self.env = mock.patch.dict(os.environ, {'TW_TIMELAPSE': str(self.folder)})
        self.env.start()

    def tearDown(self):
        self.env.stop()
        self.tmp.cleanup()

    def test_landings_with_no_frame_are_named_and_exit_1(self):
        write_sidecar(self.folder, LANDINGS[0])
        write_sidecar(self.folder, LANDINGS[1])
        with mock.patch.object(T, 'landings', return_value=LANDINGS):
            with mock.patch('sys.stdout'):
                code = T.main(['missing'])
        self.assertEqual(code, 1)

    def test_nothing_missing_is_exit_0(self):
        for c in LANDINGS:
            write_sidecar(self.folder, c)
        with mock.patch.object(T, 'landings', return_value=LANDINGS):
            with mock.patch('sys.stdout'):
                code = T.main(['missing'])
        self.assertEqual(code, 0)

    def test_limit(self):
        with mock.patch.object(T, 'landings', return_value=LANDINGS):
            with mock.patch('builtins.print') as p:
                code = T.main(['missing', '--limit', '1'])
        self.assertEqual(code, 1)
        named = '\n'.join(str(c) for c in p.call_args_list)
        self.assertIn('first', named)
        self.assertNotIn('third', named)


class ShootTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.folder = Path(self.tmp.name)
        self.env = mock.patch.dict(os.environ, {'TW_TIMELAPSE': str(self.folder)})
        self.env.start()

    def tearDown(self):
        self.env.stop()
        self.tmp.cleanup()

    def test_shoot_writes_a_sidecar_with_the_report_and_args_hold_the_standard_view(self):
        c = LANDINGS[0]
        report = dict(warnings=[], scenario=dict(seed=777, battlefield_seed=42),
                      window=dict(hash_start='DEADBEEF', tick_start=200))
        calls = []

        def fake_run(code, timeout=60):
            calls.append(code)
            if 'CaptureRig.Bench' in code:
                raw = re.search(r'out=([^\s"]+)', code).group(1)
                shot = re.search(r'shot=([^\s"]+)', code).group(1)
                Path(raw).write_text(json.dumps(report))
                Path(shot).write_bytes(b'\x89PNG')
                return 'bench armed, entering play: ...'
            return 1

        with mock.patch.object(T, 'commit_info', return_value=c), \
             mock.patch.object(T, 'editor_held', return_value=False), \
             mock.patch.object(T, 'start_editor', return_value=True), \
             mock.patch.object(T, 'stop_editor'), \
             mock.patch.object(T, 'run', side_effect=fake_run):
            code = T.shoot('irrelevant')

        self.assertEqual(code, 0)
        png, sidecar_json = T.frame_paths(self.folder, c)
        self.assertTrue(sidecar_json.exists())
        data = json.loads(sidecar_json.read_text())
        self.assertEqual(data['hash_start'], 'DEADBEEF')
        self.assertEqual(data['seed'], 777)
        self.assertEqual(data['battlefield_seed'], 42)
        bench_call = next(code for code in calls if 'CaptureRig.Bench' in code)
        self.assertIn('shot_hud=0', bench_call)
        self.assertIn('shot_tick=', bench_call)
        self.assertNotIn('fx=', bench_call)
        self.assertNotIn('fz=', bench_call)
        self.assertNotIn('ground=', bench_call)

    def test_held_project_refuses(self):
        with mock.patch.object(T, 'commit_info', return_value=LANDINGS[0]), \
             mock.patch.object(T, 'editor_held', return_value=True), \
             mock.patch('builtins.print'):
            code = T.shoot('irrelevant')
        self.assertEqual(code, 1)

    def test_shot_never_reached_fails_loudly(self):
        c = LANDINGS[0]
        report = dict(warnings=['shot_tick 200 was never reached inside the window: no still'],
                      scenario=dict(seed=1, battlefield_seed=2), window=dict(hash_start='X', tick_start=200))

        def fake_run(code, timeout=60):
            if 'CaptureRig.Bench' in code:
                raw = re.search(r'out=([^\s"]+)', code).group(1)
                Path(raw).write_text(json.dumps(report))
                return 'bench armed, entering play: ...'
            return 1

        with mock.patch.object(T, 'commit_info', return_value=c), \
             mock.patch.object(T, 'editor_held', return_value=False), \
             mock.patch.object(T, 'start_editor', return_value=True), \
             mock.patch.object(T, 'stop_editor'), \
             mock.patch.object(T, 'run', side_effect=fake_run), \
             mock.patch('builtins.print'):
            code = T.shoot('irrelevant')
        self.assertEqual(code, 1)
        _, sidecar_json = T.frame_paths(self.folder, c)
        self.assertFalse(sidecar_json.exists())


class FilmTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.folder = Path(self.tmp.name)
        self.env = mock.patch.dict(os.environ, {'TW_TIMELAPSE': str(self.folder)})
        self.env.start()

    def tearDown(self):
        self.env.stop()
        self.tmp.cleanup()

    def _png(self, path):
        try:
            from PIL import Image
            Image.new('RGB', (2, 2), (10, 20, 30)).save(path)
        except ImportError:
            path.write_bytes(b'\x89PNG\r\n')

    def test_frames_go_to_ffmpeg_in_commit_date_order_and_other_views_are_left_out(self):
        # the odd view sits on the EARLIEST landing; the newest frame's view (the other three, standard) is the
        # one the film keeps, so the mismatched one is left out rather than becoming the (wrong) reference.
        odd = dict(sha='z' * 40, sha12='z' * 12, date='2026-09-30', subject='zeroth')
        landings4 = [odd] + LANDINGS
        other_view, _ = write_sidecar(self.folder, odd, args=T.BENCH_ARGS + ' ground=winter')
        self._png(other_view)
        for c in LANDINGS:
            png, j = write_sidecar(self.folder, c)
            self._png(png)

        seen = {}

        def fake_run(cmd, **kw):
            seen['cmd'] = cmd
            seen['frames'] = sorted(Path(cmd[cmd.index('-i') + 1].rsplit('%', 1)[0]).glob('*.png'))
            Path(cmd[-1]).write_bytes(b'\x00' * 10)
            return mock.Mock(returncode=0, stderr=b'')

        with mock.patch.object(T, 'landings', return_value=landings4), \
             mock.patch.object(T, 'FFMPEG', 'ffmpeg'), \
             mock.patch('subprocess.run', side_effect=fake_run), \
             mock.patch('builtins.print'):
            code = T.main(['film'])

        self.assertEqual(code, 0)
        self.assertEqual([p.stem for p in seen['frames']], ['00000', '00001', '00002'])

    def test_no_ffmpeg_keeps_frames_and_says_so(self):
        for c in LANDINGS[:2]:
            png, j = write_sidecar(self.folder, c)
            self._png(png)
        with mock.patch.object(T, 'landings', return_value=LANDINGS[:2]), \
             mock.patch.object(T, 'FFMPEG', None), \
             mock.patch('builtins.print') as p:
            code = T.main(['film'])
        self.assertEqual(code, 0)
        self.assertTrue(any('no ffmpeg' in str(c) for c in p.call_args_list))


if __name__ == '__main__':
    unittest.main()
