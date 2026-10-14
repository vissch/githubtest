#!/usr/bin/env python3
"""Tests that a film encode never leaves a partial file at its published path (N12.4), and that a Blender time
limit ends the other workers and is reported as a failure rather than killing the whole build (N12.11): findings
of the second code review. Run from trench-warfare-3d/:
python Tools/assetboard/test_filmsafe.py
"""
import io
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest import mock

HERE = Path(__file__).resolve().parent

TEMP = Path(tempfile.mkdtemp(prefix='tw-filmsafe-'))
for _k in ('TW_NOTES', 'TW_BRIEFS', 'TW_ASSETBOARD_OUT'):
    _d = TEMP / _k.lower()
    _d.mkdir(exist_ok=True)
    os.environ[_k] = str(_d)

sys.path.insert(0, str(HERE))
import films  # noqa: E402
import thumbs  # noqa: E402


def frame_folder(n):
    from PIL import Image
    folder = Path(tempfile.mkdtemp(dir=TEMP, prefix='frames-'))
    for f in range(n):
        Image.new('RGB', (films.SIZE, films.SIZE), (f * 10, 0, 0)).save(folder / f'{f:05d}.png')
    return folder


class HalfFfmpeg:
    """Writes half a file to the target path it is given and exits with a non-zero code, as a dying ffmpeg would."""

    def __init__(self, args, **kw):
        self.target = Path(args[-1])
        self.target.parent.mkdir(parents=True, exist_ok=True)
        self.target.write_bytes(b'\x00' * 400)
        self.stdin = io.BytesIO()
        self.stderr = io.BytesIO(b'frame 3 missing')
        self._code = 1

    def wait(self):
        return self._code

    def poll(self):
        return self._code


class GoodFfmpeg(HalfFfmpeg):
    def __init__(self, args, **kw):
        super().__init__(args, **kw)
        self._code = 0


class EncodeFfmpegDies(unittest.TestCase):
    """[N12.4] ffmpeg dies mid-encode and the index must keep no trace of a partial film."""

    def test_a_dying_ffmpeg_leaves_no_file_at_the_target(self):
        stage = Path(tempfile.mkdtemp(dir=TEMP, prefix='stage-'))
        frames = frame_folder(2)
        result = dict(frames=2, segments=[])
        mp4, poster = stage / 'film' / 'x.mp4', stage / 'film' / 'x.jpg'
        with mock.patch.object(films, 'subprocess', SimpleNamespace(Popen=HalfFfmpeg, PIPE=subprocess.PIPE, TimeoutExpired=subprocess.TimeoutExpired)):
            with self.assertRaises(RuntimeError, msg='[N12.4] a dying ffmpeg must still raise, so the job is marked failed'):
                films.encode(frames, result, mp4, poster)
        self.assertFalse(mp4.exists(), '[N12.4] a half-written mp4 was left at the published path')
        self.assertFalse(poster.exists(), '[N12.4] a poster was left at the published path from a dead encode')
        left = list((stage / 'film').iterdir())
        self.assertEqual([], left, f'[N12.4] a .part file was left behind beside the target: {left}')


class EncodeFfmpegSucceeds(unittest.TestCase):
    """A clean ffmpeg exit: the target must hold the real bytes, with no .part left beside it.
    No id tag: the old code (writing straight to the target) is already green here, so tagging it would fail
    the unit's old-code run that proves N12.4."""

    def test_a_clean_encode_leaves_the_target_and_no_part_file(self):
        stage = Path(tempfile.mkdtemp(dir=TEMP, prefix='stage-'))
        frames = frame_folder(2)
        result = dict(frames=2, segments=[])
        mp4, poster = stage / 'film' / 'x.mp4', stage / 'film' / 'x.jpg'
        with mock.patch.object(films, 'subprocess', SimpleNamespace(Popen=GoodFfmpeg, PIPE=subprocess.PIPE, TimeoutExpired=subprocess.TimeoutExpired)):
            films.encode(frames, result, mp4, poster)
        self.assertEqual(400, mp4.stat().st_size, 'the encoded mp4 must hold the bytes ffmpeg wrote')
        self.assertTrue(poster.exists(), 'the poster must be written on a clean encode')
        left = [p for p in (stage / 'film').iterdir() if p.suffix == '.part']
        self.assertEqual([], left, f'a .part file was left behind after a clean encode: {left}')


class StuckBlender:
    """A Blender worker that never answers within its time limit, as a hung headless render would."""
    killed = []

    def __init__(self, *a, **kw):
        pass

    def wait(self, timeout=None):
        raise subprocess.TimeoutExpired('blender', 1)

    def poll(self):
        return None

    def kill(self):
        StuckBlender.killed.append(self)


class FilmTimeout(unittest.TestCase):
    """[N12.11] a Blender time limit in films.attach must not raise out of the build: it ends the other renderers
    and reports every job as failed."""

    def test_a_blender_time_limit_fails_the_jobs(self):
        src = Path(tempfile.mkdtemp(dir=TEMP, prefix='src-')) / 'model.fbx'
        src.write_bytes(b'\x00')
        jobs = [dict(id=f'Fake{k}', kind='turn', sources=[str(src)], models=[(dict(), dict())], title='Turntable', order=0) for k in range(4)]
        P = TEMP / 'proj' / 'Assets' / '_Project'
        P.mkdir(parents=True, exist_ok=True)
        stage = Path(tempfile.mkdtemp(dir=TEMP, prefix='stage-'))
        out = Path(tempfile.mkdtemp(dir=TEMP, prefix='out-'))
        local = Path(tempfile.mkdtemp(dir=TEMP, prefix='local-'))
        meta = dict(built='x', warnings=[], sources={})
        StuckBlender.killed = []
        with mock.patch.object(films, 'jobs_for', lambda *a, **k: jobs), \
             mock.patch.object(thumbs, 'BLENDER', Path(__file__)), \
             mock.patch.object(films, 'FFMPEG', 'ffmpeg'), \
             mock.patch.object(films, 'subprocess', SimpleNamespace(Popen=StuckBlender, DEVNULL=subprocess.DEVNULL, TimeoutExpired=subprocess.TimeoutExpired)):
            try:
                films.attach(P=P, assets={}, code={}, stage=stage, out=out, local=local, meta=meta)
            except subprocess.TimeoutExpired as e:
                self.fail(f'[N12.11] a Blender time limit ended the whole build: {e}')
        self.assertEqual(4, len(StuckBlender.killed), '[N12.11] not every stuck renderer was ended')
        failed = meta['sources']['board films']['failed']
        self.assertEqual(4, len(failed), '[N12.11] every job past the time limit must be reported as failed')
        for f in failed:
            self.assertIn('[N12.11]', f, '[N12.11] the failed job did not say why in its message')
        self.assertTrue(any('[N12.11]' in w for w in meta['warnings']), '[N12.11] no warning named the time limit')


class StuckBlenderRun:
    """A stand-in for subprocess.run that raises TimeoutExpired, as thumbs.attach's Blender call would at its limit."""

    @staticmethod
    def run(*a, **kw):
        raise subprocess.TimeoutExpired('blender', 1)


class ThumbTimeout(unittest.TestCase):
    """[N12.11] a Blender time limit in thumbs.attach must not raise out of the build: the job is reported failed."""

    def test_a_blender_time_limit_fails_the_job(self):
        src = Path(tempfile.mkdtemp(dir=TEMP, prefix='src-')) / 'model.fbx'
        src.write_bytes(b'\x00')
        job = dict(files=[dict(fbx=str(src), at=None)], texture=None, fix=False, hide=[], expect=None, view=None,
                   id='FakeThumb', asset=dict(category='vehicle', measurements={}), model=dict())
        P = TEMP / 'proj2' / 'Assets' / '_Project'
        P.mkdir(parents=True, exist_ok=True)
        stage = Path(tempfile.mkdtemp(dir=TEMP, prefix='stage-'))
        out = Path(tempfile.mkdtemp(dir=TEMP, prefix='out-'))
        local = Path(tempfile.mkdtemp(dir=TEMP, prefix='local-'))
        meta = dict(built='x', warnings=[], sources={})
        with mock.patch.object(thumbs, 'jobs_for', lambda *a, **k: [job]), \
             mock.patch.object(thumbs, 'BLENDER', Path(__file__)), \
             mock.patch.object(thumbs, 'subprocess', SimpleNamespace(run=StuckBlenderRun.run, TimeoutExpired=subprocess.TimeoutExpired)):
            try:
                thumbs.attach(P=P, assets={}, stage=stage, out=out, local=local, meta=meta)
            except subprocess.TimeoutExpired as e:
                self.fail(f'[N12.11] a Blender time limit ended the whole thumbnail build: {e}')
        failed = meta['sources']['blender']['failed']
        self.assertEqual(1, len(failed), '[N12.11] the job past the time limit must be reported as failed')
        self.assertTrue(any('[N12.11]' in w for w in meta['warnings']), '[N12.11] no warning named the time limit')


if __name__ == '__main__':
    unittest.main()
