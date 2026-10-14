#!/usr/bin/env python3
"""Tests that a film encode never leaves a partial file at its published path: the finding N12.4 of the second
code review (N12.11 is a separate leg). Run from trench-warfare-3d/:
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


if __name__ == '__main__':
    unittest.main()
