#!/usr/bin/env python3
"""Tests of what a read-only command of the asset board may write (Tools/assetboard/ops.py), the finding N10.2 of the
second code review. Run from trench-warfare-3d/:
python Tools/assetboard/test_readonly.py

`ops.py --queue` is documented as "read once and print". It ran once(), which put new step briefs in the owner's live
decisions folder and rewrote the live site, whatever --out said. A command that only shows something must write
nothing he can see.
"""
import json
import os
import struct
import tempfile
import unittest
import zlib
from pathlib import Path

HERE = Path(__file__).resolve().parent

# never the live folders: the board's own places go under one temp folder, before anything of it is imported
TEMP = Path(tempfile.mkdtemp(prefix='tw-readonly-'))
for _k in ('TW_NOTES', 'TW_BRIEFS', 'TW_ASSETBOARD_OUT'):
    _d = TEMP / _k.lower()
    _d.mkdir(exist_ok=True)
    os.environ[_k] = str(_d)

import sys
sys.path.insert(0, str(HERE))
import briefs      # noqa: E402  (after TW_BRIEFS: it must never see the live folder)
import ops         # noqa: E402
import src_ops     # noqa: E402
import src_queue   # noqa: E402


def png(path: Path, w=8, h=8):
    """A real picture of one grey, written without any library (as test_assetboard.py does)."""
    def chunk(kind, data):
        return struct.pack('>I', len(data)) + kind + data + struct.pack('>I', zlib.crc32(kind + data) & 0xffffffff)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', w, h, 8, 0, 0, 0, 0))
                     + chunk(b'IDAT', zlib.compress((b'\x00' + b'\x80' * w) * h)) + chunk(b'IEND', b''))
    return path


def a_board(root: Path):
    """A pipeline board with one item whose one stage passed with a capture: one step to put to the owner."""
    (root / 'items').mkdir(parents=True)
    (root / 'results').mkdir()
    (root / 'items' / 'shed.json').write_text(json.dumps(dict(
        id='shed', title='A shed', lane='lane/show/shed', stages=[dict(id='look', role='vfx', bands=['intact'])])), encoding='utf-8')
    png(root / 'evidence' / 'shed' / 'look' / 'intact.png')
    (root / 'results' / 'shed--look--abcd1234--1.json').write_text(json.dumps(dict(
        item='shed', stage='look', attempt=1, verdict='PASS', evidence={'intact': 'evidence/shed/look/intact.png'},
        job='shed--look--abcd1234', station='desktop', finished_at='2026-10-04T20:00:00Z', note='checked by relay')), encoding='utf-8')
    return root


class StepBriefsOnlyWhenAsked(unittest.TestCase):
    """[N10.2] step_briefs writes a brief in front of the owner only when the caller asks for it."""

    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix='tw-readonly-steps-', dir=TEMP))
        self.board = a_board(self.tmp / 'board')
        self.where = self.tmp / 'decisions'
        self.where.mkdir()
        self.keep = (ops.board_root, ops.landed_items, briefs.folder)
        ops.board_root = lambda: str(self.board)
        ops.landed_items = lambda board, repo=None: set()
        briefs.folder = lambda: self.where

    def tearDown(self):
        ops.board_root, ops.landed_items, briefs.folder = self.keep

    def test_a_read_puts_no_step_brief_to_the_owner(self):
        try:
            owed = ops.step_briefs(dict(lanes=[]), write=False)
        except TypeError as e:
            self.fail('[N10.2] ops.step_briefs cannot be told not to write: %s' % e)
        self.assertEqual(owed, [], '[N10.2] the steps that owe a capture did not read: %s' % (owed,))
        left = sorted(p.name for p in self.where.iterdir())
        self.assertEqual(left, [], '[N10.2] a read that only reads put a step brief in the owner\'s decisions folder: %s' % left)

    def test_the_step_briefs_are_written_when_they_are_asked_for(self):
        try:
            ops.step_briefs(dict(lanes=[]), write=True)
        except TypeError as e:
            self.fail('[N10.2] ops.step_briefs cannot be asked to write: %s' % e)
        got = [b['id'] for b in briefs.read_all(self.where)]
        self.assertEqual(got, ['step-shed-look-abcd1234'], '[N10.2] the step briefs were not written when they were asked for: %s' % got)


class TheQueueWritesNothing(unittest.TestCase):
    """[N10.2] the queue of `--queue` is collected and printed, and nothing of it is written."""

    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix='tw-readonly-queue-', dir=TEMP))
        self.out = self.tmp / 'site'
        self.out.mkdir()
        self.keep = (src_ops.collect, src_queue.collect, ops.board_root, briefs.folder)
        src_ops.collect = lambda repo, site, share=False: dict(now='2026-10-12 09:00', lanes=[], roster=[],
                                                               counts=dict(sessions=0, machines=0, ready=0, idle=0))
        src_queue.collect = self.a_queue
        ops.board_root = lambda: None
        briefs.folder = lambda: self.tmp / 'decisions'

    @staticmethod
    def a_queue(repo, data, **kw):
        q = {g: [] for g in src_queue.GROUPS}
        q.update(count=0, agents=0, ci='green')
        return q

    def tearDown(self):
        src_ops.collect, src_queue.collect, ops.board_root, briefs.folder = self.keep

    def test_the_queue_read_writes_no_file(self):
        read = getattr(ops, 'the_queue', None)
        self.assertTrue(callable(read), '[N10.2] ops.py has no read-only way to collect the queue (the_queue)')
        q = read(self.out)
        self.assertEqual(q['count'], 0, '[N10.2] the queue did not read: %s' % (q,))
        self.assertEqual(src_queue.lines(q)[0], '0 wait on the owner, 0 on an agent (checks on integration: green)',
                         '[N10.2] the queue cannot be printed: %s' % (src_queue.lines(q)[:1],))
        left = sorted(p.relative_to(self.out).as_posix() for p in self.out.rglob('*') if p.is_file())
        self.assertEqual(left, [], '[N10.2] reading the queue wrote into the site: %s' % left)


class OutNeedsTheOtherFolders(unittest.TestCase):
    """[N10.2] --out alone does not redirect the briefs and the notes: a run that gives it must say where those go."""

    def test_the_refusal_names_both_variables(self):
        check = getattr(ops, 'live_check', None)
        self.assertTrue(callable(check), '[N10.2] ops.py does not check that --out has TW_BRIEFS and TW_NOTES with it (live_check)')
        keep = {k: os.environ.get(k) for k in ('TW_BRIEFS', 'TW_NOTES')}
        try:
            for gone in ('TW_BRIEFS', 'TW_NOTES'):
                for k in keep:
                    os.environ[k] = str(TEMP / k.lower())
                del os.environ[gone]
                said = check(str(TEMP / 'elsewhere'))
                self.assertTrue(said and 'TW_BRIEFS' in said and 'TW_NOTES' in said,
                                '[N10.2] --out with no %s is not refused in one line naming both variables: %r' % (gone, said))
                self.assertEqual(len(said.splitlines()), 1, '[N10.2] the refusal is more than one line: %r' % said)
            for k in keep:
                os.environ[k] = str(TEMP / k.lower())
            self.assertEqual(check(str(TEMP / 'elsewhere')), '',
                             '[N10.2] --out with both variables set was refused: %r' % check(str(TEMP / 'elsewhere')))
            self.assertEqual(check(''), '', '[N10.2] a run with no --out was refused: %r' % check(''))
        finally:
            for k, v in keep.items():
                if v is None:
                    os.environ.pop(k, None)
                else:
                    os.environ[k] = v


class OutWithJsonOnlyIsRead(unittest.TestCase):
    """[found-build-py-let-json-only-out-run-without-tw-br-20261016-0] --json-only (and --check) cannot reach
    ops.page/ops.once, so --out with them must not be refused for want of TW_BRIEFS and TW_NOTES."""

    def test_a_json_only_check_with_out_runs_with_the_live_folders_unset(self):
        import build
        keep = {k: os.environ.get(k) for k in ('TW_BRIEFS', 'TW_NOTES')}
        for k in keep:
            os.environ.pop(k, None)
        try:
            tmp = Path(tempfile.mkdtemp(dir=TEMP))
            rc = build.main(['--json-only', '--check', '--out', str(tmp)])
            self.assertEqual(rc, 0,
                              '[found-build-py-let-json-only-out-run-without-tw-br-20261016-0] '
                              '--json-only --check --out exited %r with TW_BRIEFS/TW_NOTES unset' % rc)
            tmp2 = Path(tempfile.mkdtemp(dir=TEMP))
            rc2 = build.main(['--out', str(tmp2)])
            self.assertEqual(rc2, 2,
                              '[found-build-py-let-json-only-out-run-without-tw-br-20261016-0] '
                              'a plain --out run should still be refused without TW_BRIEFS/TW_NOTES, got %r' % rc2)
        finally:
            for k, v in keep.items():
                if v is None:
                    os.environ.pop(k, None)
                else:
                    os.environ[k] = v


if __name__ == '__main__':
    unittest.main()
