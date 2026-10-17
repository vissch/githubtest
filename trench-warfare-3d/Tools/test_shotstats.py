#!/usr/bin/env python3
"""Tests for shotstats.py with no editor: python Tools/test_shotstats.py. PIL only.

The two measures a capture's proof leans on, each against an image whose answer is known by hand: the width of
an edge along a line (`fall_px`) and the spread of luma in a box (`spread`), plus the exit codes of --min-fall
and --min-spread, so one command can be a whole check.
"""
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import shotstats as S
from PIL import Image

TOOL = Path(__file__).resolve().parent / 'shotstats.py'


def grey(img, box, level):
    x, y, w, h = box
    img.paste((level, level, level), (x, y, x + w, y + h))


def ramp_image():
    """200 px wide: 60 px at luma 20, a ramp rising to 200 over exactly 80 px, then 60 px at 200."""
    img = Image.new('RGB', (200, 40), (20, 20, 20))
    for i in range(80):
        v = round(20 + 180 * i / 79)
        img.paste((v, v, v), (60 + i, 0, 61 + i, 40))
    grey(img, (140, 0, 60, 40), 200)
    return img


class FallTest(unittest.TestCase):
    def test_a_ramp_over_80_px_falls_over_80_px(self):
        with tempfile.TemporaryDirectory() as d:
            p = os.path.join(d, 'ramp.png')
            ramp_image().save(p)
            img = Image.open(p).convert('RGB')
            got = S.fall(S.profile(img, 0, 20, 199, 20))
            self.assertAlmostEqual(got['fall_px'], 80, delta=2, msg=f'fall over the ramp: {got}')
            self.assertAlmostEqual(got['fall_lo'], 20, delta=2)
            self.assertAlmostEqual(got['fall_hi'], 200, delta=2)

    def test_a_hard_step_falls_over_a_few_px(self):
        img = Image.new('RGB', (200, 40), (20, 20, 20))
        grey(img, (100, 0, 100, 40), 200)
        got = S.fall(S.profile(img, 0, 20, 199, 20))
        self.assertLessEqual(got['fall_px'], 5, 'a step edge is the fault the measure must catch')

    def test_a_flat_line_has_no_edge(self):
        img = Image.new('RGB', (200, 40), (70, 70, 70))
        self.assertEqual(S.fall(S.profile(img, 0, 20, 199, 20))['fall_px'], 0)

    def test_the_line_is_sampled_one_step_per_pixel_end_included(self):
        img = Image.new('RGB', (200, 40), (20, 20, 20))
        self.assertEqual(len(S.profile(img, 0, 20, 199, 20)), 200)


class SpreadTest(unittest.TestCase):
    def test_flat_grey_has_no_spread(self):
        img = Image.new('RGB', (200, 200), (70, 70, 70)).convert('L')
        self.assertEqual(S.spread(img, (0, 0, 200, 200)), 0.0)

    def test_half_40_half_60_spreads_by_10(self):
        img = Image.new('RGB', (200, 200), (40, 40, 40))
        grey(img, (100, 0, 100, 200), 60)
        self.assertEqual(S.spread(img.convert('L'), (0, 0, 200, 200)), 10.0)

    def test_the_box_is_x_y_w_h_not_a_rectangle(self):
        img = Image.new('RGB', (200, 200), (40, 40, 40))
        grey(img, (100, 100, 100, 100), 60)
        self.assertEqual(S.spread(img.convert('L'), (100, 100, 100, 100)), 0.0, 'the lit quarter alone is flat')


def run(path, *args):
    r = subprocess.run([sys.executable, str(TOOL), path, *args], capture_output=True, text=True)
    return r.returncode, r.stdout + r.stderr


class CommandTest(unittest.TestCase):
    """The checks an agent actually types, with their exit codes."""

    @classmethod
    def setUpClass(cls):
        cls.dir = tempfile.TemporaryDirectory()
        cls.ramp = os.path.join(cls.dir.name, 'ramp.png')
        img = ramp_image()
        # A flat lit box on the left and a mottled unlit box on the right, so the two --crop boxes differ.
        for x in range(0, 200, 2):
            img.paste((30, 30, 30), (x, 0, x + 1, 20))
        img.save(cls.ramp)
        cls.step = os.path.join(cls.dir.name, 'step.png')
        step = Image.new('RGB', (200, 200), (20, 20, 20))
        grey(step, (100, 0, 100, 200), 200)
        step.save(cls.step)

    @classmethod
    def tearDownClass(cls):
        cls.dir.cleanup()

    def test_the_old_output_still_prints_and_exits_0(self):
        code, out = run(self.ramp)
        self.assertEqual(code, 0, out)
        self.assertIn('luma mean', out)
        self.assertIn('grid', out)

    def test_min_fall_passes_the_ramp_and_fails_the_step(self):
        code, out = run(self.ramp, '--line', '0,30,199,30', '--min-fall', '40')
        self.assertEqual(code, 0, out)
        self.assertIn('fall', out)
        code, out = run(self.step, '--line', '0,100,199,100', '--min-fall', '40')
        self.assertEqual(code, 1, out)
        self.assertIn('FAIL', out)

    def test_min_spread_compares_the_first_box_with_the_second(self):
        # Box A is a flat strip of the ramp image, box B the striped one above it: 0 spread against ~5 fails.
        flat = ('--crop', '0,20,40,20')
        mottled = ('--crop', '0,0,40,20')
        code, out = run(self.ramp, *flat, *mottled, '--min-spread', '0.6')
        self.assertEqual(code, 1, out)
        self.assertIn('FAIL', out)
        code, out = run(self.ramp, *mottled, *flat, '--min-spread', '0.6')
        self.assertEqual(code, 0, 'the other way round the first box is the mottled one: ' + out)

    def test_two_boxes_print_the_ratio(self):
        code, out = run(self.ramp, '--crop', '0,0,40,20', '--crop', '0,20,40,20')
        self.assertEqual(code, 0, out)
        self.assertIn('spread_ratio', out)

    def test_json_carries_the_measures(self):
        import json
        code, out = run(self.ramp, '--line', '0,30,199,30', '--crop', '0,0,40,20', '--json')
        self.assertEqual(code, 0, out)
        got = json.loads(out)
        self.assertIn('fall_px', got['a'])
        self.assertEqual(got['a']['spreads'][0], S.spread(Image.open(self.ramp).convert('L'), (0, 0, 40, 20)))

    def test_a_measure_without_its_input_is_a_usage_error(self):
        self.assertEqual(run(self.ramp, '--min-fall', '40')[0], 2)
        self.assertEqual(run(self.ramp, '--crop', '0,0,40,40', '--min-spread', '0.6')[0], 2)
        self.assertEqual(run(self.ramp, '--line', '0,0,10')[0], 2)


if __name__ == '__main__':
    unittest.main(verbosity=2)
