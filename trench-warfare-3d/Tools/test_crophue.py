#!/usr/bin/env python3
"""crophue.py with no editor: hue and value of a crop's brightest quartile.

A man standing in a lamp's pool used to take the pool's hue. These checks use pictures whose answer is
known by hand, including the --sep gate and a blown still.
"""
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import crophue as H
from PIL import Image

TOOL = Path(__file__).resolve().parent / 'crophue.py'


def solid(w, h, rgb):
    return Image.new('RGB', (w, h), rgb)


class CropHueTest(unittest.TestCase):
    def test_the_brightest_quarter_is_the_lit_side_not_the_shadow(self):
        img = solid(4, 4, (8, 12, 40))
        # four lit orange pixels; the other twelve stay cold and dark
        for x, y in ((0, 0), (1, 0), (2, 0), (3, 0)):
            img.putpixel((x, y), (220, 120, 40))
        got = H.brightest_quartile(img)
        self.assertGreater(got['hue'], 15)
        self.assertLess(got['hue'], 45)
        self.assertGreater(got['value'], 0.7)

    def test_hue_gap_takes_the_short_way_round_the_circle(self):
        self.assertEqual(H.hue_gap(350, 10), 20)
        self.assertEqual(H.hue_gap(30, 50), 20)
        self.assertEqual(H.hue_gap(42, 42), 0)

    def test_sep_passes_a_man_whose_hue_clears_the_mud(self):
        # the measured after: man hue 30 value 0.90, mud hue 50 value 1.00, gap 20 deg
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'shot.png')
            img = solid(8, 4, (255, 200, 40))
            for x in range(4):
                for y in range(4):
                    img.putpixel((x, y), (210, 130, 50))
            img.save(path)
            code = subprocess.run(
                [sys.executable, str(TOOL), path, '--crop', '0,0,4,4', '--crop', '4,0,8,4', '--sep', '12,0.2'],
                capture_output=True, text=True).returncode
            self.assertEqual(code, 0, 'a 20 deg hue gap clears --sep 12,0.2')

    def test_sep_fails_when_man_and_mud_are_the_same_hue_and_value(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'shot.png')
            solid(8, 4, (230, 160, 40)).save(path)
            proc = subprocess.run(
                [sys.executable, str(TOOL), path, '--crop', '0,0,4,4', '--crop', '4,0,8,4', '--sep', '12,0.2'],
                capture_output=True, text=True)
            self.assertEqual(proc.returncode, 1)
            self.assertIn('hue gap', proc.stdout)

    def test_a_blown_frame_fails_sep_even_when_the_crops_differ(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'shot.png')
            img = solid(8, 4, (40, 80, 200))
            for x in range(4):
                for y in range(4):
                    img.putpixel((x, y), (220, 80, 20))
            img.save(path)
            with open(os.path.splitext(path)[0] + '.json', 'w', encoding='utf-8') as f:
                json.dump({'blown_frac': 0.05}, f)
            code = subprocess.run(
                [sys.executable, str(TOOL), path, '--crop', '0,0,4,4', '--crop', '4,0,8,4', '--sep', '12,0.2'],
                capture_output=True, text=True).returncode
            self.assertEqual(code, 1, 'blown_frac at 0.02 or more invalidates the still')


if __name__ == '__main__':
    unittest.main(verbosity=1)
