#!/usr/bin/env python3
"""crophue.py and the lamp-pool figure cut, with no editor.

A man standing in a lamp's pool used to take the pool's hue. crophue.py measures two crops. TWPoolsOnFigure's
figureCut (look.poolFigure, default 1) keeps his shaded side on the moon and stops two lamps stacking.
Props and machines never pass that argument.
"""
import json
import os
import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import crophue as H
from PIL import Image

ROOT = Path(__file__).resolve().parents[1]
HLSL = ROOT / 'Assets' / '_Project' / 'Shaders' / 'TWLightPools.hlsl'
VAT = ROOT / 'Assets' / '_Project' / 'Shaders' / 'VAT_URP.shader'
TOON = ROOT / 'Assets' / '_Project' / 'Shaders' / 'Toon_URP.shader'
TANK = ROOT / 'Assets' / '_Project' / 'Shaders' / 'Tank_URP.shader'
POOLS = ROOT / 'Assets' / '_Project' / 'Presentation' / 'Terrain' / 'NightLights.Pools.cs'
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


class FigureCutTest(unittest.TestCase):
    def test_a_shaded_side_takes_less_of_the_pool_when_the_cut_is_on(self):
        text = HLSL.read_text(encoding='utf-8')
        self.assertIn('half figureCut = 0.0', text)
        self.assertIn('facingLo = lerp(0.6, 0.85, figureCut)', text)
        self.assertIn('facingHi = lerp(0.4, 0.15, figureCut)', text)
        self.assertIn('toward * facingLo + facingHi', text)
        # toward 0 is the side facing away from the lamp, so its weight is facingHi.
        hi = re.search(r'facingHi = lerp\(([0-9.]+), ([0-9.]+), figureCut\)', text)
        self.assertIsNotNone(hi, 'facingHi lerp')
        off, on = float(hi.group(1)), float(hi.group(2))
        self.assertLess(on, off, 'the shaded side must take less of the pool when the cut is on')

    def test_two_lamps_cannot_stack_and_the_rim_fades_on_a_blazing_floor(self):
        text = HLSL.read_text(encoding='utf-8')
        self.assertIn('figureShoulder = lerp(0.0, 1.2, figureCut)', text)
        self.assertIn('sum = sum / (1.0 + m * figureShoulder)', text)
        self.assertIn('rimOut *= saturate(1.0 - max(sum.r, max(sum.g, sum.b)) * 0.9 * figureCut)', text)
        self.assertIn('return TWPoolRoll(sum);', text)

    def test_only_the_men_pass_the_cut_and_it_defaults_on(self):
        vat = VAT.read_text(encoding='utf-8')
        self.assertIn('TWPoolsOnFigure(i.positionWS, n, toEye, fireRim, 0.30, _TWPoolFigure)', vat)
        self.assertIn('lerp(0.8, 0.8 * 0.5, _TWPoolFigure)', vat)
        self.assertIn('lerp(0.75, 0.75 * 0.4, _TWPoolFigure)', vat)
        for path in (TOON, TANK):
            body = path.read_text(encoding='utf-8')
            self.assertNotIn('_TWPoolFigure', body, path.name)
        pools = POOLS.read_text(encoding='utf-8')
        self.assertIn('DefaultPoolFigure = 1f', pools)
        self.assertIn('look.poolFigure', pools)
        self.assertIn('_TWPoolFigure', pools)


if __name__ == '__main__':
    unittest.main(verbosity=1)
