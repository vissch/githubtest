#!/usr/bin/env python3
"""Tests for sheetcheck.py with no editor: python Tools/assetboard/test_sheetcheck.py.

Every case draws its own sheet on the shared background grey, in a temp folder, and the expected count is the
number of blobs the test itself drew -- not a number sheetcheck.py worked out.
"""
import os
import sys
import tempfile
import unittest

import numpy as np
from PIL import Image, ImageDraw, ImageFont

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import sheetcheck as C

BG = (216, 216, 216)
INK = (70, 70, 70)
W, H = 1600, 900


def blank():
    return Image.new("RGB", (W, H), BG)


def blob(im, cx, cy, w, h, fill=INK):
    """A filled ellipse big enough to pass the 1 % area floor on its own."""
    d = ImageDraw.Draw(im)
    d.ellipse((cx - w // 2, cy - h // 2, cx + w // 2, cy + h // 2), fill=fill)


def typed(im, x, y, text="Trench Warfare 3D"):
    d = ImageDraw.Draw(im)
    try:
        font = ImageFont.truetype("C:/Windows/Fonts/segoeui.ttf", 24)
    except OSError:
        font = ImageFont.load_default()
    d.text((x, y), text, fill=INK, font=font)


def save(im, folder, name="sheet.png"):
    path = os.path.join(folder, name)
    im.save(path)
    return path


class SheetCheckTest(unittest.TestCase):
    def test_four_well_spaced_blobs_is_exactly_four(self):
        im = blank()
        for cx in (250, 650, 1050, 1450):
            blob(im, cx, 450, 200, 160)
        with tempfile.TemporaryDirectory() as tmp:
            path = save(im, tmp)
            self.assertEqual(len(C.count_weapons(path)), 4)
            self.assertEqual(C.main(["sheetcheck.py", tmp]), 0)

    def test_three_blobs_is_not_four(self):
        im = blank()
        for cx in (300, 800, 1300):
            blob(im, cx, 450, 200, 160)
        with tempfile.TemporaryDirectory() as tmp:
            path = save(im, tmp)
            self.assertEqual(len(C.count_weapons(path)), 3)
            self.assertNotEqual(C.main(["sheetcheck.py", tmp]), 0)

    def test_five_blobs_is_not_four(self):
        im = blank()
        for i, cx in enumerate((150, 450, 750, 1050, 1350)):
            blob(im, cx, 450, 180, 140)
        with tempfile.TemporaryDirectory() as tmp:
            path = save(im, tmp)
            self.assertEqual(len(C.count_weapons(path)), 5)
            self.assertNotEqual(C.main(["sheetcheck.py", tmp]), 0)

    def test_four_blobs_plus_a_title_and_labels_is_still_four(self):
        im = blank()
        for cx in (250, 650, 1050, 1450):
            blob(im, cx, 450, 200, 160)
        typed(im, 40, 20, "Trench Warfare 3D / weapon sheet 1 of 3 / toy box / one angle each")
        for cx in (250, 650, 1050, 1450):
            typed(im, cx - 60, 650, "Rifle")
        with tempfile.TemporaryDirectory() as tmp:
            path = save(im, tmp)
            self.assertEqual(len(C.count_weapons(path)), 4)

    def test_a_blob_split_in_two_close_parts_counts_as_one(self):
        im = blank()
        blob(im, 250, 450, 200, 160)
        blob(im, 650, 450, 90, 160)
        blob(im, 650 + 90 + 10, 450, 90, 160)
        blob(im, 1050, 450, 200, 160)
        blob(im, 1450, 450, 200, 160)
        with tempfile.TemporaryDirectory() as tmp:
            path = save(im, tmp)
            self.assertEqual(len(C.count_weapons(path)), 4, "the two 10 px-apart parts are one weapon")

    def test_an_empty_folder_is_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            self.assertNotEqual(C.main(["sheetcheck.py", tmp]), 0)


if __name__ == "__main__":
    unittest.main(verbosity=1)
