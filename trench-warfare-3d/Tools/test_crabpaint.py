#!/usr/bin/env python3
"""Tests for crabpaint.py's paint maths, with no Blender and no real atlas: python Tools/test_crabpaint.py.

The fixture is drawn here, so every number is a worked example and not the tool's own output: a 256 px sheet with two
labelled islands, one flat paint (the look we want) and one white with a drawn blue swirl (the look we have), and one
1 px dark line across both (a panel line). What the paint must do: take the swirl down to the flat island's noise,
keep the line, keep the emblem inside the Shield island, and do nothing the second time it runs.
"""
import os
import sys
import unittest
from pathlib import Path

import numpy as np

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import crabpaint

N = 256
FLAT = slice(8, 120)            # the flat island's columns
SWIRL = slice(136, 248)         # the swirled island's columns
ROW = 128                       # the panel line


def fixture():
    """(image float 0..1, labels uint8, names). Island 1 is flat paint, island 2 is white with a blue swirl."""
    x = np.zeros((N, N, 3), np.float32)
    labels = np.zeros((N, N), np.uint8)
    yy, xx = np.mgrid[0:N, 0:N].astype(np.float32)
    # island 1: flat sand paint
    x[8:248, FLAT] = (0.55, 0.50, 0.45)
    labels[8:248, FLAT] = 1
    # island 2: white ground with a liquid-marble swirl that follows the sheet, not the light
    s = 0.5 + 0.5 * np.sin(0.45 * xx + 0.30 * yy + 3.0 * np.sin(0.06 * yy) + 2.0 * np.sin(0.05 * xx))
    marble = (s[:, :, None] * np.array((0.18, 0.26, 0.92), np.float32)
              + (1.0 - s[:, :, None]) * np.array((0.96, 0.96, 0.98), np.float32))
    x[8:248, SWIRL] = marble[8:248, SWIRL]
    labels[8:248, SWIRL] = 2
    # the panel line across both islands
    x[ROW, 8:248] = (0.08, 0.08, 0.09)
    return x, labels, ["Body", "Shield"]


# The swirl is judged on a corner of each island, inside the trim and clear of the emblem: both are drawn in house
# blue on purpose, and a drawn edge is paint, not noise.
FLAT_CORE = (slice(20, 80), slice(24, 104))
SWIRL_CORE = (slice(20, 80), slice(150, 230))


def swirl_of(x, core):
    return crabpaint.chroma_noise(x[core])


def contrast(x):
    """How dark the panel line is against the paint just above and below it, over both islands."""
    lum = x[..., :3].mean(2)
    near = 0.5 * (lum[ROW - 3, 8:248] + lum[ROW + 3, 8:248])
    return float((near - lum[ROW, 8:248]).mean())


class Paint(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.x, cls.labels, cls.names = fixture()
        cls.painted = crabpaint.paint_atlas(cls.x, cls.labels, cls.names).astype(np.float32) / 255.0

    def test_fixture_swirl_is_twice_the_flat_paint(self):
        self.assertGreaterEqual(swirl_of(self.x, SWIRL_CORE), 2.0 * swirl_of(self.x, FLAT_CORE) + 0.01,
                                "the fixture's swirl is not noisy enough to be a test of anything")

    def test_paint_takes_the_swirl_to_flat(self):
        after, flat = swirl_of(self.painted, SWIRL_CORE), swirl_of(self.painted, FLAT_CORE)
        self.assertLessEqual(after, flat + 0.002,
                             "the swirl survived the paint: %.4f against the flat island's %.4f" % (after, flat))

    def test_panel_line_keeps_its_contrast(self):
        before, after = contrast(self.x), contrast(self.painted)
        self.assertGreaterEqual(after, 0.9 * before,
                                "the flatten ate the panel line: contrast %.3f -> %.3f" % (before, after))

    def test_emblem_stays_inside_the_shield_island(self):
        img = np.zeros((N, N, 3), np.uint8)
        m = self.labels == 2
        on = crabpaint.draw_spiral(img, m)
        self.assertTrue(on.any(), "no emblem was drawn")
        self.assertTrue(bool((~m[on]).sum() == 0), "emblem pixels fell outside the Shield island")

    def test_painting_twice_changes_almost_nothing(self):
        again = crabpaint.paint_atlas(self.painted, self.labels, self.names)
        moved = (np.abs(again.astype(np.int16) - (self.painted * 255.0 + 0.5).astype(np.int16)) > 2).any(2)
        share = float(moved[self.labels > 0].mean())
        self.assertLess(share, 0.01, "paint is not idempotent: %.1f %% of the island pixels moved" % (100 * share))


if __name__ == "__main__":
    unittest.main(verbosity=2)
