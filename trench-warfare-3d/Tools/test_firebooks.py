# Tests for firebooks.py, the flipbook cutter. Run: python Tools/test_firebooks.py
import os, sys, tempfile, unittest
import numpy as np
from PIL import Image

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import firebooks


class FirebooksTest(unittest.TestCase):
    def test_r4_soft_edge_keeps_its_ink(self):
        """[R4] a resize must not premultiply colour by alpha a second time."""
        # one 20x20 cell of solid white with a hard vertical alpha step; LANCZOS up to CELL makes that step a
        # gradient, and the shader reads ink = rgb/alpha, which must stay 1.0 (true white) across it.
        h, w = 4 * 20, 8 * 20
        a = np.zeros((h, w, 4), np.uint8)
        a[0:20, 0:20, :3] = 255
        a[0:20, 0:10, 3] = 255
        src = os.path.join(self.dir, "src.png")
        Image.fromarray(a, "RGBA").save(src)
        firebooks.OUT = self.dir
        firebooks.convert("Test", src, "light", False, (0, 0))
        q = np.array(Image.open(os.path.join(self.dir, "Test.png")).convert("RGBA")).astype(np.float32)
        soft = (q[..., 3] > 30) & (q[..., 3] < 225)          # the gradient the resize invented
        self.assertTrue(soft.any(), "[R4] no soft alpha edge in the output to measure")
        ink = q[..., 0][soft] / np.maximum(q[..., 3][soft], 16.0)
        self.assertAlmostEqual(float(ink.mean()), 1.0, delta=0.03,
                               msg="[R4] soft-edge ink is %.3f, not the drawn 1.0: alpha applied twice" % ink.mean())

    def setUp(self):
        self._out = firebooks.OUT
        self._tmp = tempfile.TemporaryDirectory()
        self.dir = self._tmp.name

    def tearDown(self):
        firebooks.OUT = self._out
        self._tmp.cleanup()


if __name__ == "__main__":
    unittest.main(verbosity=2)
