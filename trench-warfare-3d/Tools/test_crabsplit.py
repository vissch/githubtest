# Tests for crabsplit.py's leg grouping. Run: python Tools/test_crabsplit.py
# crabsplit.py imports bpy/bmesh/mathutils behind a try/except, so group_legs can be imported outside Blender.
import os, sys, unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from crabsplit import group_legs


def leg(name, x, y, heights):
    """One leg's pieces: a stack at (x, y), each a little lower than the last."""
    quad = ("L" if x >= 0 else "R") + ("F" if y < 0 else "B")
    return [("%s%d" % (name, i), x + 0.004 * i, y - 0.004 * i, quad, z) for i, z in enumerate(heights)]


class CrabsplitTest(unittest.TestCase):
    def test_r1_six_legs_stay_six_legs(self):
        # [R1] the Redoubt: six two-piece legs, three a side, so two legs share a quadrant. The old quadrant
        # binning returned four chains (four Thigh, four Shin, two Foot in the committed crabs.json).
        pieces = []
        for s, x in (("L", 0.9), ("R", -0.9)):
            for i, y in enumerate((-0.7, 0.0, 0.7)):
                pieces += leg("%s%d" % (s, i), x, y, [0.4, 0.0])
        legs, leftover = group_legs(pieces)
        self.assertEqual(6, len(legs), "[R1] six legs were not grouped as six: %s" % legs)
        self.assertEqual([], leftover, "[R1] a leg piece fell into the Body")
        self.assertEqual(6, len(set(g["label"] for g in legs)), "[R1] two legs got the same name: %s" % legs)
        for g in legs:
            self.assertEqual(2, len(g["keys"]), "[R1] a leg has the wrong number of pieces: %s" % g)
            self.assertTrue(g["keys"][0].endswith("0"), "[R1] the thigh is not the top piece: %s" % g)

    def test_r1_four_legs_keep_their_quadrant_names(self):
        # [R1] regression safety for Kettle: four three-piece legs, one a quadrant, named as before.
        pieces = []
        for x in (0.8, -0.8):
            for y in (-0.6, 0.6):
                pieces += leg("%s%s" % (x, y), x, y, [0.5, 0.2, 0.0])
        legs, leftover = group_legs(pieces, expected=4, machine="Kettle")
        self.assertEqual(["LB", "LF", "RB", "RF"], sorted(g["label"] for g in legs),
                         "[R1] the four-leg names changed")
        self.assertEqual([], leftover, "[R1] a Kettle leg piece fell into the Body")

    def test_r1_wrong_leg_count_raises(self):
        # [R1] a bad split must fail loudly in the Blender run, not ship a silently wrong FBX.
        pieces = []
        for i, y in enumerate((-0.7, 0.0, 0.7)):
            pieces += leg("L%d" % i, 0.9, y, [0.4, 0.0])
            pieces += leg("R%d" % i, -0.9, y, [0.4, 0.0])
        with self.assertRaises(ValueError) as e:
            group_legs(pieces, expected=5, machine="Redoubt")
        self.assertIn("Redoubt", str(e.exception), "[R1] the error does not name the machine")
        self.assertIn("6", str(e.exception), "[R1] the error does not say how many groups were found")


if __name__ == "__main__":
    unittest.main(verbosity=2)
