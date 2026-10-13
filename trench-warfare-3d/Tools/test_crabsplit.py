# Tests for crabsplit.py's leg grouping. Run: python Tools/test_crabsplit.py
# crabsplit.py imports bpy/bmesh/mathutils behind a try/except, so group_legs can be imported outside Blender.
import os, sys, unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from crabsplit import group_legs, pick_fittings, pair_sheets, merge_crabs, CRAB_ORDER


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

    def test_r8_a_third_jaw_or_barrel_still_reaches_the_model(self):
        # [R8] three barrel-shaped and three jaw-shaped pieces. Every piece must come back exactly once: the
        # old code marked all six used and then placed only two of each, so the third of each vanished.
        pieces = [("barrel%d" % i, 0.5 - 0.3 * i, 0.4, 0.7, 0.4, 100) for i in range(3)]
        pieces += [("jaw%d" % i, 0.5 - 0.3 * i, -0.5, 0.2, 0.1, 50) for i in range(3)]
        picks, spare = pick_fittings(pieces, 0.0, 0.0)
        got = list(spare)
        for v in picks.values(): got += v if isinstance(v, list) else [v]
        self.assertEqual(sorted(p[0] for p in pieces), sorted(got),
                         "[R8] a fitting was dropped or placed twice: picks %s spare %s" % (picks, spare))

    def test_r8_two_of_each_sort_as_before(self):
        # [R8] the ordinary crab: two barrels, two jaws, a reactor and two housings keep today's names.
        pieces = [("barrelL", 0.5, 0.4, 0.7, 0.4, 100), ("barrelR", -0.5, 0.4, 0.7, 0.4, 100),
                  ("jawL", 0.4, -0.5, 0.2, 0.1, 50), ("jawR", -0.4, -0.5, 0.2, 0.1, 50),
                  ("reactor", 0.0, 0.5, 0.4, 0.1, 80),
                  ("housL", 0.6, 0.3, 0.8, 0.1, 20), ("housR", -0.6, 0.3, 0.8, 0.1, 20)]
        picks, spare = pick_fittings(pieces, 0.0, 0.0)
        self.assertEqual("barrelL", picks["Gun_L"], "[R8] Gun_L changed side")
        self.assertEqual("barrelR", picks["Gun_R"], "[R8] Gun_R changed side")
        self.assertEqual("jawL", picks["Jaw_L"], "[R8] Jaw_L changed side")
        self.assertEqual("jawR", picks["Jaw_R"], "[R8] Jaw_R changed side")
        self.assertEqual("reactor", picks["Reactor"], "[R8] the reactor was not found")
        self.assertEqual(["housL"], picks["Turret_L"], "[R8] the left housing changed")
        self.assertEqual(["housR"], picks["Turret_R"], "[R8] the right housing changed")
        self.assertEqual([], spare, "[R8] a named fitting fell into the body")

    def test_r9_a_short_run_keeps_the_machines_it_did_not_rebuild(self):
        # [R9] two sheets rebuild Pincer and Kettle; the five other machines' entries must survive in crabs.json.
        old = {"source": "Tools/crabsplit.py",
               "crabs": {"Pincer": {"scale": 1}, "Censer": {"scale": 3}, "Cutter": {"scale": 16}}}
        new = {"source": "Tools/crabsplit.py", "crabs": {"Pincer": {"scale": 3.8}, "Kettle": {"scale": 3.2}}}
        merged, kept = merge_crabs(old, new)
        self.assertEqual(["Censer", "Cutter", "Kettle", "Pincer"], sorted(merged["crabs"]),
                         "[R9] a machine this run did not rebuild was dropped from crabs.json: %s" % merged)
        self.assertEqual(["Censer", "Cutter"], kept, "[R9] the kept entries were not reported: %s" % kept)

    def test_r9_a_rebuilt_machine_gets_this_run_s_entry(self):
        # [R9] keeping old entries must not shadow a machine the run just rebuilt.
        old = {"crabs": {"Pincer": {"scale": 1, "stale": True}}}
        new = {"crabs": {"Pincer": {"scale": 3.8}}}
        merged, kept = merge_crabs(old, new)
        self.assertEqual({"scale": 3.8}, merged["crabs"]["Pincer"], "[R9] the stale entry won: %s" % merged)
        self.assertEqual([], kept, "[R9] a rebuilt machine was reported as kept: %s" % kept)

    def test_r9_more_sheets_than_machines_raises(self):
        # [R9] the old zip() truncated silently; an extra sheet must fail loudly and name the machines.
        with self.assertRaises(ValueError) as e:
            pair_sheets(["s%d" % i for i in range(len(CRAB_ORDER) + 1)])
        self.assertIn("Pincer", str(e.exception), "[R9] the error does not name a machine")
        self.assertEqual([("Pincer", "a"), ("Kettle", "b")], pair_sheets(["a", "b"]),
                         "[R9] a short run no longer pairs from the front")


if __name__ == "__main__":
    unittest.main(verbosity=2)
