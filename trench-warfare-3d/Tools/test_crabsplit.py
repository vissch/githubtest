# Tests for crabsplit.py's leg grouping. Run: python Tools/test_crabsplit.py
# crabsplit.py imports bpy/bmesh/mathutils behind a try/except, so group_legs can be imported outside Blender.
import json, os, sys, unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from crabsplit import group_legs, pick_fittings, pair_sheets, merge_crabs, CRAB_ORDER, CRABS


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


CRABS_JSON = os.environ.get("TW_CRABS_JSON") or os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    "Assets", "_Project", "Resources", "Vehicles", "crabs.json")


def real_limbs(entry, cfg):
    """The limb tuples of a committed machine, read back from its manifest. Pivots are Unity and scaled, so
    unity(v) = [-x, z, -y] * scale is undone; the pivot's height is the piece's TOP, which is why the cases below
    assert membership and labels, not the order inside one leg."""
    piv, s = entry["pivots"], entry["scale"]
    def bl(u): return (-u[0] / s, -u[2] / s, u[1] / s)
    # the root part is "Body" on a sheet-cut machine and "Hull" on one cut again to the walking standard
    mid = bl(piv["Body"] if "Body" in piv else piv["Hull"])
    keys = [k for k in piv if k.split("_")[0] in ("Thigh", "Shin", "Foot")
            or (k.startswith("Claw") and not cfg.get("claws", True))]
    out = []
    for k in keys:
        x, y, z = bl(piv[k])
        out.append((k, x, y, ("L" if x >= mid[0] else "R") + ("F" if y < mid[1] else "B"), z))
    return out


class RealGeometryTest(unittest.TestCase):
    """[T3.1] the leg grouping run over every machine in crabs.json as it stands in the repo."""

    def setUp(self):
        with open(CRABS_JSON, encoding="utf-8") as f:
            self.crabs = json.load(f)["crabs"]

    def test_t3_1_every_machine_in_crabs_json_groups_as_it_is_committed(self):
        # [T3.1] the expected names are the committed ones (the pre-R1 output), taken from each machine's own
        # part list, not from a run of the code under test.
        for name in ("Kettle", "Censer", "Pavise", "Banner"):
            entry = self.crabs[name]
            cfg = CRABS[name]
            want = {}
            for part in entry["lods"][0]["parts"]:
                head, _, label = part.partition("_")
                if head in ("Thigh", "Shin", "Foot"):
                    want.setdefault(label, set()).add(part)
            legs, leftover = group_legs(real_limbs(entry, cfg), cfg.get("legs"), name)
            got = {g["label"]: set(g["keys"]) for g in legs}
            if name == "Banner":
                # Banner was cut again to the walking standard: its legs are numbered from the rear (R1, L2),
                # not labelled by quadrant, so only the grouping can be compared, not the labels.
                self.assertEqual(sorted(map(sorted, want.values())), sorted(map(sorted, got.values())),
                                 "[T3.1] %s no longer groups as it is committed: %s" % (name, legs))
            else:
                self.assertEqual(want, got,
                                 "[T3.1] %s no longer groups as it is committed: %s" % (name, legs))
            self.assertEqual([], leftover, "[T3.1] %s lost a leg piece into the Body" % name)

        # the Redoubt's committed split is the wrong one R1 found, so only the shape of the answer is checked:
        # six legs, six names, two pieces each, nothing dropped.
        entry, cfg = self.crabs["Redoubt"], CRABS["Redoubt"]
        legs, leftover = group_legs(real_limbs(entry, cfg), cfg.get("legs"), "Redoubt")
        self.assertEqual(6, len(legs), "[T3.1] the Redoubt is not six legs: %s" % legs)
        self.assertEqual(6, len(set(g["label"] for g in legs)), "[T3.1] two Redoubt legs share a name: %s" % legs)
        for g in legs:
            self.assertEqual(2, len(g["keys"]), "[T3.1] a Redoubt leg has the wrong pieces: %s" % g)
        self.assertEqual([], leftover, "[T3.1] a Redoubt leg piece fell into the Body")

    def test_t3_1_the_limb_tuple_orders_by_the_piece_centre_not_its_top(self):
        # [T3.1] Kettle's committed Shin_LF top (0.408 Blender) is above Thigh_LF's (0.403), so a top-z sort key
        # renames shin to thigh. build_loose must hand group_legs the piece's centre height.
        with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "crabsplit.py"), encoding="utf-8") as f:
            src = f.read()
        line = [l for l in src.splitlines() if "tuples.append((key," in l]
        self.assertEqual(1, len(line), "[T3.1] build_loose's limb tuple line was not found")
        self.assertIn("c.z", line[0], "[T3.1] the limb tuple does not use the piece centre height: %s" % line[0])
        self.assertNotIn("bm_bounds", line[0],
                         "[T3.1] the limb tuple still sorts legs by the piece top: %s" % line[0])


if __name__ == "__main__":
    unittest.main(verbosity=2)
