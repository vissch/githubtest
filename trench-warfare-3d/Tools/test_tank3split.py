# Tests for tank3split.py. It only runs inside Blender (bpy at the top of the file), so these are structural
# checks on the source, in the style of test_tanksplit.py. Run: python Tools/test_tank3split.py
import os, unittest

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = open(os.path.join(HERE, "tank3split.py"), encoding="utf-8").read()


class Tank3splitTest(unittest.TestCase):
    def test_r10_both_loop_walks_are_in_edge_index_order(self):
        """[R10] the hole filler and close_big_holes must walk boundary edges by index, not by set order."""
        self.assertEqual(2, SRC.count("bm.edges.ensure_lookup_table()"),
                         "[R10] expected both loop walks to build the edge lookup table before reading e.index")
        self.assertGreaterEqual(SRC.count("key=lambda e: e.index"), 4,
                                "[R10] a loop walk still picks its edges in address order, so the FBX "
                                "is not byte-identical between runs")

    def test_r10_no_edge_set_is_popped(self):
        """[R10] set.pop() on BMesh edges is address order: the bug this guards."""
        self.assertNotIn(".pop()", SRC,
                         "[R10] tank3split.py still pops an edge out of a set")

    def test_r11_the_snap_pass_skips_the_lods_it_will_rebuild(self):
        """[R11] TW_DERIVE must be read before the snap loop, and the loop must iterate the kept LODs only."""
        self.assertLess(SRC.index("DERIVE = os.environ.get"), SRC.index("snapped = []"),
                        "[R11] the snap pass still runs before it knows which LODs are derived, so the "
                        "manifest's \"snapped\" lists meshes that were thrown away")
        self.assertIn("for k in SNAP_LODS:", SRC,
                      "[R11] the snap loop still walks every LOD instead of SNAP_LODS")
        self.assertIn("SNAP_LODS = [k for k in (1, 2) if k not in DERIVED]", SRC,
                      "[R11] SNAP_LODS must be the LODs kept from Tripo")


if __name__ == "__main__":
    unittest.main(verbosity=2)
