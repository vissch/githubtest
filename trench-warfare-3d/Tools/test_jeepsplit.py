# Tests for jeepsplit.py. It only runs inside Blender (bpy at the top of the file), so these are structural
# checks on the source, in the style of test_tanksplit.py. Run: python Tools/test_jeepsplit.py
import os, unittest

HERE = os.path.dirname(os.path.abspath(__file__))


def block(name, start_marker):
    """The source lines of one top-level function, from its `def` to the next one."""
    lines = open(os.path.join(HERE, name), encoding="utf-8").read().splitlines()
    i = next(j for j, l in enumerate(lines) if l.startswith(start_marker))
    j = next((k for k in range(i + 1, len(lines)) if lines[k].startswith("def ")), len(lines))
    return "\n".join(lines[i:j])


class JeepsplitTest(unittest.TestCase):
    def test_r10_the_cap_walk_is_in_edge_index_order(self):
        """[R10] cap() must walk the boundary edges by index, so two runs write the same FBX bytes."""
        b = block("jeepsplit.py", "def cap(")
        self.assertIn("key=lambda e: e.index", b,
                      "[R10] cap() does not order its boundary edges by index: the walk is address order")
        self.assertNotIn(".pop()", b,
                         "[R10] cap() still pops a BMesh edge out of a set: that order is the address order")

    def test_r10_the_edge_lookup_table_is_built_first(self):
        """[R10] e.index is only valid after ensure_lookup_table(); without it the order is -1 for every edge."""
        b = block("jeepsplit.py", "def cap(")
        self.assertIn("bm.edges.ensure_lookup_table()", b,
                      "[R10] cap() reads e.index without building the edge lookup table first")


if __name__ == "__main__":
    unittest.main(verbosity=2)
