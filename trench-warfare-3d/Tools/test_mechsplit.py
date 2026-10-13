# Tests for mechsplit.py. It only runs inside Blender (bpy), so these are structural checks on the source.
# Run: python Tools/test_mechsplit.py
import os, unittest

HERE = os.path.dirname(os.path.abspath(__file__))


class MechsplitTest(unittest.TestCase):
    def test_r7_the_battle_export_does_not_demand_a_gun(self):
        """[R7] the battle export read objs["Gun"] for every kind: KeyError for a flyer, after LOD0 was written."""
        lines = open(os.path.join(HERE, "mechsplit.py"), encoding="utf-8").read().splitlines()
        flyer = [l for l in lines if l.strip().startswith("PARTS =") and "Wing_L" in l]
        self.assertEqual(1, len(flyer), "[R7] expected one flyer PARTS line naming Wing_L")
        self.assertNotIn('"Gun"', flyer[0], "[R7] the flyer has no Gun part, so nothing may read objs[\"Gun\"]")
        start = next(i for i, l in enumerate(lines) if l.startswith("if BATTLE:"))
        end = next(i for i in range(start + 1, len(lines)) if "sys.exit(0)" in lines[i])
        block = "\n".join(lines[start:end + 1])
        self.assertNotIn('objs["Gun"]', block,
                         "[R7] the battle export still reads objs[\"Gun\"], which a flyer does not have")


if __name__ == "__main__":
    unittest.main(verbosity=2)
