# Tests for tanksplit.py. It only runs inside Blender (bpy), so these are structural checks on the source.
# Run: python Tools/test_tanksplit.py
import os, unittest

HERE = os.path.dirname(os.path.abspath(__file__))


class TanksplitTest(unittest.TestCase):
    def test_r3_atlas_is_saved_with_the_standard_view_transform(self):
        """[R3] the atlas save must set view_transform = 'Standard', as crabsplit does."""
        lines = open(os.path.join(HERE, "tanksplit.py"), encoding="utf-8").read().splitlines()
        start = [i for i, l in enumerate(lines) if "TankAtlas_LOD0.jpg" in l]
        self.assertEqual(len(start), 1, "[R3] expected one atlas loop naming TankAtlas_LOD0.jpg")
        save = next(i for i in range(start[0] + 1, len(lines)) if "im.save_render(" in lines[i])
        block = "\n".join(lines[start[0]:save + 1])
        self.assertIn("view_settings.view_transform", block,
                      "[R3] the atlas is saved through the scene's view transform (Filmic/AgX), not 'Standard'")

    def test_r3_crabsplit_is_the_reference(self):
        """[R3] the rule copied here is crabsplit's; if crabsplit drops it, this test is moot."""
        src = open(os.path.join(HERE, "crabsplit.py"), encoding="utf-8").read()
        self.assertIn("view_settings.view_transform = 'Standard'", src,
                      "[R3] crabsplit.py no longer sets 'Standard': the drift this guards is gone or moved")


if __name__ == "__main__":
    unittest.main(verbosity=2)
