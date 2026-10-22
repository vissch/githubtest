"""ContinentMesh.Land fills exact-size arrays: python Tools/test_continent_land.py.

Stdlib only, no Unity. A List without a capacity doubles as the full 96 x 64 grid is filled, and the
throw-away intermediate arrays cost about as much again as the mesh that is kept (about 1.6 MB), once
per scene. The count pass sizes the vertex buffer to kept * 3 and each band's index buffer to its own
triangle count, so a bigger grid does not mean more resizes.
"""
import pathlib
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
SRC = ROOT / "Assets" / "_Project" / "Presentation" / "Meta" / "ContinentMesh.cs"


def land_body(text):
    start = text.index("public static Mesh Land(")
    end = text.index("/// <summary>The sea:", start)
    return text[start:end]


class ContinentLandTests(unittest.TestCase):
    def test_land_fills_exact_size_arrays(self):
        body = land_body(SRC.read_text(encoding="utf-8"))
        self.assertNotIn("new List<Vector3>()", body, "Land still grows a vertex list by doubling")
        self.assertNotIn(".Add(", body, "Land still appends into a list")
        self.assertIn("new Vector3[kept * 3]", body, "the vertex buffer is not sized to the kept triangles")
        self.assertIn("perBand[0] * 3", body, "a band's index buffer is not sized to its own triangles")


if __name__ == "__main__":
    unittest.main()
