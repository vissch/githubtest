#!/usr/bin/env python3
"""Tests for mechsplit.py's LOD allocation with no Blender: python Tools/test_mechsplit.py. Stdlib only.

Blender is needed to cut a model, but the per-part triangle budget is plain arithmetic, so the loop that does it is
lifted out of mechsplit.py with ast and run against stubs. That way the documented knobs (TW_PART_WEIGHT scales a
part's share, TW_PIECE_TRIS and TW_KEEP are floors under it) have a check that they change the output at all.

TW_MECHSPLIT_SRC points the lift at another copy of mechsplit.py (for evidence against an older tree).
"""
import ast, os, sys, unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
SRC = Path(os.environ.get("TW_MECHSPLIT_SRC", HERE / "mechsplit.py"))


def allocation_loop():
    """The `for n in PARTS:` loop that ends in decimated(P0[n], ...), compiled on its own."""
    src = SRC.read_text(encoding="utf-8")
    tree = ast.parse(src)
    loop = next(f for f in ast.walk(tree)
                if isinstance(f, ast.For) and getattr(f.target, "id", None) == "n"
                and getattr(f.iter, "id", None) == "PARTS"
                and "decimated(P0[n]" in (ast.get_source_segment(src, f) or ""))
    mod = ast.fix_missing_locations(ast.Module(body=[loop], type_ignores=[]))
    return compile(mod, str(SRC), "exec")


CODE = allocation_loop()


def want(have=1000, ratio=0.1, floor=32, pieces=1, weight=None, keep=None, piece_tris=None):
    """What the loop gives one part called Fan, with the flags set around the run."""
    # mechsplit sets scale = t0 / sum(tris * weight) over every part; 1.0 stands for one small weighted part in a
    # big model, where the other parts take the scaling down and this one keeps its full weight.
    scale = 1.0
    ns = {"os": os, "PARTS": ["Fan"], "P0": {"Fan": None}, "P": {},
          "ratio": ratio, "floor": floor, "wt": weight or {}, "scale": scale,
          "tris_of": lambda m: have, "islands": lambda m: [0] * pieces,
          "decimated": lambda m, f: f}
    before = {k: os.environ.get(k) for k in ("TW_KEEP", "TW_PIECE_TRIS", "TW_PART_WEIGHT")}
    try:
        os.environ["TW_KEEP"] = keep or ""
        os.environ["TW_PIECE_TRIS"] = str(piece_tris or 0)
        exec(CODE, ns)
        return ns["want"]
    finally:
        for k, v in before.items():
            os.environ.pop(k, None)
            if v is not None: os.environ[k] = v


class Allocation(unittest.TestCase):
    # [U069.2]
    def test_keep_floor_survives(self):
        self.assertEqual(want(keep="Fan:0.5"), 500,
                         "[U069.2] the TW_KEEP floor was overwritten by the plain ratio share")

    # [U069.2]
    def test_keep_floor_with_weight(self):
        self.assertEqual(want(keep="Fan:0.5", weight={"Fan": 2.0}), 500,
                         "[U069.2] the TW_KEEP floor must still hold over a weighted share")

    # [U069.2]
    def test_keep_over_one_clamps(self):
        self.assertEqual(want(keep="Fan:1.5"), 1000,
                         "[U069.2] a part never gains triangles over its LOD0 count")

    # [U069.2]
    def test_piece_floor(self):
        self.assertEqual(want(pieces=20, piece_tris=12), 240,
                         "[U069.2] the TW_PIECE_TRIS floor was overwritten by the plain ratio share")

    def test_weight_scales_the_base(self):
        self.assertEqual(want(weight={"Fan": 2.0}), 200, "the weight scales the part's share of the budget")

    def test_no_flags_is_the_ratio_share(self):
        self.assertEqual(want(), 100, "with no flag set a part gets its share of the budget")


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
    unittest.main(verbosity=1)
