# Tests for housesplit.py. It only runs inside Blender (bpy), so these are structural checks on the source,
# plus one look at the committed houses.json files. Run: python Tools/test_housesplit.py
import json, os, unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
LIMIT = 96          # HouseKit.ChunkMask: BitsPerWord 24 * Words 4


class HousesplitTest(unittest.TestCase):
    def test_r5_the_chunk_limit_is_enforced_before_done(self):
        """[R5] an over-limit house printed DONE and Unity then refused to build it (it is simply not drawn)."""
        lines = open(os.path.join(HERE, "housesplit.py"), encoding="utf-8").read().splitlines()
        const = [l for l in lines if l.startswith("MAX_CHUNKS")]
        self.assertEqual(1, len(const), "[R5] housesplit.py does not name a MAX_CHUNKS constant")
        self.assertIn(str(LIMIT), const[0], "[R5] MAX_CHUNKS is not %d" % LIMIT)
        done = next(i for i, l in enumerate(lines) if l.startswith('print("DONE"'))
        before = "\n".join(lines[:done])
        self.assertIn("MAX_CHUNKS", before.split("MAX_CHUNKS = ")[-1],
                      "[R5] MAX_CHUNKS is never tested before the DONE print")
        self.assertIn("sys.exit(1)", before, "[R5] nothing fails the run before DONE when a house is over limit")

        hk = open(os.path.join(ROOT, "Assets/_Project/Presentation/Terrain/HouseKit.cs"), encoding="utf-8").read()
        bits = hk.split("public const int BitsPerWord = ")[1].split(";")[0]
        per, words = [int(x.split("=")[-1]) for x in bits.split(",")]
        self.assertEqual(LIMIT, per * words, "[R5] HouseKit's mask no longer holds %d chunks" % LIMIT)

    def test_every_committed_house_is_within_the_limit(self):
        env = os.path.join(ROOT, "Assets/_Project/Resources/Env")
        seen = 0
        for s in sorted(os.listdir(env)):
            f = os.path.join(env, s, "houses.json")
            if not os.path.exists(f): continue
            seen += 1
            counts = {}
            for c in json.load(open(f, encoding="utf-8"))["chunks"]:
                counts[c["house"]] = counts.get(c["house"], 0) + 1
            for h, n in sorted(counts.items()):
                self.assertLessEqual(n, LIMIT, "%s/%s has %d chunks, HouseKit holds %d" % (s, h, n, LIMIT))
        self.assertGreater(seen, 0, "no committed houses.json was found")


if __name__ == "__main__":
    unittest.main(verbosity=2)
