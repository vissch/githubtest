#!/usr/bin/env python3
"""Tests for sweepfilm.py with no editor: python Tools/test_sweepfilm.py. Stdlib only.

A track written here by hand (a header with one trench of two cells, three samples at 5, 10 and 15 s with two men
each) becomes a page: every expected value below comes from that fixture.
"""
import contextlib, io, json, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import sweepfilm as F

HEAD = {"variant": "mg@0.8", "seed": 7, "swapped": False, "scenario": "match", "minutes": 1,
        "tick_seconds": 0.05, "every": 100,
        "trenches": [{"owner": 0, "cells": [[-10.0, 4.0], [10.0, 4.0]]}]}
SAMPLES = [
    {"t": 5.0, "men": [[-2.0, 1.0, 0, 0], [3.0, 9.0, 1, 1]], "alive": [1, 1], "owners": [0]},
    {"t": 10.0, "men": [[-1.0, 2.0, 0, 1], [3.0, 8.0, 1, 1]], "alive": [1, 1], "owners": [0]},
    {"t": 15.0, "men": [[0.0, 3.0, 0, 1], [3.0, 7.0, 1, 0]], "alive": [1, 1], "owners": [1]},
]


def write_run(root, stem="mg@0.8-s7"):
    tracks = root / "tracks"
    tracks.mkdir(parents=True, exist_ok=True)
    lines = [json.dumps(HEAD)] + [json.dumps(s) for s in SAMPLES]
    (tracks / (stem + ".jsonl")).write_text("\n".join(lines) + "\n", encoding="utf-8", newline="\n")
    return tracks / (stem + ".jsonl")


@contextlib.contextmanager
def quiet():
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        yield out


class PageTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.run = Path(self.tmp.name) / "20260101-0000-abc"
        write_run(self.run)
        with quiet() as o:
            self.code = F.main([str(self.run)])
        self.printed = o.getvalue()
        self.page = (self.run / "film" / "mg@0.8-s7.html")
        self.html = self.page.read_text(encoding="utf-8") if self.page.exists() else ""

    def tearDown(self):
        self.tmp.cleanup()

    def test_a_run_folder_gets_a_page_a_track_and_an_index(self):
        self.assertEqual(self.code, 0, self.printed)
        self.assertTrue(self.page.exists(), "the page is written beside the run, in film/")
        self.assertTrue((self.run / "film" / "index.html").exists(), "one index links them all")
        self.assertIn(str(self.page), self.printed, "each path is printed")

    def test_the_page_holds_the_three_samples_inlined(self):
        self.assertEqual(self.html.count('"t":'), 3, "the three samples of the fixture are in the page's JSON")
        data = json.loads(F.page(HEAD, SAMPLES, "x").split('type="application/json">')[1].split("</script>")[0])
        self.assertEqual([s["t"] for s in data["samples"]], [5.0, 10.0, 15.0])
        self.assertEqual(data["head"]["trenches"][0]["cells"], [[-10.0, 4.0], [10.0, 4.0]])

    def test_the_page_names_the_variant_and_the_seed(self):
        self.assertIn("mg@0.8", self.html)
        self.assertIn("seed 7", self.html)

    def test_the_page_loads_nothing_from_the_network(self):
        self.assertNotIn("http", self.html, "self-contained: no stylesheet, script or font fetched")

    def test_the_slider_spans_the_samples_zero_based(self):
        self.assertIn('id="slide" min="0" max="2"', self.html, "three samples, so 0 to 2")

    def test_the_index_names_the_variant_the_seating_and_the_length(self):
        html = (self.run / "film" / "index.html").read_text(encoding="utf-8")
        self.assertIn('href="mg@0.8-s7.html"', html)
        self.assertIn("as seated", html, "the fixture is not swapped")
        self.assertIn("0:15", html, "the last sample stands at 15 s")

    def test_one_track_file_on_its_own_works_too(self):
        track = self.run / "tracks" / "mg@0.8-s7.jsonl"
        out = Path(self.tmp.name) / "elsewhere"
        with quiet():
            self.assertEqual(F.main([str(track), "--out", str(out)]), 0)
        self.assertTrue((out / "mg@0.8-s7.html").exists())


def write_lines(root, stem, lines):
    """A track of exactly these lines, for the pages a half-written or header-only file must still give."""
    tracks = root / "tracks"
    tracks.mkdir(parents=True, exist_ok=True)
    path = tracks / (stem + ".jsonl")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8", newline="\n")
    return path


class ThinTrackTest(unittest.TestCase):
    """A track with no samples, or one cut short: a page that says so, never a throw. [SW.1, SW.2]"""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name) / "run"

    def tearDown(self):
        self.tmp.cleanup()

    def test_a_header_only_track_gets_a_page_that_says_there_are_no_samples(self):
        write_lines(self.root, "mg@0.8-s7", [json.dumps(HEAD)])
        with quiet() as o:
            code = F.main([str(self.root)])
        self.assertEqual(code, 0, o.getvalue())
        html = (self.root / "film" / "mg@0.8-s7.html").read_text(encoding="utf-8")
        self.assertIn("no samples", html, "the match was over before the first five-second sample")
        self.assertNotIn('id="slide"', html, "no slider, no canvas and no script with nothing to play")
        self.assertIn("0:00", (self.root / "film" / "index.html").read_text(encoding="utf-8"),
                      "the index still lists it, at no length")

    def test_a_last_line_cut_short_is_dropped_and_the_rest_plays(self):
        lines = [json.dumps(HEAD)] + [json.dumps(s) for s in SAMPLES[:2]] + ['{"t": 15.0, "men": [[0.0']
        write_lines(self.root, "mg@0.8-s7", lines)
        with quiet() as o:
            code = F.main([str(self.root)])
        printed = o.getvalue()
        self.assertEqual(code, 0, printed)
        self.assertIn("cut short", printed, "the dropped line is said out loud")
        html = (self.root / "film" / "mg@0.8-s7.html").read_text(encoding="utf-8")
        self.assertEqual(html.count('"t":'), 2, "the two whole samples are kept")
        self.assertIn('id="slide" min="0" max="1"', html)

    def test_one_bad_track_still_leaves_the_others_their_pages_and_the_index(self):
        bad = [json.dumps(HEAD), '{"t": 5.0, "men"', json.dumps(SAMPLES[1])]   # a bad line in the middle, not the last
        write_lines(self.root, "a-s1", bad)
        write_lines(self.root, "b-s2", [json.dumps(HEAD)] + [json.dumps(s) for s in SAMPLES])
        with quiet() as o:
            code = F.main([str(self.root)])
        printed = o.getvalue()
        self.assertEqual(code, 1, "a track that would not read at all is a failure")
        self.assertIn("a-s1.jsonl", printed, "the bad one is named")
        self.assertFalse((self.root / "film" / "a-s1.html").exists())
        self.assertTrue((self.root / "film" / "b-s2.html").exists(), "the good track still gets its page")
        self.assertIn('href="b-s2.html"', (self.root / "film" / "index.html").read_text(encoding="utf-8"))


class RefusalTest(unittest.TestCase):
    def test_a_folder_with_no_track_exits_1(self):
        with tempfile.TemporaryDirectory() as d:
            with quiet() as o:
                self.assertEqual(F.main([d]), 1)
            self.assertIn("no track", o.getvalue())

    def test_a_header_that_will_not_parse_exits_1_naming_the_file(self):
        with tempfile.TemporaryDirectory() as d:
            root = Path(d) / "run"
            track = write_run(root)
            track.write_text("{not json\n", encoding="utf-8", newline="\n")
            with quiet() as o:
                self.assertEqual(F.main([str(root)]), 1)
            self.assertIn(track.name, o.getvalue())


if __name__ == "__main__":
    unittest.main(verbosity=1)
