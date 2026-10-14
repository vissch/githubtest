#!/usr/bin/env python3
"""Tests for Tools/make_missing_clips.py: the recipe strings written into made-clips.csv must agree with the code.

The script imports animforge and needs the owner's FBX library, so it cannot be run here. These tests read it as
text: the recipe row for a clip and the source line that builds it are both in the file, and the numbers quoted
in the one are checked against the arguments of the other.
Run: python Tools/test_make_missing_clips.py
"""
import os
import re
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = open(os.path.join(HERE, "make_missing_clips.py"), encoding="utf-8").read()


def recipe(clip_name):
    """The recipe string of the MAKE row for `clip_name`."""
    m = re.search(r"\('%s', \w+, '\w+', '([^']*)'\)" % re.escape(clip_name), SRC)
    assert m, "no MAKE row for %s" % clip_name
    return m.group(1)


def call_args(fn_name, call):
    """The argument list of `call` inside `def fn_name():`, as a list of strings."""
    body = SRC.split("def %s():" % fn_name, 1)[1].split("\ndef ", 1)[0]
    m = re.search(r"%s\(([^)]*)\)" % re.escape(call), body)
    assert m, "no %s( call in %s()" % (call, fn_name)
    return [a.strip() for a in m.group(1).split(",")]


class RecipeTest(unittest.TestCase):
    """[R13] made-clips.csv recipes must quote the numbers the code actually uses."""

    def _check_high_step(self, clip_name, fn_name):
        args = call_args(fn_name, "high_step")          # high_step(base, lift, fwd, back, knee)
        lift, fwd, back = args[1], args[2], args[3]
        r = recipe(clip_name)
        self.assertIn("%s deg" % lift, r,
                      "[R13] %s: the recipe does not name the %s degree lift (%r)" % (clip_name, lift, r))
        self.assertIn("%sx" % fwd, r,
                      "[R13] %s: the recipe does not name the forward swing %sx (%r)" % (clip_name, fwd, r))
        self.assertIn("%sx" % back, r,
                      "[R13] %s: the recipe does not name the trailing swing %sx (%r)" % (clip_name, back, r))
        # the swing clause only (the retime speed elsewhere in the recipe may legitimately read 0.5x)
        swing = r[r.index("swing"):]
        for wrong in ("1.5x", "0.6x", "0.5x"):
            if wrong in (fwd + "x", back + "x"):
                continue
            self.assertNotIn(wrong, swing,
                             "[R13] %s: the swing clause still quotes %s, which is in no high_step argument (%r)"
                             % (clip_name, wrong, r))

    def test_r13_the_wade_recipe_matches_its_high_step_call(self):
        """[R13] Wade Forward said 1.5x / 0.6x where wade() calls high_step(base, 14, 1.1, 0.4, 1.1)."""
        self._check_high_step("Wade Forward", "wade")
        self.assertIn("1.7x", recipe("Wade Forward"), "[R13] Wade Forward: retime(1 / 1.7) is not in the recipe")

    def test_r13_the_wire_recipe_matches_its_high_step_call(self):
        """[R13] Wire Crossing said 1.5x / 0.5x where wire() calls high_step(base, 18, 0.85, 0.35, 1.8)."""
        self._check_high_step("Wire Crossing", "wire")
        self.assertIn("0.5x", recipe("Wire Crossing"), "[R13] Wire Crossing: retime(1 / 2.0) is not in the recipe")

    def test_r13_the_get_up_recipe_names_the_fade_it_uses(self):
        """[R13] get_up() concatenates with fade=12; the recipe said 8 frames."""
        self.assertIn("fade=12", SRC.split("def get_up():", 1)[1].split("\ndef ", 1)[0])
        self.assertIn("12 frames", recipe("Get Up From Prone"),
                      "[R13] Get Up From Prone: the recipe does not name the 12-frame cross-fade")
        self.assertNotIn("8 frames", recipe("Get Up From Prone"),
                         "[R13] Get Up From Prone: the recipe still claims an 8-frame cross-fade")

    def test_r13_the_whistle_recipe_does_not_claim_a_length_it_is_not(self):
        """[R13] whistle() resamples to 48 frames (1.6 s at 30 fps); the recipe said 0.6 s."""
        body = SRC.split("def whistle():", 1)[1].split("\ndef ", 1)[0]
        n = int(re.search(r"resample\((\d+)", body).group(1))
        self.assertEqual(n, 48, "[R13] whistle() no longer resamples to 48 frames; the recipe needs rewording")
        self.assertNotIn("0.6 s", recipe("Officer Whistle"),
                         "[R13] Officer Whistle: the recipe still claims 0.6 s for a %d-frame clip" % n)
        self.assertIn("1.6 s", recipe("Officer Whistle"),
                      "[R13] Officer Whistle: the recipe does not give its real length, 1.6 s")


if __name__ == "__main__":
    unittest.main(verbosity=2)
