#!/usr/bin/env python3
"""The men's outline has a pixel floor, and the rig measures the body, not the mud.

No editor: python Tools/test_man_outline.py

The floor is what VAT_URP's Outline pass does with _TWManOutlineMinPx. A world-space
line thinner than that many pixels is topped up; a thicker line is left alone; a
floor of 0 is the old line. The rig still skips vehicles, and takes the chest from
the drawn foot stood on the terrain, reporting man_luma and bg_luma beside the ratio.
"""
import re
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True

ROOT = Path(__file__).resolve().parents[1]
SHADER = ROOT / "Assets" / "_Project" / "Shaders" / "VAT_URP.shader"
RENDERER = ROOT / "Assets" / "_Project" / "Presentation" / "Units" / "VATRenderer.cs"
RIG = ROOT / "Assets" / "_Project" / "Editor" / "CaptureRig.cs"

TOPUP = "cs1.xy += dir * max(_TWManOutlineMinPx - px, 0.0) * 2.0 / _ScreenParams.xy * cs1.w;"


def outline_px(world_px, floor_px):
    """Screen pixels of ink after the outline top-up.

    world_px is length(dpx) in the shader. Under 1e-4 there is no direction, so
    the add is zero and the line stays invisible. Otherwise the add is
    max(floor - world, 0), which never shrinks a line that is already thicker.
    """
    if world_px <= 1e-4:
        return 0.0
    return world_px + max(floor_px - world_px, 0.0)


def _text(path):
    return path.read_text(encoding="utf-8-sig")


def floor_px(renderer):
    match = re.search(r"public const float DefaultOutlineMinPx = ([0-9.]+)f;", renderer)
    if match is None:
        raise AssertionError("VATRenderer has no DefaultOutlineMinPx")
    return float(match.group(1))


class OutlineFloorTest(unittest.TestCase):
    def test_a_thin_line_is_raised_to_the_floor_and_a_thick_one_is_not(self):
        renderer = _text(RENDERER)
        shader = _text(SHADER)
        floor = floor_px(renderer)
        self.assertEqual(floor, 1.6)
        self.assertEqual(outline_px(0.4, floor), 1.6)
        self.assertEqual(outline_px(3.0, floor), 3.0)
        self.assertEqual(outline_px(0.4, 0.0), 0.4)
        self.assertEqual(outline_px(0.0, floor), 0.0)
        self.assertIn(TOPUP, shader)
        self.assertIn("float _TWManOutlineMinPx;", shader)
        late = renderer.split("void LateUpdate()", 1)[1].split("void ", 1)[0]
        set_at = late.find("Shader.SetGlobalFloat(OutlineMinPxId, OutlineMinPx);")
        bail_at = late.find("if (figures == null")
        self.assertGreater(set_at, 0, "LateUpdate must publish the floor every frame")
        self.assertGreater(bail_at, set_at, "the publish has to run even when nothing is drawn")

    def test_the_rig_samples_the_drawn_foot_and_keeps_vehicles_out(self):
        rig = _text(RIG)
        loop = rig.split("for (int s = 0; s < world.HighWater; s++)", 1)[1].split("contrasts.Sort()", 1)[0]
        foot = loop.find("IsFootFigure(world.Flags[s])")
        sample = loop.find("RenderGround.Sample(host.Local.Map, foot.x, foot.z)")
        chest = loop.find("Vector3 chest = foot + Vector3.up * 1.3f;")
        self.assertGreater(foot, 0)
        self.assertGreater(sample, foot, "vehicles stay skipped, and the foot is then stood on the terrain")
        self.assertGreater(chest, sample)
        self.assertIn('"man_luma"', rig)
        self.assertIn('"bg_luma"', rig)
        self.assertIn('"man_luma", "bg_luma"', rig)


if __name__ == "__main__":
    unittest.main(verbosity=2)
