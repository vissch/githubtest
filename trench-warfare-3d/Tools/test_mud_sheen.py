#!/usr/bin/env python3
"""Night mud keeps a smaller share of the moon than the old sheen, and mirrors less sky than still water.

No editor: python Tools/test_mud_sheen.py

look.mudSheen (0.45) multiplies the broad sheen and the fine spark. A knob of 0, or day
(look.wet 0), is the old sheen of 1. look.mudSky (0.85) takes sky off the mud; still water
stays at today's 0.55. The close men cut (_TWMenSheenCut) stays on the spark.
"""
import os
import re
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True

ROOT = Path(os.environ.get("TW_MUD_ROOT", Path(__file__).resolve().parents[1]))
NIGHT = ROOT / "Assets" / "_Project" / "Presentation" / "Terrain" / "Atmosphere.NightLook.cs"
ATMO = ROOT / "Assets" / "_Project" / "Presentation" / "Terrain" / "Atmosphere.cs"
SHADER = ROOT / "Assets" / "_Project" / "Shaders" / "Toon_URP.shader"
POOLS = ROOT / "Assets" / "_Project" / "Shaders" / "TWLightPools.hlsl"

MUD_SHEEN_LINE = "half mudSheen = lerp(1.0, _TWMudSheen > 0.0 ? _TWMudSheen : 1.0, _TWWetLook);"


def mud_sheen_share(knob, wet_look):
    """Shader lerp(1, knob > 0 ? knob : 1, wet). Day and a 0 knob are the old sheen."""
    kept = knob if knob > 0.0 else 1.0
    return 1.0 + (kept - 1.0) * wet_look


def sky_keep(mud_sky, wet_look, gloss):
    """Sky-mirror factor: 1 - wet * lerp(knob > 0 ? knob : 0.55, 0.55, saturate(gloss * 2 - 1)).

    Still water (gloss high) stays at 0.55. Mud (gloss low) uses mudSky. A 0 knob is the old 0.55.
    """
    still = min(1.0, max(0.0, gloss * 2.0 - 1.0))
    sky_knob = mud_sky if mud_sky > 0.0 else 0.55
    share = sky_knob + (0.55 - sky_knob) * still
    return 1.0 - wet_look * share


def _text(path):
    return path.read_text(encoding="utf-8-sig")


class MudSheenTest(unittest.TestCase):
    def test_defaults_cut_the_mud_and_leave_the_old_night_at_zero(self):
        night = _text(NIGHT)
        match = re.search(
            r"public const float DefaultMudSheen = ([0-9.]+)f, DefaultMudSky = ([0-9.]+)f;",
            night,
        )
        self.assertIsNotNone(match, "NightLook publishes look.mudSheen and look.mudSky")
        sheen = float(match.group(1))
        sky = float(match.group(2))
        self.assertAlmostEqual(sheen, 0.45)
        self.assertAlmostEqual(sky, 0.85)
        self.assertLess(sheen, 1.0)
        self.assertGreater(sky, 0.55)
        # Night, wet on: the mud's moon is 0.45 of the old broad sheen.
        self.assertAlmostEqual(mud_sheen_share(sheen, 1.0), 0.45)
        # Day (look.wet 0) and a 0 knob are the old sheen exactly.
        self.assertAlmostEqual(mud_sheen_share(sheen, 0.0), 1.0)
        self.assertAlmostEqual(mud_sheen_share(0.0, 1.0), 1.0)
        # Mud (low gloss) loses more sky than still water, which stays at 0.55.
        self.assertAlmostEqual(sky_keep(sky, 1.0, 0.2), 0.15)
        self.assertAlmostEqual(sky_keep(sky, 1.0, 1.0), 0.45)
        self.assertAlmostEqual(sky_keep(0.0, 1.0, 0.2), 0.45)
        self.assertAlmostEqual(sky_keep(sky, 0.0, 0.2), 1.0)
        self.assertIn("night ? mudSheen : 0f", night)
        self.assertIn("night ? mudSky : 0f", night)
        self.assertIn('Knobs.Get(MenCloseKnob, DefaultMenClose)', night)
        self.assertIn("Shader.SetGlobalFloat(MenSheenCutId, night ? SheenCut(menClose) : 0f);", night)

    def test_the_shader_dims_both_highlights_and_keeps_the_close_cut(self):
        shader = _text(SHADER)
        pools = _text(POOLS)
        atmo = _text(ATMO)
        self.assertIn(MUD_SHEEN_LINE, shader)
        broad = next(line for line in shader.splitlines() if "pow(toLight, 14.0)" in line)
        spark = next(line for line in shader.splitlines() if "spark * 1.7" in line)
        sky = next(line for line in shader.splitlines() if "lerp(color, sky," in line)
        self.assertIn("mudSheen", broad)
        self.assertNotIn("_TWMenSheenCut", broad)
        self.assertIn("mudSheen", spark)
        self.assertIn("_TWMenSheenCut", spark)
        self.assertIn("_TWMudSky", sky)
        self.assertIn("0.55", sky)
        self.assertIn("float _TWMudSheen;", pools)
        self.assertIn("float _TWMudSky;", pools)
        self.assertIn("float _TWMenSheenCut;", pools)
        clear = atmo.split("void OnDestroy()", 1)[1].split("ClearBiome()", 1)[0]
        self.assertIn("MudSheenId, 0f", clear)
        self.assertIn("MudSkyId, 0f", clear)
        self.assertIn("MenSheenCutId, 0f", clear)


if __name__ == "__main__":
    unittest.main(verbosity=2)
