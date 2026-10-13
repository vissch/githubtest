#!/usr/bin/env python3
"""Checks for the code-review findings K1s-K8s against .claude/skills docs. Stdlib only."""
import pathlib
import re
import unittest

REPO = pathlib.Path(__file__).resolve().parents[2]
PROJ = REPO / "trench-warfare-3d"


class SkillDocsTest(unittest.TestCase):
    def test_k1s_unity_pipeline_warns_against_bare_unity_commands(self):
        """[K1s] unity-pipeline/SKILL.md must warn to drive via Tools/tw, not a bare `unity` call."""
        text = (REPO / ".claude/skills/unity-pipeline/SKILL.md").read_text(encoding="utf-8")
        head = "\n".join(text.splitlines()[:15]).lower()
        self.assertIn("tools/tw", head)
        self.assertTrue(
            ("never" in head or "only" in head) and ("install" in head or "package" in head),
            "[K1s] expected a header warning against bare `unity install`/`package_add` in the first 15 lines",
        )

    def test_k2s_balance_doc_matches_roster_costs_and_sapper(self):
        """[K2s] tw-balance-sim docs must match RosterEntry.cs costs and know about Sapper."""
        roster_src = (PROJ / "Assets/_Project/Sim/Core/RosterEntry.cs").read_text(encoding="utf-8")
        mg_cost = int(re.search(r"Machinegunner\s*=>.*?Cost\s*=\s*(\d+)", roster_src).group(1))
        sniper_cost = int(re.search(r"Sniper\s*=>.*?Cost\s*=\s*(\d+)", roster_src).group(1))

        balance = (REPO / ".claude/skills/tw-balance-sim/references/balance.md").read_text(encoding="utf-8")
        doc_mg = int(re.search(r"\|\s*Machinegunner\s*\|\s*(\d+)\s*\|", balance).group(1))
        doc_sniper = int(re.search(r"\|\s*Sniper\s*\|\s*(\d+)\s*\|", balance).group(1))
        self.assertEqual(doc_mg, mg_cost, "[K2s] balance.md Machinegunner cost is stale")
        self.assertEqual(doc_sniper, sniper_cost, "[K2s] balance.md Sniper cost is stale")

        self.assertNotIn("no sapper", balance.lower(), "[K2s] balance.md still claims there is no Sapper")
        self.assertTrue(
            "InfantryArchetype.Sapper" in balance or "Sapper = 35" in balance,
            "[K2s] balance.md should name the real Sapper archetype",
        )

        skill = (REPO / ".claude/skills/tw-balance-sim/SKILL.md").read_text(encoding="utf-8")
        not_in_code_line = next((l for l in skill.splitlines() if "Not in code yet" in l), "")
        self.assertNotIn("Sapper", not_in_code_line, "[K2s] SKILL.md still lists Sapper as not in code")

    def test_k3s_driving_and_evidence_uses_file_flag(self):
        """[K3s] driving-and-evidence.md's eval_file flag must match workflow.md (--file, not --path)."""
        workflow = (REPO / "docs/reference/workflow.md").read_text(encoding="utf-8")
        workflow_flag = re.search(r"eval_file[^\n]*?--(\w+)\s", workflow).group(1)

        doc = (REPO / ".claude/skills/pipeline/references/driving-and-evidence.md").read_text(encoding="utf-8")
        doc_flag = re.search(r"eval_file[^\n]*?--(\w+)\s", doc).group(1)

        self.assertEqual(doc_flag, workflow_flag)

    def test_k4s_pipeline_skill_names_a_job_for_status(self):
        """[K4s] pipeline/SKILL.md must not use bare `$R status` and must note there is no list command."""
        text = (REPO / ".claude/skills/pipeline/SKILL.md").read_text(encoding="utf-8")
        bare = re.findall(r"`\$R status`[^<]", text)
        self.assertEqual(bare, [], "[K4s] found bare `$R status` with no job name")
        self.assertIn(
            "no list command", text.lower(),
            "[K4s] doc should say run_detached.py has no list command",
        )

    def test_k5s_docs_say_to_set_tw_station_on_a_hostless_station(self):
        """[K5s] pipeline/SKILL.md and tw-master/SKILL.md must tell you to set TW_STATION on a hostless station."""
        stations = __import__("json").loads(
            (PROJ / "Tools/pipeline/stations.json").read_text(encoding="utf-8")
        )
        hostless = [name for name, info in stations.items() if not info.get("hosts")]
        self.assertTrue(hostless, "[K5s] expected at least one hostless station (e.g. laptop) to check against")

        pipeline_doc = (REPO / ".claude/skills/pipeline/SKILL.md").read_text(encoding="utf-8")
        master_doc = (REPO / ".claude/skills/tw-master/SKILL.md").read_text(encoding="utf-8")
        for name in hostless:
            self.assertTrue(
                "TW_STATION" in pipeline_doc and name in pipeline_doc,
                "[K5s] pipeline/SKILL.md should tell you to set TW_STATION=%s" % name,
            )
            self.assertTrue(
                "TW_STATION" in master_doc and name in master_doc,
                "[K5s] tw-master/SKILL.md should tell you to set TW_STATION=%s" % name,
            )

    def test_k6s_pipeline_skill_cites_no_missing_claude_path(self):
        """[K6s] pipeline/SKILL.md must not point at a nonexistent .claude/agents folder."""
        for path in (REPO / ".claude/skills").rglob("*.md"):
            text = path.read_text(encoding="utf-8")
            self.assertNotIn(
                ".claude/agents", text, "[K6s] %s cites .claude/agents, which does not exist" % path
            )

        pipeline_doc = (REPO / ".claude/skills/pipeline/SKILL.md").read_text(encoding="utf-8")
        self.assertIn(
            ".claude/skills/", pipeline_doc,
            "[K6s] pipeline/SKILL.md should name the role skill under .claude/skills/",
        )
        for prefix in re.findall(r"\.claude/(\w+)/", pipeline_doc):
            self.assertTrue(
                (REPO / ".claude" / prefix).exists(),
                "[K6s] pipeline/SKILL.md cites .claude/%s/, which does not exist" % prefix,
            )

    def test_k7s_bench_is_documented_as_edit_mode(self):
        """[K7s] Bench/Stress must be documented as edit-mode-only, not lumped in with the in-Play CaptureRig calls."""
        capturerig = (PROJ / "Assets/_Project/Editor/CaptureRig.cs").read_text(encoding="utf-8")
        for method in ("Bench", "Stress"):
            m = re.search(r"public static \w[\w<>, ]*\s+" + method + r"\s*\(", capturerig)
            self.assertIsNotNone(m, "[K7s] expected to find %s in CaptureRig.cs" % method)
            body_start = capturerig.index("{", m.end())
            body = capturerig[body_start:body_start + 400]
            self.assertIn(
                "stop play first", body,
                "[K7s] %s should guard against being called while already in Play" % method,
            )

        doc = (REPO / ".claude/skills/pipeline/references/driving-and-evidence.md").read_text(encoding="utf-8")
        lines = doc.splitlines()
        for i, line in enumerate(lines):
            if "Bench(" in line:
                window = "\n".join(lines[max(0, i - 1):i + 1]).lower()
                self.assertIn(
                    "edit mode", window,
                    "[K7s] the line naming Bench( (or the one above it) should say edit mode: %r" % line,
                )

        in_play_bullet = next(l for l in lines if l.strip().startswith("- `Hold(weatherClock)`"))
        self.assertNotIn("Bench(args)", in_play_bullet, "[K7s] Bench(args) should not be in the in-Play bullet")

        opening = "\n".join(lines[:12]).lower()
        self.assertIn(
            "except", opening,
            "[K7s] the 'open and in Play' sentence should name the Bench/Stress exception",
        )

        vfx_doc = (REPO / ".claude/skills/tw-destruction-vfx/SKILL.md").read_text(encoding="utf-8")
        vfx_lines = vfx_doc.splitlines()
        for i, line in enumerate(vfx_lines):
            if "Bench(" in line:
                window = "\n".join(vfx_lines[max(0, i - 1):i + 1]).lower()
                self.assertIn(
                    "edit mode", window,
                    "[K7s] tw-destruction-vfx/SKILL.md's Bench( line (or the line above) should say edit mode",
                )

    def test_k8s_vfx_sheets_knows_the_batch_bc_go(self):
        """[K8s] tw-vfx-sheets/SKILL.md must know the owner already gave the go for explosion batches B and C."""
        decisions = (REPO / "docs/reference/decisions.md").read_text(encoding="utf-8")
        self.assertTrue(
            re.search(r"2026-09-28.*explosion batches B and C", decisions),
            "[K8s] expected a 2026-09-28 decisions.md row naming explosion batches B and C",
        )

        doc = (REPO / ".claude/skills/tw-vfx-sheets/SKILL.md").read_text(encoding="utf-8")
        bc_line = next(l for l in doc.splitlines() if "batches B and C" in l)
        self.assertNotIn(
            "open question", bc_line.lower(),
            "[K8s] tw-vfx-sheets/SKILL.md still calls explosion batches B and C an open question",
        )
        self.assertTrue(
            "2026-09-28" in bc_line or "go given" in bc_line.lower(),
            "[K8s] tw-vfx-sheets/SKILL.md should name the 2026-09-28 go",
        )


if __name__ == "__main__":
    unittest.main(verbosity=1)
