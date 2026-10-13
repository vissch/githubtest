#!/usr/bin/env python3
"""Checks for the code-review findings K1s-K4s against .claude/skills docs. Stdlib only."""
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


if __name__ == "__main__":
    unittest.main(verbosity=1)
