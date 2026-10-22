#!/usr/bin/env python3
"""Checks for the code-review findings K1s-K8s against .claude/skills docs. Stdlib only."""
import pathlib
import re
import unittest

REPO = pathlib.Path(__file__).resolve().parents[2]
PROJ = REPO / "trench-warfare-3d"


def text_of(path):
    return path.read_text(encoding="utf-8")


class SkillDocsTest(unittest.TestCase):
    def test_k1s_unity_pipeline_warns_against_bare_unity_commands(self):
        """[K1s] unity-pipeline/SKILL.md must send every command through Tools/tw and tell nobody to install.

        Word-counting is not enough: the package's own description line already holds "install" and "only", so a
        check for those words passes on a file whose body still says "Invoke commands with `unity command`" and
        still opens with an "Install & verify" recipe. These assertions name what must and must not be run.
        """
        text = (REPO / ".claude/skills/unity-pipeline/SKILL.md").read_text(encoding="utf-8")
        head = "\n".join(text.splitlines()[:15]).lower()
        self.assertIn("tools/tw", head, "[K1s] the first 15 lines never name Tools/tw")
        self.assertRegex(head, r"never run a bare `unity\s+command",
                         "[K1s] the head does not forbid a bare `unity command` by name")

        # Lines inside ``` fences are what an agent copies and runs. None of them may install a package.
        fenced, inside = [], False
        for ln in text.splitlines():
            if ln.startswith("```"):
                inside = not inside
            elif inside:
                fenced.append(ln)
        bad = [ln for ln in fenced if "pipeline install" in ln or "package_add" in ln]
        self.assertFalse(bad, "[K1s] SKILL.md still gives a runnable install line: %r" % bad)

        # And nothing in it may tell the agent that a command is invoked with a bare `unity command`.
        tells = [ln for ln in text.splitlines()
                 if re.match(r"\s*(invoke|run) commands? with `unity command", ln, re.I)]
        self.assertFalse(tells, "[K1s] SKILL.md still tells the agent to invoke commands bare: %r" % tells)

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
        """[K5s] pipeline/SKILL.md and tw-master/SKILL.md must tell you to set TW_STATION on a hostless station.

        [K5s.1] "TW_STATION" and the station name both being somewhere in the file is not enough: it also passes
        a doc that only ever discusses the two words separately. This asserts the literal setup line instead.
        """
        stations = __import__("json").loads(
            (PROJ / "Tools/pipeline/stations.json").read_text(encoding="utf-8")
        )
        hostless = [name for name, info in stations.items() if not info.get("hosts")]
        self.assertTrue(hostless, "[K5s] expected at least one hostless station (e.g. laptop) to check against")

        pipeline_doc = (REPO / ".claude/skills/pipeline/SKILL.md").read_text(encoding="utf-8")
        master_doc = (REPO / ".claude/skills/tw-master/SKILL.md").read_text(encoding="utf-8")
        for name in hostless:
            pattern = re.compile(r"TW_STATION\s*=\s*['\"]?" + re.escape(name))
            self.assertRegex(
                pipeline_doc, pattern,
                "[K5s] pipeline/SKILL.md should give the literal TW_STATION=%s setup line" % name,
            )
            self.assertRegex(
                master_doc, pattern,
                "[K5s] tw-master/SKILL.md should give the literal TW_STATION=%s setup line" % name,
            )

    def test_k6s_pipeline_skill_cites_no_missing_claude_path(self):
        """[K6s] a skill doc must not point at a .claude/<dir> that does not exist.

        [K6s.1] banning the literal string ".claude/agents" was wrong: on the integration branch that folder is
        real and several skill docs cite it, so this test went red the moment a lane carrying it was rebased or
        landed. The actual rule is narrower (and survives that): whatever `.claude/<dir>/` a doc names must exist.
        """
        for path in (REPO / ".claude/skills").rglob("*.md"):
            text = path.read_text(encoding="utf-8")
            for prefix in re.findall(r"\.claude/([\w.-]+)/", text):
                self.assertTrue(
                    (REPO / ".claude" / prefix).is_dir(),
                    "[K6s] %s cites .claude/%s/, which does not exist" % (path, prefix),
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

    def test_k10s_stale_symbols_are_gone_from_skill_docs(self):
        """[K10s] no skill doc should cite CrabNames/CrabArchetypes, ChainV9, or call Mercy Playground-only,
        and the FormatVersion / hash-chain names they do cite must match the real code."""
        skills_dir = REPO / ".claude/skills"
        for path in skills_dir.rglob("*.md"):
            text = path.read_text(encoding="utf-8")
            for stale in ("CrabNames", "CrabArchetypes", "ChainV9"):
                self.assertNotIn(stale, text, "[K10s] %s still cites %s" % (path, stale))
            self.assertNotRegex(
                text, r"Mercy[^\n]*only in the Playground",
                "[K10s] %s still calls Mercy Playground-only" % path,
            )

        replay_src = (PROJ / "Assets/_Project/Sim/Core/Replay.cs").read_text(encoding="utf-8")
        real_format_version = int(re.search(r"FormatVersion\s*=\s*(\d+)", replay_src).group(1))

        hash_tests_src = (PROJ / "Assets/_Project/Tests/Sim/SimHashTests.cs").read_text(encoding="utf-8")
        self.assertIn(
            "const string SystemChain", hash_tests_src,
            "[K10s] expected SimHashTests.cs to still name its chain const SystemChain",
        )

        balance = (REPO / ".claude/skills/tw-balance-sim/references/balance.md").read_text(encoding="utf-8")
        balance_fv = int(re.search(r"FormatVersion\s*(\d+)", balance).group(1))
        self.assertEqual(
            balance_fv, real_format_version, "[K10s] balance.md's FormatVersion is stale"
        )

        det = (REPO / ".claude/skills/tw-bug-catcher/references/determinism-and-verdicts.md").read_text(encoding="utf-8")
        det_fv = int(re.search(r"FormatVersion\s*==?\s*(\d+)", det).group(1))
        self.assertEqual(det_fv, real_format_version, "[K10s] determinism-and-verdicts.md's FormatVersion is stale")

        det_lines = det.splitlines()
        routing_line = next(l for l in det_lines if l.strip().startswith("7.") and "Routing" in l)
        self.assertNotIn(
            "the fix,", routing_line,
            "[K10s] the bug-catcher routing bullet must only name an inbox note, never the fix, to match "
            "SKILL.md's 'Never fix'",
        )

    def test_k12s_master_skill_names_the_quoted_file_exception(self):
        """[K12s] tw-master/SKILL.md must say a docs/Tools-only lane still needs the full gate when it touches
        a file a test quotes by name, matching land.py's is_code/tested_names check."""
        land_src = (PROJ / "Tools/land.py").read_text(encoding="utf-8")
        self.assertIn("def is_code(", land_src, "[K12s] expected land.py to still define is_code")
        self.assertIn("tested_names", land_src, "[K12s] expected land.py's is_code to still use tested_names")

        text = (REPO / ".claude/skills/tw-master/SKILL.md").read_text(encoding="utf-8")
        lines = text.splitlines()
        start = next((i for i, l in enumerate(lines) if "Lanes that touch only" in l), None)
        self.assertIsNotNone(start, "[K12s] expected the docs/Tools-only lane line in tw-master/SKILL.md")
        exception_block = "\n".join(lines[start:start + 4])
        self.assertTrue(
            "--dry-run" in exception_block or "quotes" in exception_block.lower(),
            "[K12s] the docs/Tools-only lane line should name the --dry-run check or the quoted-file exception",
        )

    def test_k13s_examples_write_outside_the_checkout(self):
        """[K13s] no skill doc example should write out.csv, dossier.html or perf.json as a bare checkout-relative
        path: land.py then refuses a dirty tree, and none of the three is git-ignored."""
        gitignore = (REPO / ".gitignore").read_text(encoding="utf-8")
        for bare_name in ("out.csv", "dossier.html", "perf.json"):
            self.assertNotIn(
                bare_name, gitignore,
                "[K13s] %s is not git-ignored, so an example writing it bare would land as a dirty file" % bare_name,
            )

        skills_dir = REPO / ".claude/skills"
        offenders = []
        for path in list(skills_dir.rglob("*.md")) + [REPO / "docs/reference/pipelines.md"]:
            for line in text_of(path).splitlines():
                if ("absolute path" in line.lower() or "run folder" in line.lower()
                        or "not git-ignored" in line.lower()):
                    continue
                for m in re.finditer(r"[`\"]([\w./\\%-]*\b(?:out\.csv|dossier\.html|perf\.json))[`\"]", line):
                    arg = m.group(1)
                    if "/" not in arg and "\\" not in arg and "%" not in arg:
                        offenders.append("%s: %s" % (path, arg))
        self.assertEqual(
            offenders, [],
            "[K13s] found example(s) writing a bare filename into the checkout: %s" % offenders,
        )

    def test_k11s_tests_paths_resolve_and_no_stale_line_citations(self):
        """[K11s] every P\\Tests\\...cs path named in a skill doc must resolve to a real file (not the retired
        Tests/EditMode layout), no skill doc may cite workflow.md:<digits> or gate.ps1:<digits> (both drift), and
        the Long tier must be documented with a count matching the real [Category("Long")] tests."""
        skills_dir = REPO / ".claude/skills"
        docs = list(skills_dir.rglob("*.md"))

        for path in docs:
            text = text_of(path)
            self.assertNotRegex(
                text, r"workflow\.md:\d", "[K11s] %s cites a drifted workflow.md:<line>" % path
            )
            self.assertNotRegex(
                text, r"gate\.ps1:\d", "[K11s] %s cites a drifted gate.ps1:<line>" % path
            )
            for m in re.finditer(r"P\\Tests\\(\w+)\\([\w.]+\.cs)", text):
                rel = "Assets/_Project/Tests/%s/%s" % (m.group(1), m.group(2))
                self.assertTrue(
                    (PROJ / rel).exists(),
                    "[K11s] %s cites %s, which does not exist" % (path, m.group(0)),
                )

        real_long_count = 0
        for cs_file in (PROJ / "Assets/_Project/Tests").rglob("*.cs"):
            real_long_count += text_of(cs_file).count('Category("Long")')
        self.assertGreater(real_long_count, 0, "[K11s] expected at least one Long-tagged test to check against")

        det = (skills_dir / "tw-bug-catcher/references/determinism-and-verdicts.md").read_text(encoding="utf-8")
        m = re.search(r'\[Category\("Long"\)\].*?\((\d+)', det)
        self.assertIsNotNone(m, "[K11s] expected determinism-and-verdicts.md to name the Long tier with a count")
        self.assertEqual(
            int(m.group(1)), real_long_count,
            "[K11s] determinism-and-verdicts.md's Long-test count is stale",
        )
        self.assertIn("!Long", det, "[K11s] expected determinism-and-verdicts.md to name -testCategory !Long")

    def test_k14s_vfx_sheets_working_dirs_and_no_fake_slash_commands(self):
        """[K14s] the headroom line must name --min-headroom-gb with run_detached.py's real default of 10, every
        Tools/ call in tw-vfx-sheets docs must carry its own `cd`, every machine-only path is marked desktop-only,
        no skill doc names a slash command that has no .claude/commands/<name>.md, and no skill doc says
        `--mode editor` (gate.ps1/workflow.md use EditMode/PlayMode)."""
        run_detached_src = (PROJ / "Tools/pipeline/run_detached.py").read_text(encoding="utf-8")
        m = re.search(r'"--min-headroom-gb".*?default=([\d.]+)', run_detached_src)
        self.assertIsNotNone(m, "[K14s] expected run_detached.py to still define --min-headroom-gb")
        self.assertEqual(float(m.group(1)), 10.0, "[K14s] run_detached.py's --min-headroom-gb default is no longer 10")

        vfx_skill = (REPO / ".claude/skills/tw-vfx-sheets/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("--min-headroom-gb", vfx_skill, "[K14s] expected the headroom line to name --min-headroom-gb")
        self.assertIn("default is 10", vfx_skill, "[K14s] expected the headroom line to say the default is 10")

        skills_dir = REPO / ".claude/skills"
        for line in vfx_skill.splitlines():
            if "python Tools/" in line or "python trench-warfare-3d/Tools/" in line:
                self.assertTrue(
                    "cd trench-warfare-3d" in line or "python trench-warfare-3d/Tools/" in line,
                    "[K14s] tw-vfx-sheets line calls Tools/ without a cd: %r" % line,
                )

        marked_files = (
            "tw-vfx-sheets/SKILL.md",
            "tw-vfx-sheets/references/generator.md",
            "tw-vfx-sheets/references/firebooks-contract.md",
            "tw-balance-sim/SKILL.md",
            "tw-optimizer/SKILL.md",
        )
        for rel in marked_files:
            path = skills_dir / rel
            lines = text_of(path).splitlines()
            for i, line in enumerate(lines):
                if re.search(r"C:[/\\]Users[/\\]PC[/\\]|G:[/\\]My Drive[/\\]", line):
                    window = "\n".join(lines[max(0, i - 10):i + 1]).lower()
                    self.assertIn(
                        "desktop", window,
                        "[K14s] %s: machine-only path not marked desktop-only nearby: %r" % (path, line),
                    )

        commands_dir = REPO / ".claude/commands"
        pipeline_related = (
            skills_dir / "tw-master/SKILL.md",
            skills_dir / "tw-balance-sim/SKILL.md",
            skills_dir / "pipeline/SKILL.md",
        )
        for path in pipeline_related:
            text = text_of(path)
            for m in re.finditer(r"[`\"](/pipeline[a-z-]*)\s", text):
                name = m.group(1)[1:]
                self.assertTrue(
                    commands_dir.exists() and (commands_dir / (name + ".md")).exists(),
                    "[K14s] %s names /%s, but .claude/commands/%s.md does not exist" % (path, name, name),
                )

        for path in skills_dir.rglob("*.md"):
            self.assertNotIn(
                "--mode editor", text_of(path), "[K14s] %s says --mode editor, not --mode EditMode" % path
            )


    def test_h12_driving_and_evidence_names_the_namespaced_aosa_keys(self):
        """[H12] the tier-script line must not send an agent to the bare aosa.dir/stem/hold keys (the script reads
        aosa.<ns>.*, so a bare key can never satisfy it) and must give the eval that resolves <ns>."""
        path = REPO / ".claude/skills/pipeline/references/driving-and-evidence.md"
        text = text_of(path)
        line = [l for l in text.splitlines() if "aosa_tiers.cs" in l]
        self.assertTrue(line, "[H12] expected driving-and-evidence.md to still name AgentScripts/aosa_tiers.cs")
        lines = text.splitlines()
        block = chr(10).join(lines[lines.index(line[0]):][:10])
        for bare in ("`aosa.dir`", "`aosa.stem`", "`aosa.hold`"):
            self.assertNotIn(bare, block, "[H12] %s names the dead key %s" % (path, bare))
        self.assertIn("dataPath", block, "[H12] expected the eval that resolves <ns> from Application.dataPath")
        self.assertIn('"x8"', block, "[H12] expected the x8 hash format of the namespace prefix")
        self.assertNotIn("Math.Abs", block, "[H12] Math.Abs(int.MinValue) throws; the eval must cast to uint")

    def test_h11_aosa_readme_names_the_per_checkout_slot(self):
        """[H11] the editor slot is per checkout since editor_lock.slot_path digests the project path, so the README
        must not call it a shared `editor-slot.json`."""
        pattern = re.search(r'"editor-slot-%s\.json"', text_of(PROJ / "Tools/editor_lock.py"))
        self.assertIsNotNone(pattern, "[H11] expected editor_lock.py to still name editor-slot-<digest>.json")
        readme = text_of(REPO / "docs/reference/aosa/README.md")
        self.assertNotRegex(readme, r"editor-slot\.json",
                            "[H11] the AOSA README still names a shared editor-slot.json")
        self.assertIn("editor-slot-", readme, "[H11] expected the README to name the per-checkout slot file")


if __name__ == "__main__":
    unittest.main(verbosity=1)
