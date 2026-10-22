#!/usr/bin/env python3
"""The Banner film lane is a leftover ref. Its work is on integration.

python Tools/test_banner_branch_gone.py

`lane/show/banner-day-film` landed on 2026-10-15 (decisions.md: "lands as passed").
The ref still points at the older wider top-kick: every machine takes it. Integration
keeps that kick for the Banner alone. Rebasing the ref would put the wider rule back.
The ref has to be gone locally and on origin, and the landed row has to stay.
"""
import subprocess
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True

REPO = Path(__file__).resolve().parents[2]
REF = "lane/show/banner-day-film"
ROW = "`lane/show/banner-day-film` lands as passed"


def git(*args):
    r = subprocess.run(["git", *args], cwd=REPO, capture_output=True, text=True)
    return r.returncode, r.stdout, r.stderr


def branch_names(text):
    names = []
    for ln in text.splitlines():
        ln = ln.strip().lstrip("*").strip()
        if " -> " in ln:
            ln = ln.split(" -> ", 1)[0].strip()
        names.append(ln)
    return names


class BannerBranchGone(unittest.TestCase):
    def test_the_film_ref_is_gone_locally_and_on_origin(self):
        code, out, err = git("branch", "-a")
        self.assertEqual(code, 0, err)
        wanted = {REF, "remotes/origin/" + REF}
        hits = [n for n in branch_names(out) if n in wanted]
        self.assertEqual(hits, [], "delete %s; still seeing %s" % (REF, hits))

    def test_the_landed_row_stays_on_integration(self):
        text = (REPO / "docs" / "reference" / "decisions.md").read_text(encoding="utf-8")
        self.assertIn(ROW, text)


if __name__ == "__main__":
    unittest.main()
