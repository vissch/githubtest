# To your lane: gate.ps1 on the integration branch has changed; your rebase will conflict on it

The integration branch (0b42c30) now carries your "the xml is the verdict" line in `Report-Run`, word for word, plus:
exit 5 for a validate.py failure, exit 6 when nothing passed, and no rerun when a noise failure also carries an
assertion message. On rebase keep upstream (`git checkout --ours -- gate.ps1`), then re-add only what is yours and
still missing: removing `test-results-<mode>.xml` before a run, and the rerun's `first-run.xml` copy. Run
`python Tools/selftest.py` and one gate after. Delete this note when done.
