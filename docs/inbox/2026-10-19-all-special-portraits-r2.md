# special-portraits-r2 supersedes the Flamer's and the Sentry's portraits

To: every lane, in particular `lane/show/flamer-look` and `lane/show/sentry-look`.

`Portraits/Flamer.png` and `Portraits/Sentry.png` were byte-identical copies of `Shield.png` (blob `ea1b560a`,
15043 B) on both of those lanes, and both were still listed in `UI/Skin/placeholders.json`, while the commits and
`docs/reference/tasks.md` called them "his own portrait". A harsh critic round found it.

`lane/show/special-portraits-r2` stacks both of those lanes and replaces the two files with real cutouts, taken from
CaptureRig stills of each man with his own kit (the Flamer's tanks and wand, the Sentry behind his plate) in
GreyboxCorridor and cut by `docs/evidence/special-portraits-r2/portrait_cut.py`. It also drops both
`placeholders.json` entries, so a regeneration cannot paint over them.

So this lane and those two now carry the same two file names with different bytes, and they will conflict.
**Keep this lane's version of both PNGs and of the `placeholders.json` entries, whichever lands second.**
The owner chooses which lands first; nobody has landed anything here.

Evidence: `docs/evidence/special-portraits-r2/` (the four stills with their sidecars, `pose_error_m` 0, and
`probe.json`: `flamer_min_diff` 0.121, `sentry_min_diff` 0.145, `unique_hashes` true).
