# The push guard is loosened on this lane, but the live copy has not picked it up

2026-10-16, from the `fallback-push-heroes-off-bench` leg on `lane/show/heroes-off-bench`. For every lane.

The owner decided (Decide page, 2026-10-16): "Loosen the guard to allow a fallback push under a new branch name".
That is done, in commit `90199269b` on this lane, in both layers that must agree:

- `Tools/relay/cmdrules.py`, `push_ok`: now also accepts `origin HEAD:<lane>-<suffix>` and
  `origin HEAD:refs/heads/<lane>-<suffix>`, with the suffix matching `[a-z0-9][a-z0-9.-]*` (anchored, so
  `<lane>-b/y` and another lane's name stay refused). Before this, it took only the lane itself.
- `Tools/relay/prepush.py`, `rebased_name`: the old `-[b-j]` is now the same suffix pattern. It still demands a
  branch that does not exist yet and still refuses a delete.

**What is left to do, and it is not a leg's to do.** The rules that actually run on this machine are the relay-run
checkout's copies (`C:/Users/PC/Documents/GitHub/githubtest-relay-run/trench-warfare-3d/Tools/relay/`), which a leg
may not edit. They still carry the old `push_ok`, so `git push origin HEAD:refs/heads/lane/show/heroes-off-bench-e`
was refused before git ran, in this leg, exactly once. Until this lane's change reaches that checkout, no leg can
push a fallback name, and a leg that tries will end BLOCKED the same way.

**Nothing is lost meanwhile.** The heroes-off-bench work is on origin as `lane/show/heroes-off-bench-d`
(`7b732d400`), an ancestor of integration. The stale `origin/lane/show/heroes-off-bench` (`1eb8ac418`) is still
there; clearing it was the other option the owner did not take, so leave it.

Delete this note when the relay-run checkout runs the loosened rules.
