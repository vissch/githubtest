# Trench Warfare 3D: for an agent that is not Claude Code

Read `CLAUDE.md` at the repo root first, all of it. It is the working agreement for every agent, whatever the
vendor (Codex, Grok Build, any other): lanes, the seam, the gate, commits. This page copies none of it. It says
what the Claude Code words in that file and in the skills mean for you. Claude Code itself loads `CLAUDE.md`, not
this page.

- **Skills** are folders, `.claude/skills/<name>/`: a `SKILL.md` (front matter `name` and `description`, then the
  method), often with `references/` beside it. The list is that folder: read the `description` lines, and when
  one matches your task read its `SKILL.md` by path before you start. "Load the skill `tw-review`" means read
  `.claude/skills/tw-review/SKILL.md`; a `/pipeline ...` line is a request to the skill `pipeline`, not a shell
  command. Which skill a pipeline role takes: the role table in `.claude/skills/pipeline/SKILL.md`. A skill with
  no folder there (`unity-package-management`) ships with Claude Code's Unity plugin: without it, ask the owner
  before you do that job.
- **Agents.** `.claude/agents/` holds three role texts: `tw-critic` (a critic round), `tw-reviewer` (a review
  unit, or the second reader of a fix) and `tw-scout` (search and mapping). Claude Code starts them by name. A
  tool that cannot reads the file and takes that role, in a fresh session or sub-agent that has not seen your own
  work on the thing it judges: the text under the front matter is the brief, and `skills:` names the skill to
  read with it. `tools:` says the role only reads: it changes nothing. `model:` is Claude Code's: take your
  nearest. "In the foreground" means: wait for its report before you go on.
- **"AskUserQuestion", "ask the owner":** ask in the chat, one decision per question. A decision that waits goes
  to him as a brief on the board's Decide page: `CLAUDE.md` gives the command in the row "what the owner decided,
  and what is still waiting on them" of its table, and the rest under "Asking and remembering".
- **Landing a lane and any push to the integration branch are the owner's.** `CLAUDE.md`, "Integration", says how
  a lane is made ready. Push your own lane branch and nothing else.
