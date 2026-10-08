#!/usr/bin/env python3
"""What a shell command really runs, and the rules a leg's commands are held to. A command is cut into its parts
(at ; && || | & and newlines), each part into words, and wrappers are peeled off (bash -c "..", env, VAR=1, time,
xargs, a leading parenthesis), so a rule looks at the program and its arguments, never at text inside a commit
message. This is the SECOND line of defence: text rules cannot see every way to write a command. The first line is
outside the model (the git pre-push hook in prepush.py, land.py and pipeline.py refusing inside a leg, the runner's
audit after each leg).

  never(cmd, leg)        why no leg may run this, in any phase (land, claim, force-push, another lane, ...)
  skips_guard(cmd)       True when it names git's push guard or tells git to skip it (a commit message may)
  read_only_ok(cmd)      True when every part only looks (the plan, critic and retro phases)
  red_ok(cmd, relay_py)  True when it is one close-out command (context at red)
  look_only(cmd)         True for a note's prediction command
Stdlib only. ASCII only.
"""
import os, re

INTEGRATION = "claude/trench-warfare-2d-3d-plan-idt7lf"
OPS = set(";&|\n")
HEREDOC = re.compile(r"<<-?\s*(['\"]?)(\w+)\1[^\n]*\n.*?\n\s*\2\b", re.S)
HARMLESS = {"2>&1", "1>&2", "2>/dev/null", ">/dev/null", "2>nul", ">nul", "2>$null", "&>/dev/null"}
SKIP_WORDS = {"(", "{", "!", "then", "do", "else", "elif", "if", "while", "until", "time", "nohup", "command", "exec",
              "sudo", "env", "xargs", "builtin", "&", "start-process", "start", "timeout", "nice", "winpty", "stdbuf",
              "done", "fi", "esac"}
VALUE_FLAGS = {"env": ("-u", "--unset", "-C", "--chdir"), "xargs": ("-I", "-n", "-P", "-L", "-d", "-E", "-s", "-a"),
               "sudo": ("-u", "-g"), "timeout": ("-s", "-k", "--signal", "--kill-after"), "nice": ("-n",),
               "start-process": (), "start": (), "stdbuf": ()}       # wrappers with flags; these take a value
ASSIGN = re.compile(r"^(?:\$env:)?([A-Za-z_]\w*)=")
ENV_OK = {"GIT_EDITOR", "GIT_SEQUENCE_EDITOR", "GIT_PAGER", "GIT_TERMINAL_PROMPT", "GIT_MERGE_AUTOEDIT",
          "GIT_AUTHOR_DATE", "GIT_COMMITTER_DATE", "GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL", "GIT_COMMITTER_NAME",
          "GIT_COMMITTER_EMAIL", "GIT_OPTIONAL_LOCKS", "GIT_LFS_SKIP_SMUDGE"}
RELAY_VARS = ("TW_RELAY", "TW_WORKER_PID", "TW_BOARD", "TW_RUNS")
GUARD_WORDS = ("relay-leg.json", "hooks/pre-push")
PUSH_FLAGS = ("-u", "--set-upstream", "-q", "--quiet", "-v", "--verbose", "--progress")
SUB_LOOK = {"merge-base": None, "rev-parse": ("--short", "--verify", "-q", "--abbrev-ref", "--show-toplevel",
                                              "--git-dir", "--absolute-git-dir", "--git-common-dir")}
SHELLS = {"bash": ("-c", "-lc"), "sh": ("-c",), "zsh": ("-c",), "cmd": ("/c", "/k"), "wsl": ("--", "-e"),
          "powershell": ("-command", "-c"), "pwsh": ("-command", "-c")}
EVALS = {"eval", "iex", "invoke-expression"}
GIT_LOOK = {"status", "diff", "log", "show", "rev-parse", "ls-files", "ls-tree", "cat-file", "grep", "blame",
            "merge-base", "describe", "rev-list", "shortlog", "diff-tree", "name-rev", "check-ignore", "for-each-ref",
            "merge-tree", "reflog", "show-ref", "ls-remote", "version", "help", "count-objects", "whatchanged"}
GIT_WORK = {"fetch", "branch", "worktree", "add", "commit", "push", "pull", "rebase", "restore", "rm", "mv", "reset",
            "checkout", "switch", "stash", "apply", "cherry-pick", "revert", "tag", "remote", "config", "merge",
            "init", "clone", "symbolic-ref", "update-index", "read-tree", "write-tree", "format-patch", "am",
            "submodule", "lfs", "gc", "bisect", "notes", "clean"}
GIT_BAD_ARG = ("--output", "--ext-diff", "--no-index", "--exec", "--upload-pack", "--receive-pack", "-O",
               "--open-files-in-pager")
BAD_CONFIG = ("alias.", "core.hookspath", "core.sshcommand", "remote.", "url.", "credential", "core.fsmonitor",
              "core.editor", "core.pager")
LOOK_PROGS = {"ls", "cat", "head", "tail", "grep", "rg", "wc", "pwd", "dir", "type", "sort", "uniq", "cut", "echo",
              "get-content", "get-childitem", "select-string", "gc", "gci", "sls", "measure-object", "test-path",
              "which", "findstr", "true", "sed", "awk", "jq", "diff", "stat", "file", "du", "tree", "date", "cd",
              "test", "basename", "dirname", "realpath", "false", "nl", "tr", "tac", "rev", "comm", "cmp", "column",
              "printf", "seq", "expr", "fold", "md5sum", "sha1sum", "sha256sum", "cksum", "xxd", "od", "strings",
              "whoami", "hostname", "uname", "id", "printenv", "where", "df", "egrep", "fgrep", "zcat", "readlink",
              "get-item", "get-itemproperty", "get-location", "get-date", "get-filehash", "get-command",
              "resolve-path", "join-path", "split-path", "select-object", "select", "sort-object", "group-object",
              "format-table", "format-list", "ft", "fl", "out-string", "out-host", "write-output", "write-host",
              "set-location", "push-location", "pop-location", "get-process", "convertfrom-json", "compare-object"}
LOOK_SCRIPTS = {"pipeline.py": ("status", "why", "next"), "health.py": None, "validate.py": None,
                "codemap.py": ("--check",), "run_detached.py": ("status",), "aosa.py": ("status", "pick"),
                "editor_lock.py": ("status", "guard")}       # the lock is how a leg asks "is the project free"
PYTHONS = {"python", "python3", "py", "pythonw"}
PY_WRITES = re.compile(r"open\s*\([^)]*,\s*['\"][^'\"]*[wax+]|mode\s*=\s*['\"][^'\"]*[wax+]|\.open\s*\(\s*['\"][^'\"]*[wax+]"
                       r"|\.write|write_(text|bytes)|unlink|rmtree|rmdir|remove\s*\(|rename|mkdir|makedirs|touch\s*\("
                       r"|chmod|subprocess|os\.system|popen|shutil|exec\s*\(|eval\s*\(|__import__|importlib|socket"
                       r"|urllib|requests|ctypes|environ\s*\[|putenv|input\s*\(|\bpip\b", re.I)
PY_RUNS = re.compile(r"exec\s*\(|runpy|run_path|subprocess|os\.system|popen|import_module|spec_from_file|__import__",
                     re.I)


def norm_path(p):
    p = str(p).strip("\"'").replace("\\", "/")
    for lead in ("//?/", "//./"):
        if p.startswith(lead):
            p = p[len(lead):]
    m = re.match(r"^/([a-zA-Z])/(.*)$", p)                  # Git Bash: /c/Users -> c:/Users
    if m:
        p = "%s:/%s" % (m.group(1), m.group(2))
    try:
        p = os.path.realpath(p)                             # long names, junctions and .. resolved
    except OSError:
        p = os.path.abspath(p)
    return os.path.normcase(p).replace("\\", "/").rstrip("/")


def parts(cmd):
    """[[word, ...], ...]: the command cut at its operators. A heredoc body is text, not commands."""
    cmd = HEREDOC.sub(" HEREDOC ", cmd.replace("\r", ""))
    out, cur, word, quote, i, n = [], [], "", None, 0, len(cmd)

    def end_part():
        nonlocal cur, word
        if word:
            cur.append(word)
        if cur:
            out.append(cur)
        cur, word = [], ""

    while i < n:                                            # read it the way a shell does: quotes, comments, escapes
        c, nxt = cmd[i], cmd[i + 1] if i + 1 < n else ""
        if quote:
            word += c
            if c == quote:
                quote = None
            elif c == "\\" and quote == '"' and nxt:
                word, i = word + nxt, i + 1
        elif c in "\"'":
            quote, word = c, word + c
        elif c == "\\" and nxt == "\n":
            i += 1                                          # a line continuation
        elif c == "\\" and nxt == " ":
            word, i = word + " ", i + 1                     # an escaped space stays in the word
        elif c == "\\" and nxt and nxt in "\"'#;&|":
            word, i = word + c + nxt, i + 1                 # an escaped quote or operator is text: it opens nothing
        elif c in " \t":
            if word:
                cur.append(word)
            word = ""
        elif c == "#" and not word:
            while i + 1 < n and cmd[i + 1] != "\n":         # a comment runs to the end of the line
                i += 1
        elif c in ";\n":
            end_part()
        elif c == "|":
            end_part()
            i += nxt == "|"
        elif c == "&":
            if word.endswith(">") or nxt == ">":
                word += c                                   # 2>&1 and &> are redirects, not operators
            else:
                end_part()
                i += nxt == "&"
        else:
            word += c
        i += 1
    end_part()
    return out


def bare(word):
    """The word without the one pair of quotes around it."""
    if len(word) >= 2 and word[0] in "\"'" and word[-1] == word[0]:
        return word[1:-1]
    return word.strip("\"'") if word.count('"') + word.count("'") == 1 else word


def prog(words):
    name = os.path.basename(words[0].replace('"', "").replace("'", "").replace("\\", "/")).lower()
    for end in (".exe", ".cmd", ".bat"):
        if name.endswith(end):
            name = name[:-len(end)]
    return name


def unwrap(words, depth=0, env=None):
    """The real commands inside one part: wrappers and shell keywords peeled off, `bash -c ".."` opened up. env, when
    given, collects the variables set (NAME) or removed (-NAME) in front of the command."""
    words = list(words)
    while words:
        w = bare(words[0])
        low = w.lower()
        if low == "for" and len(words) > 2 and bare(words[2]) == "in":
            return []                                       # `for f in a b c`: a list of words, no command
        m = ASSIGN.match(w)
        if m and env is not None:
            env.append(m.group(1))
        if low in SKIP_WORDS or m:
            words = words[1:]
            takes = VALUE_FLAGS.get(low)
            while takes is not None and words and bare(words[0]).startswith("-"):
                flag = bare(words[0])
                if low == "env" and flag in ("-S", "--split-string"):       # the rest is one command line
                    out = []
                    for part in parts(" ".join(bare(x) for x in words[1:])):
                        out += unwrap(part, depth + 1, env)
                    return out
                if low == "env" and env is not None and flag.startswith(("-u", "--unset")):
                    env.append("-" + (flag.split("=", 1)[1] if "=" in flag else bare(words[1]) if len(words) > 1 else ""))
                words = words[2:] if flag in takes else words[1:]
            if low == "timeout" and words and re.match(r"^\d+(\.\d+)?[smhd]?$", bare(words[0])):
                words = words[1:]
        elif w[:1] in "({" and len(w) > 1:
            words[0] = w.lstrip("({")
        else:
            break
    if not words:
        return []
    p = prog(words)
    rest = [bare(w) for w in words[1:]]
    inner = None
    if p in SHELLS and depth < 4:
        for i, w in enumerate(rest):
            if w.lower() in SHELLS[p]:
                inner = " ".join(rest[i + 1:])
                break
        if inner is None and p in ("powershell", "pwsh") and rest and "-file" not in [r.lower() for r in rest]:
            inner = " ".join(r for r in rest if not r.startswith("-"))
    elif p in EVALS and depth < 4:
        inner = " ".join(rest)
    if inner is not None:
        out = []
        for part in parts(inner):
            out += unwrap(part, depth + 1, env)
        return out
    if env is not None:                                     # export NAME=.., unset NAME, $env:NAME = .., env:NAME
        if p in ("export", "declare", "typeset", "setx", "set"):
            env += [m.group(1) for m in (ASSIGN.match(r) for r in rest) if m]
        elif p == "unset":
            env += ["-" + r for r in rest if not r.startswith("-")]
        env += [m.group(1) for m in (re.search(r"env:[\\/]?(\w+)", bare(w), re.I) for w in words) if m]
    return [words]


def commands(cmd, env=None):
    out = []
    for part in parts(cmd):
        out += unwrap(part, 0, env)
    return out


def substitutions(cmd):
    """(the command with every $( ) and backtick part replaced by the word SUB, [the commands inside them]).
    Text in single quotes is not a command. A nested one stays in its parent's text: the caller looks again."""
    cmd = HEREDOC.sub(" HEREDOC ", cmd.replace("\r", ""))
    out, inner, i, n, sq, dq = "", [], 0, len(cmd), False, False
    while i < n:
        c = cmd[i]
        if sq:
            sq = c != "'"
        elif c == "\\" and i + 1 < n:
            out, i = out + c + cmd[i + 1], i + 2
            continue
        elif c == "'" and not dq:
            sq = True
        elif c == '"':
            dq = not dq
        elif c == "`":
            j = cmd.find("`", i + 1)
            j = n if j < 0 else j
            inner.append(cmd[i + 1:j])
            out, i = out + "SUB", j + 1
            continue
        elif c == "$" and cmd[i + 1:i + 2] == "(":
            depth, j = 1, i + 2
            while j < n and depth:
                depth += (cmd[j] == "(") - (cmd[j] == ")")
                j += 1
            inner.append(cmd[i + 2:j if depth else j - 1])
            out, i = out + "SUB", j
            continue
        out += c
        i += 1
    return out, inner


def redirects(words):
    """Words that send output to a file or read one in (quoted words are text, the harmless ones are dropped)."""
    out = []
    for i, w in enumerate(words):
        if w[:1] in "\"'" or w.lower() in HARMLESS:
            continue
        if re.search(r"[<>]", w) and not (w in (">", "2>") and i + 1 < len(words)
                                         and bare(words[i + 1]).lower() in ("/dev/null", "nul", "$null")):
            out.append(w)
    return out


def plain(words):
    """The words without redirects, quotes stripped."""
    skip, out = False, []
    for w in words:
        if skip:
            skip = False
        elif w in (">", "2>", ">>"):
            skip = True
        elif w[:1] in "\"'" or not re.search(r"[<>]", w):
            out.append(bare(w))
    return out


def git_parse(words):
    """(subcommand, its arguments, the -C folder or None, the -c keys, words before the subcommand)."""
    cdir, keys, i = None, [], 1
    while i < len(words):
        w = bare(words[i])
        if w == "-C" and i + 1 < len(words):
            cdir, i = bare(words[i + 1]), i + 2
        elif w == "-c" and i + 1 < len(words):
            keys.append(bare(words[i + 1]).split("=")[0].lower())
            i += 2
        elif w.startswith("-"):
            if w.startswith(("--git-dir", "--work-tree", "--exec-path", "--config-env")):
                keys.append("core.hookspath")               # treated like a setting no leg may change
            i += 1
        else:
            break
    sub = bare(words[i]).lower() if i < len(words) else ""
    return sub, plain(words[i + 1:]), cdir, keys, words[:i + 1]


def scripts(words):
    return [os.path.basename(bare(w).replace("\\", "/")).lower() for w in words[1:]]


def is_board(folder, leg):
    return bool(leg.get("board")) and norm_path(folder) == norm_path(leg["board"])


def is_own(folder, leg):
    """True when the folder is the leg's own checkout (or inside it); a relative one is read from the checkout."""
    wt = leg.get("worktree")
    if not wt:
        return False
    f = str(folder).strip("\"'")
    if not (os.path.isabs(f) or f.startswith(("/", "\\"))):
        f = os.path.join(wt, f)
    a, b = norm_path(f), norm_path(wt)
    return a == b or a.startswith(b + "/")


def push_ok(args, cdir, leg):
    """A push goes to the leg's own lane: named, or left to git as the current branch (`git push`, `origin HEAD`).
    git's pre-push hook holds whatever git makes of it to the lane. Or it is `git -C <board> push`, no refspec."""
    if cdir is not None:
        return is_board(cdir, leg) and not args
    rest, lane = [a for a in args if a not in PUSH_FLAGS], leg.get("lane")
    return bool(lane) and (rest in ([], ["origin"]) or rest[:1] == ["origin"] and len(rest) == 2 and rest[1] in (
        lane, "HEAD", "HEAD:" + lane, "HEAD:refs/heads/" + lane, "%s:%s" % (lane, lane)))


def never_git(words, leg):
    sub, args, cdir, keys, lead = git_parse(words)
    lane, home = leg.get("lane"), "origin/" + INTEGRATION
    if any("$" in w or "`" in w for w in lead):
        return "git with a shell variable before its command"
    if cdir is not None and is_own(cdir, leg):
        cdir = None                                         # git -C . : the leg's own checkout, named
    if not sub:
        return None
    if sub not in GIT_LOOK and sub not in GIT_WORK:
        return "unknown git command '%s' (an alias?)" % sub
    if any(k.startswith(BAD_CONFIG) for k in keys):
        return "no per-command git setting like that (-c %s)" % keys[0]
    if sub == "push":
        return None if push_ok(args, cdir, leg) else \
            "push only your own lane: git push origin %s" % lane
    if cdir is not None and sub not in GIT_LOOK and not is_board(cdir, leg):
        return "a leg changes only its own checkout and the board (git -C %s %s)" % (cdir, sub)
    if sub == "commit" and "--amend" in args:
        return "a leg does not rewrite history (commit --amend)"
    if sub == "reset" and any(a in ("--hard", "--keep", "--merge") or "~" in a or "^" in a for a in args):
        return "a leg does not throw commits or work away (git reset)"
    if sub == "branch" and any(a in ("-f", "-D", "-M", "-m", "--force", "-d", "--delete") for a in args):
        return "a leg does not move or delete branches"
    if sub == "rebase" and any(a in ("-i", "--interactive") for a in args):
        return "no interactive rebase"
    if sub == "rebase" and not any(a in (home, "origin/%s" % lane, "--continue", "--abort", "--skip") for a in args):
        return "rebase only onto %s" % home
    if sub == "merge" and not any(a in ("--abort", "--continue") for a in args):
        return "lanes rebase, they never merge"
    if sub == "pull" and [a for a in args if not a.startswith("-")] not in ([], ["origin"], ["origin", lane],
                                                                              ["origin", INTEGRATION]):
        return "pull only your own lane or the integration branch"
    if sub in ("switch", "checkout") and "--" not in args:
        if any(a in ("-B", "-C") for a in args) or any((a.startswith("lane/") or a in ("main", INTEGRATION))
                                                        and a != lane for a in args):
            return "a leg stays on its own lane"
    if sub == "stash" and args[:1] and args[0] in ("drop", "clear", "pop"):
        return "the stash is shared by every checkout: no stash %s" % args[0]
    if sub == "worktree" and args[:1] and args[0] != "list":
        return "a leg does not add or remove checkouts"
    if sub == "remote" and args[:1] and args[0] in ("set-url", "add", "remove", "rm", "rename"):
        return "a leg does not change where origin points"
    if sub == "config" and not (any(a in ("--get", "--list", "-l", "--get-all") for a in args)
                                or len([a for a in args if not a.startswith("-")]) == 1):
        return "a leg does not change git settings"
    if sub == "clean":
        return "a leg does not run git clean"
    if sub == "fetch" and any(":" in a for a in args):
        return "fetch does not write branches"
    return None


def never_part(words, leg):
    p = prog(words)
    if p == "git":
        return never_git(words, leg)
    raw = [bare(w) for w in words[1:]]
    low = [r.lower() for r in raw]
    if p in PYTHONS or p.endswith(".py"):
        sc = scripts(words) + ([p] if p.endswith(".py") else [])
        mod = [low[i + 1] for i, w in enumerate(raw[:-1]) if w == "-m"]
        code = " ".join(raw[i + 1] for i, w in enumerate(raw[:-1]) if w == "-c").lower()
        if ("land.py" in sc or any(m.split(".")[-1] == "land" for m in mod)
                or re.search(r"\bimport\s+land\b|\bfrom\s+land\b|\bland\.main\b", code)
                or "land.py" in code and PY_RUNS.search(code)):
            return "legs never land"
        if (("pipeline.py" in sc or any(m.split(".")[-1] == "pipeline" for m in mod))
                and any(a in ("claim", "complete", "release") for a in low)):
            return "the runner claims and completes pipeline jobs"
        if "run_detached.py" in sc and "--" in raw:
            return never(" ".join(words[words.index("--") + 1:] if "--" in words else []), leg)
    elif p == "gh":
        if low[:2] == ["pr", "merge"] or (low[:1] == ["api"] and any("merge" in a for a in low)):
            return "legs never merge a pull request"
    return None


def never_env(names):
    for n in names:
        raw = n.lstrip("-").upper()
        if raw.startswith("TW_RELAY") or raw in RELAY_VARS:
            return "a leg does not change the relay's own variables (%s)" % raw
        if raw.startswith("GIT_") and raw not in ENV_OK:
            return "no git variable like %s in front of a command" % raw
    return None


def never(cmd, leg, depth=0):
    env = []
    for words in commands(cmd, env):
        why = never_part(words, leg)
        if why:
            return why
    why = never_env(env)
    if why or depth >= 4:
        return why
    for inner in substitutions(cmd)[1]:                     # what runs inside $( ) and backticks is a command too
        why = never(inner, leg, depth + 1)
        if why:
            return why
    return None


def skips_guard(cmd, depth=0):
    """True when a command names git's push guard (the hook, the leg marker) or tells git to skip it. The text of a
    commit message is not a command: `git commit -m "explain --no-verify"` is fine."""
    for words in commands(cmd):
        is_git, skip = prog(words) == "git", False
        for w in words[1:]:
            b = bare(w).replace("\\", "/").lower()
            if skip:
                skip = False
            elif is_git and b in ("-m", "--message"):
                skip = True
            elif is_git and (b.startswith("--message=") or b.startswith("-m") and not b.startswith("--")):
                pass
            elif b == "--no-verify" or any(g in b for g in GUARD_WORDS):
                return True
    return depth < 4 and any(skips_guard(s, depth + 1) for s in substitutions(cmd)[1])


def look_part(words):
    if redirects(words) or any(c in w for w in words if w[:1] != "'" for c in ("`", "$(")):
        return False
    p, args = prog(words), plain(words[1:])
    if p == "git":
        sub, args, cdir, keys, lead = git_parse(words)
        if keys or any(a.startswith(GIT_BAD_ARG) for a in args):
            return False
        if not sub:
            return [bare(w) for w in words[1:]] in (["--version"], ["--help"], ["-v"], ["-h"])
        if sub == "branch":
            return all(a in ("--list", "-a", "-r", "-v", "-vv", "--show-current", "--contains", "--merged")
                       or not a.startswith("-") and args[0] in ("--contains", "--merged") for a in args)
        if sub in ("worktree", "stash", "tag"):
            return args[:1] in (["list"], ["--list"], ["-l"])
        if sub == "remote":
            return args in ([], ["-v"])
        if sub == "fetch":
            return args in ([], ["origin"], ["-q"], ["-q", "origin"])
        if sub == "config":
            return any(a in ("--get", "--list", "-l") for a in args)
        return sub in GIT_LOOK
    if any(c in w for w in words[1:] if w[:1] not in "\"'" for c in "({"):
        return False                                        # a script block or a sub-expression can do anything
    if p in PYTHONS:
        if "-m" in args:
            return False
        if "-c" in args:                                    # a one-liner that reads (json, counts): no write, no run
            code = args[args.index("-c") + 1] if args.index("-c") + 1 < len(args) else ""
            return args.index("-c") == 0 and bool(code.strip()) and not PY_WRITES.search(code)
        if args in (["--version"], ["-V"]):
            return True
        py = [a for a in args if a.lower().endswith(".py")]
        name = os.path.basename(py[0].replace("\\", "/")).lower() if len(py) == 1 else ""
        if name not in LOOK_SCRIPTS or not re.search(r"(^|/)Tools/([\w-]+/)?[\w.-]+$", py[0].replace("\\", "/")):
            return False
        after = [a for a in args[args.index(py[0]) + 1:] if not a.startswith("-") or a == "--check"]
        return LOOK_SCRIPTS[name] is None or after[:1] != [] and after[0] in LOOK_SCRIPTS[name]
    if p == "find":
        return not any(a.startswith(("-delete", "-exec", "-fprint", "-fls", "-ok")) for a in args)
    if p == "sort":
        return not any(a == "-o" or a.startswith("--output") for a in args)
    if p == "uniq":
        return len([a for a in args if not a.startswith("-")]) <= 1
    if p == "sed":
        return not any(a.startswith("-i") or a == "--in-place" for a in args) and "w " not in " ".join(args)
    if p == "awk":
        return not re.search(r"system\s*\(|>|\|", " ".join(args))
    if p == "rg":
        return not any(a.startswith("--pre") for a in args)
    return p in LOOK_PROGS


def sub_look(cmd):
    """A command whose output may be pasted into a look command: it prints commit ids or names, never an option."""
    cs = commands(cmd)
    if len(parts(cmd)) != 1 or len(cs) != 1 or prog(cs[0]) != "git" or not look_part(cs[0]):
        return False
    sub, args, cdir, keys, lead = git_parse(cs[0])
    return sub in SUB_LOOK and all(not a.startswith("-") or a in (SUB_LOOK[sub] or ()) for a in args)


def leg_done_part(words, relay_py):
    """`python <relay.py> leg done`: the one relay command a leg that only reads may run (it checks its paper)."""
    w = plain(words)
    return (bool(relay_py) and prog(words) in PYTHONS and not redirects(words) and len(w) == 4
            and norm_path(w[1]) == norm_path(relay_py) and w[2:] == ["leg", "done"])


def read_only_ok(cmd, relay_py=None):
    """Every line and every part only looks, or is the leg's own `leg done`. A $( ) may hold one git look-up
    (merge-base, rev-parse); nothing in front of a command sets a variable."""
    flat, inner = substitutions(cmd)
    if "`" in cmd or not all(sub_look(s) for s in inner):
        return False
    env = []
    cs = commands(flat, env)
    return bool(cs) and not env and all(look_part(w) or leg_done_part(w, relay_py) for w in cs)


def red_ok(cmd, relay_py):
    cs = commands(cmd)
    if "\n" in cmd.strip() or len(parts(cmd)) != 1 or len(cs) != 1:
        return False
    words = cs[0]
    if redirects(words) or any("`" in w or "$(" in w for w in words):
        return False
    if prog(words) == "git":
        sub, args, cdir, keys, lead = git_parse(words)
        return (look_part(words) and sub in ("status", "diff", "log", "rev-parse") and cdir is None
                and not any(a in ("-p", "--patch", "--all") or a.startswith("-U") for a in args))
    if prog(words) in PYTHONS and len(words) >= 4:
        return (norm_path(words[1]) == norm_path(relay_py) and bare(words[2]) == "leg"
                and bare(words[3]) in ("gate", "finish", "done"))
    return False


def look_only(cmd):
    """A prediction command: one part, a git look command or a look script, nothing that writes."""
    cs = commands(cmd)
    if "\n" in cmd or len(parts(cmd)) != 1 or len(cs) != 1 or not look_part(cs[0]):
        return False
    if prog(cs[0]) == "git":
        return git_parse(cs[0])[0] in ("status", "log", "rev-parse", "diff", "ls-files", "cat-file")
    return prog(cs[0]) in PYTHONS


def argv(cmd):
    """The words of a one-part command, quotes stripped: what to hand to subprocess."""
    return [bare(w) for w in parts(cmd)[0]]
