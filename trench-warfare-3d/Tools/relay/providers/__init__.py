#!/usr/bin/env python3
"""The other vendors' command-line agents, for second opinions (second.py). One module per vendor, the same shape:

  find()            the executable as a list; TW_RELAY_<VENDOR> (a JSON list) replaces it: the tests use a stand-in
  argv(job)         one turn, read-only, the output as JSON lines
  env(job)          variables the run needs on top of the caller's
  stdin(job)        the file to pipe in as the prompt, or None when the prompt travels in argv
  read_only(argv)   why this command line is not held to reading, or None: second.py starts nothing that fails it
  read(out, job)    the run's own output as the keys a leg record uses, plus `calls` (what it did) and `ok`
  distrust(rec)     why the run cannot be trusted, as short strings (empty = trusted)
job: {"cwd", "prompt", "last", "model", "images", "max_turns"}, all paths as strings.
Nothing here is used by a Claude leg: launch.py is the path for those. Stdlib only. ASCII only.
"""
import importlib, json, os

NAMES = ("grok", "codex")


def load(name):
    if name not in NAMES:
        raise SystemExit("second: no vendor named %r (known: %s)" % (name, ", ".join(NAMES)))
    return importlib.import_module("providers." + name)


def stub(env_name):
    """The stand-in command from the environment, or None."""
    raw = os.environ.get(env_name)
    return json.loads(raw) if raw else None


def lines(path):
    """The JSON objects of a JSON-lines file; a line that is not JSON is skipped (a CLI may print a warning)."""
    out = []
    try:
        with open(path, "rb") as f:
            for line in f:
                line = line.strip()
                if line[:1] != b"{":
                    continue
                try:
                    out.append(json.loads(line.decode("utf-8", "replace")))
                except ValueError:
                    continue
    except OSError:
        pass
    return out


def flag(argv, name):
    """The value after a flag, or None."""
    return argv[argv.index(name) + 1] if name in argv and argv.index(name) + 1 < len(argv) else None
