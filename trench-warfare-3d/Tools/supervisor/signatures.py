"""Sort why a leg did not finish into a fault kind: the machine's, the harness's, or the unit's own. Pure, no IO.

    from signatures import classify, MACHINE
    kind = classify(leg_report + '\\n' + '\\n'.join(gate_lines))   # 'licence', 'gate-stall', ..., or 'unit'
    kind in MACHINE                                               # True: retrying another unit will not help

WHY. From 2026-10-31 13:00 every gate was red because a stray Unity held the machine's licence; about 70 legs wrote
so in their reports ("machine-wide licence contention, not my edit"), each ended BLOCKED, and the loop fed the next
unit for 2.5 days (~$337 for 2 passes). Nothing read the reports as one fault. This table does: the breaker counts
machine faults across units, the retry rule does not count them as a unit's try.

The patterns are rows in KINDS: a new fault is a row, not code. Order matters: the first kind that matches wins, so
the machine kinds come before the harness kinds, and 'unit' is what is left.
"""
import re

KINDS = [
    ('licence', r'licen[cs]|entitlement|com\.unity\.burst|BurstCompile|Burst(/| )(licen|compile|package|`using`)|'
                r'missing Burst|package fault|Unity\.Licensing'),
    ('broken-base', r'(Mathematics|Collections|Burst)[^.]{0,40}(not found|unresolved|cannot be found|can.t resolve)|'
                    r"can.t resolve .?Unity\.(Mathematics|Burst)|pre-existing[^.]{0,30}compil|"
                    r'compil[^.]{0,60}(not mine|unrelated|pre-existing|never touched|this leg never)|'
                    r'parent commit fails (exactly )?the same|fails to compile on .{0,40}(integration|tip)|'
                    r'will not compile|cannot compile this checkout|compiler errors on Unity\.(Mathematics|Burst)'),
    ('unity-wont-start', r'Unity (will not|won.t|cannot|can.t|never) (start|came up|come up)|editor never came up|'
                         r'no (Unity |live Unity )?editor (was reachable|can start)|editor lock is held|'
                         r'has this project open|Unity editor never|stuck duplicate Unity|'
                         r'stray (stuck )?(editor|instances|Unity)|held by stray|editor holds this worktree|'
                         r'could not get a live Unity|no desktop here'),
    ('gate-stall', r'no log growth|gate (stalled|hung|hang)|stalled .{0,40}gate|GATE FAULT|gate faulted|'
                   r'ran out its \d+ s|gate never finished|died inside .?toolcheck|toolcheck.{0,60}(hang|stall|died)|'
                   r'test_relay\.py hang|no verdict|stalled/timed out|gate could not finish|'
                   r'fails in .?toolcheck\.py.? regardless'),
    ('login-expired', r'not logged in|login (has )?expired|authentication (failed|error)|please run /login|'
                      r'claude exited 1 in \d s'),
    ('disk', r'no space left|disk (is )?full|commit headroom|out of memory|MemoryError'),
    ('board-touched', r"the board's .{1,80} changed|broke a rule: the board"),
    ('push-refused', r'push(ed)? (was |is )?(refused|rejected)|non-fast-forward|cannot push|could not be pushed|'
                     r'refused by the (live )?(permission|pre-tool) guard|no command a leg may run can push|'
                     r'pushing the rebased lane (is )?(impossible|refused)'),
    ('refused', r'refused by a hook|was refused by the (sandbox|permission)|permission classifier|'
                r'Stop-Process.{0,20}refused|hook refuses'),
    ('model-error', r'^API Error|Start a new session to continue'),
    ('leg-time', r'leg time|time budget|ran out of (leg )?time|\(red\)|context (is )?(full|limit)|'
                 r'only from running out of time'),
]
MACHINE = frozenset({'licence', 'broken-base', 'unity-wont-start', 'gate-stall', 'login-expired', 'disk'})
HARNESS = frozenset({'board-touched', 'push-refused', 'refused', 'model-error', 'leg-time'})
_RX = [(k, re.compile(p, re.I)) for k, p in KINDS]


def classify(text):
    """The first kind whose pattern is in the text, else 'unit'."""
    text = text or ''
    for kind, rx in _RX:
        if rx.search(text):
            return kind
    return 'unit'


def is_machine(kind):
    return kind in MACHINE
