"""May a new leg or a new relay start? Asks the breaker (modules/breaker.py) through its state file. Stdlib only.

    import breaker_gate
    ok, why = breaker_gate.allow()        # (False, reason) only when the breaker's switch is 'on', the canary's
                                          # too (only a green canary closes it), and it is open

Hooked in two places, one check each: Tools/relay/runner.py (Run.no_room, before every leg: a refused leg ends the run
with reason_kind 'breaker') and Tools/assetboard/autoloop.py (World.start, before a relay is started). With the
breaker's switch at dry or off (dry is its default) this always says yes, so the hooks change nothing until the owner
switches it on in the supervisor's modules.json.

It fails open: a state file that cannot be read says yes, so a fault in the supervisor never stops the relays.
"""
import importlib.util
from pathlib import Path

_core = None


def core():
    """core.py, loaded by its path: the relay's and the loop's folders have their own modules, and no name of theirs
    may be shadowed by importing this one."""
    global _core
    if _core is None:
        spec = importlib.util.spec_from_file_location('tw_sup_core_for_gate', Path(__file__).resolve().parent / 'core.py')
        _core = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(_core)
    return _core


def allow(home=None):
    try:
        c = core()
        h = Path(home) if home else c.home()
        switches = c.read_json(h / 'modules.json', {}) or {}
        switch = switches.get('breaker', 'dry')
        if switch != 'on':
            return True, f'the breaker is {switch}'
        if switches.get('canary', 'dry') != 'on':      # only a green canary closes it: without one it never would
            return True, 'the breaker is on but the canary is not, so nothing could close it: it holds nothing'
        st = c.read_state('breaker', h) or {}
        if st.get('open'):
            return False, f"breaker open ({st.get('kind')}): {st.get('reason')}"
        return True, 'the breaker is closed'
    except Exception as e:  # noqa: BLE001 - fail open
        return True, f'the breaker could not be read ({type(e).__name__})'
