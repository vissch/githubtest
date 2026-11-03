#!/usr/bin/env python3
"""Tests of the registry (registry.py, modules/registry_refresh.py) and its hooks. Run: python Tools/supervisor/test_registry.py

The replay proof: from 10-19 to 11-02 the relays ran 133 legs on 28 units the Grok worker had already pushed on
lane/show/grok-<id> (fixtures/grok_done_20261103.json, from its DONE.md and the commits' times). Today's picker asks
only <board>/relay/done, so it skipped none of them; the registry skips every leg that started after its refresh
could have seen the Grok commit."""
import datetime
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import core  # noqa: E402
import hooks  # noqa: E402
import registry  # noqa: E402
from replay import records  # noqa: E402

GROK = HERE / 'replay' / 'fixtures' / 'grok_done_20261103.json'
INT = registry.INTEGRATION


def utc(s):
    return datetime.datetime.fromisoformat(s.replace('Z', '+00:00'))


def git(repo, *a):
    r = subprocess.run(['git', '-C', str(repo), *a], capture_output=True, text=True)
    assert r.returncode == 0, (a, r.stderr)
    return r.stdout.strip()


class Tags(unittest.TestCase):
    def test_only_the_tags_that_open_the_subject_count(self):
        self.assertEqual(registry.tags('[fix-job-ordering-tests] [allow-forced-push-own-lane] relay: a leg may'),
                         {'fix-job-ordering-tests', 'allow-forced-push-own-lane'})
        self.assertEqual(registry.tags('[sup-core] the [old-id] check reads the board'), {'sup-core'})
        self.assertEqual(registry.tags('relay: notes for [x] and [y]'), set())
        self.assertEqual(registry.tags('Revert "[x] something"'), set())
        self.assertEqual(registry.tags('[a b] spaces are no id'), set())


class Where(unittest.TestCase):
    cache = dict(boards={'b-done': 'board:tw3d-board-3'}, integration={'landed': 'aaaaaaaaaaaa'},
                 lanes={'lane/show/grok-g1': {'g1': 'bbbbbbbbbbbb'}, 'lane/show/review-tools': {'r1': 'cccccccccccc'},
                        'lane/show/found': {'mentioned': 'dddddddddddd'}})

    def test_each_source(self):
        w = registry.where_done
        self.assertEqual(w('b-done', cache=self.cache), 'board:tw3d-board-3')
        self.assertEqual(w('landed', cache=self.cache), 'integration:aaaaaaaaaa')
        self.assertEqual(w('g1', cache=self.cache), 'lane:lane/show/grok-g1@bbbbbbbbbb')
        self.assertEqual(w('rec', cache=self.cache, records={'rec': dict(lane='lane/show/x', sha='abcdef1')}), 'record:lane/show/x@abcdef1')

    def test_a_tag_on_a_lane_that_is_not_grok_is_not_done(self):
        """Must still fail: fix-job-ordering-tests' own lane carried its tag while it failed (a leg's handoff, the push
        refused), and a review lane carries the tags of what it looks at."""
        self.assertEqual(registry.where_done('r1', 'lane/show/review-tools', cache=self.cache), '')
        self.assertEqual(registry.where_done('mentioned', 'lane/show/mentioned', cache=self.cache), '')
        self.assertEqual(registry.where_done('r1', None, cache=self.cache), '')
        self.assertEqual(registry.where_done('nope', 'lane/show/nope', cache=self.cache), '')


class Refresh(unittest.TestCase):
    """Against a real origin: a bare repo, a clone, lanes pushed with tags."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        t = Path(self.tmp.name)
        self.origin, self.repo = t / 'origin.git', t / 'clone'
        git(t, 'init', '-q', '--bare', str(self.origin))
        git(t, 'clone', '-q', str(self.origin), str(self.repo))
        for k, v in (('user.email', 't@t'), ('user.name', 't'), ('commit.gpgsign', 'false')):
            git(self.repo, 'config', k, v)
        git(self.repo, 'checkout', '-q', '-b', INT)
        self.commit('[landed-1] on integration')
        git(self.repo, 'push', '-q', 'origin', INT)
        git(self.repo, 'checkout', '-q', '-b', 'lane/show/relay-y')
        self.commit('[y] a leg hands half-done work on')
        git(self.repo, 'push', '-q', 'origin', 'lane/show/relay-y')
        git(self.repo, 'checkout', '-q', '-b', 'lane/show/grok-g1')      # Grok cut its lane from the relay's
        self.commit('[g1] [z] built by grok')
        git(self.repo, 'push', '-q', 'origin', 'lane/show/grok-g1')
        git(self.repo, 'checkout', '-q', INT)
        git(self.repo, 'checkout', '-q', '-b', 'lane/show/grok-g2')
        self.commit('grok: notes on [g2] and [g3]')
        git(self.repo, 'push', '-q', 'origin', 'lane/show/grok-g2')
        git(self.repo, 'checkout', '-q', INT)
        git(self.repo, 'checkout', '-q', '-b', 'lane/show/own-lane')
        self.commit('[own] a leg hands half-done work on')
        git(self.repo, 'push', '-q', 'origin', 'lane/show/own-lane')
        git(self.repo, 'fetch', '-q')
        self.calls = []

    def tearDown(self):
        self.tmp.cleanup()

    def commit(self, msg):
        git(self.repo, 'commit', '-q', '--allow-empty', '-m', msg)

    def gitf(self, repo, *a):
        self.calls.append(a[0])
        return registry.git(repo, *a)

    def test_tags_are_read_and_a_second_refresh_reads_only_what_moved(self):
        st, lines = registry.refresh({}, self.repo, bds={}, gitf=self.gitf)
        self.assertEqual(st['integration'].keys(), {'landed-1'})
        self.assertEqual(st['lanes']['lane/show/grok-g1'].keys(), {'g1'}, 'only the lane\'s own id')
        self.assertEqual(registry.where_done('y', 'lane/show/relay-y', cache=st), '', 'inherited from the relay lane: not done')
        self.assertEqual(registry.where_done('z', cache=st), '', 'a second tag on a Grok commit: not done')
        self.assertEqual(st['lanes']['lane/show/grok-g2'], {}, 'a tag inside the subject is no tag')
        self.assertNotIn('lane/show/own-lane', st['lanes'], 'a lane that is not Grok\'s is not read')
        self.assertEqual(registry.where_done('own', 'lane/show/own-lane', cache=st), '')
        self.assertEqual(registry.where_done('g1', cache=st), 'lane:lane/show/grok-g1@' + st['lanes']['lane/show/grok-g1']['g1'][:10])
        first = self.calls.count('log')
        self.calls.clear()
        st2, _ = registry.refresh(st, self.repo, bds={}, gitf=self.gitf)
        self.assertEqual(self.calls, ['for-each-ref'], 'nothing moved: no log is read')
        git(self.repo, 'checkout', '-q', 'lane/show/grok-g2')
        self.commit('[g2] built at last')
        git(self.repo, 'push', '-q', 'origin', 'lane/show/grok-g2')
        git(self.repo, 'push', '-q', 'origin', '--delete', 'lane/show/grok-g1')
        git(self.repo, 'fetch', '-q', '--prune')
        self.calls.clear()
        st3, _ = registry.refresh(st2, self.repo, bds={}, gitf=self.gitf)
        self.assertEqual(self.calls.count('log'), 1, 'only the lane that moved is read again')
        self.assertEqual(first, 3, 'integration and the two Grok lanes')
        self.assertEqual(st3['lanes']['lane/show/grok-g2'].keys(), {'g2'})
        self.assertNotIn('lane/show/grok-g1', st3['lanes'], 'a deleted lane leaves the cache')

    def test_the_board_done_records_are_read(self):
        b = Path(self.tmp.name) / 'tw3d-board-2'
        (b / 'relay' / 'done').mkdir(parents=True)
        (b / 'relay' / 'done' / 'u1.json').write_text('{}', encoding='utf-8')
        st, _ = registry.refresh({}, self.repo, bds={'tw3d-board-2': b})
        self.assertEqual(registry.where_done('u1', cache=st), 'board:tw3d-board-2')


class RecordAndHooks(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.home = Path(self.tmp.name) / 'home'
        os.environ['TW_SUP_HOME'] = str(self.home)

    def tearDown(self):
        os.environ.pop('TW_SUP_HOME', None)
        self.tmp.cleanup()

    def test_record_writes_its_own_file_and_is_done_finds_it(self):
        self.assertEqual(registry.main(['record', 'outside-1', '--lane', 'lane/show/x', '--sha', '1234abc', '--by', 'grok']), 0)
        self.assertEqual(registry.is_done('outside-1'), (True, 'record:lane/show/x@1234abc'))
        self.assertEqual(registry.main(['is', 'outside-1']), 0)
        self.assertEqual(registry.main(['record', 'bad id', '--lane', 'l', '--sha', '1234abc']), 2)
        self.assertEqual(registry.main(['record', 'x', '--lane', 'l', '--sha', 'not-a-sha']), 2)
        self.assertFalse(core.state_path(registry.STATE, self.home).exists(), 'record never writes the cache')

    def test_the_hooks_act_only_when_the_switch_is_on(self):
        core.write_state(registry.STATE, dict(lanes={'lane/show/grok-g1': {'g1': 'abcdef12'}}), self.home)
        self.assertEqual(hooks.skip('g1', None, 'lane/show/g1'), '', 'dry (the default) changes nothing')
        self.assertEqual(hooks.done_ids({'g1': {'lane': 'lane/show/g1'}}), set())
        core.write_json(self.home / 'modules.json', {'registry_refresh': 'on'})
        self.assertEqual(hooks.skip('g1', None, 'lane/show/g1'), 'done (lane:lane/show/grok-g1@abcdef12)')
        self.assertEqual(hooks.done_ids({'g1': {'lane': 'lane/show/g1'}, 'g2': {}}), {'g1'})

    def test_the_relay_picker_passes_a_registry_done_unit_by(self):
        """The relay's own lane source (Tools/relay/sources/lane.py next) with the hook in it."""
        sys.path.insert(0, str(HERE.parent / 'relay'))
        from sources import lane
        board = Path(self.tmp.name) / 'board'
        (board / 'relay' / 'queue').mkdir(parents=True)
        for uid, pr in (('g1', 1), ('other', 2)):
            (board / 'relay' / 'queue' / f'{uid}.json').write_text(json.dumps(dict(
                id=uid, lane=f'lane/show/{uid}', role='lane', goal='g', done_when=['python', '-c', '1'], priority=pr)), encoding='utf-8')
        core.write_state(registry.STATE, dict(lanes={'lane/show/grok-g1': {'g1': 'abcdef12'}}), self.home)
        ctx = dict(board=board, skip=set())
        self.assertEqual(lane.next(ctx)['id'], 'g1', 'registry dry: today\'s picker')
        core.write_json(self.home / 'modules.json', {'registry_refresh': 'on'})
        self.assertEqual(lane.next(ctx)['id'], 'other')


class Replay(unittest.TestCase):
    """The proof on the records of 10-19..11-02."""

    @classmethod
    def setUpClass(cls):
        cls.rs = records.load()
        cls.grok = json.loads(GROK.read_text(encoding='utf-8'))

    def cache_at(self, t, records=True):
        """(cache, records) a refresh at time t would hold: the tags of each Grok commit there by then (only those its
        subject opens with); a Grok unit whose commit carries no tag of its own (3 of 34) is known only when the worker
        calls `registry.py record` (`records`)."""
        lanes, recs = {}, {}
        for uid, g in self.grok.items():
            if utc(g['committed_at']) > t:
                continue
            own = g['lane'][len(registry.GROK):]
            if own in registry.tags(g['subject']):
                lanes.setdefault(g['lane'], {})[own] = g['sha']
            if records and uid not in lanes.get(g['lane'], {}):
                recs[uid] = dict(lane=g['lane'], sha=g['sha'])
        return dict(lanes=lanes), recs

    def skipped(self, old=False, records=True):
        n, usd, units = 0, 0.0, set()
        for l in self.rs.legs:
            if l['unit'] not in self.grok:
                continue
            seen = utc(l['started_at']) - datetime.timedelta(seconds=600)    # one refresh late at worst
            done = '' if old else registry.where_done(l['unit'], l.get('lane'), *self.cache_at(seen, records))
            if done:
                n, usd = n + 1, usd + (l.get('cost_usd') or 0)
                units.add(l['unit'])
        return n, round(usd, 1), units

    def test_the_registry_skips_the_legs_on_grok_built_units(self):
        n, usd, units = self.skipped()
        t = self.skipped(records=False)
        print(f'\n  registry: {n} legs (${usd}) on {len(units)} Grok-built units would not have started '
              f'({t[0]} legs, ${t[1]}, from the lane tags alone; the rest needs Grok to call record)')
        self.assertGreaterEqual(n, 120)
        self.assertGreaterEqual(len(units), 25)

    def test_todays_picker_skipped_none(self):
        """The old behaviour: only <board>/relay/done counts, and none of these units had a done record."""
        self.assertEqual(self.skipped(old=True)[0], 0)

    def test_a_leg_before_the_grok_commit_still_runs(self):
        early = [l for l in self.rs.legs if l['unit'] in self.grok
                 and utc(l['started_at']) < utc(self.grok[l['unit']]['committed_at'])]
        self.assertTrue(early, 'the fixture has legs from before Grok built the unit')
        for l in early:
            self.assertEqual(registry.where_done(l['unit'], l.get('lane'), *self.cache_at(utc(l['started_at']))), '')


if __name__ == '__main__':
    unittest.main()
