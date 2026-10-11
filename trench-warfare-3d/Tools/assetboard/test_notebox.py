#!/usr/bin/env python3
"""Tests of the asset board's note box (static/board.js), the way the owner's notes and his clicks on the Decide
page reach the agents. Run from trench-warfare-3d/: python Tools/assetboard/test_notebox.py

A note of his is a decision: none may be lost, and none written twice. These cases drive the page half of board.js
under node with a fake fetch, a fake store and a mini-DOM, so the sending can be watched note by note.
"""
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
import urllib.error
import urllib.request
from pathlib import Path

HERE = Path(__file__).resolve().parent
# a check may point the harness at an older board.js, to see its own red on the code before the fix
BOARD = Path(os.environ.get('TW_NOTEBOX_BOARD') or HERE / 'static' / 'board.js')

# never the live folders: the board's own places go under one temp folder, before anything of it is imported
TEMP = Path(tempfile.mkdtemp(prefix='tw-notebox-'))
for _k in ('TW_NOTES', 'TW_BRIEFS', 'TW_ASSETBOARD_OUT'):
    _d = TEMP / _k.lower()
    _d.mkdir(exist_ok=True)
    os.environ[_k] = str(_d)

sys.path.insert(0, str(HERE))
import notes        # noqa: E402  (after TW_NOTES: it must never see the live folder)

HARNESS = r'''
const fs = require('fs');
const src = fs.readFileSync(process.argv[2], 'utf8');
const which = process.argv[3];

// ---- a mini-DOM: only what notesBlock() and changed() touch
function find(e, sel) {
  let kids = e.children || [], scope = false;
  if (sel.indexOf(':scope > ') === 0) { sel = sel.slice(9); scope = true; }
  const parts = sel.trim().split(/\s+/);
  const hit = (x, p) => p.charAt(0) === '.' ? String(x.className || '').split(' ').indexOf(p.slice(1)) >= 0 : x.tagName === p;
  for (const k of kids) {
    if (hit(k, parts[0])) {
      if (parts.length === 1) return k;
      const deep = find(k, parts.slice(1).join(' '));
      if (deep) return deep;
    }
    if (!scope) { const deep = find(k, sel); if (deep) return deep; }
  }
  return null;
}
function mk(tag) {
  const e = {
    tagName: tag, className: '', textContent: '', value: '', type: '', dataset: {}, children: [], _on: {},
    hidden: false, disabled: false, isConnected: true, maxLength: 0, rows: 0,
    classList: { add() {}, remove() {}, toggle() {}, contains() { return false; } },
    setAttribute() {}, getAttribute() { return null; }, requestSubmit() { e.fire('submit'); },
    appendChild(c) { this.children.push(c); return c; },
    insertBefore(c, ref) { const i = ref ? this.children.indexOf(ref) : -1; if (i < 0) this.children.push(c); else this.children.splice(i, 0, c); return c; },
    replaceChild(nw, old) { const i = this.children.indexOf(old); if (i >= 0) this.children[i] = nw; else this.children.push(nw); return old; },
    remove() {}, scrollIntoView() {},
    addEventListener(t, f) { (this._on[t] = this._on[t] || []).push(f); },
    fire(t) { (this._on[t] || []).forEach(f => f({ preventDefault() {}, stopPropagation() {}, key: '' })); },
    querySelector(sel) { return find(this, sel); },
    querySelectorAll() { return []; },
    get firstChild() { return this.children[0] || null; },
    get innerHTML() { return ''; },
    set innerHTML(v) { if (v === '') this.children = []; },
  };
  return e;
}
const body = mk('body');
body.dataset.root = '';
const inline = mk('div');
inline.dataset.board = 'asset'; inline.dataset.id = 'Maw'; inline.dataset.title = 'The Maw';
const document = {
  body: body, activeElement: null, createElement: mk, addEventListener() {},
  querySelector(sel) { return sel === 'header.top' ? null : find(body, sel); },
  querySelectorAll(sel) { return sel === '[data-board]' ? [inline] : []; },
  getElementById() { return null; },
};

// ---- the store, the fetch, the clock
const LS = 'tw3d-notes-unsent';
const kept = {};
const localStorage = { getItem(k) { return k in kept ? kept[k] : null; }, setItem(k, v) { kept[k] = String(v); }, removeItem(k) { delete kept[k]; } };
const note = (id, text) => ({ id: id, when: '2026-10-11 02:00', from: 'owner', state: 'unsent', text: text, kind: 'asset',
  about: 'Maw', title: 'The Maw', lane: '', asset: 'Maw', page: 'index.html', then: '', answers: [] });
const posts = [];
const slow = v => new Promise(res => setTimeout(() => res(v), 10));          // an answer never comes back in the same turn
function fetchFake(url, opt) {
  if (String(url).indexOf('/ping') >= 0) return Promise.resolve({ json: () => Promise.resolve({ ok: true }) });
  const b = JSON.parse(opt.body);
  posts.push(b);
  if (b.text === 'B') return slow(null).then(() => { throw new Error('the listener dropped it'); });
  if (b.text === 'C') return new Promise(() => {});                            // an answer that never comes back
  return slow({ json: () => Promise.resolve({ ok: true, note: { id: 'note-' + b.text, when: '2026-10-11 02:00', from: 'owner', state: 'open', text: b.text, kind: b.kind, about: b.about, title: b.title, lane: b.lane, asset: b.asset, answers: [] } }) });
}

const window = { NOTEBOX: { url: 'http://x', key: 'k' }, NOTES: [] };
const location = { pathname: '/index.html', search: '' };
let tick = null;

function load(timer) {
  // module is undefined, so the source walks past its `module.exports = pure` and builds the page half
  const f = new Function('module', 'window', 'document', 'localStorage', 'fetch', 'location', 'setInterval', 'setTimeout', src);
  f(undefined, window, document, localStorage, fetchFake, location, function (cb) { tick = cb; return 1; }, timer || setTimeout);
}
// the send's own time limit, fired at once: what the page does when the note box takes the note and says nothing
const atOnce = cb => { cb(); return 0; };

const sleep = ms => new Promise(res => setTimeout(res, ms));

async function main() {
  const out = {};
  if (which === 'double') {
    // two notes wait in this browser from an earlier visit; the page's start flushes, and the 20 s tick flushes too
    kept[LS] = JSON.stringify([note('unsent-1', 'A'), note('unsent-2', 'B')]);
    load();
    tick();
    await sleep(300);
    out.posted = posts.map(p => p.text);
    out.store = JSON.parse(kept[LS] || '[]').map(n => n.text);
  } else if (which === 'slow') {
    // the listener takes the note and never answers: the send must give up, and the note must still be here
    load(atOnce);
    await sleep(10);                                            // the page's own start (ping, an empty flush) is through
    let settled = false;
    window.Board.note({ kind: 'asset', id: 'Maw', title: 'The Maw', asset: 'Maw' }, 'C').then(() => { settled = true; }, () => { settled = true; });
    await sleep(300);                                           // a flush that never settles is reported, not waited for
    out.settled = settled;
    out.store = JSON.parse(kept[LS] || '[]').map(n => ({ text: n.text, why: n.why || '' }));
  } else if (which === 'panelswap') {
    // one form, two runs of one agent type: they share kind+id, so the form made for the first is kept for the second
    load(atOnce);
    await sleep(10);
    window.Board.open({ kind: 'queue', id: 'u-1', title: 'run one', lane: 'lane/show/one' });
    window.Board.open({ kind: 'queue', id: 'u-1', title: 'run two', lane: 'lane/show/two' });
    find(body, '.b-form').children[0].value = 'x';
    find(body, '.b-form').fire('submit');
    await sleep(50);
    out.posts = posts;
  } else if (which === 'dockdrawer') {
    // the drawer is open over the workers' place: the dock's form is still about the worker standing there
    load(atOnce);
    await sleep(10);
    const dock = mk('div');
    body.appendChild(dock);
    window.Board.dock(dock);
    window.Board.open({ kind: 'worker', id: 'frog-1', uid: 'u-a', title: 'Claude session', lane: 'lane/show/one', worker: { kind: 'session', state: 'working' } });
    window.Board.open({ kind: 'queue', id: 'u-1', lane: 'lane/sim/x' });
    find(dock, '.b-form').children[0].value = 'x';
    find(dock, '.b-form').fire('submit');
    await sleep(50);
    out.posts = posts;
  } else { throw new Error('no such case: ' + which); }
  console.log(JSON.stringify(out));
}
main().then(null, e => { console.error(e && e.stack || e); process.exit(2); });
'''


def drive(case):
    """Run one case of the page half under node and give back what it reported."""
    node = shutil.which('node')
    if not node:
        raise AssertionError('no node on this machine: the note box cannot be driven, so no case ran')
    h = TEMP / 'harness.js'
    h.write_text(HARNESS, encoding='utf-8')
    p = subprocess.run([node, str(h), str(BOARD), case], capture_output=True)
    line = p.stdout.decode('utf-8', 'replace').strip().split('\n')[-1]
    try:
        return json.loads(line)
    except Exception:
        raise AssertionError('the harness said nothing usable: ' + line[:300] + ' / ' + p.stderr.decode('utf-8', 'replace')[-600:])


SENDS = ('const B = require(process.argv[1]);'
         'console.log(JSON.stringify(B.sends({id: "unsent-7", text: "t", kind: "asset", about: "Maw", title: "The Maw", lane: "", asset: "Maw", page: "index.html"})));')


class Box(unittest.TestCase):
    def test_double_flush(self):
        """[N12.1] two flushes at once, two notes waiting, the second note's send failing."""
        o = drive('double')
        self.assertEqual([t for t in o['posted'] if t == 'A'], ['A'],
                         '[N12.1] the waiting note was posted twice: ' + repr(o['posted']))
        self.assertIn('B', o['store'],
                      '[N12.1] the second note is in neither the page nor the store: ' + repr(o))


    def test_send_never_answered(self):
        """[N12.2] a send that never comes back gives up, and the note stays in this browser."""
        o = drive('slow')
        self.assertTrue(o['settled'], '[N12.2] the send never came back and the flush never settled: ' + repr(o))
        self.assertEqual([n['text'] for n in o['store']], ['C'],
                         '[N12.2] the send never came back and the note is gone from the store: ' + repr(o))
        self.assertTrue(o['store'][0]['why'], '[N12.2] the page does not say why the note is still waiting: ' + repr(o))

    def test_form_reads_its_own_panel(self):
        # [N12.3b] the drawer is open over the dock: a note from the dock's form is about the worker there
        o = drive('dockdrawer')
        self.assertTrue(o['posts'], "[N12.3b] the form in the dock sent nothing: " + repr(o))
        self.assertEqual(o['posts'][-1]['about'], 'frog-1',
                         "[N12.3b] the note from the form in the dock carries the subject of the drawer: " + repr(o['posts'][-1]))

    def test_form_reads_the_subject_shown(self):
        # [N12.3] a form made for the first run of an agent type must send about the run the panel shows now
        o = drive('panelswap')
        self.assertTrue(o['posts'], '[N12.3] the form sent nothing: ' + repr(o))
        self.assertEqual(o['posts'][-1]['lane'], 'lane/show/two',
                         '[N12.3] the note carries the subject the form was made for, not the one shown: ' + repr(o['posts'][-1]))

    def test_sends_carries_the_id(self):
        """[N12.2] what the page sends carries the page's own id of the note."""
        node = shutil.which('node')
        if not node:
            raise AssertionError('no node on this machine: the note box cannot be driven, so no case ran')
        p = subprocess.run([node, '-e', SENDS, str(BOARD)], capture_output=True)
        got = json.loads(p.stdout.decode('utf-8', 'replace') or 'null')
        self.assertEqual((got or {}).get('id'), 'unsent-7',
                         '[N12.2] the note is sent without its own id: ' + repr(got) + ' / ' + p.stderr.decode('utf-8', 'replace')[-300:])


class Listener(unittest.TestCase):
    def test_one_note_per_id(self):
        """[N12.2] the same note posted twice (its first answer was lost) is one file, answered with the same note."""
        where = TEMP / 'twice'
        where.mkdir(exist_ok=True)
        server, key = notes.serve(where, port=0)
        self.assertIsNotNone(server, 'the listener did not start')
        try:
            url = 'http://127.0.0.1:%d/note' % server.server_address[1]
            body = json.dumps(dict(key=key, id='unsent-42', text='one note of his', kind='asset', about='Maw', title='The Maw', asset='Maw')).encode('utf-8')
            answers = []
            for _ in range(2):
                req = urllib.request.Request(url, data=body, headers={'Content-Type': 'text/plain'})
                with urllib.request.urlopen(req, timeout=10) as r:
                    answers.append(json.loads(r.read().decode('utf-8')))
        finally:
            server.shutdown()
            server.server_close()
        files = sorted(f.name for f in where.glob('*.md'))
        self.assertEqual(len(files), 1, '[N12.2] one note of his was written twice: ' + repr(files))
        self.assertTrue(all(a.get('ok') for a in answers), '[N12.2] a note of his was refused: ' + repr(answers))
        self.assertEqual(answers[0]['note']['id'], answers[1]['note']['id'],
                         '[N12.2] one note of his was written twice: ' + repr([a['note']['id'] for a in answers]))


if __name__ == '__main__':
    unittest.main()
