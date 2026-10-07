#!/usr/bin/env python3
"""Tests for the agents sessions spawn outside the relay (Tools/relay/agents.py). Run: python Tools/relay/test_agents.py
Each test works in a temporary folder with session logs, a board and a relay home of its own. That a run books them
before a unit is tested in test_relay.py (class Pace).
"""
import contextlib, datetime, io, json, os, shutil, sys, tempfile, unittest
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import agents, config, ledger, relay   # noqa: E402

ENV = ("TW_RELAY_HOME", "TW_BOARD", "TW_STATION", agents.LOGS, agents.IDEAS, ledger.ALSO)
OPUS, HAIKU = "claude-opus-5-5", "claude-haiku-4-5-20251001"


def stamp(days_ago=0):
    t = datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(days=days_ago)
    return t.strftime("%Y-%m-%dT%H:%M:%S.000Z")


def day(days_ago=0):
    return (datetime.datetime.now() - datetime.timedelta(days=days_ago)).strftime("%Y-%m-%d")


class Base(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="agents-test-"))
        self.logs, self.board, self.home = self.tmp / "projects", self.tmp / "board", self.tmp / "home"
        self.logs.mkdir()
        self.old = {k: os.environ.get(k) for k in ENV}
        self.ideas = self.tmp / "ideas"                     # never the Drive's: its runs are real money of the day
        os.environ.update({"TW_RELAY_HOME": str(self.home), "TW_BOARD": str(self.board), "TW_STATION": "laptop",
                           agents.LOGS: str(self.logs), agents.IDEAS: str(self.ideas)})
        os.environ.pop(ledger.ALSO, None)
        self.st, self.n = agents.settings(), 0

    def tearDown(self):
        for k, v in self.old.items():
            os.environ.pop(k, None) if v is None else os.environ.__setitem__(k, v)
        shutil.rmtree(self.tmp, ignore_errors=True)

    def agent(self, answers, cwd="C:\\Users\\me\\Documents\\GitHub\\githubtest-pipe", prompt="Find the bug.",
              session="s1", project="p1"):
        """One agent's log. answers: (answer id, model, usage, days ago), in the order they were logged."""
        self.n += 1
        f = self.logs / project / session / "subagents" / ("agent-a%02d.jsonl" % self.n)
        f.parent.mkdir(parents=True, exist_ok=True)
        rows = [{"type": "user", "isSidechain": True, "cwd": cwd, "timestamp": stamp(),
                 "message": {"role": "user", "content": [{"type": "text", "text": prompt}]}}]
        rows += [{"type": "assistant", "isSidechain": True, "cwd": cwd, "timestamp": stamp(ago), "uuid": "u%d" % i,
                  "message": {"role": "assistant", "id": aid, "model": model, "usage": use}}
                 for i, (aid, model, use, ago) in enumerate(answers)]
        f.write_text("\n".join(json.dumps(r) for r in rows) + "\nnot json\n", encoding="utf-8")
        return f

    def run_of_ideas(self, usd, host=None, days_ago=0, **more):
        """One line of the ideas folder's spend.jsonl, as ideas.py writes it when a run of its agent ends."""
        self.ideas.mkdir(exist_ok=True)
        line = dict({"when": day(days_ago) + " 14:02", "run": "r%d" % self.n, "asked": "auto", "usd": usd, "ideas": 1,
                     "why": ""}, **more)
        if host != "":
            line["host"] = host or agents.socket.gethostname()
        with open(self.ideas / "spend.jsonl", "a", encoding="utf-8") as f:
            f.write(json.dumps(line, sort_keys=True) + "\n")

    def main(self, *argv):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            code = relay.main(list(argv))
        return code, buf.getvalue()


M = 1000000


class Count(Base):
    def test_tokens_become_dollars_at_the_list_price_of_the_model(self):
        self.agent([("m1", OPUS, {"input_tokens": M, "output_tokens": M, "cache_read_input_tokens": M,
                                  "cache_creation_input_tokens": M}, 0)])
        c = agents.count()
        self.assertEqual((c["day"], c["agents"], c["unpriced"]), (day(), 1, []))
        self.assertAlmostEqual(c["usd"], 4.0 + 20.0 + 0.2 + 4.0 * 1.25)        # a write counts as kept five minutes
        self.assertEqual(c["models"][OPUS]["cache_write_5m"], M)

    def test_a_cache_write_kept_an_hour_costs_twice_the_input_price(self):
        self.agent([("m1", OPUS, {"cache_creation_input_tokens": 3 * M, "cache_creation": {
            "ephemeral_5m_input_tokens": M, "ephemeral_1h_input_tokens": 2 * M}}, 0)])
        self.assertAlmostEqual(agents.count()["usd"], 4.0 * 1.25 + 2 * 4.0 * 2.0)

    def test_a_dated_model_id_takes_the_price_of_its_model_and_the_longest_name_wins(self):
        self.assertEqual(agents.price(HAIKU, self.st), (self.st["usd_per_million_tokens"]["claude-haiku-4-5"], True))
        self.assertEqual(agents.price("claude-opus-5-5", self.st)[0]["input"], 4.0)     # not claude-opus-5's 5.0
        self.assertEqual(agents.price("claude-opus-5", self.st)[0]["input"], 5.0)

    def test_a_model_the_file_does_not_name_is_priced_as_the_dearest_and_said(self):
        self.agent([("m1", "claude-next-9", {"output_tokens": M}, 0)])
        c = agents.count()
        self.assertEqual((c["usd"], c["unpriced"]), (50.0, ["claude-next-9"]))
        self.assertIn("agents.json names no price for claude-next-9", "\n".join(agents.lines(c)))

    def test_an_answer_logged_once_per_block_is_counted_once_at_its_largest_count(self):
        self.agent([("m1", OPUS, {"output_tokens": 10}, 0), ("m1", OPUS, {"output_tokens": M}, 0),
                    ("m1", OPUS, {"output_tokens": M}, 0), ("m2", OPUS, {"output_tokens": M}, 0)])
        self.assertAlmostEqual(agents.count()["usd"], 40.0)

    def test_only_the_answers_of_that_local_day_count(self):
        self.agent([("m1", OPUS, {"output_tokens": M}, 1), ("m2", OPUS, {"output_tokens": 2 * M}, 0)])
        self.assertAlmostEqual(agents.count()["usd"], 40.0)
        self.assertAlmostEqual(agents.count(day(1))["usd"], 20.0)
        self.assertEqual(agents.count(day(3))["agents"], 0)

    def test_an_agent_counts_when_its_folder_or_its_first_prompt_names_the_project(self):
        use = [("m1", OPUS, {"output_tokens": M}, 0)]
        self.agent(use)                                                         # in a checkout of the project
        self.agent(use, cwd="C:\\Users\\me\\Documents\\claude",                 # elsewhere, sent into the project
                   prompt="Read C:/Users/me/Documents/GitHub/githubtest/trench-warfare-3d/Tools/relay/ledger.py")
        self.agent(use, cwd="C:\\Users\\me\\Documents\\claude", prompt="Plan my trip to Lisbon.")    # other work
        c = agents.count()
        self.assertEqual((c["agents"], c["usd"]), (2, 40.0))

    def test_the_agents_of_a_relay_leg_are_not_counted_again(self):
        use = [("m1", OPUS, {"output_tokens": M}, 0)]
        self.agent(use, session="leg-session")                                  # a leg this machine ran
        leg = self.home / "runs" / "20261007-100000-1" / "legs" / "01"
        leg.mkdir(parents=True)
        (leg / "session.json").write_text(json.dumps({"session_id": "leg-session"}), encoding="utf-8")
        self.agent(use, cwd="C:\\Users\\PC\\Documents\\GitHub\\githubtest-relay-work2", session="s2")   # a leg's checkout
        self.agent(use, cwd="C:\\Users\\PC\\AppData\\Local\\TrenchWarfare\\relay2\\runs\\r\\legs\\03\\desk\\bundle",
                   prompt="Score the tw3d evidence.", session="s3")              # a blind critic's own folder
        self.assertEqual(agents.count()["agents"], 0)
        self.agent(use, session="s4")
        self.assertEqual(agents.count()["agents"], 1)

    def test_no_logs_and_an_agent_that_answered_nothing_count_as_nothing(self):
        self.agent([])
        self.agent([("m1", "<synthetic>", {"input_tokens": 0, "output_tokens": 0}, 0)])
        c = agents.count()
        self.assertEqual((c["usd"], c["models"]), (0.0, {}))
        os.environ[agents.LOGS] = str(self.tmp / "nowhere")
        self.assertEqual(agents.count()["agents"], 0)

    def test_the_shipped_prices_cover_the_models_the_relay_and_its_sessions_run_on(self):
        for model in ("claude-opus-5-5", "claude-sonnet-5-5", "claude-fable-5-1", "claude-haiku-4-5-20251001",
                      "claude-opus-4-8"):
            p, named = agents.price(model, self.st)
            self.assertTrue(named, model)
            self.assertTrue(0 < p["cache_read"] < p["input"] < p["output"], model)
        self.assertEqual((self.st["cache_write_5m_factor"], self.st["cache_write_1h_factor"]), (1.25, 2.0))


class IdeasRuns(Base):
    def test_a_run_the_board_started_here_today_is_counted_and_no_other(self):
        self.run_of_ideas(1.76)
        self.run_of_ideas(1.70)
        self.run_of_ideas(9.0, host="THE-OTHER-PC")            # the Drive shows it here too: it is that machine's
        self.run_of_ideas(9.0, days_ago=1)
        self.run_of_ideas(0.0)                                 # a run that cost nothing is no run to book
        self.run_of_ideas("1.5")
        with open(self.ideas / "spend.jsonl", "a", encoding="utf-8") as f:
            f.write("not json\n[1]\n")
        c = agents.count()
        self.assertEqual((c["runs"], round(c["runs_usd"], 2), round(c["usd"], 2), c["agents"]), (2, 3.46, 3.46, 0))
        self.assertEqual(agents.count(host="the-other-pc")["runs_usd"], 9.0)    # a host's name in any case
        self.assertEqual(agents.count(day(1))["runs_usd"], 9.0)

    def test_the_runs_are_added_to_what_the_agents_cost(self):
        self.agent([("m1", OPUS, {"output_tokens": M}, 0)])
        self.run_of_ideas(2.5)
        c = agents.count()
        self.assertEqual((c["usd"], c["agents"], c["runs"]), (22.5, 1, 1))
        said = "\n".join(agents.lines(c))
        self.assertIn("1 agent, about $22.50 at list prices", said)
        self.assertIn("the ideas agent", said)
        self.assertIn("1 run the board started here", said)

    def test_a_run_that_names_no_host_is_counted_nowhere_and_said(self):
        self.run_of_ideas(1.7, host="")
        for host in (None, "THE-OTHER-PC"):
            c = agents.count(host=host)
            self.assertEqual((c["runs"], c["usd"], c["runs_unowned"]), (0, 0.0, 1))
        self.assertIn("1 ideas run of the day names no host", "\n".join(agents.lines(agents.count())))

    def test_with_no_ideas_folder_nothing_is_counted_and_the_folder_is_the_one_the_board_uses(self):
        self.assertEqual((agents.count()["runs"], agents.count()["usd"]), (0, 0.0))       # no folder: not an error
        self.assertEqual(agents.ideas_folder(), self.ideas)
        os.environ.pop(agents.IDEAS)
        self.assertIn(agents.ideas_folder().as_posix().split("/")[-2:],
                      (["TW3D-pipeline", "ideas"], ["assetboard", "ideas"]))


class Book(Base):
    def setUp(self):
        super().setUp()
        self.limits = config.limits
        config.limits = lambda *a, **k: dict(self.limits(*a, **k), week_usd=1000)      # a dollar is 0.1 points

    def tearDown(self):
        config.limits = self.limits
        super().tearDown()

    def held(self, station="laptop"):
        return json.loads((self.board / "relay" / station / "agents" / (day() + ".json")).read_text(encoding="utf-8"))

    def test_a_booking_is_the_days_total_of_the_station_and_the_ledger_counts_it(self):
        self.agent([("m1", OPUS, {"output_tokens": M}, 0)])
        rec, before = agents.book(self.board, "laptop", by="me")
        self.assertEqual((rec["usd"], rec["agents"], rec["station"], rec["by"], before), (20.0, 1, "laptop", "me", None))
        self.assertEqual(self.held()["models"], {OPUS: 20.0})
        s = ledger.spent(self.board)
        self.assertEqual((s["usd"], s["agents_usd"], s["agents"], s["legs"]), (20.0, 20.0, 1, 0))
        self.agent([("m2", OPUS, {"output_tokens": M}, 0)], session="s2")       # later the same day: the new total
        rec, before = agents.book(self.board, "laptop")
        self.assertEqual((rec["usd"], rec["agents"], before["usd"]), (40.0, 2, 20.0))
        self.assertEqual(ledger.spent(self.board)["usd"], 40.0)                 # replaced, not added twice

    def test_a_day_with_only_runs_of_the_ideas_agent_is_booked_and_the_ledger_counts_it(self):
        self.run_of_ideas(1.76)
        rec, before = agents.book(self.board, "laptop", by="me")
        self.assertEqual((rec["usd"], rec["agents"], rec["runs"], rec["runs_usd"], before), (1.76, 0, 1, 1.76, None))
        self.assertEqual((self.held()["runs"], ledger.spent(self.board)["usd"]), (1, 1.76))
        self.run_of_ideas(1.70)                                                 # later the same day: the new total
        self.assertEqual(agents.book(self.board, "laptop")[0]["usd"], 3.46)
        self.assertEqual(ledger.spent(self.board)["usd"], 3.46)
        keep, agents.socket.gethostname = agents.socket.gethostname, lambda: "THE-OTHER-PC"
        try:
            self.assertEqual(agents.book(self.board, "desktop")[0]["runs"], 0)  # the other machine books none of them
        finally:
            agents.socket.gethostname = keep
        self.assertEqual(ledger.spent(self.board)["usd"], 3.46)

    def test_the_command_counts_and_books_the_runs_of_the_ideas_agent(self):
        self.run_of_ideas(2.0)
        code, out = self.main("agents")
        self.assertIn("this machine: 0 agents, about $2.00 at list prices (about 0.2% of the week).", out)
        self.assertIn("Not booked for the day yet", out)
        f = self.tmp / "laptop.json"
        self.assertIn("about $2.00, 0 agents and 1 ideas run)", self.main("agents", "book", "--out", str(f))[1])
        self.assertIn("booked laptop %s: about $2.00, 0 agents and 1 ideas run." % day(), self.main("agents", "book")[1])

    def test_a_day_with_no_such_agent_writes_nothing(self):
        rec, before = agents.book(self.board, "laptop")
        self.assertEqual((rec["agents"], before), (0, None))
        self.assertFalse((self.board / "relay").exists())

    def test_a_booking_that_arrives_during_a_run_waits_and_is_taken_up(self):
        rec = agents.record({"day": day(), "usd": 7.5, "agents": 3, "models": {}, "unpriced": []}, "laptop", "me")
        f = agents.hand_in(self.home, rec)
        (f.parent / "junk.json").write_text("{\"usd\": -1}", encoding="utf-8")
        self.assertEqual(ledger.spent(self.board)["usd"], 0.0)                  # not on the board yet
        self.assertEqual([r["usd"] for r in agents.take_in(self.board, self.home)], [7.5])
        self.assertEqual((self.held()["usd"], list(f.parent.glob("*.json"))), (7.5, []))
        self.assertEqual(agents.take_in(self.board, self.home), [])

    def test_what_is_no_booking_is_refused(self):
        good = {"day": day(), "station": "laptop", "usd": 1.0, "agents": 1}
        self.assertIsNone(agents.check(good))
        for bad in (None, dict(good, day="today"), dict(good, station="../x"), dict(good, usd=-1),
                    dict(good, usd="1"), dict(good, agents=True)):
            self.assertIsNotNone(agents.check(bad), bad)
            with self.assertRaises(SystemExit):
                agents.keep(self.board, bad)

    def test_the_command_says_what_was_spawned_and_books_it(self):
        self.agent([("m1", OPUS, {"output_tokens": M}, 0)])
        code, out = self.main("agents")
        self.assertEqual(code, 0)
        self.assertIn("this machine: 1 agent, about $20.00 at list prices (about 2.0% of the week).", out)
        self.assertIn("Not booked for the day yet", out)
        code, out = self.main("agents", "book", "--who", "me")
        self.assertIn("booked laptop %s: about $20.00, 1 agent. Board: no board repo." % day(), out)
        self.assertIn("Booked for the day: $20.00, 1 agent", self.main("agents")[1])
        self.assertIn("about 2.0% of the week used by the relay and 1 agent outside it", self.main("status")[1])

    def test_a_machine_without_the_board_writes_a_file_and_the_boards_machine_books_it(self):
        self.agent([("m1", OPUS, {"output_tokens": M}, 0)])
        f = self.tmp / "laptop.json"
        code, out = self.main("agents", "book", "--out", str(f))
        self.assertIn("written to %s (laptop %s: about $20.00, 1 agent)" % (f, day()), out)
        self.assertFalse((self.board / "relay").exists())
        os.environ.update({"TW_STATION": "desktop", agents.LOGS: str(self.tmp / "nowhere")})
        code, out = self.main("agents", "book", "--file", str(f))
        self.assertEqual(code, 0)
        self.assertEqual((self.held("laptop")["usd"], ledger.spent(self.board)["agents"]), (20.0, 1))
        self.assertIn("nothing to book", self.main("agents", "book")[1])        # the desktop itself spawned none
        f.write_text("{}", encoding="utf-8")
        with self.assertRaises(SystemExit):
            self.main("agents", "book", "--file", str(f))


if __name__ == "__main__":
    unittest.main(verbosity=1)
