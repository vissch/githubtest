// Phase: tooling (record-match, 2026-10-14) — the debug option that records a played match.
// Two things the owner asked to be proven (2026-10-06, "Record only when switched on"): with the option on a short
// played match records to the existing replay format and replays to the same hash, and with it off nothing is
// recorded and the per-tick hash is not paid. The expected hashes are not recomputed from the recorded world: a
// second world is built from scratch and stepped by the replay's commands (ReplayPlayer.Verify), the way
// Tests/Sim/DeterminismReplayTests proves a replay.
using System.IO;
using NUnit.Framework;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class MatchRecordingTests
    {
        const int Ticks = 300;
        string folder;

        [SetUp]
        public void SetUp()
        {
            folder = Path.Combine(Path.GetTempPath(), "tw-record-" + System.Guid.NewGuid().ToString("N"));
        }

        [TearDown]
        public void TearDown()
        {
            Knobs.Clear();
            if (Directory.Exists(folder)) Directory.Delete(folder, true);
        }

        [Test]
        public void RecordedMatchReplaysToTheSameHash()
        {
            Knobs.Set(MatchRecording.Knob, "1");
            Assert.That(MatchRecording.Wanted, Is.True);
            var cfg = Config();
            using var session = new LockstepSession(() => MatchSim.CreatePlaytest(cfg), false, 0, 0, 0f, cfg.Seed,
                                                    m => MatchRecording.For(m, cfg));
            Assert.That(session.Recorder, Is.Not.Null, "the option is on, so the match is recorded");
            Assert.That(session.Local.World.HashInterval, Is.EqualTo(1), "a recorded match pays the per-tick hash");
            Play(session);

            string path = MatchRecording.Write(session.Recorder, folder);
            Assert.That(path, Is.Not.Null.And.EndsWith(".twr"));
            Assert.That(File.Exists(path), Is.True, "the recording was not written where its path says");

            var player = ReplayPlayer.Parse(File.ReadAllBytes(path));
            Assert.That(player.TickCount, Is.EqualTo(Ticks), "the recording is short of ticks");
            using var fresh = MatchSim.CreatePlaytest(cfg);
            fresh.World.HashInterval = 1;
            Assert.AreEqual(-1, player.Verify(fresh.World), "a world built from scratch and stepped by the recorded commands diverged");
        }

        [Test]
        public void WithTheOptionOffNothingIsRecorded()
        {
            Knobs.Clear();
            Assert.That(MatchRecording.Wanted, Is.False, "the option must be off by default");
            var cfg = Config();
            using var session = new LockstepSession(() => MatchSim.CreatePlaytest(cfg), false, 0, 0, 0f, cfg.Seed);
            Assert.That(session.Recorder, Is.Null);
            Assert.That(session.Local.World.HashInterval, Is.EqualTo(0), "an unrecorded match must hash nothing");
            Play(session);
            Assert.That(session.Local.World.LastHash, Is.EqualTo(0UL), "the per-tick check was paid for a match nobody records");
            Assert.That(Directory.Exists(Path.Combine(folder, "Replays")), Is.False, "nothing may be written with the option off");
        }

        static SimConfig Config()
        {
            var cfg = SimConfig.Default;
            cfg.StartingSilver = 4000;
            return cfg;
        }

        static void Play(LockstepSession session)
        {
            var ai = new ScriptedEnemy { Enabled = true, DeployEveryTicks = 20, Attacks = true, AttackGarrison = 6 };
            uint lastOrder = uint.MaxValue;
            int guard = Ticks * 40;
            while (session.Local.World.Tick < Ticks && guard-- > 0)
            {
                uint t = session.Local.World.Tick;
                if (t % 30 == 0 && t != lastOrder) { lastOrder = t; session.LocalDriver.Issue(SimCommand.Deploy(t, 0, (int)(t / 30 % 3))); }
                session.StepOnce(ai);
            }
            Assert.That(guard, Is.GreaterThan(0), "lockstep stalled");
        }
    }
}
