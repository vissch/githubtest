// Phase: tooling (2026-10-19) - a gym run inside a relay leg must not write the board's lessons.md: the leg's TW_BOARD
// is the relay's own board, and the relay failed legs on relays 2, 3 and 5 with "the board's lessons.md changed".
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using TW.Editor;

namespace TW.Tests
{
    public class GymLessonsTests
    {
        string board, oldBoard, oldRelay;

        [SetUp]
        public void SetUp()
        {
            board = Path.Combine(Path.GetTempPath(), "tw-gym-lessons-" + System.Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(board);
            File.WriteAllText(Path.Combine(board, "lessons.md"), "| day | who | lesson | source |\n");
            oldBoard = System.Environment.GetEnvironmentVariable("TW_BOARD");
            oldRelay = System.Environment.GetEnvironmentVariable("TW_RELAY");
            System.Environment.SetEnvironmentVariable("TW_BOARD", board);
        }

        [TearDown]
        public void TearDown()
        {
            System.Environment.SetEnvironmentVariable("TW_BOARD", oldBoard);
            System.Environment.SetEnvironmentVariable("TW_RELAY", oldRelay);
            try { Directory.Delete(board, true); } catch (IOException) { }
        }

        string Lessons() => File.ReadAllText(Path.Combine(board, "lessons.md"));

        [Test]
        public void ARelayLegsGymRunLeavesTheBoardAlone()
        {
            System.Environment.SetEnvironmentVariable("TW_RELAY", "1");
            string before = Lessons();
            GymRun.AppendLessons("abc1234567", new List<string> { "Fire | no shot event" });
            Assert.AreEqual(before, Lessons(), "a gym run inside a relay leg (TW_RELAY) must not change the board's lessons.md");
        }

        [Test]
        public void AGymRunOutsideALegStillWritesRecurringFlags()
        {
            System.Environment.SetEnvironmentVariable("TW_RELAY", null);
            GymRun.AppendLessons("abc1234567", new List<string> { "Fire | no shot event" });
            StringAssert.Contains("recurring flag: Fire / no shot event | gym run abc1234567", Lessons(),
                "outside a leg the recurring flag still becomes a lesson");
        }
    }
}
