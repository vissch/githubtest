// Phase: tooling (owner, 2026-10-06) — "keep the names the optimiser and its stored results read; drop the other
// three": the report (PerfBench.cs) wrote the same three FrameBudget counts twice, once under frame_draw_calls /
// frame_vertices / frame_indirect_draws and again under frame_budget_draws / frame_budget_vertices /
// frame_budget_indirect, which only Tools/aosa/aosa.py and the stored runs read. The series are private and only
// a Play run builds them, so this reads the source instead.
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace TW.Tests
{
    public class BenchSeriesNameTests
    {
        const string Path = "Assets/_Project/Perf/PerfBench.cs";

        static readonly string[] Kept = { "frame_budget_draws", "frame_budget_vertices", "frame_budget_indirect" };
        static readonly string[] Dropped = { "frame_draw_calls", "frame_vertices", "frame_indirect_draws" };

        static string Code()
        {
            string src = File.ReadAllText(Path);
            src = Regex.Replace(src, @"/\*.*?\*/", m => Regex.Replace(m.Value, @"[^\n]", " "), RegexOptions.Singleline);
            return Regex.Replace(src, @"//[^\n]*", "");
        }

        static List<string> SeriesKeys(string code)
        {
            var keys = new List<string>();
            foreach (Match m in new Regex(@"New\(""([^""]+)""\)").Matches(code)) keys.Add(m.Groups[1].Value);
            foreach (Match m in new Regex(@"Counter\([^,]+,[^,]+,\s*""([^""]+)""").Matches(code)) keys.Add(m.Groups[1].Value);
            return keys;
        }

        [Test]
        public void TheReportWritesTheFrameBudgetCountsOnce()
        {
            Assert.That(File.Exists(Path), Is.True, Path + " is missing: the test would pass by reading nothing");
            string code = Code();

            var keys = SeriesKeys(code);
            Assert.That(keys.Count, Is.GreaterThan(20), "the scan found almost no series; PerfBench's shape moved");

            foreach (var name in Kept)
                Assert.That(keys.FindAll(k => k == name).Count, Is.EqualTo(1), name + " must appear exactly once");

            foreach (var name in Dropped)
                Assert.That(code.Contains(name), Is.False, name + " must be gone: the owner kept only the frame_budget_* names");
        }
    }
}
