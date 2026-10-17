// Phase: code look 2026-10-17 (found-chunk-cursor-is-moved-inside-the-loop-that-r-20261017-0). chunkCursor was read at
// the top of the loop and assigned twice inside the same body, so a frame's pass drifted two chunks for every one it
// finished. ChunkPass plans the whole pass up front, before any chunk moves the cursor.
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class TerrainChunkPassTests
    {
        const string Path = "Assets/_Project/Presentation/Terrain/GreyboxTerrainView.cs";

        // [found-chunk-cursor-is-moved-inside-the-loop-that-r-20261017-0]
        [Test]
        public void APassVisitsEveryChunkOnce()
        {
            var order = new List<int>();
            GreyboxTerrainView.ChunkPass(0, 10, order);
            Assert.AreEqual(new List<int> { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, order,
                "[found-chunk-cursor-is-moved-inside-the-loop-that-r-20261017-0] a pass starting at 0 must visit 0..9 once each");

            order.Clear();
            GreyboxTerrainView.ChunkPass(7, 10, order);
            Assert.AreEqual(new List<int> { 7, 8, 9, 0, 1, 2, 3, 4, 5, 6 }, order,
                "[found-chunk-cursor-is-moved-inside-the-loop-that-r-20261017-0] a pass starting at 7 must wrap and visit each index once");

            // The oracle: the fault as the finding describes it, written out independently of the production code.
            int cursor = 0;
            var faulty = new List<int>();
            for (int n = 0; n < 5; n++)
            {
                int i = (cursor + n) % 10;
                faulty.Add(i);
                cursor = (i + 1) % 10;
            }
            Assert.AreEqual(new List<int> { 0, 2, 5, 9, 4 }, faulty,
                "[found-chunk-cursor-is-moved-inside-the-loop-that-r-20261017-0] the oracle no longer matches the finding's own bad sequence");
        }

        // [found-chunk-cursor-is-moved-inside-the-loop-that-r-20261017-0]
        [Test]
        public void TheChunkLoopDoesNotReadTheCursorItMoves()
        {
            Assert.That(File.Exists(Path), Is.True, Path + " is missing: the test would pass by reading nothing");
            string src = File.ReadAllText(Path);
            src = Regex.Replace(src, @"/\*.*?\*/", m => Regex.Replace(m.Value, @"[^\n]", " "), RegexOptions.Singleline);
            src = Regex.Replace(src, @"//[^\n]*", "");

            var bad = new List<string>();
            foreach (Match m in Regex.Matches(src, @"\bchunkCursor\b"))
            {
                int end = m.Index + m.Length;
                string after = src.Substring(end, System.Math.Min(40, src.Length - end));
                bool isDecl = Regex.IsMatch(src.Substring(System.Math.Max(0, m.Index - 8), System.Math.Min(8, m.Index)) + m.Value + after, @"int\s+chunkCursor\s*;");
                bool isAssign = Regex.IsMatch(after, @"^\s*=(?!=)");
                bool isHelperCall = Regex.IsMatch(after, @"^\s*,");
                if (isDecl || isAssign || isHelperCall) continue;
                int line = 1;
                for (int i = 0; i < m.Index; i++) if (src[i] == '\n') line++;
                bad.Add(Path + ":" + line);
            }

            Assert.That(bad, Is.Empty,
                "[found-chunk-cursor-is-moved-inside-the-loop-that-r-20261017-0] chunkCursor is read somewhere other than its " +
                "declaration, an assignment, or the ChunkPass(chunkCursor, ...) call that plans the pass before any chunk moves it:\n" +
                string.Join("\n", bad));
        }
    }
}
