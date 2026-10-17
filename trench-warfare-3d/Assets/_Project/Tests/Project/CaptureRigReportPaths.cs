// Phase: review-fix (2026-10-17) — guards found-create-the-report-folder-only-when-the-path-20261017-0.
using System;
using System.IO;
using NUnit.Framework;
using TW.Editor;

namespace TW.Tests
{
    public class CaptureRigReportPaths
    {
        // [found-create-the-report-folder-only-when-the-path-20261017-0]
        [Test]
        public void Report_Path_Without_A_Folder_Is_Written_Anyway()
        {
            string bare = "tw-capturerig-pathtest.json";
            string nestedDir = Path.Combine(Path.GetTempPath(), "tw-capturerig-" + Guid.NewGuid().ToString("N"));
            string nested = Path.Combine(nestedDir, "sub", "report.json");
            try
            {
                CaptureRig.Textures(bare, 1);
                Assert.IsTrue(File.Exists(bare),
                    "[found-create-the-report-folder-only-when-the-path-20261017-0] a bare file name wrote no report");

                CaptureRig.Textures(nested, 1);
                Assert.IsTrue(File.Exists(nested),
                    "[found-create-the-report-folder-only-when-the-path-20261017-0] a nested path wrote no report");
            }
            finally
            {
                if (File.Exists(bare)) File.Delete(bare);
                if (Directory.Exists(nestedDir)) Directory.Delete(nestedDir, true);
            }
        }
    }
}
