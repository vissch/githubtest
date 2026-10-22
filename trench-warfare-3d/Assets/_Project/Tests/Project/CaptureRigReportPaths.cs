// Phase: review-fix (2026-10-22) — guards rebase-create-the-report-folder-only-when-the-p-20261019.
using System;
using System.IO;
using NUnit.Framework;
using TW.Editor;

namespace TW.Tests
{
    public class CaptureRigReportPaths
    {
        // [rebase-create-the-report-folder-only-when-the-p-20261019]
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
                    "[rebase-create-the-report-folder-only-when-the-p-20261019] a bare file name wrote no report");

                CaptureRig.Textures(nested, 1);
                Assert.IsTrue(File.Exists(nested),
                    "[rebase-create-the-report-folder-only-when-the-p-20261019] a nested path wrote no report");
            }
            finally
            {
                if (File.Exists(bare)) File.Delete(bare);
                if (Directory.Exists(nestedDir)) Directory.Delete(nestedDir, true);
            }
        }
    }
}
