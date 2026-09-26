// Phase: tooling (docs/21 phase 1) — writes the asset scale audit (AssetScaleReport) to docs/reference/asset-scale.md.
// From the menu (TW/Audit/Asset Scale) in an open editor, or with the editor closed:
//   Unity.exe -batchmode -quit -projectPath <checkout>/trench-warfare-3d -executeMethod TW.Editor.AssetScaleAudit.Run -logFile audit.log
// (TW_AUDIT_OUT overrides the output path). It builds the whole kit once, so it takes a few seconds.
using System.IO;
using UnityEditor;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Editor
{
    public static class AssetScaleAudit
    {
        public const uint Seed = 1917;
        public static string DefaultPath => Path.GetFullPath(Path.Combine(Application.dataPath, "..", "..", "docs", "reference", "asset-scale.md"));

        [MenuItem("TW/Audit/Asset Scale")]
        public static void Menu()
        {
            string path = Write(DefaultPath);
            Debug.Log("Asset scale audit written to " + path);
        }

        /// <summary>The batch entry point: -executeMethod TW.Editor.AssetScaleAudit.Run.</summary>
        public static void Run()
        {
            string path = System.Environment.GetEnvironmentVariable("TW_AUDIT_OUT");
            Write(string.IsNullOrEmpty(path) ? DefaultPath : path);
        }

        public static string Write(string path)
        {
            var kit = new BattlefieldKit();
            try
            {
                var layout = Resources.Load<PropLayout>(PropLayout.ResourcePath((int)Seed));
                var rows = AssetScaleReport.Measure(kit, layout, Seed);
                var edits = AssetScaleReport.Edits(layout);
                Directory.CreateDirectory(Path.GetDirectoryName(path));
                File.WriteAllText(path, AssetScaleReport.Markdown(rows, edits, Seed));
                int fails = 0; foreach (var r in rows) if (r.Verdict == "FAIL") fails++;
                Debug.Log("Asset scale audit: " + rows.Count + " rows, " + fails + " FAIL");
                return path;
            }
            finally { kit.Dispose(); }
        }
    }
}
