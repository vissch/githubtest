// Phase: P0 (implemented) — the platform gate from docs/03-determinism-rules.md
// Steps the greybox match for 2,000 ticks with a scripted command stream and writes the per-tick hashes to
// DeterminismReport-<platform>.txt in the project folder. Compare the files across machines.
using System.IO;
using System.Text;
using Unity.Collections;
using UnityEditor;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Editor
{
    public static class DeterminismPlatformReport
    {
        public const int Ticks = 2000;

        [MenuItem("TW/Determinism/Write Platform Report")]
        public static void Write()
        {
            var cfg = SimConfig.Default;
            cfg.StartingSilver = 5000;
            using var match = MatchSim.CreateGreybox(cfg);
            var sb = new StringBuilder();
            sb.AppendLine($"platform={Application.platform} arch={System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture} unity={Application.unityVersion}");
            sb.AppendLine($"seed={cfg.Seed:X} ticks={Ticks}");
            var cmds = new NativeList<SimCommand>(8, Allocator.Temp);
            for (uint t = 0; t < Ticks; t++)
            {
                cmds.Clear();
                if (t % 7 == 0) cmds.Add(SimCommand.Deploy(t, 0, (int)(t / 7) % 5));
                if (t % 11 == 0) cmds.Add(SimCommand.Deploy(t, 1, (int)(t / 11) % 5));
                match.Step(cmds.AsArray());
                sb.AppendLine($"{t} {match.World.LastHash:X16}");
            }
            cmds.Dispose();
            string file = Path.Combine(Directory.GetCurrentDirectory(), $"DeterminismReport-{Application.platform}-{System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture}.txt");
            File.WriteAllText(file, sb.ToString());
            Debug.Log($"TW: wrote {file}. Final hash {match.World.LastHash:X16}. Compare with reports from other platforms.");
        }
    }
}
