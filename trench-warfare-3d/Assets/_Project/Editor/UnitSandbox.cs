// Phase: tooling (2026-09-27) — depends on: SimHost, ProvingGround.Place, UnitLook
// TW > Unit Sandbox: in Play, put any unit type on the field for either side, a few at a time, so every model and unit
// type can be watched without it being in anyone's roster. It spawns through ProvingGround.Place (every world the
// match has) in ranks at the side's rally point, and never touches a roster, a faction or the shipped game: an editor window only.
using UnityEditor;
using UnityEngine;
using TW.Sim;

namespace TW.Editor
{
    public sealed class UnitSandbox : EditorWindow
    {
        int count = 1;
        Vector2 scroll;

        [MenuItem("TW/Unit Sandbox")]
        static void Open() => GetWindow<UnitSandbox>("Unit Sandbox");

        void OnGUI()
        {
            if (!Application.isPlaying) { EditorGUILayout.HelpBox("Enter Play (GreyboxCorridor) to spawn units.", MessageType.Info); return; }
            var host = Object.FindFirstObjectByType<TW.Presentation.SimHost>();
            if (host == null || host.Local == null) { EditorGUILayout.HelpBox("No SimHost in this scene.", MessageType.Warning); return; }
            var w = host.Local.World;
            count = EditorGUILayout.IntSlider("How many", count, 1, 10);
            if (GUILayout.Button("Give both sides 5000 silver")) host.WriteWorlds(m => { m.World.Silver[0] += 5000; m.World.Silver[1] += 5000; });
            EditorGUILayout.Space();
            scroll = EditorGUILayout.BeginScrollView(scroll);
            for (int a = 0; a < Archetypes.Count; a++)
            {
                if (w.Units.Roster[a].Hp <= 0f) continue;   // an id no unit uses
                string name = TW.Presentation.UnitLook.Name((byte)a);
                if (string.IsNullOrEmpty(name)) continue;
                EditorGUILayout.BeginHorizontal();
                EditorGUILayout.LabelField($"{a,2}  {name}", GUILayout.Width(180));
                if (GUILayout.Button("Ours")) Spawn(host, 0, a);
                if (GUILayout.Button("Theirs")) Spawn(host, 1, a);
                EditorGUILayout.EndHorizontal();
            }
            EditorGUILayout.EndScrollView();
        }

        /// <summary>The Proving Ground's placing (ranks at the side's rally, the match table's numbers), in every world.</summary>
        void Spawn(TW.Presentation.SimHost host, int team, int archetype)
        {
            int placed = 0;
            bool ok = host.WriteWorlds(m => placed = TW.Presentation.ProvingGround.Place(m, team, (byte)archetype, count));
            Debug.Log($"Unit Sandbox: {placed} of {count} {TW.Presentation.UnitLook.Name((byte)archetype)} for team {team}" + (ok ? "" : " (worlds a tick apart: try again)"));
        }
    }
}
