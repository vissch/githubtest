// Phase: C1 (stub) — depends on: MapData (P0)
// Editor window: paint heightfield, draw trench splines (→ cells, fire-steps, links), wire belts, mud, bunkers,
// objectives, spawn points, supply road, trigger volumes; export to a MapData asset (binary) + preview mesh.
// The three mission maps (docs/08-missions.md) are authored here.
using UnityEditor;
using UnityEngine;

namespace TW.Editor
{
    public sealed class MapAuthoringWindow : EditorWindow
    {
        [MenuItem("TW/Map Authoring")]
        public static void Open() => GetWindow<MapAuthoringWindow>("TW Map Authoring");

        void OnGUI()
        {
            EditorGUILayout.HelpBox("Phase C1: map authoring tool. Greybox maps come from GreyboxMapGenerator until this lands.", MessageType.Info);
        }
    }
}
