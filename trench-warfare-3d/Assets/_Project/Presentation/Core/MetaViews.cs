// Phase: B6 / docs/21 phase 6 (implemented) — the seam between the campaign screens (TW.UI) and the 3D views behind
// them (TW.Presentation.Meta: the Home Front diorama and the strategic map). TW.UI cannot reference the terrain
// assemblies, so the screens talk to these interfaces and the Meta assembly installs its factories into MetaServices
// when it loads; a test installs a fake. Everything a view is told is plain data: which buildings at which stage,
// which country nodes in which state.
using System;
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation
{
    public enum NodeState : byte { Locked = 0, Available = 1, Current = 2, Complete = 3 }

    /// <summary>A country node as the map draws it.</summary>
    public struct NodeView
    {
        public string Id, Name;
        public Vector2 MapPos;   // 0..1 across the continent
        public NodeState State;
        public int Done, Total;  // missions
    }

    /// <summary>A Home Front building as the diorama draws it: which model of which set, where, and how far built
    /// (the stage's share of the model's height, its chimneys and lamps: the table in TW.UI decides, the view draws).</summary>
    public struct BuildingView
    {
        public string Id, Name, Set, Model;
        public int Stage;        // 0..3
        public Vector3 Place;
        public float Yaw;
        public float ShownHeight; // 0..1 of the model's height
        public int Chimneys, Lamps;
    }

    public interface IHomeFrontView
    {
        void Show(byte faction, IReadOnlyList<BuildingView> buildings);
        /// <summary>The building's new stage, as a view carrying the id; animate raises it over a moment.</summary>
        void SetStage(BuildingView view, bool animate);
        void Highlight(string id);
        void Hide();
        event Action<string> Picked;
    }

    public interface IStrategicMapView
    {
        void Show(IReadOnlyList<NodeView> nodes, IReadOnlyList<string> frontLine);
        void SetState(string id, NodeState state);
        void Focus(string id, bool animate);
        void Hide();
        event Action<string> Picked;
    }

    /// <summary>The live views and the factories that make them. The Meta assembly fills the factories at load; a
    /// screen asks Ensure* for a view and gets the same one until Reset. Reset when a scene ends (SceneStatics).</summary>
    public static class MetaServices
    {
        public static Func<IHomeFrontView> MakeHomeFront;
        public static Func<IStrategicMapView> MakeMap;
        public static IHomeFrontView HomeFront { get; private set; }
        public static IStrategicMapView Map { get; private set; }

        static MetaServices() => SceneStatics.Register(nameof(MetaServices), Reset);

        public static IHomeFrontView EnsureHomeFront()
        {
            if (HomeFront == null && MakeHomeFront != null) HomeFront = MakeHomeFront();
            return HomeFront;
        }

        public static IStrategicMapView EnsureMap()
        {
            if (Map == null && MakeMap != null) Map = MakeMap();
            return Map;
        }

        /// <summary>Forget the live views (the scene they lived in is gone); the factories stay.</summary>
        public static void Reset() { HomeFront = null; Map = null; }
    }
}
