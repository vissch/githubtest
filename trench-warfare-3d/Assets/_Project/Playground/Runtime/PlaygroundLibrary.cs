// Phase: Playground (2026-09-26, lane/show/playground) — the asset list the playground spawns from
// The playground's asset list: what can be spawned in it and the files each thing is made of. Built by
// TW/Playground/Build (Editor/PlaygroundSetup.cs) from Playground/Art, never edited by hand. The playground scene holds
// the one reference to it, so nothing here goes into a player build unless that scene does.
using System;
using UnityEngine;

namespace TW.Playground
{
    public sealed class PlaygroundLibrary : ScriptableObject
    {
        /// <summary>A vehicle split by Tools/tank3split.py: one FBX and one atlas per LOD, and the manifest.</summary>
        [Serializable]
        public sealed class VehicleEntry
        {
            public string Name;
            public TextAsset Manifest;          // tank3.json
            public GameObject[] Lods;           // <Name>_LOD0..n.fbx
            public Texture2D[] Atlas;           // <Name>_LOD<k>_Base.jpg, one per LOD (each LOD has its own UV layout)
        }

        /// <summary>A figure rigged by Tools/frogrig.py: one FBX holding the armature and every LOD mesh.</summary>
        [Serializable]
        public sealed class UnitEntry
        {
            public string Name;
            public GameObject Model;            // Frog.fbx: Armature + <Name>_LOD0..n skinned meshes
            public Texture2D[] Atlas;           // one per LOD; a LOD past the end uses the last
            public TextAsset Rig;               // frogrig.json
        }

        /// <summary>A clip of the game's own (Art/Characters/Clips, Mixamo, Generic), played on the unit by retargeting.</summary>
        [Serializable]
        public sealed class ClipEntry
        {
            public string Name;
            public AnimationClip Clip;
            public bool Loop;
            public bool Death;
        }

        public VehicleEntry[] Vehicles = new VehicleEntry[0];
        public UnitEntry[] Units = new UnitEntry[0];
        public ClipEntry[] Clips = new ClipEntry[0];
        /// <summary>The Mixamo-rigged figure the clips are sampled on before they are carried over (Art/Characters/Soldier.fbx):
        /// the same rig the game's VAT baker samples them on, so the clips are read exactly as the game reads them.</summary>
        public GameObject ClipSource;
        /// <summary>Clip whose first frame is the source character standing (hip height reference), as VATBaker does.</summary>
        public string ReferenceClip = "Rifle Idle";

        public int ClipIndex(string name)
        {
            for (int i = 0; i < Clips.Length; i++) if (string.Equals(Clips[i].Name, name, StringComparison.OrdinalIgnoreCase)) return i;
            for (int i = 0; i < Clips.Length; i++) if (Clips[i].Name.IndexOf(name, StringComparison.OrdinalIgnoreCase) >= 0) return i;
            return -1;
        }
    }
}
