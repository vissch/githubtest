// Phase: B2 (editor stand-in for one batched prop; see EnvPropEditor)
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    /// <summary>
    /// An imported prop picked in the Scene view (EnvPropEditor): a GameObject the usual move, rotate and scale tools work
    /// on while it is selected. Its batched instance is held out of the draw (BattlefieldProps.Hold) until it is deselected.
    /// </summary>
    public sealed class PropHandle : MonoBehaviour
    {
        public string Key, Module;
        public bool Added;
        /// <summary>Set by the editor for the handles it made; a copy (Ctrl+D) or an undone delete arrives without it.</summary>
        [System.NonSerialized] public bool Registered;

        public static readonly HashSet<PropHandle> All = new HashSet<PropHandle>();
        void OnEnable() => All.Add(this);
        void OnDisable() => All.Remove(this);
    }
}
