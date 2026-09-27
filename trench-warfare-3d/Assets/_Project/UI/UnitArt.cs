// Phase: B6 (implemented) — the unit artwork: the painted concept of every unit cut out of its background, and for
// the ones with a face a six-state sheet (neutral, talking, shouting, cheering, wounded, critical) that the dialogue
// strip and the battlefield status reports speak through. Files live in Resources/UnitArt (full cutouts, 512 px) and
// Resources/UnitArt/States (<Name>_<state>, 256 px); names are HudText.PortraitName's, so an archetype finds its art
// the way it finds its card portrait. The Sergeant is the narrator and has no archetype.
using System.Collections.Generic;
using UnityEngine;

namespace TW.UI
{
    public enum Mood : byte { Neutral, Talking, Shouting, Cheering, Wounded, Critical }

    public static class UnitArt
    {
        public const string Folder = "UnitArt";
        public const string Narrator = "Sergeant";
        public static readonly string[] StateNames = { "neutral", "talking", "shouting", "cheering", "wounded", "critical" };
        /// <summary>Everyone with a face, hence a state sheet.</summary>
        public static readonly string[] Faces = { "Rifleman", "Assault", "MG", "Sniper", "Sergeant", "Pincer", "Kettle", "Maw" };
        /// <summary>Every full cutout shipped: the nineteen archetypes, the Cutter boat and the narrator. The seven
        /// units of 2026-09-25 are placeholder plaques (a bust, the one thing the unit carries, its name) and are
        /// replaced by taking their file names, exactly as a painted portrait replaces a generated one.</summary>
        public static readonly string[] Cutouts =
        {
            "Rifleman", "Assault", "MG", "Sniper", "Maw", "Tusk", "Pincer", "Kettle", "Censer", "Pavise", "Banner", "Redoubt",
            "Officer", "Shield", "Medic", "Engineer", "Para", "Jetpack", "Breaker", "Cutter", "Sergeant",
        };

        static readonly Dictionary<string, Texture2D> cache = new Dictionary<string, Texture2D>();

        public static bool HasFace(string name) => System.Array.IndexOf(Faces, name) >= 0;
        public static string NameOf(byte archetype) => HudText.PortraitName(archetype);

        public static string FullPath(string name) => Folder + "/" + name;
        public static string StatePath(string name, Mood m) => Folder + "/States/" + name + "_" + StateNames[(int)m];

        /// <summary>The full cutout, or null when the unit has none.</summary>
        public static Texture2D Full(string name) => Load(FullPath(name));

        /// <summary>The unit in a mood; falls back to neutral, then to the full cutout, so a caller always gets a face.</summary>
        public static Texture2D State(string name, Mood m)
        {
            if (!HasFace(name)) return Full(name);
            return Load(StatePath(name, m)) ?? Load(StatePath(name, Mood.Neutral)) ?? Full(name);
        }

        static Texture2D Load(string path)
        {
            if (cache.TryGetValue(path, out var t) && t != null) return t;
            t = Resources.Load<Texture2D>(path);
            if (t != null) cache[path] = t;
            return t;
        }
    }
}
