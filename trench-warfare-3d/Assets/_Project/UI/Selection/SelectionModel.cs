// Phase: B6 (implemented) — what the player has selected, and the ten control groups (owner, 2026-09-23: selection
// "as sleek and great as Dust Front"; selection is for inspecting and grouping, the trenches keep the orders). A unit
// is held by (slot, generation): the sim reuses a dead man's slot, and the generation bump is what tells a stale
// handle from the new man standing in it. Pure data, no Unity objects, so the rules are tested without a scene.
using System;
using System.Collections.Generic;

namespace TW.UI
{
    /// <summary>A unit as the selection holds it: its sim slot and the spawn generation it had when selected.</summary>
    public readonly struct UnitHandle : IEquatable<UnitHandle>
    {
        public readonly int Slot;
        public readonly ushort Gen;
        public UnitHandle(int slot, ushort gen) { Slot = slot; Gen = gen; }
        public bool Equals(UnitHandle o) => Slot == o.Slot && Gen == o.Gen;
        public override bool Equals(object o) => o is UnitHandle h && Equals(h);
        public override int GetHashCode() => Slot * 65599 + Gen;
        public override string ToString() => $"{Slot}#{Gen}";
    }

    public sealed class SelectionModel
    {
        /// <summary>Groups 1-9 and 0 (the tenth), keyed by the digit.</summary>
        public const int GroupCount = 10;

        readonly List<UnitHandle> items = new List<UnitHandle>();
        readonly HashSet<UnitHandle> set = new HashSet<UnitHandle>();
        readonly List<UnitHandle>[] groups = new List<UnitHandle>[GroupCount];

        public SelectionModel() { for (int g = 0; g < GroupCount; g++) groups[g] = new List<UnitHandle>(); }

        public IReadOnlyList<UnitHandle> Items => items;
        public int Count => items.Count;
        /// <summary>Bumps on every change, so a panel redraws only when the selection moved.</summary>
        public int Version { get; private set; }

        public bool Contains(UnitHandle h) => set.Contains(h);

        public void Clear() { if (items.Count == 0) return; items.Clear(); set.Clear(); Version++; }

        public void Set(IEnumerable<UnitHandle> hs)
        {
            items.Clear(); set.Clear();
            foreach (var h in hs) if (set.Add(h)) items.Add(h);
            Version++;
        }

        public bool Add(UnitHandle h) { if (!set.Add(h)) return false; items.Add(h); Version++; return true; }

        public bool Remove(UnitHandle h) { if (!set.Remove(h)) return false; items.Remove(h); Version++; return true; }

        /// <summary>Shift-click: in if it was out, out if it was in.</summary>
        public void Toggle(UnitHandle h) { if (!Remove(h)) Add(h); }

        /// <summary>Drop every handle whose unit died or whose slot was reused, from the selection and every group.</summary>
        public void Prune(Func<UnitHandle, bool> alive)
        {
            int before = items.Count;
            for (int i = items.Count - 1; i >= 0; i--) if (!alive(items[i])) { set.Remove(items[i]); items.RemoveAt(i); }
            if (items.Count != before) Version++;
            foreach (var g in groups) g.RemoveAll(h => !alive(h));
        }

        /// <summary>Ctrl+digit: the group becomes the current selection (an empty selection empties the group).</summary>
        public void Assign(int group)
        {
            if (group < 0 || group >= GroupCount) return;
            groups[group].Clear(); groups[group].AddRange(items);
        }

        /// <summary>Shift+digit: the selection becomes the group. False when the group is empty (nothing changes).</summary>
        public bool Recall(int group)
        {
            if (group < 0 || group >= GroupCount || groups[group].Count == 0) return false;
            Set(groups[group]);
            return true;
        }

        public int GroupSize(int group) => group >= 0 && group < GroupCount ? groups[group].Count : 0;
        public IReadOnlyList<UnitHandle> Group(int group) => groups[group];

        /// <summary>The digit a group is recalled with: groups 0-8 are keys 1-9, group 9 is key 0.</summary>
        public static int KeyDigit(int group) => group == 9 ? 0 : group + 1;
    }
}
