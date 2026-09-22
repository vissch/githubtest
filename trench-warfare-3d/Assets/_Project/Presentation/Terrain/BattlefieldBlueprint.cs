// Phase: B2 (declarative composition templates shared by generated battlefields)
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public sealed class BattlefieldBlueprint
    {
        public readonly struct Socket
        {
            public readonly string Name;
            public readonly BattlefieldKit.Module Module;
            public readonly Matrix4x4 Local;
            public readonly bool Grounded;
            public Socket(string name, BattlefieldKit.Module module, Vector3 position, Vector3 rotation, Vector3 scale, bool grounded = false)
            { Name = name; Module = module; Local = Matrix4x4.TRS(position, Quaternion.Euler(rotation), scale); Grounded = grounded; }
        }
        public readonly string Name;
        public readonly Socket[] Sockets;
        public readonly Bounds Footprint;
        public readonly Bounds Foundation;
        public readonly Vector3 Entrance;
        public BattlefieldBlueprint(string name, Vector3 entrance, params Socket[] sockets)
        {
            Name = name; Entrance = entrance; Sockets = sockets;
            if (sockets.Length == 0) throw new System.ArgumentException("A composition needs at least one socket.", nameof(sockets));
            Foundation = default;
            var bounds = new Bounds(entrance, Vector3.zero);
            bool first = true;
            foreach (var socket in sockets)
            {
                var b = socket.Module.Mesh.bounds;
                var partBounds = new Bounds(socket.Local.MultiplyPoint3x4(b.center), Vector3.zero);
                for (int corner = 0; corner < 8; corner++)
                {
                    var p = b.center + Vector3.Scale(b.extents, new Vector3((corner & 1) == 0 ? -1 : 1, (corner & 2) == 0 ? -1 : 1, (corner & 4) == 0 ? -1 : 1));
                    partBounds.Encapsulate(socket.Local.MultiplyPoint3x4(p));
                }
                bounds.Encapsulate(partBounds);
                if (first || socket.Name == "shell") Foundation = partBounds;
                first = false;
            }
            Footprint = bounds;
        }

        public static BattlefieldBlueprint[] Stock(BattlefieldKit k)
        {
            var result = new List<BattlefieldBlueprint>();
            for (int variant = 0; variant < 2; variant++)
                result.Add(new BattlefieldBlueprint(variant == 0 ? "Timber supply recess" : "Damaged concrete post", new Vector3(.2f, 0f, -1.65f),
                    new Socket("shell", variant == 0 ? k.dugout : k.bunker, Vector3.zero, Vector3.zero, Vector3.one),
                    new Socket("buried roof and shoulders", k.roof, Vector3.zero, Vector3.zero, Vector3.one),
                    new Socket("wall storage", k.supplies, new Vector3(-.8f, 0f, -.25f), new Vector3(0f, 8f, 0f), Vector3.one),
                    new Socket("stacked storage", k.supplies, new Vector3(-.75f, .76f, -.20f), new Vector3(0f, -5f, 0f), Vector3.one * .85f),
                    new Socket("right threshold crate", k.supplies, new Vector3(2.25f, 0f, -1.25f), new Vector3(0f, 17f, 0f), Vector3.one, true),
                    new Socket("recess supplies", k.supplies, new Vector3(.8f, 0f, .4f), new Vector3(0f, 82f, 0f), new Vector3(.7f, 1.5f, .7f)),
                    new Socket("entrance bags", k.sandbags, new Vector3(-2.1f, 0f, -1.55f), new Vector3(0f, -24f, 0f), new Vector3(.9f, 1f, 1f), true),
                    new Socket("threshold boards", k.duckboards, new Vector3(.2f, 0f, -1.85f), Vector3.zero, new Vector3(.8f, 1f, .85f), true)));
            // the imported sets (owner, 2026-09-22): a sod-roofed timber shelter half fallen in, and a concrete pillbox with
            // its rubble. The shelter's opening is its front (+Z), turned to face the entrance side (-Z) like the others.
            result.Add(new BattlefieldBlueprint("Sod-roofed shelter", new Vector3(0f, 0f, -1.55f),
                new Socket("shell", k.sodShelter, Vector3.zero, new Vector3(0f, 180f, 0f), Vector3.one),
                new Socket("left gabion", k.gabion, new Vector3(-1.75f, 0f, -1.15f), new Vector3(0f, 40f, 0f), Vector3.one * .85f, true),
                new Socket("right gabion", k.gabion, new Vector3(1.72f, 0f, -1.3f), new Vector3(0f, -25f, 0f), Vector3.one * .78f, true),
                new Socket("shells by the wall", k.shellStack, new Vector3(2.05f, 0f, .35f), new Vector3(0f, -82f, 0f), Vector3.one * .9f, true),
                new Socket("dropped sack", k.sandbag, new Vector3(-1.05f, 0f, -1.95f), new Vector3(0f, 28f, 0f), Vector3.one, true),
                new Socket("threshold boards", k.duckboards, new Vector3(0f, 0f, -1.85f), Vector3.zero, new Vector3(.75f, 1f, .8f), true)));
            result.Add(new BattlefieldBlueprint("Concrete pillbox", new Vector3(0f, 0f, -2.0f),
                new Socket("shell", k.pillbox, Vector3.zero, new Vector3(0f, 22f, 0f), Vector3.one),
                new Socket("blown slab", k.rebarSlab, new Vector3(-2.35f, 0f, -.8f), new Vector3(0f, 35f, 8f), Vector3.one * .55f, true),
                new Socket("broken slab", k.rebarSlab, new Vector3(2.25f, 0f, .95f), new Vector3(0f, -60f, -6f), Vector3.one * .45f, true),
                new Socket("sandbag wall", k.sandbags, new Vector3(-1.85f, 0f, -1.95f), new Vector3(0f, -35f, 0f), new Vector3(.9f, 1f, 1f), true),
                new Socket("loose sack", k.sandbag, new Vector3(1.55f, 0f, -2.05f), new Vector3(0f, 70f, 0f), Vector3.one, true),
                new Socket("gabion", k.gabion, new Vector3(2.1f, 0f, -1.05f), new Vector3(0f, 10f, 0f), Vector3.one * .82f, true)));
            return result.ToArray();
        }
    }
}
