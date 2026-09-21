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
            return result.ToArray();
        }
    }
}
