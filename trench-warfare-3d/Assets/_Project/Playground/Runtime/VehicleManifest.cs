// Phase: Playground (2026-09-26, lane/show/playground) — tank3.json for JsonUtility
// tank3.json as JsonUtility reads it (Tools/tank3split.py writes list forms of every map for this).
using System;
using UnityEngine;

namespace TW.Playground
{
    [Serializable]
    public sealed class VehicleManifest
    {
        [Serializable] public sealed class PartDef { public string name; public string parent; public float[] pivot; public int tier; public float mass; }
        [Serializable] public sealed class SocketDef { public string name; public string part; public float[] pos; }
        [Serializable] public sealed class LodPart { public string name; public int verts; public int tris; public float[] min; public float[] max; }
        [Serializable] public sealed class LodDef { public int lod; public int verts; public int tris; public LodPart[] parts; }

        public string name;
        public float scale;
        public bool walker;                 // Tools/mechsplit.py: legs to walk on (WalkerDrive)
        public bool flyer;                  // Tools/mechsplit.py TW_KIND=flyer: flies (FlyerDrive)
        public PartDef[] partList;
        public SocketDef[] socketList;
        public LodDef[] lodList;

        public static VehicleManifest Parse(TextAsset json) => JsonUtility.FromJson<VehicleManifest>(json.text);
        public static Vector3 V(float[] a) => a != null && a.Length >= 3 ? new Vector3(a[0], a[1], a[2]) : Vector3.zero;

        public int Find(string part)
        {
            for (int i = 0; i < partList.Length; i++) if (partList[i].name == part) return i;
            return -1;
        }
    }
}
