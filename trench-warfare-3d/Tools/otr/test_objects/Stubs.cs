// Stand-ins for what Objects.cs and Json.cs name outside themselves, so both compile on their own (no Unity dlls, no
// Mono engine), plus the two types the [O14] callback case needs. The cases drive the extracted statics, not Install.
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

static class FakeEngineAccess
{
    static readonly List<GCHandle> kept = new List<GCHandle>();
    public static IntPtr Ptr(object o) => IntPtr.Zero;
    public static void Keep(GCHandle h) => kept.Add(h);
}

namespace UnityEngine
{
    /// <summary>Unity's own interface, by name: Json.cs finds it by reflection, exactly as it finds [SerializeField].</summary>
    public interface ISerializationCallbackReceiver
    {
        void OnBeforeSerialize();
        void OnAfterDeserialize();
    }
}

[Serializable]
class CallbackAsset : UnityEngine.ISerializationCallbackReceiver
{
    public int value;
    public static int Before, After;
    public void OnBeforeSerialize() { Before++; }
    public void OnAfterDeserialize() { After++; }
}

[Serializable]
class Wide
{
    public long big;
    public int xs;      // the depth cases send a deep array at a key this type does not have
}
