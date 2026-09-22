// Phase: B6 (implemented) — owns SimHost.TimeScale: one speed, several reasons to hold it at zero.
// The tactical pause (Space, the HUD stays live and orders queue in the lockstep driver), the Esc menu, the debrief
// and a key-capture are independent holds; the sim runs only when all are clear, at Speed (1, 2, 4, 8). Code that
// still writes Host.TimeScale directly (the IMGUI speed buttons) is adopted rather than fought: an external write to
// zero becomes a Tactical hold, any other value becomes the Speed. Two clocks exist in this game and this is the
// SIM one; Storm.Freeze/Thaw own Time.timeScale for the lightning freeze and would undo a menu that used it.
using System;
using UnityEngine;

namespace TW.Presentation
{
    [DefaultExecutionOrder(-150)]   // before SimHost.Update (-100) reads TimeScale
    public sealed class MatchClock : MonoBehaviour
    {
        [Flags]
        public enum Hold : byte { None = 0, Tactical = 1, Menu = 2, Debrief = 4, Modal = 8, Loading = 16 }

        public SimHost Host;
        /// <summary>The speed the sim runs at when nothing holds it: 1, 2, 4 or 8.</summary>
        public float Speed = 1f;
        public Hold Holds { get; private set; }
        public bool Paused => Holds != Hold.None;
        public bool Has(Hold h) => (Holds & h) != 0;
        public float Effective => Paused ? 0f : Speed;
        /// <summary>Raised when Holds or Speed change (the HUD's PAUSED plate and speed buttons redraw from it).</summary>
        public event Action Changed;

        public static readonly float[] Speeds = { 1f, 2f, 4f, 8f };
        float lastWritten = float.NaN;

        /// <summary>The clock on the host's GameObject, added if missing.</summary>
        public static MatchClock For(SimHost host)
        {
            if (host == null) return null;
            var c = host.GetComponent<MatchClock>();
            if (c == null) { c = host.gameObject.AddComponent<MatchClock>(); c.Host = host; }
            if (c.Host == null) c.Host = host;
            return c;
        }

        public void Add(Hold h) { if ((Holds & h) == h) return; Holds |= h; Push(); }
        public void Remove(Hold h) { if ((Holds & h) == 0) return; Holds &= ~h; Push(); }
        public void Toggle(Hold h) { if (Has(h)) Remove(h); else Add(h); }
        public void SetSpeed(float s)
        {
            s = Mathf.Clamp(s, 0.25f, 8f);
            if (Mathf.Approximately(s, Speed)) return;
            Speed = s; Push();
        }
        /// <summary>Step to the next / previous entry of Speeds; clamps at the ends.</summary>
        public void StepSpeed(int dir)
        {
            int i = Array.FindIndex(Speeds, v => Mathf.Approximately(v, Speed));
            if (i < 0) i = 0;
            SetSpeed(Speeds[Mathf.Clamp(i + dir, 0, Speeds.Length - 1)]);
        }

        void Push()
        {
            if (Host != null) lastWritten = Host.TimeScale = Effective;
            Changed?.Invoke();
        }

        void OnEnable() { if (Host == null) Host = GetComponent<SimHost>(); }

        void Update()
        {
            if (Host == null) return;
            // adopt writes from code that has not migrated
            if (!float.IsNaN(lastWritten) && !Mathf.Approximately(Host.TimeScale, lastWritten))
            {
                if (Host.TimeScale <= 0f) Holds |= Hold.Tactical;
                else { Speed = Host.TimeScale; Holds &= ~Hold.Tactical; }
                Changed?.Invoke();
            }
            lastWritten = Host.TimeScale = Effective;
        }
    }
}
