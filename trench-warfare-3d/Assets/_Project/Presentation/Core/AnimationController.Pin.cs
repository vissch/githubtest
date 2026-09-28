// Phase: tooling (the gym, 2026-09-28) — part of AnimationController: hold one man on one clip, for the gym (Perf/GymDirector,
// TW > Gym) to show every clip of the atlas on a real battle figure. Presentation only: the sim never hears of it, so a
// pinned man still moves, shoots and dies as the sim says; only the drawn clip is held. Tick asks Pinned before Decide:
// a living man with a pin plays the pinned clip (a loop keeps running, a one-shot plays, holds PinHold seconds and plays
// again) instead of climbing the ladder. Death is decided before the ladder, so a pinned man who dies still dies. A pin
// belongs to the man, not the slot: when the slot is filled by someone else (a new generation) the pin lets go.
// The arrays are made on the first Pin, so a match that never pins costs one int compare per man per tick, no memory.
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Presentation
{
    public sealed partial class AnimationController
    {
        /// <summary>Seconds a pinned one-shot rests on its last frame before it plays again.</summary>
        public const float PinHold = 0.5f;

        NativeArray<byte> pinClip;      // the Clip held per slot, 0 (Clip.None) = not pinned
        NativeArray<ushort> pinGen;     // the generation of the man it was pinned on
        NativeArray<float> pinRate;
        /// <summary>How many men are pinned now (0: Tick skips the check).</summary>
        public int PinnedCount { get; private set; }

        /// <summary>Hold the man in `slot` on `clip` at `rate`. `generation` is the WORLD's (SimWorld.Generation[slot]): a man
        /// spawned this frame is not in State until the next Tick, so State's generation would be the slot's last man's
        /// and the pin would let go on its first tick. Clip.None lets him go. False for a slot out of range.</summary>
        public bool Pin(int slot, Clip clip, ushort generation, float rate = 1f)
        {
            if (!State.IsCreated || slot < 0 || slot >= State.Length) return false;
            if (clip == Clip.None) { Unpin(slot); return true; }
            if (clip >= Clip.Count) return false;
            if (!pinClip.IsCreated)
            {
                pinClip = new NativeArray<byte>(State.Length, Allocator.Persistent);
                pinGen = new NativeArray<ushort>(State.Length, Allocator.Persistent);
                pinRate = new NativeArray<float>(State.Length, Allocator.Persistent);
            }
            if (pinClip[slot] == 0) PinnedCount++;
            pinClip[slot] = (byte)clip; pinGen[slot] = generation; pinRate[slot] = math.max(0.05f, rate);
            return true;
        }

        public void Unpin(int slot)
        {
            if (!pinClip.IsCreated || slot < 0 || slot >= pinClip.Length || pinClip[slot] == 0) return;
            pinClip[slot] = 0; PinnedCount--;
        }

        public void UnpinAll()
        {
            if (!pinClip.IsCreated) return;
            for (int i = 0; i < pinClip.Length; i++) pinClip[i] = 0;
            PinnedCount = 0;
        }

        /// <summary>The clip `slot` is pinned on, or Clip.None.</summary>
        public Clip PinnedClip(int slot) => pinClip.IsCreated && slot >= 0 && slot < pinClip.Length ? (Clip)pinClip[slot] : Clip.None;

        /// <summary>From Tick, for a living man: true when a pin chose his clip this tick (the ladder is skipped).</summary>
        bool Pinned(int i, ref AnimState s)
        {
            byte c = pinClip[i];
            if (c == 0) return false;
            if (pinGen[i] != s.Generation) { pinClip[i] = 0; PinnedCount--; return false; }   // another man has the slot now
            var clip = (Clip)c;
            var info = Clips.Table[c];
            float rate = pinRate[i];
            bool restart = s.Clip != clip
                || (!info.Loop && s.Frame >= info.Seconds - 0.02f && (tick - s.ClipStart) * tickSeconds >= info.Seconds / rate + PinHold);
            if (restart) { Start(i, ref s, clip, Rung.Action, i == FollowSlot ? "gym pin " + clip : null, rate); s.Frame = 0f; }   // from its first frame, so "mid-clip" is mid-clip
            s.Rate = rate;
            return true;
        }

        void DisposePins()
        {
            if (pinClip.IsCreated) pinClip.Dispose();
            if (pinGen.IsCreated) pinGen.Dispose();
            if (pinRate.IsCreated) pinRate.Dispose();
            PinnedCount = 0;
        }
    }
}
