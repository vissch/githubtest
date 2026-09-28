// Phase: A3 (implemented 2026-09-28) — the owner, playing the game: "units are now walking in rows in seemingly
// defined paths, we need them to spread over the map instead."
// A flow field gives every man at one place the same way on, so a company walks the field's cheapest line in file.
// Each man therefore has a LANE: his own line across the width of the field, which he keeps to wherever the ground
// lets him. It is a pure function of his slot and generation (no state, nothing to hash): consecutive slots fall
// far apart (a golden-ratio sequence), so the men of one deployment cover the width between them instead of
// bunching. MoveJob and the vehicles turn the flow toward the lane with Turn; the flow still decides whether the
// turn is allowed (it never walks a man uphill in the field, into wire, or through a wall).
using Unity.Mathematics;

namespace TW.Sim
{
    public static class Lane
    {
        public const float Margin = 4f;   // metres of the field's edge no lane lies in
        public const float Pull = 0.9f;   // the most his lane turns him off the flow, as a share of his way on: 42 degrees
        public const float Ease = 10f;    // metres off his lane at which the pull is whole; nearer, it eases to nothing

        /// <summary>His line across the field, in metres from its left edge: the same on every machine.</summary>
        public static float Of(int slot, ushort generation, float width)
        {
            uint h = (uint)slot * 2654435769u + generation * 1640531527u;   // 2^32 / phi, and 2^32 / phi^2 for a slot's next tenant
            float t = (h >> 8) * (1f / 16777216f);
            return Margin + t * math.max(0f, width - 2f * Margin);
        }

        /// <summary>How far to turn off the flow (along its left-hand normal, -Pull..Pull) to make for the lane. Only
        /// the part of the way to the lane that lies ACROSS the flow counts: where the field itself runs sideways (along
        /// a wire belt to its gap) the lane does not fight it.</summary>
        public static float Turn(float2 flow, float x, float lane)
        {
            float across = (lane - x) * -flow.y;   // (lane - x, 0) on the flow's left-hand normal (-flow.y, flow.x)
            return math.sign(across) * Pull * math.saturate(math.abs(across) / Ease);
        }
    }
}
