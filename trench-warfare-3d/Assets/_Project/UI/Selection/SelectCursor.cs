// Phase: B6 (implemented) — the cursor over a unit: four thin corner brackets round a dot, the selection bracket's
// off-white over ours and rust over the enemy (inspect), so the field says "this can be picked" before the click.
// Painted in code (like the marker hexagon), set only when it changes, and put back to the system arrow when the cursor
// leaves the men, when the field loses the input (aiming a barrage keeps its own reticle) and when the HUD goes.
using UnityEngine;

namespace TW.UI
{
    public sealed class SelectCursor : System.IDisposable
    {
        public const int Size = 32;
        public enum Kind : byte { None, Ours, Theirs }

        readonly Texture2D ours, theirs;
        Kind current;

        public SelectCursor()
        {
            ours = Paint(SelectionMarkers.Ours);
            theirs = Paint(SelectionMarkers.Theirs);
        }

        public void Set(Kind k)
        {
            if (k == current) return;
            current = k;
            var tex = k == Kind.Ours ? ours : k == Kind.Theirs ? theirs : null;
            Cursor.SetCursor(tex, tex != null ? new Vector2(Size / 2f, Size / 2f) : Vector2.zero, CursorMode.Auto);
        }

        public void Dispose()
        {
            Set(Kind.None);
            if (ours != null) Object.Destroy(ours);
            if (theirs != null) Object.Destroy(theirs);
        }

        /// <summary>Corner brackets and a centre dot in this colour, with a one-pixel dark contour so it reads on mud and snow.</summary>
        public static Texture2D Paint(Color c)
        {
            var t = new Texture2D(Size, Size, TextureFormat.RGBA32, false) { filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
            var ink = new bool[Size * Size];
            const int lo = 5, hi = Size - 6, arm = 7;
            for (int i = 0; i < arm; i++)
            {
                foreach (int y in new[] { lo, hi }) { Mark(ink, lo + i, y); Mark(ink, hi - i, y); }   // horizontal arms
                foreach (int x in new[] { lo, hi }) { Mark(ink, x, lo + i); Mark(ink, x, hi - i); }   // vertical arms
            }
            for (int y = 15; y <= 16; y++) for (int x = 15; x <= 16; x++) Mark(ink, x, y);           // the dot
            var px = new Color32[Size * Size];
            Color32 fill = c, edge = new Color32(10, 11, 12, 220);
            fill.a = 255;
            for (int y = 0; y < Size; y++) for (int x = 0; x < Size; x++)
            {
                if (ink[y * Size + x]) { px[y * Size + x] = fill; continue; }
                bool near = false;
                for (int dy = -1; dy <= 1 && !near; dy++) for (int dx = -1; dx <= 1 && !near; dx++)
                {
                    int nx = x + dx, ny = y + dy;
                    near = nx >= 0 && ny >= 0 && nx < Size && ny < Size && ink[ny * Size + nx];
                }
                px[y * Size + x] = near ? edge : new Color32(0, 0, 0, 0);
            }
            t.SetPixels32(px); t.Apply(false, false);   // stays readable: a hardware cursor needs the pixels
            return t;
        }

        static void Mark(bool[] ink, int x, int y) { if (x >= 0 && y >= 0 && x < Size && y < Size) ink[y * Size + x] = true; }
    }
}
