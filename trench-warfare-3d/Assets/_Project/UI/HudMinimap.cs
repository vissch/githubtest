// Phase: B6 (implemented) — the minimap: the ground as the nav layers colour it, both sides' men as dots, the camera's
// frame, and click or drag to move the view. Port of BattleHud.Minimap (482-558): same colours, same cadences (ground
// every 3 s, dots every 0.15 s, on unscaled time), same orientation (u along world Z, you left and the enemy right;
// v along world X, X 0 at the top), same drag maths. The two textures are assigned once as background images and
// updated in place, so a frame that changes nothing writes nothing.
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.UI
{
    public sealed class HudMinimap
    {
        readonly HudRefs refs;
        readonly SimHost host;
        readonly TacticalCamera cam;
        Texture2D ground, dots;
        Color32[] dotPixels, groundPixels;
        float nextGround, nextDots;
        int tw, th;
        float lastVx = float.NaN, lastVy, lastVw, lastVh;
        bool dragging;

        public int MyMen { get; private set; }
        public int TheirMen { get; private set; }
        /// <summary>Minimap pixels per nav cell (reference px).</summary>
        public float Scale => HudLayout.MinimapScale;
        public float WidthPx => tw * Scale;
        public float HeightPx => th * Scale;

        static readonly Color32 Mine = new Color32(0x59, 0xD9, 0xFF, 255);     // --tw-team-a
        static readonly Color32 Theirs = new Color32(0xE0, 0x40, 0x29, 255);   // --tw-team-b

        public HudMinimap(HudRefs refs, SimHost host, TacticalCamera cam)
        {
            this.refs = refs; this.host = host; this.cam = cam;
            var map = host.Local.Map;
            tw = map.NavLength; th = map.NavWidth;
            ground = new Texture2D(tw, th, TextureFormat.RGBA32, false) { filterMode = FilterMode.Bilinear, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
            dots = new Texture2D(tw, th, TextureFormat.RGBA32, false) { filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
            dotPixels = new Color32[tw * th]; groundPixels = new Color32[tw * th];
            refs.Minimap.style.width = WidthPx; refs.Minimap.style.height = HeightPx;
            refs.MinimapGround.style.backgroundImage = new StyleBackground(ground);
            refs.MinimapDots.style.backgroundImage = new StyleBackground(dots);
            refs.Minimap.RegisterCallback<PointerDownEvent>(OnDown);
            refs.Minimap.RegisterCallback<PointerMoveEvent>(OnMove);
            refs.Minimap.RegisterCallback<PointerUpEvent>(OnUp);
            refs.Minimap.RegisterCallback<PointerCaptureOutEvent>(_ => dragging = false);
            nextGround = 0f; nextDots = 0f;
        }

        public void Dispose()
        {
            if (ground != null) Object.Destroy(ground);
            if (dots != null) Object.Destroy(dots);
        }

        public void Refresh()
        {
            var w = host.Local.World;
            var map = host.Local.Map;
            float now = Time.unscaledTime;
            if (now >= nextGround)   // craters, cut wire and floods change it slowly
            {
                nextGround = now + 3f;
                for (int x = 0; x < th; x++)
                for (int z = 0; z < tw; z++)
                {
                    var layer = (NavLayer)map.NavLayers[map.NavIndex(x, z)];
                    Color32 c = new Color32(92, 76, 54, 255);
                    if ((layer & NavLayer.Mud) != 0) c = new Color32(70, 57, 40, 255);
                    if ((layer & NavLayer.Crater) != 0) c = new Color32(58, 49, 38, 255);
                    if ((layer & NavLayer.Wire) != 0) c = new Color32(120, 118, 116, 255);
                    if ((layer & NavLayer.Blocked) != 0) c = map.WaterDepthAtCell(x, z) > 0.5f ? new Color32(52, 70, 74, 255) : new Color32(40, 52, 34, 255);
                    if ((layer & NavLayer.Trench) != 0) c = (layer & NavLayer.Link) != 0 ? new Color32(200, 170, 110, 255) : new Color32(30, 22, 16, 255);
                    groundPixels[(th - 1 - x) * tw + z] = c;   // texture rows run bottom-up
                }
                ground.SetPixels32(groundPixels); ground.Apply(false, false);
            }
            if (now >= nextDots)
            {
                nextDots = now + 0.15f;
                System.Array.Clear(dotPixels, 0, dotPixels.Length);
                int mine = 0, theirs = 0;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                    bool me = w.Team[i] == 0;
                    if (me) mine++; else theirs++;
                    var c = map.NavCellOf(w.Position[i]);
                    int u = c.y, v = th - 1 - c.x;
                    for (int dv = 0; dv < 2; dv++) for (int du = 0; du < 2; du++)
                    {
                        int uu = Mathf.Min(tw - 1, u + du), vv = Mathf.Max(0, v - dv);
                        dotPixels[vv * tw + uu] = me ? Mine : Theirs;
                    }
                }
                MyMen = mine; TheirMen = theirs;
                dots.SetPixels32(dotPixels); dots.Apply(false, false);
            }
            if (cam == null) return;
            // the view: a frame around the camera's focus, as wide as the ground it shows
            var unity = Camera.main;
            float aspect = unity != null ? unity.aspect : 16f / 9f;
            float viewZ = cam.Zoom * aspect * 0.55f, viewX = cam.Zoom * 0.55f;
            float cx = cam.Focus.y / MapData.NavCellSize * Scale, cy = cam.Focus.x / MapData.NavCellSize * Scale;
            float fw = Mathf.Clamp(viewZ * 2f / MapData.NavCellSize * Scale, 8f, WidthPx), fh = Mathf.Clamp(viewX * 2f / MapData.NavCellSize * Scale, 6f, HeightPx);
            float vx = Mathf.Clamp(cx - fw * 0.5f, 0f, WidthPx - fw), vy = Mathf.Clamp(cy - fh * 0.5f, 0f, HeightPx - fh);
            if (float.IsNaN(lastVx) || Mathf.Abs(vx - lastVx) > 0.5f || Mathf.Abs(vy - lastVy) > 0.5f || Mathf.Abs(fw - lastVw) > 0.5f || Mathf.Abs(fh - lastVh) > 0.5f)
            {
                refs.MinimapView.style.left = vx; refs.MinimapView.style.top = vy; refs.MinimapView.style.width = fw; refs.MinimapView.style.height = fh;
                lastVx = vx; lastVy = vy; lastVw = fw; lastVh = fh;
            }
        }

        void OnDown(PointerDownEvent e)
        {
            if (e.button != 0 || cam == null) return;
            dragging = true;
            refs.Minimap.CapturePointer(e.pointerId);
            MoveTo(e.localPosition);
            e.StopPropagation();
        }

        void OnMove(PointerMoveEvent e)
        {
            if (!dragging || !refs.Minimap.HasPointerCapture(e.pointerId)) return;
            MoveTo(e.localPosition);
            e.StopPropagation();
        }

        void OnUp(PointerUpEvent e)
        {
            if (refs.Minimap.HasPointerCapture(e.pointerId)) refs.Minimap.ReleasePointer(e.pointerId);
            dragging = false;
        }

        void MoveTo(Vector2 local)
        {
            if (cam == null) return;
            cam.Focus = new Vector2(local.y / Scale * MapData.NavCellSize, local.x / Scale * MapData.NavCellSize);
        }
    }
}
