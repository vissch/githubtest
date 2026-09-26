// Phase: B6 (implemented) — the mission select's map picture: the battlefield for a seed, coloured as the minimap
// colours it (HudMinimap), one nav cell per pixel. Generates the map with BattlefieldGenerator and disposes it; a
// seed's picture is what the player will see on the minimap when the match starts, so the two agree by construction.
using Unity.Collections;
using UnityEngine;
using TW.Presentation;
using TW.Sim.Terrain;

namespace TW.UI
{
    public static class MapThumbnail
    {
        public static Texture2D Render(uint seed) => Render(seed, Ground.ShelledForest);

        /// <summary>The picture of a seed on a ground (the campaign's coast and winter missions).</summary>
        public static Texture2D Render(uint seed, Ground ground)
        {
            var p = MatchLaunch.Field(ground, seed);
            MapData map;
            try { map = BattlefieldGenerator.Create(p, Allocator.Persistent); }
            catch (System.Exception e) { Debug.LogWarning($"MapThumbnail: could not generate seed {seed}: {e.Message}"); return null; }
            try
            {
                int tw = map.NavLength, th = map.NavWidth;
                var tex = new Texture2D(tw, th, TextureFormat.RGBA32, false) { filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, name = "MapThumbnail " + seed };
                var px = new Color32[tw * th];
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
                    px[(th - 1 - x) * tw + z] = c;
                }
                tex.SetPixels32(px); tex.Apply(false, false);
                return tex;
            }
            finally { map.Dispose(); }
        }
    }
}
