// Phase: in-game effects review (2026-10-13) — the six effects a player sees most (EffectCensus counted them in a real
// GreyboxCorridor match: Shot, Explosion, CraterStamp, Hit, Death, PropChanged), each filmed in that same match with the
// game's own shaders, lights and LODs. An instrument, not a gate check: it asserts only that films were written.
// Every effect is made by the game's systems through TW.Editor.VfxLab (men who really shoot each other, a real shell,
// a real prop shelled down), never by a queued one-off burst.
// A film is a locked-off CaptureRig series, one still a frame. The rig poses one frame and renders the next, so the game
// clock is stepped 1/48 s a frame (Time.captureDeltaTime) and a still lands every 1/24 s of game time: 24 fps.
// Per effect three films: two takes at the zoom the game is played at (30) and one close (12). Frames and a stats json
// a film (FrameBudget draw calls and vertices while twenty guns fire; FlipbookFx exposes no live card count, so cards
// are not reported) go to TW_STILLS_DIR; the mp4s are made from the frames afterwards with ffmpeg.
// Env: TW_STILLS_DIR the folder (default Captures/effects/_frames), TW_STILLS_FIELD the look (Winter is the day field;
// the default is the scene's own night), TW_FILM_EFFECTS a comma list (default all six), TW_FILM_SECONDS (default 6).
// Run by name, with a graphics device: -runTests -testPlatform EditMode -testFilter TW.Tests.EffectFilmStills
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using NUnit.Framework;
using Unity.Mathematics;
using UnityEditor.SceneManagement;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.UI;

namespace TW.Tests
{
    public class EffectFilmStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        static readonly string[] AllEffects = { "muzzle", "impact", "shell", "crater", "death", "prop" };
        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>Open ground for an effect centred on (x, z): the same reading as VfxStills.Open.</summary>
        static bool Open(MapData map, float x, float z, float r)
        {
            var size = map.SizeMeters;
            for (float zz = z - r; zz <= z + r; zz += 2f)
                for (float xx = x - r; xx <= x + r; xx += 2f)
                {
                    if (xx < 3f || zz < 3f || xx > size.x - 3f || zz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(xx, 0f, zz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = map.Props[i].Pos;
                if (math.abs(q.x - x) < r && math.abs(q.z - z) < r) return false;
            }
            return true;
        }

        /// <summary>How long after the staging the camera starts rolling, so the effect fires inside the film.</summary>
        static float Lead(string effect)
        {
            switch (effect)
            {
                case "muzzle": case "impact": return 2.5f;   // the men acquire, then fire for the whole film
                case "shell": case "prop": return 0.9f;      // the burst is 0.3 s in: the film holds its pop-in too
                case "crater": return 2.6f;                  // the burst is over: crater, scorch and lingering smoke
                case "death": return 1.6f;
                default: return 1f;
            }
        }

        /// <summary>What a film is aimed at above the ground; NaN keeps the rig's default.</summary>
        static float AimY(string effect) => effect == "crater" ? 0.5f : float.NaN;

        /// <summary>Stages one effect at its spot with the game's own systems; returns what the camera looks at.</summary>
        static Vector2 Stage(string effect, float x, float z, StringBuilder log)
        {
            switch (effect)
            {
                case "muzzle":
                case "impact":
                    // a real firefight: ten riflemen a side, twenty guns, twenty metres apart
                    log.Append(VfxLab.Men(10, x - 8f, z - 10f, 0)).Append("; ");
                    log.Append(VfxLab.Men(10, x - 8f, z + 10f, 1));
                    return effect == "muzzle" ? new Vector2(x, z - 10f) : new Vector2(x, z + 10f);
                case "shell":
                case "crater":
                    log.Append(VfxLab.Later(1.2f, () => VfxLab.Shell(x, z)));
                    return new Vector2(x, z);
                case "death":
                    log.Append(VfxLab.Men(10, x - 8f, z, 1)).Append("; ");
                    log.Append(VfxLab.Later(2.0f, () => VfxLab.Shell(x, z, 6f, 400f)));
                    return new Vector2(x, z);
                case "prop":
                    log.Append(VfxLab.Later(1.2f, () => VfxLab.Shell(x, z, 5f, 900f)));
                    return new Vector2(x, z);
                default:
                    return new Vector2(x, z);
            }
        }

        /// <summary>The look asked for: TW_STILLS_FIELD, or -twfield on the command line. Read again after the test has
        /// entered Play, never kept in a local across it: entering Play reloads the domain and a local read before it
        /// came back empty, which filmed a whole day run in the scene's own night (2026-10-13).</summary>
        static string FieldName()
        {
            string name = System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD");
            var argv = System.Environment.GetCommandLineArgs();
            for (int i = 0; i < argv.Length - 1; i++) if (argv[i] == "-twfield") name = argv[i + 1];
            return name;
        }

        static IEnumerator Settle(float seconds)
        {
            float due = Time.time + seconds;
            while (Time.time < due) yield return null;
        }

        // Six effects, three films each, about a minute an effect: well past the test runner's 180 s default, which
        // killed the first run in the middle of a film.
        [UnityTest, Timeout(2400000), Explicit("the six commonest effects filmed in a real match: run by name, with a graphics device.")]
        public IEnumerator TheSixCommonestEffectsAreFilmed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            string fieldName = FieldName();
            TestContext.Out.WriteLine("field asked for: " + (string.IsNullOrEmpty(fieldName) ? "(none: the scene's own night)" : fieldName));
            if (System.Enum.TryParse(fieldName, true, out TW.Presentation.Terrain.Biome wantField))
            {
                foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = wantField;
                foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = wantField;
            }
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            // and again inside Play: the scene objects are rebuilt entering it, so the look set before would not hold
            if (System.Enum.TryParse(FieldName(), true, out TW.Presentation.Terrain.Biome playField))
            {
                foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = playField;
                foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = playField;
            }

            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Directory.GetCurrentDirectory(), "Captures", "effects", "_frames");
            Directory.CreateDirectory(dir);
            string wanted = System.Environment.GetEnvironmentVariable("TW_FILM_EFFECTS");
            string[] effects = string.IsNullOrEmpty(wanted) ? AllEffects : wanted.Split(',');
            float seconds = 6f;
            if (float.TryParse(System.Environment.GetEnvironmentVariable("TW_FILM_SECONDS"), NumberStyles.Float, Inv, out float fromEnv) && fromEnv > 1f)
                seconds = fromEnv;
            string look = System.Enum.TryParse(FieldName(), true, out TW.Presentation.Terrain.Biome shotField)
                ? shotField.ToString().ToLowerInvariant() : "night";

            // nothing that lights or wets the whole field, and no peer attack, may wander into a film
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            yield return Settle(1f);
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            var spots = new Dictionary<string, Vector2>();
            var used = new List<Vector2>();
            foreach (string effect in effects)
            {
                if (effect == "prop")
                {
                    // a prop the shell can bring down, not open ground
                    for (int i = 0; i < map.Props.Length; i++)
                    {
                        var p = map.Props[i].Pos;
                        bool clear = p.x > 20f && p.z > 20f && p.x < size.x - 20f && p.z < size.y - 20f;
                        foreach (var u in used) if (Mathf.Abs(u.x - p.x) < 30f && Mathf.Abs(u.y - p.z) < 30f) clear = false;
                        if (clear) { spots[effect] = new Vector2(p.x, p.z); break; }
                    }
                }
                else
                {
                    // the firefight wants a wide clear strip, but this map has none: give up room before giving up the
                    // effect (the first run found no ground at all for muzzle and impact and filmed neither)
                    float want = effect == "muzzle" || effect == "impact" ? 14f : 8f;
                    for (float room = want; room >= 5f && !spots.ContainsKey(effect); room -= 2f)
                        for (float zz = 40f; zz < size.y - 40f && !spots.ContainsKey(effect); zz += 6f)
                            for (float xx = 20f; xx < size.x - 20f && !spots.ContainsKey(effect); xx += 6f)
                            {
                                bool clear = true;
                                foreach (var u in used) if (Mathf.Abs(u.x - xx) < 40f && Mathf.Abs(u.y - zz) < 40f) clear = false;
                                if (clear && Open(map, xx, zz, room)) spots[effect] = new Vector2(xx, zz);
                            }
                }
                if (spots.ContainsKey(effect)) used.Add(spots[effect]);
                else TestContext.Out.WriteLine(effect + ": no ground for it");
            }

            Time.captureDeltaTime = 1f / 48f;   // two frames a still: a still every 1/24 s of game time
            int films = 0, stills = 0;
            foreach (string effect in effects)
            {
                if (!spots.ContainsKey(effect)) continue;
                float x = spots[effect].x, z = spots[effect].y;
                for (int take = 0; take < 3; take++)
                {
                    float zoom = take == 2 ? 12f : 30f;
                    string stem = "e_" + effect + "_" + look + "_" + (take == 2 ? "close" : take == 0 ? "take1" : "take2");
                    string shotDir = Path.Combine(dir, stem);
                    Directory.CreateDirectory(shotDir);
                    if (take > 0)
                    {
                        // clear what the last take left standing, then let its smoke go: the same spot, restaged
                        VfxLab.Shell(x, z, 16f, 50000f);
                        yield return Settle(9f);
                    }
                    var log = new StringBuilder();
                    var focus = Stage(effect, x, z, log);
                    TestContext.Out.WriteLine(stem + " at " + focus.x.ToString("0", Inv) + ", " + focus.y.ToString("0", Inv) + ": " + log);
                    yield return Settle(Lead(effect));

                    int frames = Mathf.RoundToInt(seconds * 24f);
                    if (tc != null) tc.BaseYaw = -90f;
                    CaptureRig.Series(shotDir, "f", focus.x, focus.y, zoom, 40f, 28f, frames, 1, 960, 540, AimY(effect));
                    int draws = 0, peakDraws = 0, samples = 0; long verts = 0;
                    for (int f = 0; f < frames * 40 + 1200 && CaptureRig.Pending() != "0"; f++)
                    {
                        yield return null;
                        int d = FrameBudget.DrawCalls;
                        if (d > 0) { draws += d; verts += FrameBudget.Vertices; samples++; if (d > peakDraws) peakDraws = d; }
                    }
                    int got = Directory.GetFiles(shotDir, "f_*.png").Length;
                    stills += got; if (got > 0) films++;
                    int meanDraws = samples > 0 ? draws / samples : 0;
                    var js = new StringBuilder();
                    js.Append("{\n");
                    js.Append("  \"film\": \"").Append(stem).Append("\",\n");
                    js.Append("  \"effect\": \"").Append(effect).Append("\",\n");
                    js.Append("  \"look\": \"").Append(look).Append("\",\n");
                    js.Append("  \"zoom\": ").Append(zoom.ToString("0.#", Inv)).Append(",\n");
                    js.Append("  \"focus\": [").Append(focus.x.ToString("0.#", Inv)).Append(", ").Append(focus.y.ToString("0.#", Inv)).Append("],\n");
                    js.Append("  \"seconds\": ").Append(seconds.ToString("0.#", Inv)).Append(",\n");
                    js.Append("  \"stills\": ").Append(got).Append(",\n");
                    js.Append("  \"draw_calls_mean\": ").Append(meanDraws).Append(",\n");
                    js.Append("  \"draw_calls_peak\": ").Append(peakDraws).Append(",\n");
                    js.Append("  \"vertices_mean\": ").Append(samples > 0 ? verts / samples : 0).Append(",\n");
                    js.Append("  \"staging\": \"").Append(log.ToString().Replace('"', '\'')).Append("\"\n");
                    js.Append("}\n");
                    File.WriteAllText(Path.Combine(dir, stem + "-stats.json"), js.ToString(), new UTF8Encoding(false));
                    TestContext.Out.WriteLine(stem + ": " + got + " stills, draws mean " + meanDraws + " peak " + peakDraws);
                }
            }
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            TestContext.Out.WriteLine("wrote " + films + " films, " + stills + " stills to " + dir);
            yield return new ExitPlayMode();
            Assert.Greater(stills, 0, "no stills were written");
        }
    }
}
