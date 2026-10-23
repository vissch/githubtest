// Phase: fire pass (owner, 2026-10-23: "the ground the flamethrower sets alight does not burn on screen") — part of
// CombatFx (see CombatFx.cs for the event dispatch and the shared pools). The sim burns NAV CELLS and hurts the men
// standing on them (BurningSystem); until now nothing drew them, so a flamethrower's swept ground was clean mud that
// killed people. A burning cell is drawn for the sim's own lifetime of it: a low wide fire card, an occasional tongue
// standing out of it, dark smoke rising (High and Epic), the heat-glow through SceneHooks.FireLight (and its pool on
// the mud at night), and a scorch mark where it goes out.
//
// The cell list is read out of the sim every frame and Sync'd (BurningGround): the event's own seconds are only the
// FIRST ignition's, so a swept cell burns far longer in the sim than its CellBurning said. Nothing is random here —
// the variety is FxQuality.Hash01 of the cell — so an A/B of two quality tiers moves nothing but the budget.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Terrain;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>Burning cells drawn at once at HIGH; the tier's Cap scales it.</summary>
        public const int MaxBurnCells = 48;
        /// <summary>A cell's fire renews its cards this often (seconds), and its light this often.</summary>
        public const float BurnRelight = 0.42f, BurnLightEvery = 0.16f;
        /// <summary>Scorches a burst may lay in BurnScorchWindow seconds: the gag-mark pool is shared with the deaths' blood.</summary>
        public const int BurnScorchBurst = 12;
        public const float BurnScorchWindow = 6f, BurnScorchLife = 70f;
        static readonly Color BurnLight = new Color(1f, 0.46f, 0.03f);

        readonly BurningGround burning = new BurningGround();
        readonly List<BurningGround.SimCell> burnSim = new List<BurningGround.SimCell>(64);
        BurningSystem burnSystem;
        bool burnSystemAsked;
        int burnDrawnLast, burnScorched, burnScorchRun;
        float burnScorchSince;
        int burnFramesBurning, burnFramesCovered, burnUncovered, burnLate;

        /// <summary>The holder, for the EditMode test and the Play probe.</summary>
        public BurningGround Burning => burning;

        /// <summary>The sim lit a nav cell (e.A the cell, e.Pos its centre, e.Scalar the seconds of THIS ignition).</summary>
        void OnCellBurning(in SimEvent e)
        {
            if (Host == null || Host.Local == null) return;
            Vector3 at = (Vector3)e.Pos;
            at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z);
            burning.Distance = BurnDistance;
            burning.Ignite(e.A, at, SimNow, e.Scalar, FxQuality.Now.Cap(MaxBurnCells));
        }

        static float BurnDistance(Vector3 at) => CameraShake.DistanceToLook(at);

        /// <summary>Once a frame: raise every held fire against the sim's live cells, draw them, scorch the ones that went out.</summary>
        void TickBurningGround(float now)
        {
            if (Host == null || Host.Local == null) return;
            var w = Host.Local.World;
            if (!burnSystemAsked) { burnSystem = w.GetSystem<BurningSystem>(); burnSystemAsked = true; }   // null in a bare world and in a replay

            List<BurningGround.SimCell> live = null;
            if (burnSystem != null && burnSystem.Cells.IsCreated)
            {
                burnSim.Clear();
                float tickSeconds = Mathf.Max(1e-4f, w.Config.TickSeconds);
                for (int k = 0; k < burnSystem.Cells.Length; k++)
                {
                    var c = burnSystem.Cells[k];
                    // the sim's end is a TICK; what is left of it in sim seconds, laid on the sim clock the drawing uses
                    float left = c.Until > w.Tick ? (c.Until - w.Tick) * tickSeconds : 0f;
                    burnSim.Add(new BurningGround.SimCell { Cell = c.Cell, EndSim = SimNow + left });
                }
                live = burnSim;
            }
            burning.Sync(SimNow, live);

            // what went out: a scorch each, capped so a swept burst cannot flush the deaths' marks
            for (int i = 0; i < burning.WentOut.Count; i++)
            {
                if (now - burnScorchSince > BurnScorchWindow) { burnScorchSince = now; burnScorchRun = 0; }
                if (burnScorchRun >= BurnScorchBurst) break;
                burnScorchRun++; burnScorched++;
                var f = burning.WentOut[i];
                float yaw = FxQuality.Hash01((uint)f.Cell * 2654435761u) * 360f;
                AddGagMark(f.At, yaw, new Vector2(MapData.NavCellSize, MapData.NavCellSize), BurnScorchLife, 4, 0f);
            }

            burnDrawnLast = 0;
            if (burning.Count == 0) return;

            // the probe's accounting: every cell the sim says burns must have a flame inside half a metre of its centre
            if (live != null && live.Count > 0)
            {
                burnFramesBurning++;
                bool all = true;
                for (int k = 0; k < live.Count; k++)
                {
                    Vector3 c = BurnCellCentre(live[k].Cell);
                    if (!burning.Covers(c, 0.5f)) { all = false; burnUncovered++; }
                }
                if (all) burnFramesCovered++;
            }

            if (books == null || !books.Ready) return;
            var q = FxQuality.Now;
            bool smoke = q.Tier >= FxTier.High;
            float glow = SceneMood.Night ? 1.5f : 1f;
            var fires = burning.Live;
            for (int i = 0; i < fires.Count; i++)
            {
                var f = fires[i];
                if (!f.Stood) { f.At.y = RenderGround.Sample(Host.Local.Map, f.At.x, f.At.z); f.Stood = true; }
                float span = Mathf.Max(0.1f, f.EndSim - f.BornSim);
                float k = Mathf.Clamp01((SimNow - f.BornSim) / span);
                float ebb = 1f - Mathf.SmoothStep(0f, 1f, Mathf.Clamp01((k - 0.55f) / 0.45f));   // it burns down, it does not blink out
                if (SimNow > f.EndSim) burnLate++;   // should never happen: Sync retires a cell the sim has dropped

                if (now >= f.NextLight)
                {
                    f.NextLight = now + BurnLightEvery;
                    float flick = 0.7f + Mathf.PerlinNoise(f.Seed, now * 6.5f) * 0.6f;
                    SceneHooks.FireLight?.Invoke(f.At + Vector3.up * 0.5f, BurnLight, 2.1f * ebb * flick, 5f, BurnLightEvery * 1.15f, 0f);
                    if (SceneMood.Night) SceneHooks.FirePool?.Invoke(f.At, BurnLight, 0.8f * ebb, 3.4f, BurnLightEvery * 1.15f);
                }
                if (now >= f.Next)
                {
                    float vary = FxQuality.Hash01((uint)(f.Cell * 7919 + Mathf.RoundToInt(now * 4f)));
                    f.Next = (f.Next <= 0f ? now : f.Next) + BurnRelight * (0.75f + vary * 0.5f);
                    // LOW and WIDE. A nav cell is 2 m across, and fire lying in the grass is not the column a pool of
                    // fuel makes: the body of it is a flat card not much taller than a man's boot.
                    float wide = 1.6f * (0.85f + 0.25f * ebb);
                    var kind = FlipbookFx.Kind.Anchored | FlipbookFx.Kind.Upright | (vary < 0.5f ? FlipbookFx.Kind.Mirror : 0);
                    books.Add(FlipbookFx.Book.Fire, f.At + Vector3.up * 0.18f, wide, BurnRelight * 1.6f, kind,
                              grow: 0.2f, alpha: 0.72f * ebb, glow: glow, height: wide * 0.42f, pop: 0.4f, startFrame: 4f + vary * 9f);
                    // a tongue licking up out of it, set off centre so the cell is never symmetrical
                    if (vary < 0.5f)
                        books.Add(FlipbookFx.Book.Pyre, f.At + new Vector3((vary - 0.25f) * 1.4f, 0.05f, (0.5f - vary) * 1.4f),
                                  0.9f * (0.8f + 0.4f * ebb), BurnRelight * 1.6f, kind,
                                  velocity: Vector3.up * 0.3f, alpha: 0.8f * ebb, glow: glow, startFrame: 4f + vary * 9f);
                    if (smoke)
                        books.Add(FlipbookFx.Book.Smoke, f.At + Vector3.up * (0.8f + vary * 0.4f), 2.6f, 2.0f + vary,
                                  velocity: Vector3.up * (1.1f + vary), grow: 1.0f, alpha: 0.14f, pop: 0.5f, startFrame: 3.2f + vary * 1.2f);
                    burnDrawnLast++;
                }
                fires[i] = f;
            }
        }

        Vector3 BurnCellCentre(int cell)
        {
            var map = Host.Local.Map;
            float x = (cell % map.NavWidth + 0.5f) * MapData.NavCellSize;
            float z = (cell / map.NavWidth + 0.5f) * MapData.NavCellSize;
            return new Vector3(x, RenderGround.Sample(map, x, z), z);
        }

        /// <summary>One line on the burning ground, for `tw eval` in Play: what is held, what the sim says burns, what the
        /// last frame drew, and whether every burning cell had a flame on it. Nothing in the game calls it; it is what
        /// tells a laying fault from a draw fault, and it is what proves the 80 % coverage the owner asked for.</summary>
        public string BurnReport()
        {
            int pct = burnFramesBurning > 0 ? Mathf.RoundToInt(100f * burnFramesCovered / burnFramesBurning) : 0;
            return $"cells={burning.Count} sim={(burnSystem != null && burnSystem.Cells.IsCreated ? burnSystem.Cells.Length : -1)} "
                 + $"drawn={burnDrawnLast} covered={burnFramesCovered}/{burnFramesBurning} ({pct}%) uncovered={burnUncovered} "
                 + $"late={burnLate} scorch={burnScorched} retired={burning.Retired} tier={FxQuality.Names[(int)FxQuality.Tier]} "
                 + $"cap={FxQuality.Now.Cap(MaxBurnCells)}";
        }
    }
}
