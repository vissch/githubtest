// Phase: B1 / C4 (implemented) — part of CombatFx (see CombatFx.cs for the event dispatch and the shared pools): what
// only a close camera sees: things come to rest, marks pressed into the mud (boots, ruts, walker feet), trails,
// breath, exhaust and crater steam. Pools: rests, marks, trails. Wires: SceneHooks.FootFall (set in Start), laid
// through AddMark.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        // ---- what only a close camera sees (SceneHooks.CloseUp): nothing below is made or drawn at the standard view
        struct Rest { public Matrix4x4 At; public float Until; public byte Kind; }      // things come to rest: 0 brass, 1 helmet, 2 clod
        struct Mark { public Matrix4x4 At; public float Born, Life; public byte Kind; }  // pressed into the mud: 0 boot print, 1 track rut, 2 walker's foot
        struct Trail { public Vector3 Last; public bool Left; public float Seen; }
        readonly List<Rest> rests = new List<Rest>(256);
        readonly List<Mark> marks = new List<Mark>(512);
        readonly Dictionary<int, Trail> trails = new Dictionary<int, Trail>(128);
        readonly List<Vector4> hotCraters = new List<Vector4>(16);   // xyz, w = cools at
        readonly List<int> trailSweep = new List<int>(64);
        const int MaxRests = 320, MaxMarks = 900;   // machine marks lie for minutes and are laid out to MachineMarkReach, so the field holds more of them than when only boots printed
        const float CloseReach = 42f;
        /// <summary>How far out a machine's marks are laid and drawn. A boot print is invisible past CloseReach and is not
        /// made past it; a tank's ruts and a walker's footfalls are metres across and belong on the ground at the standard
        /// view, where the game is actually played, so they run to the edge of what the camera holds.</summary>
        const float MachineMarkReach = 130f;
        readonly Mesh[] fallen = new Mesh[8];
        /// <summary>One a shape (boot, rut, walker's foot). The fade used to be three materials a shape, which meant the
        /// mark list was walked nine times a frame and the alpha stepped in three visible jumps; the fade now rides in
        /// each instance's object Y scale (a flat quad has no other use for it), so one sweep fills three buckets and
        /// the mark thins out continuously.</summary>
        readonly Material[] markMats = new Material[3];
        readonly List<Matrix4x4>[] markBatch = { new List<Matrix4x4>(256), new List<Matrix4x4>(256), new List<Matrix4x4>(256) };
        Material fallenMat, brassMat, helmetMat, vapourMat;
        Mesh markQuad;
        float nextPrint, nextBreath, nextExhaust; int breathCursor;
        /// <summary>The ground's tilt at a point, so a print or a body lies on the slope and not in the air above it.</summary>
        Quaternion Lie(float x, float z, float yawDegrees, float span = 0.3f)
        {
            var map = Host.Local.Map;
            float sx = RenderGround.Sample(map, x + span, z) - RenderGround.Sample(map, x - span, z), sz = RenderGround.Sample(map, x, z + span) - RenderGround.Sample(map, x, z - span);
            return Quaternion.FromToRotation(Vector3.up, new Vector3(-sx, 2f * span, -sz).normalized) * Quaternion.Euler(0f, yawDegrees, 0f);
        }

        void AddRest(Matrix4x4 at, float seconds, byte kind)
        {
            if (rests.Count >= MaxRests) rests.RemoveAt(0);
            rests.Add(new Rest { At = at, Until = Time.time + seconds, Kind = kind });
        }

        void AddMark(float x, float z, float yawDegrees, Vector2 size, float life, byte kind)
        {
            Vector3 at = new Vector3(x, RenderGround.Sample(Host.Local.Map, x, z) + 0.025f, z);
            var mark = new Mark { At = Matrix4x4.TRS(at, Lie(x, z, yawDegrees, 0.2f), new Vector3(size.x, 1f, size.y)), Born = Time.time, Life = life, Kind = kind };
            if (marks.Count < MaxMarks) { marks.Add(mark); return; }
            // Full. RemoveAt(0) shifted seventy kilobytes of matrices for every print laid, and on a snowfield - where
            // marks live for minutes and the pool sits at its ceiling - that is the whole time. Overwrite the one
            // nearest gone instead: no shift, and a mark on its way out is a better thing to lose than the oldest,
            // which on this field may be a rut with four minutes left while a boot print beside it has two seconds.
            int worst = 0; float gone = -1f, now = Time.time;
            for (int i = 0; i < marks.Count; i++)
            {
                float k = (now - marks[i].Born) / marks[i].Life;
                if (k > gone) { gone = k; worst = i; }
            }
            marks[worst] = mark;
        }

        /// <summary>What has come to rest (brass, helmets, clods) and what is pressed into the mud (boot prints, track ruts): close camera only.</summary>
        void DrawClose(float now, Bounds bounds)
        {
            Prune(rests, now, static (r, at) => at > r.Until);
            Prune(marks, now, static (m, at) => at - m.Born > m.Life);
            var cam = Camera.main; if (cam == null) return;
            Vector3 eye = cam.transform.position;
            bool close = SceneHooks.CloseUp > 0f;
            for (int kind = 0; close && kind < 3; kind++)
            {
                batch.Clear();
                var rp = new RenderParams(kind == 0 ? brassMat : kind == 1 ? helmetMat : dirtMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off, receiveShadows = true };
                for (int i = 0; i < rests.Count; i++)
                {
                    var r = rests[i]; if (r.Kind != kind) continue;
                    float dx = r.At.m03 - eye.x, dz = r.At.m23 - eye.z; if (dx * dx + dz * dz > CloseReach * CloseReach) continue;
                    batch.Add(r.At);
                    if (batch.Count == 1023) Flush(kind == 1 ? sphere : cube, rp);
                }
                if (batch.Count > 0) Flush(kind == 1 ? sphere : cube, rp);
            }
            if (markMats[0] == null) return;
            // One pass over the marks, sorted into a bucket a shape as it goes, instead of nine passes each throwing
            // away eight marks in nine. MaxMarks is below the 1023 an instanced draw takes, so no bucket can overflow
            // mid-sweep and none of this needs to flush early.
            for (int k = 0; k < markBatch.Length; k++) markBatch[k].Clear();
            float bootReach = CloseReach * CloseReach, machineReach = MachineMarkReach * MachineMarkReach;
            for (int i = 0; i < marks.Count; i++)
            {
                var m = marks[i];
                if (m.Kind == 0 && !close) continue;   // boot prints only once the camera is in among the men
                float dx = m.At.m03 - eye.x, dz = m.At.m23 - eye.z;
                if (dx * dx + dz * dz > (m.Kind == 0 ? bootReach : machineReach)) continue;
                // the mark's age travels to the shader in its object Y scale: the quad is flat, so scaling Y moves no
                // vertex, and the shader reads the Y axis's length back out whatever tilt the ground put on it. It
                // holds most of its strength, then goes - which is what the three stages were approximating.
                // The three stages this replaces held 0.88 for half the life, then 0.58, then 0.26. A square falls off
                // too slowly against that - a mark at 80% of its life comes out half again as strong as it used to be,
                // and a field of hundreds of them never looks like it clears. age^1.5 sits on the old curve, and this
                // polynomial sits on age^1.5 to within a percent without a sqrt, which at a mark a matrix is worth it.
                float age = (now - m.Born) / m.Life;
                float left = 1f - 0.82f * (0.35f * age + 0.65f * age * age);
                var at = m.At; at.m01 *= left; at.m11 *= left; at.m21 *= left;
                markBatch[m.Kind].Add(at);
            }
            for (int k = 0; k < markBatch.Length; k++)
                if (markBatch[k].Count > 0)
                    Flush(markQuad, markBatch[k], new RenderParams(markMats[k]) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });
        }

        /// <summary>Boot prints behind walking men, ruts and flung mud behind tanks, breath in the cold, steam off fresh craters.</summary>
        void CloseLife(float now, Camera cam)
        {
            // A machine's marks are made whatever the zoom: they are the size of the machine, and a field the tanks have
            // crossed should show it at the standard view. Only the small things below wait for the camera to come in.
            bool close = SceneHooks.CloseUp > 0f;
            var w = Host.Local.World; var map = Host.Local.Map;
            Vector3 eye = cam.transform.position;
            float rain = Shader.GetGlobalVector(WetId).z;
            if (now >= nextPrint)
            {
                nextPrint = now + 0.1f;
                for (int i = 0; i < w.HighWater; i++)
                {
                    uint flags = w.Flags[i];
                    if ((flags & (uint)UnitFlags.Alive) == 0) continue;
                    bool tank = (flags & (uint)UnitFlags.Vehicle) != 0;
                    if (!tank && !close) continue;   // boot prints are a close-camera thing; a machine's marks are not
                    var p = w.Position[i]; float dx = p.x - eye.x, dz = p.z - eye.z;
                    float reach = tank ? MachineMarkReach : CloseReach;
                    if (dx * dx + dz * dz > reach * reach) continue;
                    Vector3 here = new Vector3(p.x, 0f, p.z);
                    if (!trails.TryGetValue(i, out var trail)) { trails[i] = new Trail { Last = here, Seen = now }; continue; }
                    trail.Seen = now;
                    var profile = tank ? TW.Sim.Nav.VehicleProfile.ForArchetype(w.Archetype[i]) : default;
                    bool walker = tank && profile.Walker;
                    bool drawnWalker = walker && SceneHooks.TanksDrawn && (SceneHooks.IsTankSlot == null || SceneHooks.IsTankSlot(i));
                    // a walker's stride is its own length, not a tank's shuffle: far fewer marks over the same ground, and
                    // six legs step shorter and more often than four of the same reach (Pincer and Redoubt against Kettle)
                    Vector3 step = here - trail.Last;
                    float far = step.magnitude;
                    float stride = walker ? Mathf.Max(1.5f, profile.HalfLength * (profile.Legs >= 6 ? 0.62f : 0.85f)) : tank ? 0.85f : 0.72f;
                    if (far > 6f) { trail.Last = here; trails[i] = trail; continue; }   // the slot was reused by another man
                    if (far >= stride)
                    {
                        Vector3 dir = step / far, side = new Vector3(dir.z, 0f, -dir.x);
                        float yawDeg = Mathf.Atan2(dir.x, dir.z) * Mathf.Rad2Deg;
                        // Duckboards take no print - but SNOW ON duckboards does, and a winter trench floor is packed
                        // snow and ice rather than dry boards. Measured on the winter line: every man within the
                        // close reach was InTrench, so this one flag was suppressing every track on the field where
                        // tracks matter most. The mark pool, the trail stepping and the draw were all working.
                        bool snowfield = SceneTints.Now.Frozen;
                        bool dryFooting = !tank && !snowfield && (flags & (uint)UnitFlags.InTrench) != 0;
                        for (float d = stride; d <= far && d < stride * 4.5f; d += stride)
                        {
                            Vector3 at = trail.Last + dir * d;
                            if (dryFooting || (SceneHooks.IsWater != null && SceneHooks.IsWater(at.x, at.z))) continue;
                            if (tank && walker && drawnWalker)
                            {
                                // TankRenderer is putting this one's legs on the ground and stamping each real footfall
                                // (SceneHooks.FootFall). Guessing a second set from the body's path would double them.
                            }
                            else if (tank && walker)
                            {
                                // A machine on legs leaves no rut at all: it puts its whole weight through one pad at a
                                // time, so the ground carries a line of deep prints to either side of its path, further
                                // apart and further between than anything else on the field. Alternating left and right is
                                // the gait as seen from above; TankRenderer's WalkerGait knows the true footfalls, but it
                                // is another session's file, so the stride is stepped here instead.
                                float out_ = profile.HalfWidth * 0.78f;   // the feet fall wide, outside the hull
                                float footSide = trail.Left ? -out_ : out_; trail.Left = !trail.Left;
                                // the pad is a fraction of the machine, not a slab the width of it: a Pincer is 9.5 m
                                // across and puts about a metre of foot on the ground
                                var pad = new Vector2(profile.HalfWidth * 0.22f, profile.HalfLength * 0.28f);
                                AddMark(at.x + side.x * footSide, at.z + side.z * footSide, yawDeg + (trail.Left ? 5f : -5f),
                                    pad, snowfield ? 300f : 110f, 2);
                            }
                            else if (tank)
                            {
                                float gauge = SceneHooks.VehicleTracks != null ? SceneHooks.VehicleTracks(i).x : 0.78f;
                                // Mud closes over a rut; snow does not until more snow falls on it.
                                float rutLife = snowfield ? 240f : 70f;
                                AddMark(at.x + side.x * gauge, at.z + side.z * gauge, yawDeg, new Vector2(0.62f, 0.92f), rutLife, 1);
                                AddMark(at.x - side.x * gauge, at.z - side.z * gauge, yawDeg, new Vector2(0.62f, 0.92f), rutLife, 1);
                            }
                            else
                            {
                                float foot = trail.Left ? -0.11f : 0.11f; trail.Left = !trail.Left;
                                AddMark(at.x + side.x * foot, at.z + side.z * foot, yawDeg + (trail.Left ? 7f : -7f), new Vector2(0.15f, 0.34f), snowfield ? 210f : 45f, 0);
                            }
                        }
                        if (tank && chunks.Count < 560)
                        {
                            // the tracks fling what they lift
                            var tracks = SceneHooks.VehicleTracks != null ? SceneHooks.VehicleTracks(i) : new Vector2(0.78f, 1.9f);
                            Vector3 rear = new Vector3(p.x, RenderGround.Sample(map, p.x, p.z) + 0.3f, p.z) - dir * tracks.y;
                            Throw(rear + side * tracks.x, 1, 0, 2.6f, 0.07f); Throw(rear - side * tracks.x, 1, 0, 2.6f, 0.07f);
                        }
                        trail.Last = here;
                    }
                    trails[i] = trail;
                }
                if (trails.Count > 96)
                {
                    trailSweep.Clear();
                    foreach (var kv in trails) if (now - kv.Value.Seen > 1.5f) trailSweep.Add(kv.Key);
                    for (int k = 0; k < trailSweep.Count; k++) trails.Remove(trailSweep[k]);
                }
            }
            // everything past here is a close-camera thing and always was: exhaust, steam off a fresh hole, breath on a
            // cold night. Only the marks above were lifted out of the close band, because a machine's are the size of a
            // machine; the rest must not start costing the standard view anything.
            if (!close) return;
            if (now >= nextExhaust)
            {
                nextExhaust = now + 0.3f;
                for (int i = 0; i < w.HighWater && chunks.Count < 560 && !SceneHooks.TanksDrawn; i++)   // the tanks' own exhaust is TankRenderer's
                {
                    if ((w.Flags[i] & ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) != ((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle)) continue;
                    var p = w.Position[i]; float dx = p.x - eye.x, dz = p.z - eye.z; if (dx * dx + dz * dz > 60f * 60f) continue;
                    float yaw = w.Yaw[i];
                    Vector3 back = new Vector3(-Mathf.Sin(yaw), 0f, -Mathf.Cos(yaw));
                    chunks.Add(new Chunk { Pos = new Vector3(p.x, RenderGround.Sample(map, p.x, p.z) + 1.5f, p.z) + back * 1.7f, Vel = back * 0.8f + Vector3.up * 0.9f, Born = now, Life = UnityEngine.Random.Range(1.6f, 2.4f), Size = 0.2f, Kind = 2 });
                }
                // rain on the hot earth of a fresh hole
                Prune(hotCraters, now, static (h, at) => at > h.w);
                if (rain > 0.05f)
                    for (int k = 0; k < hotCraters.Count && chunks.Count < 560; k++)
                    {
                        Vector3 at = hotCraters[k]; float dx = at.x - eye.x, dz = at.z - eye.z; if (dx * dx + dz * dz > 60f * 60f) continue;
                        float heat = (hotCraters[k].w - now) / 22f;
                        if (UnityEngine.Random.value > heat) continue;
                        Vector2 r = UnityEngine.Random.insideUnitCircle * 1.2f;
                        chunks.Add(new Chunk { Pos = at + new Vector3(r.x, 0.1f, r.y), Vel = new Vector3(0f, 0.7f, 0f), Born = now, Life = UnityEngine.Random.Range(1.8f, 3f), Size = 0.22f, Kind = 7 });
                    }
            }
            if (SceneMood.Night && now >= nextBreath && w.HighWater > 0)
            {
                // a cold night: the men nearest the view breathe out a little cloud, one man at a time
                nextBreath = now + 0.35f;
                for (int n = 0; n < w.HighWater && n < 400; n++)
                {
                    int i = (breathCursor + n) % w.HighWater;
                    uint flags = w.Flags[i];
                    if ((flags & (uint)UnitFlags.Alive) == 0 || (flags & (uint)UnitFlags.Vehicle) != 0) continue;
                    var p = w.Position[i]; float dx = p.x - eye.x, dz = p.z - eye.z; if (dx * dx + dz * dz > 20f * 20f) continue;
                    var stance = (Stance)w.StanceOf[i];
                    float head = stance == Stance.Prone || stance == Stance.Pinned ? 0.35f : stance == Stance.Crouch ? 1.05f : 1.58f;
                    float yaw = w.Yaw[i]; Vector3 ahead = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw));
                    if (chunks.Count < 600)
                        chunks.Add(new Chunk { Pos = new Vector3(p.x, RenderGround.Sample(map, p.x, p.z) + head, p.z) + ahead * 0.16f, Vel = ahead * 0.45f + Vector3.up * 0.1f, Born = now, Life = UnityEngine.Random.Range(0.9f, 1.4f), Size = 0.05f, Kind = 7 });
                    breathCursor = i + 1; break;
                }
            }
        }
    }
}
