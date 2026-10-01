// Phase: B7 (2026-10-01, first pass) — the battle's sound effects, played from the sim's events (the owner,
// 2026-10-01: "go through all the sfx"; the game had none but Storm's thunder). The sounds are made at load (SfxSynth);
// what is heard, how loud, from which side and whether near or far is SfxMix's. A pool of Voices 2D AudioSources: the
// camera looks down from high, so a sound's level comes from its distance to where the view looks, its pan from where it
// is across the screen, and a far blast arrives late (sound at 343 m/s), as Storm's thunder does.
// Installs itself on the SimHost of any scene that has one (no scene edit). Its level is AudioLevels.Sfx times the
// listener's (Master, which the settings default to 0: the game is muted until the player turns it up).
// Not here yet (the B7 plan): occlusion by the heightfield, engine loops, barrage rumble bed, music stingers.
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.SceneManagement;
using TW.Sim;

namespace TW.Presentation.Audio
{
    public sealed class EventAudioRouter : MonoBehaviour
    {
        SimHost host;
        bool subscribed;
        AudioClip[,] near, far;                       // [sound, variant]
        AudioSource[] voices;
        readonly SfxGroup[] voiceGroup = new SfxGroup[SfxMix.Voices];
        readonly float[] voiceGain = new float[SfxMix.Voices];
        readonly float[] voiceEnd = new float[SfxMix.Voices];
        readonly Dictionary<int, float> lastShot = new Dictionary<int, float>(256);
        struct Pending { public Sfx Sound; public Vector3 At; public float Due, Loud; }
        readonly List<Pending> pending = new List<Pending>(64);
        int startsThisFrame, startsFrame = -1;
        uint dice = 0x2545F491u;
        float lastSnap, lastCrackle;

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.AfterSceneLoad)]
        static void Install()
        {
            SceneManager.sceneLoaded -= OnLoaded; SceneManager.sceneLoaded += OnLoaded;
            Attach();
        }

        static void OnLoaded(Scene s, LoadSceneMode m) => Attach();

        static void Attach()
        {
            var h = Object.FindFirstObjectByType<SimHost>();
            if (h != null && h.GetComponent<EventAudioRouter>() == null) h.gameObject.AddComponent<EventAudioRouter>();
        }

        void Start()
        {
            host = GetComponent<SimHost>();
            int kinds = (int)Sfx.Count;
            near = new AudioClip[kinds, SfxSynth.Variants]; far = new AudioClip[kinds, SfxSynth.Variants];
            for (int s = 0; s < kinds; s++)
                for (int v = 0; v < SfxSynth.Variants; v++)
                {
                    near[s, v] = Clip((Sfx)s, v, false);
                    far[s, v] = Clip((Sfx)s, v, true);
                }
            var holder = new GameObject("Sfx voices") { hideFlags = HideFlags.DontSave };
            holder.transform.SetParent(transform, false);
            voices = new AudioSource[SfxMix.Voices];
            for (int i = 0; i < voices.Length; i++)
            {
                var a = holder.AddComponent<AudioSource>();
                a.playOnAwake = false; a.spatialBlend = 0f; a.loop = false; a.dopplerLevel = 0f;
                voices[i] = a;
            }
        }

        static AudioClip Clip(Sfx s, int v, bool isFar)
        {
            var data = SfxSynth.Make(s, v, isFar);
            var clip = AudioClip.Create($"Sfx {s} {(isFar ? "far" : "near")} {v}", data.Length, 1, SfxSynth.Rate, false);
            clip.SetData(data, 0);
            return clip;
        }

        void OnDestroy()
        {
            if (subscribed && host != null) host.Events.OnEvent -= OnSimEvent;
            if (near != null) foreach (var c in near) if (c != null) Destroy(c);
            if (far != null) foreach (var c in far) if (c != null) Destroy(c);
        }

        void Update()
        {
            if (host == null || voices == null) return;
            if (!subscribed) { host.Events.OnEvent += OnSimEvent; subscribed = true; }
            float now = Time.unscaledTime;
            for (int k = pending.Count - 1; k >= 0; k--)
                if (now >= pending[k].Due) { var p = pending[k]; pending.RemoveAt(k); Begin(p.Sound, p.At, p.Loud, now); }
        }

        // ------------------------------------------------------------------ events to sounds
        void OnSimEvent(SimEvent e)
        {
            if (host == null || host.Local == null) return;
            var w = host.Local.World;
            Vector3 at = e.Pos;
            switch (e.Type)
            {
                case SimEventType.Shot:
                {
                    if (e.A < 0 || e.A >= w.HighWater || e.Scalar > 0.5f) return;   // an arcing round is heard where it lands
                    byte a = w.Archetype[e.A];
                    Vector3 from = (Vector3)(float3)w.Position[e.A];
                    Sfx s = a == InfantryArchetype.Flamethrower ? Sfx.Crackle
                          : a == InfantryArchetype.Machinegunner || ChassisKind.IsArmoured(w.ChassisOf(a)) ? Sfx.Mg : Sfx.Rifle;
                    float now = Time.unscaledTime;
                    if (s != Sfx.Crackle && lastShot.TryGetValue(e.A, out float last) && now - last < SfxMix.Spacing(s)) return;
                    lastShot[e.A] = now;
                    Play(s, from, s == Sfx.Mg ? 0.75f : 0.85f);
                    return;
                }
                case SimEventType.NearMiss:
                    if (e.A < 0 || e.A >= w.HighWater || Time.unscaledTime - lastSnap < 0.08f) return;
                    lastSnap = Time.unscaledTime;
                    Play(Sfx.Snap, (Vector3)(float3)w.Position[e.A], 0.45f);
                    return;
                case SimEventType.Hit:
                    if (e.Scalar < 0f) Play(Sfx.Ricochet, at, 0.5f);
                    return;
                case SimEventType.Explosion:
                    Play(e.Scalar >= 5f ? Sfx.BoomBig : Sfx.BoomSmall, at, math.saturate(0.6f + e.Scalar * 0.06f));
                    return;
                case SimEventType.VehicleFired:
                    if (e.A >= 0 && e.A < w.HighWater) Play(Sfx.TankGun, (Vector3)(float3)w.Position[e.A], 0.95f);
                    return;
                case SimEventType.VehicleArmourHit:
                    Play(Sfx.Clang, at, e.Scalar > 0f ? 0.95f : 0.6f);
                    if (e.Scalar < 0f) Play(Sfx.Ricochet, at, 0.45f);
                    return;
                case SimEventType.VehicleCookOff:
                    Play(Sfx.CookOff, at, 1f);
                    return;
                case SimEventType.AbilityFired:
                {
                    var id = (TW.Sim.Match.OffMapAbilityId)e.A;
                    if (id == TW.Sim.Match.OffMapAbilityId.HeBarrage || id == TW.Sim.Match.OffMapAbilityId.CreepingBarrage
                        || id == TW.Sim.Match.OffMapAbilityId.MortarSalvo || id == TW.Sim.Match.OffMapAbilityId.BomberRun) Play(Sfx.Whistle, at, 0.7f);
                    else if (id == TW.Sim.Match.OffMapAbilityId.ChlorineGas || id == TW.Sim.Match.OffMapAbilityId.MustardGas) Play(Sfx.Hiss, at, 0.8f);
                    else if (id == TW.Sim.Match.OffMapAbilityId.SmokeScreen) Play(Sfx.Hiss, at, 0.55f);
                    return;
                }
                case SimEventType.UnitAlight:
                case SimEventType.VehicleOnFire:
                    if (e.B == 1 && Time.unscaledTime - lastCrackle > 0.15f) { lastCrackle = Time.unscaledTime; Play(Sfx.Crackle, at, e.Type == SimEventType.VehicleOnFire ? 0.9f : 0.6f); }
                    return;
            }
        }

        // ------------------------------------------------------------------ playing
        /// <summary>Queues a sound at a place: at once when it is near, after its travel time when it is far.</summary>
        void Play(Sfx s, Vector3 at, float loud)
        {
            if (!Listen(out var focus, out float height, out _)) return;
            float d = Vector2.Distance(new Vector2(at.x, at.z), new Vector2(focus.x, focus.z));
            if (SfxMix.Gain(s, d, height) <= 0f) return;
            float delay = SfxMix.Delay(s, d);
            float now = Time.unscaledTime;
            if (delay > 0.03f) { if (pending.Count < 64) pending.Add(new Pending { Sound = s, At = at, Due = now + delay, Loud = loud }); }
            else Begin(s, at, loud, now);
        }

        void Begin(Sfx s, Vector3 at, float loud, float now)
        {
            if (Time.frameCount != startsFrame) { startsFrame = Time.frameCount; startsThisFrame = 0; }
            if (startsThisFrame >= SfxMix.StartsPerFrame) return;
            if (!Listen(out var focus, out float height, out var cam)) return;
            float d = Vector2.Distance(new Vector2(at.x, at.z), new Vector2(focus.x, focus.z));
            float gain = SfxMix.Gain(s, d, height) * loud * AudioLevels.Sfx;
            if (gain <= 0.001f) return;
            var group = SfxMix.GroupOf(s);
            int voice = PickVoice(group, gain, now);
            if (voice < 0) return;
            bool isFar = SfxMix.Far(s, d, height);
            int variant = (int)(Roll() * SfxSynth.Variants) % SfxSynth.Variants;
            var clip = isFar ? far[(int)s, variant] : near[(int)s, variant];
            var a = voices[voice];
            a.Stop();
            a.clip = clip;
            a.volume = gain;
            a.pitch = 0.94f + 0.12f * Roll();
            a.panStereo = SfxMix.Pan(cam.WorldToViewportPoint(at).x);
            a.Play();
            voiceGroup[voice] = group; voiceGain[voice] = gain; voiceEnd[voice] = now + clip.length / Mathf.Max(0.5f, a.pitch);
            startsThisFrame++;
        }

        /// <summary>A free voice, or the quietest one of the same group when its budget is spent (only if the new sound is
        /// louder); -1 when neither.</summary>
        int PickVoice(SfxGroup group, float gain, float now)
        {
            int inGroup = 0, quietest = -1, free = -1;
            for (int i = 0; i < voices.Length; i++)
            {
                bool busy = voices[i].isPlaying && now < voiceEnd[i];
                if (!busy) { if (free < 0) free = i; continue; }
                if (voiceGroup[i] != group) continue;
                inGroup++;
                if (quietest < 0 || voiceGain[i] < voiceGain[quietest]) quietest = i;
            }
            if (inGroup < SfxMix.Budget(group) && free >= 0) return free;
            return quietest >= 0 && voiceGain[quietest] < gain ? quietest : -1;
        }

        /// <summary>Where the view looks (its ray met with the ground's level), how high the camera is above it, and the camera.</summary>
        bool Listen(out Vector3 focus, out float height, out Camera cam)
        {
            cam = Camera.main; focus = default; height = 0f;
            if (cam == null) return false;
            var t = cam.transform;
            float groundY = host != null && host.Local != null ? RenderGround.Sample(host.Local.Map, t.position.x, t.position.z) : 0f;
            var plane = new Plane(Vector3.up, new Vector3(0f, groundY, 0f));
            var ray = new Ray(t.position, t.forward);
            focus = plane.Raycast(ray, out float hit) && hit > 0f ? ray.GetPoint(hit) : new Vector3(t.position.x, groundY, t.position.z);
            height = Mathf.Max(5f, t.position.y - focus.y);
            return true;
        }

        float Roll() { dice ^= dice << 13; dice ^= dice >> 17; dice ^= dice << 5; return (dice & 0xFFFFFF) / 16777216f; }
    }
}
