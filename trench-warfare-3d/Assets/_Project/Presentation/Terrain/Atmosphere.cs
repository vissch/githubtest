// Phase: B2 (implemented) — the overcast day of the visual target: grey-blue haze that swallows the far ground and
// the horizon, a sky of the same colour, a soft warm key light with pale shadows. The haze starts behind what the
// camera is looking at and keeps its depth as the view zooms, so the overview is not a wall of fog.
// Ground mist: a cold pale layer that lies in the hollows and the far trenches (TW/Toon and the soldier shader read
// _TWMist / _TWMistColor); it starts beyond the focus so the men you are commanding stay clear.
// Fog bank: the ground nobody fights over, beyond the battlefield's edges, stands in a wall of fog that closes the
// view on every side (TWAtmosphere.hlsl: a function of world position, so it adds no geometry and no overdraw).
// Mood: Look picks the whole set. Night (owner's target, 2026-09-21) is a cold blue moon low behind the field, so every
// wet plane facing the camera glints; near-black blue shade; dark blue haze, mist and fog bank; soaked, shining mud;
// a grade that keeps the blues cold and lets the warm lights (NightLights) bloom. OvercastDay is the earlier look.
// Grade: one global volume. Shadows and midtones lean to sepia, highlights to cold slate, a touch less saturation,
// a light vignette. It is a single LUT pass, so it costs the same on the minimum GPU whatever it does.
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

namespace TW.Presentation.Terrain
{
    public sealed class Atmosphere : MonoBehaviour
    {
        public enum Mood { OvercastDay, Night }
        [Tooltip("Legacy. Field chooses the battlefield now; this is kept so old scenes deserialize and is overwritten in Start.")]
        public Mood Look = Mood.Night;
        public static Mood Current { get; private set; }

        [Tooltip("Which battlefield this is (docs/18). NightMud reproduces the old night set exactly.")]
        public Biome Field = Biome.NightMud;
        /// <summary>What this battlefield is made of. Read by the terrain, the weather and the effects. Never null after Start.</summary>
        public static BiomeProfile Profile { get; private set; } = BiomeProfile.NightMud();
        [Header("Mood")]
        public Color ShadeTint = Color.white;
        public Color SkyMirror = new Color(0.60f, 0.61f, 0.60f);
        [Range(0f, 1f)] public float Wetness = 0f, WetGlint = 0f;
        [Range(0f, 1f)] public float Rain = 0f;
        [Tooltip("How much the rain swells and slackens: 0 steady, 1 from almost nothing to a downpour.")]
        [Range(0f, 1f)] public float Squalls = 0.85f;
        /// <summary>The rain right now, 0..1, and the wind it falls in (m/s, world XZ). Rain.cs and the shaders follow these.</summary>
        public static float RainNow { get; private set; }
        public static Vector2 WindNow { get; private set; }
        /// <summary>
        /// Tooling only (CaptureRig.Hold): the point on the weather cycle to hold the sky at. Below zero the weather
        /// runs from the game clock as it always has. Two captures taken minutes or days apart otherwise fall in
        /// different squalls and different gusts, so every before-and-after this project has ever judged by eye was
        /// confounded by the rain. Pinned, a still of the same field is the same weather forever.
        /// </summary>
        public static float PinnedClock = -1f;
        [Tooltip("Let the storm's lightning (Storm.cs) flash the whole field (night only).")]
        public bool Lightning = true;
        [Tooltip("In the super zoom the far field goes softly out of focus and the vignette closes a little. Costs nothing at any other zoom; off = never.")]
        public bool CloseLens = true;
        Color shadeNow; float flashNow;
        /// <summary>Set by Storm each frame: how bright the strike is right now, 0..1, and the way its light travels.</summary>
        public static float StormFlash;
        public static Vector3 StormLightFrom = Vector3.down;
        Light key;
        public Vector3 KeyEuler = new Vector3(52f, 35f, 0f);   // cross-light reveals rounded bags and timber depth from the standard view
        public Color Ambient = new Color(0.52f, 0.52f, 0.56f);
        bool filmic;
        float exposure = 0.32f, contrast = 12f, saturation = -18f, vignetteAmount = 0.24f, bloom = 0f;
        Vector4 gradeShadows = new Vector4(1.03f, 0.99f, 0.95f, 0f), gradeMids = new Vector4(1.03f, 1.0f, 0.95f, 0f), gradeHighs = new Vector4(0.95f, 0.99f, 1.05f, 0f);
        public Color Haze = new Color(0.60f, 0.61f, 0.60f);
        public Color Key = new Color(1.0f, 0.95f, 0.86f);
        public float KeyIntensity = 1.0f;
        [Range(0f, 1f)] public float ShadowStrength = 0.40f;
        [Tooltip("Haze begins this far behind the point the camera looks at, as a fraction of the camera's distance to it.")]
        public float StartFactor = 0.85f;
        public float Depth = 300f;
        [Header("Ground mist")]
        public Color Mist = new Color(0.74f, 0.76f, 0.75f);
        [Range(0f, 1f)] public float MistDensity = 0.55f;
        [Tooltip("Metres above the water table where the mist ends, and how deep it takes to reach full density.")]
        public float MistTop = 2.3f, MistDepth = 2.2f;
        [Header("Fog bank round the battlefield")]
        public Color Bank = new Color(0.66f, 0.67f, 0.66f);
        [Range(0f, 1f)] public float BankDensity = 0.97f;
        [Tooltip("Metres outside the map edge where the bank begins (negative = inside), and how far it takes to close.")]
        public float BankStart = -5f, BankRange = 28f;
        [Tooltip("Height of the bank at the edge, and how much taller it stands for each metre further out.")]
        public float BankTop = 7f, BankRise = 0.45f;
        [Header("Grade")]
        public bool Grade = true;
        Camera cam;
        VolumeProfile profile;
        static readonly int MistId = Shader.PropertyToID("_TWMist"), MistColorId = Shader.PropertyToID("_TWMistColor");
        static readonly int ShadeTintId = Shader.PropertyToID("_TWShadeTint"), SkyId = Shader.PropertyToID("_TWSky"), WetId = Shader.PropertyToID("_TWWet");
        static readonly int WindId = Shader.PropertyToID("_TWWind");
        static readonly int FieldId = Shader.PropertyToID("_TWField"), FieldFogId = Shader.PropertyToID("_TWFieldFog"), FieldFogColorId = Shader.PropertyToID("_TWFieldFogColor");
        static readonly int SnowId = Shader.PropertyToID("_TWSnow"), SnowColorId = Shader.PropertyToID("_TWSnowColor");
        static readonly int HeatId = Shader.PropertyToID("_TWHeat"), HeatColorId = Shader.PropertyToID("_TWHeatColor");
        static readonly int GroundLightId = Shader.PropertyToID("_TWGroundLight");
        static readonly int WorldTintId = Shader.PropertyToID("_TWWorldTint");
        static readonly int LampScaleId = Shader.PropertyToID("_TWLampScale");

        /// <summary>
        /// Copy a battlefield's profile into the fields this component drives. This replaces ApplyNight, which
        /// assigned the same nineteen values by literal; the values for NightMud are byte-for-byte the ones it used,
        /// so the night field is unchanged by the profile existing. Private because several of the grade fields are.
        /// </summary>
        void Apply(BiomeProfile p)
        {
            Haze = p.Haze;
            Key = p.Key; KeyIntensity = p.KeyIntensity; ShadowStrength = p.ShadowStrength;
            KeyEuler = p.KeyEuler;
            Ambient = p.Ambient;
            ShadeTint = p.ShadeTint;
            SkyMirror = p.SkyMirror;
            Wetness = p.Wetness; WetGlint = p.WetGlint; Rain = p.Rain;
            Depth = p.Depth;
            Mist = p.Mist; MistDensity = p.MistDensity;
            Bank = p.Bank;
            exposure = p.Exposure; filmic = p.Filmic; contrast = p.Contrast; saturation = p.Saturation;
            vignetteAmount = p.Vignette; bloom = p.Bloom;
            gradeShadows = p.GradeShadows; gradeMids = p.GradeMids; gradeHighs = p.GradeHighs;
        }

        /// <summary>The biome terms every world shader reads. Set once: none of them changes during a match.</summary>
        void PushBiome(BiomeProfile p)
        {
            Shader.SetGlobalVector(SnowId, new Vector4(p.SnowCoverage, p.SnowShedBelow, p.SnowSparkle, p.SnowBreakup));
            Shader.SetGlobalVector(SnowColorId, new Vector4(p.SnowColor.r, p.SnowColor.g, p.SnowColor.b, p.SnowColor.a));
            Shader.SetGlobalVector(HeatId, new Vector4(p.HeatStrength, p.HeatPlates, p.HeatCrackWidth, p.MoltenLevel));
            Shader.SetGlobalVector(HeatColorId, new Vector4(p.HeatColor.r, p.HeatColor.g, p.HeatColor.b, 1f));
            Shader.SetGlobalVector(GroundLightId, new Vector4(p.GroundLight.r, p.GroundLight.g, p.GroundLight.b, p.GroundLight.a));
            Shader.SetGlobalVector(WorldTintId, new Vector4(p.WorldTint.r, p.WorldTint.g, p.WorldTint.b, p.WorldTint.a));
            Shader.SetGlobalFloat(LampScaleId, p.LampScale);
            TW.Presentation.Tactical.DebrisRenderer.Biome = p.DebrisTint;
            SceneTints.Push(new SceneTints.Set
            {
                Splash = p.SplashTint, Column = p.ColumnTint, Dust = p.DustTint, Smoke = p.SmokeTint,
                Glow = p.GlowScale, MoltenLiquid = p.MoltenLiquid,
            });
            // Lava eats what falls into it. Guarded on HeatStrength rather than on MoltenLevel, because MoltenLevel
            // is 0 on every other field and pushing a level of 0 would swallow every piece that came to rest on flat
            // ground. One authority: this is the same number the shader reads as _TWHeat.w.
            TW.Presentation.Tactical.DebrisRenderer.LavaLevel = p.HeatStrength > 0f ? p.MoltenLevel : -10000f;
        }

        /// <summary>Put every biome term back to nothing, so a scene without an Atmosphere draws the plain look.</summary>
        static void ClearBiome()
        {
            // Every global PushBiome sets, and not the five of seven this used to clear. _TWSnowColor and
            // _TWHeatColor survived a biome change, and that was safe only by accident: their guards happen to
            // key off _TWSnow.x and _TWHeat.x, which ARE cleared. Leaving a colour behind and trusting a
            // neighbouring field to mask it is exactly the shape of the lamp bug this cycle fixed.
            Shader.SetGlobalVector(SnowId, Vector4.zero);
            Shader.SetGlobalVector(SnowColorId, Vector4.zero);
            Shader.SetGlobalVector(HeatId, Vector4.zero);
            Shader.SetGlobalVector(HeatColorId, Vector4.zero);
            Shader.SetGlobalVector(GroundLightId, Vector4.zero);
            Shader.SetGlobalVector(WorldTintId, Vector4.zero);
            Shader.SetGlobalFloat(LampScaleId, 1f);
            TW.Presentation.Tactical.DebrisRenderer.Biome = new Color(1f, 1f, 1f, 0f);
            SceneTints.Reset();
            TW.Presentation.Tactical.DebrisRenderer.LavaLevel = -10000f;
        }

        void Start()
        {
            var biome = BiomeProfile.For(Field);
            Apply(biome);
            Profile = biome;
            PushBiome(biome);
            // SceneMood.Night keeps the only meaning it ever had: is it dark. The lava field is dark too — lit from
            // below by its own floor — so the twenty-odd glow and smoke branches that read this bool get the values
            // they were tuned for rather than the daylight ones. Everything else that used to ride on "night" is a
            // named field on the profile.
            Look = biome.Dark ? Mood.Night : Mood.OvercastDay;
            Current = Look; SceneMood.Night = biome.Dark;
            RenderSettings.fog = true;
            RenderSettings.fogMode = FogMode.Linear;
            RenderSettings.fogColor = Haze;
            RenderSettings.ambientMode = AmbientMode.Flat;
            RenderSettings.ambientLight = Ambient;
            foreach (var l in FindObjectsByType<Light>(FindObjectsSortMode.None))
            {
                if (l.type != LightType.Directional) continue;
                l.color = Key; l.intensity = KeyIntensity; l.shadows = LightShadows.Soft; l.shadowStrength = ShadowStrength;
                l.transform.rotation = Quaternion.Euler(KeyEuler);
                key = l;
            }
            if (Grade) BuildGrade();
        }

        Vignette vignette; DepthOfField focus;

        void BuildGrade()
        {
            profile = ScriptableObject.CreateInstance<VolumeProfile>();
            profile.hideFlags = HideFlags.HideAndDontSave;
            var adjust = profile.Add<ColorAdjustments>(true);
            adjust.postExposure.Override(exposure); adjust.contrast.Override(contrast); adjust.saturation.Override(saturation);
            var tones = profile.Add<ShadowsMidtonesHighlights>(true);
            tones.shadows.Override(gradeShadows);      // day: umber in the dark; night: blue
            tones.midtones.Override(gradeMids);        // day: sepia through the middle
            tones.highlights.Override(gradeHighs);     // day: cold slate on the brightest planes; night: warm, for the lamps
            if (filmic)
            {
                // a film's shoulder, so lamps, flames and flashes roll off to white instead of clipping, and a little grain
                // to break up the dark blue gradients. Both live in the final post pass: no extra full-screen work.
                profile.Add<Tonemapping>(true).mode.Override(TonemappingMode.Neutral);
                var grain = profile.Add<FilmGrain>(true);
                grain.type.Override(FilmGrainLookup.Thin1); grain.intensity.Override(0.20f); grain.response.Override(0.8f);
            }
            if (bloom > 0f)
            {
                var glow = profile.Add<Bloom>(true);   // muzzle flashes, lamps, tracers, the flare
                glow.threshold.Override(0.85f); glow.intensity.Override(bloom); glow.scatter.Override(0.62f); glow.tint.Override(new Color(1f, 0.92f, 0.82f));
            }
            vignette = profile.Add<Vignette>(true);
            vignette.intensity.Override(vignetteAmount); vignette.smoothness.Override(0.5f); vignette.color.Override(new Color(0.10f, 0.08f, 0.06f));
            if (CloseLens)
            {
                // among the men the far field falls softly out of focus. Off (not merely weak) everywhere else: the base look
                // does without depth of field for its cost on the minimum GPU.
                focus = profile.Add<DepthOfField>(true);
                focus.mode.Override(DepthOfFieldMode.Gaussian); focus.gaussianMaxRadius.Override(0.9f); focus.highQualitySampling.Override(false);
                focus.gaussianStart.Override(40f); focus.gaussianEnd.Override(120f);
                focus.active = false;
            }
            var go = new GameObject("Grade") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            var volume = go.AddComponent<Volume>();
            volume.isGlobal = true; volume.priority = 10f; volume.sharedProfile = profile;
        }

        void OnDestroy()
        {
            Shader.SetGlobalVector(MistColorId, Vector4.zero);
            Shader.SetGlobalVector(ShadeTintId, Vector4.zero); Shader.SetGlobalVector(SkyId, Vector4.zero); Shader.SetGlobalVector(WetId, Vector4.zero);
            Shader.SetGlobalVector(FieldFogColorId, Vector4.zero);
            ClearBiome();
            if (profile != null) Destroy(profile);
        }

        void LateUpdate()
        {
            if (cam == null)
            {
                cam = Camera.main;
                if (cam != null && Grade) cam.GetUniversalAdditionalCameraData().renderPostProcessing = true;
            }
            if (cam == null) return;
            if (focus != null)
            {
                float lens = Mathf.InverseLerp(0.80f, 1f, SceneHooks.CloseUp);   // the super zoom only
                bool on = lens > 0.001f;
                if (focus.active != on) focus.active = on;
                if (on)
                {
                    float reach = cam.transform.position.y / Mathf.Max(0.12f, -cam.transform.forward.y);   // to the ground the view looks at
                    focus.gaussianStart.Override(reach * 2.2f + 8f); focus.gaussianEnd.Override(reach * 6f + 40f);
                    focus.gaussianMaxRadius.Override(0.9f * lens);
                }
                vignette.intensity.Override(vignetteAmount + 0.10f * lens);
            }
            // lightning: while a bolt burns (Storm.cs) the whole field jumps out of the dark. The moon's light becomes the
            // bolt's: far brighter, white, and coming from where it struck, so every shadow swings round for an instant.
            float flash = 0f;
            if (Lightning && Look == Mood.Night && key != null)
            {
                flash = Mathf.Clamp01(StormFlash);
                key.intensity = KeyIntensity * (1f + 1.9f * flash);
                key.color = Color.Lerp(Key, new Color(.90f, .94f, 1f), flash);
                Vector3 from = StormLightFrom; from.y = Mathf.Min(from.y, -.35f);   // never so low that the shadows run to the horizon
                key.transform.rotation = flash > .02f ? Quaternion.LookRotation(from.normalized) : Quaternion.Euler(KeyEuler);
            }
            Color sky = Color.Lerp(Haze, Profile.FlashSky, flash * .40f);
            RenderSettings.fogColor = sky;
            cam.clearFlags = CameraClearFlags.SolidColor;
            cam.backgroundColor = sky;
            shadeNow = Color.Lerp(ShadeTint, Profile.FlashShade, flash * .4f);
            flashNow = flash;
            float height = Mathf.Max(1f, cam.transform.position.y);
            float pitch = Mathf.Max(0.12f, -cam.transform.forward.y);
            float toFocus = height / pitch;   // distance to the ground along the view
            RenderSettings.fogStartDistance = toFocus * StartFactor;
            // a squall closes the distance in: the far ground sinks into the rain
            float squall = Rain > 0f ? Mathf.Clamp01(RainNow / (Rain * 1.25f)) : 0f;
            RenderSettings.fogEndDistance = toFocus * StartFactor + Depth * (1f - .38f * squall) + toFocus;

            float water = RenderGround.Map != null && RenderGround.Map.WaterLevel > TW.Sim.Terrain.MapData.NoWater ? RenderGround.Map.WaterLevel : 0f;
            Shader.SetGlobalVector(MistId, new Vector4(water + MistTop, 1f / Mathf.Max(0.05f, MistDepth), toFocus * 0.8f, 1f / Mathf.Max(10f, toFocus * 0.55f)));
            Shader.SetGlobalVector(MistColorId, new Vector4(Mist.r, Mist.g, Mist.b, MistDensity));

            Shader.SetGlobalVector(ShadeTintId, new Vector4(shadeNow.r, shadeNow.g, shadeNow.b, 1f));
            Color mirror = Color.Lerp(SkyMirror, Profile.FlashMirror, flashNow * .55f);   // lightning shows in every puddle
            Shader.SetGlobalVector(SkyId, new Vector4(mirror.r, mirror.g, mirror.b, 1f));
            // Weather: two slow noises make the rain swell to a downpour and slacken to a drizzle over a minute or so, with
            // shorter gusts on top; the wind swings and freshens with it. Everything that shows rain reads the same number.
            float clock = PinnedClock >= 0f ? PinnedClock : Time.time;
            float swell = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(.28f, .72f, Mathf.PerlinNoise(clock * .021f, 3.7f)));
            float gust = Mathf.PerlinNoise(clock * .13f, 11.3f);
            float level = Mathf.Clamp01(swell * .8f + gust * .35f - .05f);
            RainNow = Rain * Mathf.Lerp(1f, Mathf.Lerp(.12f, 1.25f, level), Squalls);
            float heading = (Mathf.PerlinNoise(clock * .017f, 23.1f) - .5f) * 2.4f + .6f;
            float blow = Mathf.Lerp(1.5f, 9f, Mathf.Clamp01(level * .7f + gust * .5f));
            WindNow = new Vector2(Mathf.Cos(heading), Mathf.Sin(heading)) * blow;
            Shader.SetGlobalVector(WetId, new Vector4(Wetness, WetGlint, Mathf.Clamp01(RainNow), 0f));
            Vector2 breeze = Rain > 0f ? WindNow : new Vector2(1.6f, .7f) * (.7f + .5f * Mathf.PerlinNoise(clock * .11f, 5.5f));   // a dry day still has a breeze
            Shader.SetGlobalVector(WindId, new Vector4(breeze.x * .034f, breeze.y * .034f, 0f, 1f));

            var map = RenderGround.Map;
            if (map != null) Shader.SetGlobalVector(FieldId, new Vector4(0f, 0f, map.SizeMeters.x, map.SizeMeters.y));
            Shader.SetGlobalVector(FieldFogId, new Vector4(BankStart, 1f / Mathf.Max(1f, BankRange), BankTop, BankRise));
            Shader.SetGlobalVector(FieldFogColorId, new Vector4(Bank.r, Bank.g, Bank.b, map != null ? BankDensity : 0f));
        }
    }
}
