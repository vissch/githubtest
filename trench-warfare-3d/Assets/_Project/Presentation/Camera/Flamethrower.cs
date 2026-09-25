// Phase: A5c (show side) - the Flammenwerfer-Trupp's fire. Four things burn here:
//
//   the JET      a stream held on a target for as long as the man has fuel
//   a POOL       burning fuel left on the ground where the stream swept, which outlives the burst
//   a TORCH      a man alight, running (AnimationController's Clip.Burning) with his own fire on him
//   a PYRE       a big thing burning: a wreck, a dugout, a house the fire got into
//
// Each is a DRAWING from the pack rather than a shape built out of a general-purpose one. That sounds obvious and it
// was not how this started: rounds 1 and 2 had every one of them built from FireBall, a ball of flame rolling, because
// FireBall was one of only three sheets that had been converted - the rest of the pack was skipped as the wrong
// colour, when Tools/firebooks.py throws colour away and the shader recolours from value. So the jet was a chain of
// nine round fireballs, the burning man was a ball stood on end, the pool was one lying on its side, and the tank
// going up was three small bursts staggered. The pack had a stream, a standing flame with a skirt, a puddle and a
// soot-ringed fireball in it the whole time. Book.Jet / Stand / Pool / Blast / Fan are those drawings, and the rule
// they came with is the one worth keeping: when a thing does not read, ask first whether it is the wrong DRAWING, and
// only then reach for size and timing.
//
// The jet is still the part worth explaining. A flamethrower is thickened petrol thrown under pressure: it leaves the
// nozzle as a hard rod, tumbles into a widening boil as it slows, and at the end of its reach stops being a jet and
// becomes fire going up. Book.Jet is drawn exactly that way and played like a VALVE rather than looped - f0-11 the
// stream reaching out, f11-18 held at full length while he holds the trigger, f19-28 breaking up when the pressure
// goes - so the frame comes off the burst's own clock. Two copies, the second mirrored about the aim and a few frames
// behind, keep it boiling instead of being one drawing held up to the camera. A card lies in the screen plane, so a
// stream aimed at the eye has no length left to draw along; under about 0.6 of full screen length the old chain of
// round cards, which does not care about angle, fades back in. Where the stream meets ground, Hit() stops it and
// Book.Fan climbs the obstacle.
//
// Nothing here is a GameObject and nothing here allocates per frame: every effect is cards handed to FlipbookFx, which
// packs them into one instanced draw a book. A pool, a torch and a pyre are all "a place that keeps re-lighting": they
// hold a little state and drop a fresh card every Relight seconds, because the books play once and fire does not.
//
// The sim knows none of this yet (Phase A5's BurningSystem is a stub). CombatFx drives it from what the sim DOES say -
// a Shot from a unit whose class is Flamethrower is a burst, its Death is the tank going up - so when the sim grows
// the real cone and the real Burning cells, the show side is already here and only the caller changes.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed class Flamethrower
    {
        /// <summary>
        /// The one CombatFx is running, or null outside a match. Whatever wants to set something on fire asks for it
        /// here rather than being handed a reference: the sim cannot ask for fire yet (Phase A5's BurningSystem is a
        /// stub), so for now the askers are the debug drawer and, later, the burning cells the sim will report.
        /// </summary>
        public static Flamethrower Active;

        // The books are NOT interchangeable, and which one carries a given fire is the first decision, not a detail.
        // Book.Stand is a flame drawn standing with a skirt, Book.Fire is a ball of it rolling, Book.Pool is a puddle
        // with a blob lifting off it: a thing burning on the ground is the first shape and never the second. Book.Pyre
        // (FireColumn) stays what it always was - a burst of smoke with a crescent of flame at its foot, three quarters
        // nothing on its own - so it is only ever an accent laid beside a body card for a different silhouette.
        // No book fills its cell, so a card is drawn bigger than the fire it stands for. How much bigger is per book
        // now (FlipbookFx.Geometry, measured by Tools/firebooks.py); Cell is the fallback for the books drawn before
        // that was measured.
        const float Cell = 1.35f;           // a card is drawn this much bigger than the fire it stands for. Only a
                                            // little: the drawing does not fill its cell, but 1.6 made every pool in a
                                            // cook-off ring overlap its neighbours into one sheet

        /// <summary>How far the stream reaches (m). The sim's cone is 12 m; the fire falls apart a little short of it.</summary>
        public const float Reach = 11f;
        /// <summary>Seconds a burst runs for from one Shot event.</summary>
        public const float BurstSeconds = 0.85f;

        const int MaxJets = 6, MaxPools = 48, MaxTorches = 24, MaxPyres = 16;
        const float RootEvery = 1f / 12f;
        const int JetLinks = 9;              // the fallback chain, for when the stream is aimed too near the eye to draw   // the root is renewed on the book's own clock, not once a frame
        const float PuffEvery = 0.027f;     // a puff this often per jet. Fewer and larger than it wants to be: at
                                            // thirty a second there are two dozen cels over each other and the stream
                                            // is a bright smear, which is the one thing a drawn flame must not be.
        const float SpillEvery = 0.28f;     // the stream lays fuel down this often. It used to be laid on the light's
                                            // clock, eleven times a second, which put four dozen pools under one burst:
                                            // the far end of the jet became one solid sheet and the pool list was full
                                            // of the same fire, so nothing else on the field could catch.
        // How much of full brightness one fire card is worth. Fire is additive and fires overlap, so this is set well
        // under 1: a single card is a translucent tongue, and only where two or three cross does it reach white. It is
        // opacity and not glow because glow is a birth flare that decays to 1 over a card's first sixth (FlipbookFx),
        // which cannot set the level a thing that burns for half a minute sits at.
        const float Coal = 0.92f;   // fire composites rather than sums now, so a card can be nearly opaque - and an
                                    // opaque cel is the only kind with a visible edge
        // A fresh card this often, and each lives about as long, so a fire is very nearly ONE drawing at a time.
        // The books are played at the 12 fps they were drawn at and cut cel to cel (FlipbookFx.Snap/Fps); stacking
        // three of them a third of a second apart put three different cels of the same drawing over each other and
        // undid that at the last step. One cel, held its frame, then the next.
        const float Relight = 0.70f;        // a pool, a torch and a pyre drop a fresh card this often
        const float Held = 1.35f;           // and it lives this many Relights, so one card is out at a time
        const float TorchEvery = 0.20f;     // a man alight is re-lit this often: he runs out from under his own fire otherwise
        const float CatchEvery = 0.12f;     // how often fire looks to see who is standing in it
        const float CatchSeconds = 0.9f;    // how long a big fire takes to take hold, whatever it has left to burn
        const float LightEvery = 0.09f;     // the pooled light is only lent to us; do not ask for one every frame
        // The last argument of FireLight is the LIGHT'S GLOW CARD, and for fire it is now ZERO everywhere.
        // ...and then, with the disc off, the peaks went too far the other way and the light became a broad tint over
        // ground with no fire near it - a wash reaching further than the fire could throw at the same value as the
        // dimmer flame cels, so the picture had two competing warm areas and the fire stopped being the value centre.
        // The cut that followed should have been RANGE ONLY. Taking the peaks down with it left the small fires
        // throwing nothing at all - the mud directly under a burning man was the same value as mud three hundred pixels
        // away, so he floated - while the pull-in on range was correct and stays. Tight and bright, not tight and shy.
        // A fire's light has to DIE inside the fire's own reach. These peaks and ranges are the third setting and the
        // rule behind them is: the falloff edge belongs just outside the flame, never out in the open mud.
        //
        // Only the DISC was ever the decal - the light under it was doing the work, and zeroing the card without
        // raising the peak left the wreck and the standing fires touching nothing: white sandbags eighty pixels from a
        // body of fire stayed the same grey as sandbags six hundred pixels away. Card at zero, peak up.
        // A light has no edge; a card does, and what kept reading as a flat pink decal painted on the mud - a
        // uniform oval with a visible elliptical boundary and no falloff keyed to the flame - was always this
        // disc rather than the light under it. Cutting it to a tenth made it fainter and just as hard-edged.
        // The original note below is why it was cut at all; it turned out not to go far enough.
        //
        // The last argument of FireLight is the LIGHT'S GLOW CARD - a featureless disc drawn a third of the light's
        // reach wide and blown out so the bloom takes it. That is exactly right for a muzzle flash, which is one frame
        // of an event too fast to draw, and exactly wrong for a fire, which IS drawn: at the values these calls started
        // with, the brightest thing in every flamethrower shot was a soft peach blob with no contour, no shape and no
        // flicker, and every hand-drawn lick in the frame was darker than it. The eye has to land on a drawing. So the
        // fires keep their light and give up nearly all of their card - what is left is a faint warm haze in the air
        // around the flame, not a lamp standing in front of it.
        const float FireLightEvery = 0.16f; // a standing fire renews its light this often, and it lasts barely longer
        const float LipClear = 0.45f;       // the stream leaves this far above the parapet it has to get over
        const float MaxLip = 1.5f;          // and he can raise the wand this far and no further: past that he is standing on the parapet
        const float FireLit = 3.8f;   // the LIGHT a fire throws. Raise this, never the glow card: peak lights the mud, the card only paints a white disc over it         // a fire's light. NightLights burns its own at 7 over 11 m, but that one is
                                            // not renewed on top of itself, and it does not stand a metre from a parapet

        struct Jet { public float NextCatch; public int Slot; public Vector3 Nozzle, Offset, Aim, ManVel, WasAt; public float Started, Until, NextPuff, NextLight, NextSpill, NextRoot, Seed; public bool Rides; }
        struct Pool { public Vector3 At; public float Born, Life, Size, Next, NextLight, NextCatch, Seed; public bool Stood; }
        struct Torch { public int Slot; public float Born, Life, Next, NextLight, Fire, Seed; }
        struct Pyre { public Vector3 At; public float Born, Life, Size, Next, NextLight, Seed; public bool Stood; }

        readonly List<Jet> jets = new List<Jet>(MaxJets);
        readonly List<Pool> pools = new List<Pool>(MaxPools);
        readonly List<Torch> torches = new List<Torch>(MaxTorches);
        readonly List<Pyre> pyres = new List<Pyre>(MaxPyres);

        // Less blue than it looks like it wants. Spread wide over cold mud, even an amber light mixes to a flat
        // lavender-pink disc with a visible rim and no gradient toward the flame - it stops reading as firelight and
        // starts reading as a gel painted on the ground. The blue channel is what does that.
        // Amber, and no further. Pushed harder than this the whole picture drops chroma into umber: the flame and
        // the wet ground land in the same hue family, the dark contour that cuts a fire out of a blue night stops
        // reading, and the fire goes soft exactly where it should be hardest.
        static readonly Color Firelight = new Color(1f, 0.46f, 0.03f);

        // what the last Update was handed. A caller wants to say "this goes up" and nothing more, so the books to
        // draw it in and the terrain to stand it on are remembered here rather than passed in at every call site.
        FlipbookFx books;
        System.Func<float, float, float> ground;
        System.Func<int, Vector3> drawn;

        /// <summary>
        /// Where a man's nozzle is this frame and which way it points, from the figure's baked sockets (CombatFx
        /// fills it; the same sockets the rifle's muzzle flare is hung on). Zero when the man has no figure drawn.
        /// </summary>
        public System.Func<int, (Vector3 at, Vector3 forward)> Nozzle;

        /// <summary>Told when a man catches fire or stops burning, so the controller can put him on Clip.Burning and
        /// take him off it again (CombatFx fills it from AnimationController.SetAlight / Douse).</summary>
        public System.Action<int, float> Alighted;

        /// <summary>
        /// "There is fire at this point, this wide, and it will hold for this long - whoever is standing in it catches."
        /// Flamethrower knows where its fire is and nothing at all about who is on the field, so the search belongs to
        /// the host, which calls Ignite back for each man it touches. Fire only sets men ALIGHT: who dies of it is the
        /// sim's to say, and this never speaks for it.
        /// </summary>
        public System.Action<Vector3, float, float> Catch;

        public int Jets => jets.Count;
        public int Fires => pools.Count + torches.Count + pyres.Count;

        public void Clear() { jets.Clear(); pools.Clear(); torches.Clear(); pyres.Clear(); }

        // ------------------------------------------------------------------ what the fight asks for

        /// <summary>
        /// He squeezes the trigger: a burst from his nozzle along his barrel. Held by his slot, so the stream follows
        /// the man as he is drawn rather than hanging where he stood when the tick fired, and a second Shot while the
        /// first is still running lengthens the burst instead of starting a second one on top of it.
        /// </summary>
        public void Burst(int slot, Vector3 nozzle, Vector3 aim, Vector3 manAt, float seconds = BurstSeconds)
        {
            aim.y *= 0.35f;                                    // he plays it along the ground, never up at the sky
            aim = aim.sqrMagnitude > 1e-4f ? aim.normalized : Vector3.forward;
            bool rides = slot >= 0 && manAt.sqrMagnitude > 0f;
            Vector3 offset = rides ? nozzle - manAt : Vector3.zero;
            for (int i = 0; i < jets.Count; i++)
                if (jets[i].Slot == slot && slot >= 0)
                {
                    var held = jets[i];
                    held.Nozzle = nozzle; held.Offset = offset; held.Rides = rides;
                    held.Aim = Vector3.Slerp(held.Aim, aim, 0.5f);                         // the stream swings onto the new target, it does not cut
                    held.Until = Mathf.Max(held.Until, Time.time + seconds);
                    jets[i] = held; return;
                }
            if (jets.Count >= MaxJets) jets.RemoveAt(0);
            jets.Add(new Jet { Slot = slot, Nozzle = nozzle, Offset = offset, Rides = rides, Aim = aim, Started = Time.time,
                               Until = Time.time + seconds, NextPuff = 0f, NextLight = 0f, Seed = Random.value * 10f });
        }

        /// <summary>
        /// He turns his nozzle on a point: the burst is taken from the figure's own weapon socket and aimed from
        /// there, so it leaves the weapon and not the middle of the man. False if he is not being drawn.
        /// </summary>
        public bool BurstFrom(int slot, Vector3 target, float seconds = BurstSeconds)
        {
            if (Nozzle == null) return false;
            var (at, forward) = Nozzle(slot);
            if (at.sqrMagnitude <= 0f) return false;
            Vector3 aim = target - at;
            if (aim.sqrMagnitude < 1e-4f) aim = forward;
            Burst(slot, at, aim, drawn != null ? drawn(slot) : Vector3.zero, seconds);
            return true;
        }

        /// <summary>Burning fuel left on the ground. Pools near the camera are kept when the list is full.</summary>
        public void Spill(Vector3 at, float size, float seconds)
        {
            // Fuel already burning here is FED, not stacked on. A jet lays three and a half of these a second, so a
            // burst held for two seconds used to leave seven separate discs overlapping in the same square metre, and
            // three men firing filled the whole list - at which point nothing else on the field could catch at all,
            // and the eviction churned through whatever was furthest from the camera. Merging makes a swept area read
            // as one spreading patch of burning fuel, which is also what it is.
            for (int i = 0; i < pools.Count; i++)
            {
                var e = pools[i];
                float dx = e.At.x - at.x, dz = e.At.z - at.z;
                // the reach of the merge is capped, or a pool that has grown swallows fire laid metres away and a jet
                // swept along a trench leaves one blob where it started instead of the line it actually burned
                float reach = Mathf.Min(e.Size, 2.0f) * 0.8f;
                if (dx * dx + dz * dz > reach * reach) continue;
                e.Size = Mathf.Min(e.Size + size * 0.22f, 4.5f);              // it spreads a little, it does not double
                e.Life = Mathf.Max(e.Life, Time.time + seconds - e.Born);     // and it is kept alight
                pools[i] = e; return;
            }
            if (pools.Count >= MaxPools)
            {
                int worst = 0; float far = -1f;
                for (int i = 0; i < pools.Count; i++) { float d = CameraShake.DistanceToLook(pools[i].At); if (d > far) { far = d; worst = i; } }
                if (CameraShake.DistanceToLook(at) > far) return;   // the new one is further off than everything alight: let it go
                pools.RemoveAt(worst);
            }
            pools.Add(new Pool { At = at, Born = Time.time, Life = seconds, Size = size, Next = 0f, Seed = Random.value * 10f });
        }

        /// <summary>A man is alight and will run until he drops (the controller has him on Clip.Burning).</summary>
        public void Ignite(int slot, float seconds)
        {
            for (int i = 0; i < torches.Count; i++)
                if (torches[i].Slot == slot) { var t = torches[i]; t.Life = Mathf.Max(t.Life, Time.time + seconds - t.Born); torches[i] = t; return; }
            if (torches.Count >= MaxTorches) torches.RemoveAt(0);
            torches.Add(new Torch { Slot = slot, Born = Time.time, Life = seconds, Next = 0f, Seed = Random.value * 10f });
            Alighted?.Invoke(slot, seconds);
        }

        /// <summary>He is dead or the fire is out: stop drawing him alight (the corpse keeps a pool under it).</summary>
        public void Douse(int slot)
        {
            for (int i = torches.Count - 1; i >= 0; i--) if (torches[i].Slot == slot) torches.RemoveAt(i);
            Alighted?.Invoke(slot, 0f);
        }

        /// <summary>A big thing burning: a wreck, a dugout, a house the fire got into. Size is its width in metres.</summary>
        public void Alight(Vector3 at, float size, float seconds)
        {
            for (int i = 0; i < pyres.Count; i++)
                // horizontal only: a fire already stepped has been stood on the mud, and the caller's point has not,
                // so comparing heights too would stop a raised bank's fire from ever being fed
                if (new Vector2(pyres[i].At.x - at.x, pyres[i].At.z - at.z).sqrMagnitude < 4f)
                {
                    var q = pyres[i]; q.Size = Mathf.Max(q.Size, size); q.Life = Mathf.Max(q.Life, Time.time + seconds - q.Born); pyres[i] = q; return;
                }
            if (pyres.Count >= MaxPyres) pyres.RemoveAt(0);
            pyres.Add(new Pyre { At = at, Born = Time.time, Life = seconds, Size = size, Next = 0f, Seed = Random.value * 10f });
        }

        /// <summary>
        /// His tank goes up. The spec is "explodes on death", and a fuel tank is not a shell: there is no fragment
        /// case and no crater, so this is all light and fire - a white star, a fireball that keeps climbing after the
        /// flash has gone, the smoke rolling off it, and burning fuel thrown out around the place he stood.
        /// </summary>
        public void TankCookOff(Vector3 at)
        {
            if (books == null || !books.Ready) return;
            Vector3 foot = at; foot.y = ground != null ? ground(at.x, at.z) : at.y;
            float glow = (SceneMood.Night ? 3.4f : 2.0f) * SceneTints.Now.Glow;

            // the flash has to outlast the fireball's first frames, or the bang and the fire arrive as two separate events
            books.Add(FlipbookFx.Book.Flash, at + Vector3.up * 0.6f, 13f, 0.42f, glow: glow * 1.5f);
            books.Add(FlipbookFx.Book.Star, at + Vector3.up * 0.6f, 10f, 0.32f, roll: Random.value * 6.2832f, glow: glow * 1.4f);
            // the fireball: born out of a point, rising as it swells, and it outlives the flash by a long way.
            // FireBurst is drawn at 12 fps over 32 frames, so a card has to LIVE 2.7s to be allowed to finish - shorter
            // than that and it is cut off mid-bloom, which is what made the whole cook-off read as one flat bang.
            // Three of them, staggered, are the beat: the punch at the ground, the climb, and the head rolling off the top.
            // The tank going up is ONE drawing. The pack has a fireball in it - orange_electric_explosion - with a hot
            // heart that rolls off into soot rings over two seconds, which is a cook-off from beginning to end; three
            // staggered copies of a small burst were only ever an impression of one. It fills nine tenths of its cell,
            // so unlike the tongues it needs no Cell multiplier, and it is entered at its own frame 0 because
            // firebooks.py already cut the streak of the shell arriving off the front of it.
                        // Twenty-eight frames, so the card lives long enough to play them. Chasing the old sheet's annulus down to
            // ten frames left a tank going up rendering nearly four times smaller and forty luminance darker than an
            // ambient campfire burning behind it in the same shot - the event was over before the eye arrived.
            books.Add(FlipbookFx.Book.Blast, at + new Vector3(-0.95f, 1.15f, -0.35f), 7.2f, 2.33f,
                      velocity: Vector3.up * 0.9f, grow: 0.35f, glow: glow, pop: 0.15f, roll: Lean());
            // ONE card. There was a second, mirrored and a couple of frames behind, to give the ball a far side, and
            // with the annulus cels cut there is nothing left for it to do but double the silhouette: the two copies
            // overlapped into a single opaque loaf two and a half times the area of the fire it replaced. A fireball
            // is one drawing; the far side of it is the drawing's own business.
            // and what a fireball leaves: smoke that goes on climbing after the fire in it has gone out
            for (int i = 0; i < 2; i++)
            {
                float a = (i + 0.35f + Random.value * 0.5f) * 2.1f;
                // Dimmer than the fire, late - and BIGGER than the frame rather than smaller than the fire, which is the
            // opposite of what it was set to last round and is the correct answer to a different question. The Smoke
            // sheet's cells are drawn as soft discs. At a size that fits in shot you can see every one of those circles
            // and the plume reads as four or five brown bubbles stacked behind the flame; blown up past the frame edge
            // there is no complete circle left to recognise and what is left is haze. Faint enough that it cannot take
            // the value centre off the fire, which is the thing it kept doing at every size. At three quarters opacity and half again the
                // fire's size it won the shot outright: a pale grey cauliflower with cleaner edges and a more legible
                // silhouette than the flame at its foot, so a tank cooking off read as a smoke machine. Smoke is what
                // the fire LEAVES - it has to be behind it in value and it has to arrive second.
                books.Add(FlipbookFx.Book.Smoke, at + new Vector3(Mathf.Cos(a), 0.9f + Random.value, Mathf.Sin(a)) * 1.3f,
                          6.5f + Random.value * 3f, 2.6f + Random.value, velocity: new Vector3(Mathf.Cos(a) * 1.1f, 2.1f + Random.value, Mathf.Sin(a) * 1.1f),
                          grow: 1.3f, alpha: 0.17f, pop: 0.35f, delay: 0.85f + i * 0.12f, startFrame: 3.2f + Random.value * 1.2f);
            }
            // burning fuel thrown out: a ring of pools, uneven, the near ones bigger
            int spills = 5;
            for (int i = 0; i < spills; i++)
            {
                float a = (i + Random.value * 0.8f) * (6.2832f / spills), r = 1.1f + Random.value * 3.4f;
                Vector3 p = new Vector3(at.x + Mathf.Cos(a) * r, 0f, at.z + Mathf.Sin(a) * r);
                p.y = ground != null ? ground(p.x, p.z) : foot.y;
                Spill(p, 1.2f + Random.value * 1.1f, 5.5f + Random.value * 4f);   // thrown fuel, not a second explosion
            }
            Alight(foot, 3.2f, 8f);                                  // and the place he stood goes on burning
            Catch?.Invoke(at, 4.5f, 5f + Random.value * 3f);         // and everyone who was standing near him is alight
            CameraShake.Add(at, 7f);
            // Bright with a CENTRE, rather than either wide and even or small and shy. At 15 m the wash reached the
            // whole trench floor and sat at one value with nothing to read as the hot place; pulled in to 9 m it stopped
            // reaching the ground the tank is standing on, and snow a metre from an open fire was the same blue as snow
            // forty metres away. The radius is not what makes a light look local - the FALLOFF is (see TWHearthLight).
            SceneHooks.FireLight?.Invoke(at + Vector3.up * 1.2f, Firelight, 13f, 7.5f, 0.55f, 0f);
            SceneHooks.Sparks?.Invoke(at + Vector3.up * 0.8f, 26);
        }

        // ------------------------------------------------------------------ the frame

        /// <summary>
        /// Move every fire on a tick and hand this frame's cards to the books. drawn gives where a slot's man is being
        /// DRAWN, feet on the ground he is drawn standing on - not his place on the sim's field, whose y is zero, and
        /// not the mud beneath him either, because a man can be climbing or in the air. ground samples the drawn
        /// terrain under a point, and is for fire that lies on the ground rather than fire carried by someone.
        /// </summary>
        public void Update(float now, Camera cam, FlipbookFx books, System.Func<int, Vector3> drawn, System.Func<float, float, float> ground)
        {
            this.books = books; this.ground = ground; this.drawn = drawn;
            if (books == null || !books.Ready) return;
            float glowNight = SceneMood.Night ? 1.0f : 0.8f, tint = SceneTints.Now.Glow;   // a fire burns, it does not flash: see the note on Relight

            for (int i = jets.Count - 1; i >= 0; i--)
            {
                var j = jets[i];
                if (now > j.Until) { jets.RemoveAt(i); continue; }
                StepJet(ref j, now, cam, books, drawn, ground, glowNight * tint);
                jets[i] = j;
            }
            for (int i = pools.Count - 1; i >= 0; i--)
            {
                var p = pools[i];
                if (now - p.Born > p.Life) { pools.RemoveAt(i); continue; }
                StepPool(ref p, now, books, glowNight * tint);
                pools[i] = p;
            }
            for (int i = torches.Count - 1; i >= 0; i--)
            {
                var t = torches[i];
                if (now - t.Born > t.Life) { Alighted?.Invoke(t.Slot, 0f); torches.RemoveAt(i); continue; }
                StepTorch(ref t, now, books, drawn, ground, glowNight * tint);
                torches[i] = t;
            }
            for (int i = pyres.Count - 1; i >= 0; i--)
            {
                var q = pyres[i];
                if (now - q.Born > q.Life) { pyres.RemoveAt(i); continue; }
                StepPyre(ref q, now, books, glowNight * tint);
                pyres[i] = q;
            }
        }

        /// <summary>
        /// Where the stream really leaves him. A man firing out of a trench has his own parapet up to a metre above
        /// his weapon and a metre and a half in front of it (measured on the 1917 field: muzzle at 2.15 m, the bank
        /// cresting at 3.13 m), so a jet laid level goes straight into the earth he is standing behind and is never
        /// seen. A rifle gets away with it because a tracer is thin and lasts a tenth of a second; eleven metres of
        /// fire does not. He does what the man actually did: brings the wand up and lays it over the lip. The ground
        /// is read along the aim rather than assumed, so he only raises it where there is something to clear - in the
        /// open, or firing along the trench, the stream stays on the weapon.
        /// </summary>
        /// <summary>
        /// Where the stream actually ends: the first point along the aim the ground has risen above, or the full reach
        /// if it meets nothing. A jet drawn to its nominal reach goes THROUGH a parapet and comes out the far side;
        /// what it should do is pile up against it, which is also where the splash belongs and where the fire it lays
        /// down should be.
        /// </summary>
        /// <summary>
        /// Where a point at u along the stream sits relative to the straight chord from the nozzle to the target.
        /// Thrown fuel falls: the apex belongs around two thirds of the way out with the head nose-down past it, and
        /// what the eye judges is the deviation from the CHORD, not from the horizon. So the drop is perpendicular to
        /// the chord IN THE CARD PLANE and budgeted as a fraction of the run's own on-screen length - a world-space
        /// drop in metres was invisible, because the chord already climbs the picture (the man is near the camera and
        /// firing away up-slope) by far more than the sag ever took away.
        /// </summary>
        /// <summary>
        /// Hold a card within twice its own drawing's aspect. A sheet drawn 1:1.67 rendered at 15:1 is not that
        /// drawing stretched, it is a ruled almond - measured, one mouth-end card came out with a 374 px edge sitting
        /// 4.4 px RMS off a straight line, which is a thing no brush makes and the eye finds instantly. Stretch is the
        /// cheapest way to get a long thin shape and the fastest way to lose the hand-drawn read.
        /// </summary>
        static void Shapely(FlipbookFx.Book b, ref float w, ref float h)
        {
            FlipbookFx.Geometry(b, out float lo, out float hi, out float fill);
            float drawn = fill / Mathf.Max(0.05f, hi - lo);          // the sheet's own width:height
            float most = drawn * 1.6f, least = drawn * 0.45f;   // 2.2 still let a contour's wobble scale below a pixel
            float a = w / Mathf.Max(0.01f, h);
            if (a > most) h = w / most;
            else if (a < least) w = h * least;
        }

        static Vector3 Arc(Camera cam, float u, float len, Vector3 along)
        {
            Vector3 down = cam != null
                ? -(cam.transform.up - Vector3.Project(cam.transform.up, along)).normalized
                : Vector3.down;
            // skewed so the low point is past the middle and the last quarter is still falling
            float t = Mathf.Sin(Mathf.Pow(u, 0.78f) * Mathf.PI);
            return down * (t * len * 0.135f);
        }

        Vector3 Hit(Vector3 mouth, Vector3 aim, float reach, out bool struck)
        {
            struck = false;
            if (ground == null) return mouth + aim * reach;
            for (float d = 1.5f; d <= reach; d += 0.5f)
            {
                Vector3 q = mouth + aim * d;
                if (ground(q.x, q.z) > q.y) { struck = true; return mouth + aim * Mathf.Max(1.5f, d - 0.5f); }
            }
            return mouth + aim * reach;
        }

        Vector3 OverTheLip(Vector3 nozzle, Vector3 aim)
        {
            if (ground == null) return nozzle;
            float crest = nozzle.y;
            for (float d = 0.5f; d <= 3.5f; d += 0.5f)
            {
                Vector3 q = nozzle + aim * d;
                crest = Mathf.Max(crest, ground(q.x, q.z));
            }
            return nozzle + Vector3.up * Mathf.Clamp(crest + LipClear - nozzle.y, 0f, MaxLip);
        }

        void StepJet(ref Jet j, float now, Camera cam, FlipbookFx books, System.Func<int, Vector3> drawn, System.Func<float, float, float> ground, float glow)
        {
            // the nozzle rides the man. A Shot event arrives once a burst, but a burst lasts most of a second and he
            // does not stand still for it: the nozzle is held as an offset ON him, so the stream leaves the weapon
            // through the whole burst instead of hanging in the air where he was when the tick fired.
            if (j.Rides && drawn != null)
            {
                Vector3 man = drawn(j.Slot);
                if (man.sqrMagnitude > 0f)
                {
                    // his speed, so a card laid this beat keeps travelling with him until the next one replaces it
                    if (j.WasAt.sqrMagnitude > 0f) j.ManVel = (man - j.WasAt) / Mathf.Max(1e-3f, Time.deltaTime);
                    j.WasAt = man;
                    j.Nozzle = man + j.Offset;
                }
            }
            Vector3 mouth = OverTheLip(j.Nozzle, j.Aim);   // over his own parapet, if there is one in the way
            float age = now - j.Started, left = j.Until - now;
            // the burst opens and closes: pressure comes up over the first moments and drops away at the end, so a
            // burst starts and stops like a valve instead of appearing and vanishing
            float valve = Mathf.Clamp01(age / 0.10f) * Mathf.Clamp01(left / 0.14f);
            if (valve <= 0.01f) return;

            // The body of the jet is a DRAWING. The pack has a stream in it - blue_direction_explosion, rooted at its
            // left edge and thrown right - and it was skipped for two rounds because it is blue, when the converter
            // throws colour away and the shader recolours from value. Before that it was faked: first a crowd of
            // ballistic puffs (which came apart into drifting blobs), then a chain of nine round fireballs (which held
            // together but is nine cards pretending to be one shape). One card that was drawn as a stream beats both.
            //
            // It is played like a valve rather than looped, because the book is drawn that way: f0-11 the stream
            // reaching out, f11-18 it standing at full length, f19-28 it breaking up when the pressure goes. So the
            // frame is taken from the burst's own clock and not from Body().
            // Rolled to an aim tilted a little ABOVE the one he is pointing: fire leaves the nozzle flat and rises as it
            // slows, so the drawing has to climb across the screen rather than run level with the ground. Rolled to the
            // bare aim, the stream lay along the mud like a stain and nothing in it said the fuel was in the air.
            float roll = FlipbookFx.ScreenRoll(cam, (j.Aim + Vector3.up * 0.20f).normalized);
            Vector3 along = cam != null ? cam.transform.right * Mathf.Cos(roll) + cam.transform.up * Mathf.Sin(roll) : j.Aim;
            Vector3 side = Vector3.Cross(Vector3.up, j.Aim);
            Vector3 far = Hit(mouth, j.Aim, Reach * valve, out bool struck);
            // A card lies in the screen plane, so a stream pointed at the camera has no length to draw along. How much
            // of its world length survives on screen is the sine of the angle to the view; under about 0.6 there is not
            // enough of it left to read and the old chain, which is round cards and does not care, takes back over.
            float onScreen = cam != null ? Mathf.Sqrt(Mathf.Max(0f, 1f - Mathf.Pow(Vector3.Dot(j.Aim, cam.transform.forward), 2f))) : 1f;
            float chainWeight = 1f - Mathf.SmoothStep(0.35f, 0.70f, onScreen);
            float drawnWeight = 1f - chainWeight;

            if (now >= j.NextRoot)
            {
                j.NextRoot = (j.NextRoot <= 0f ? now : j.NextRoot) + RootEvery;
                float reach = Reach * valve;
                if (drawnWeight > 0.05f)
                {
                    // the valve: out over the first second, held while he holds it, and played out when he lets go
                    const float Open = 11f, Hold = 18f, Last = 28f;
                    float frame = left < (Last - Hold) / 12f ? Mathf.Min(Last, Hold + (((Last - Hold) / 12f) - left) * 12f)
                                : age * 12f < Open ? age * 12f
                                : Open + Mathf.Repeat(age * 12f - Open, Hold - Open);
                    float len = Vector3.Distance(mouth, far) * onScreen;
                    FlipbookFx.Geometry(FlipbookFx.Book.Jet, out float lo, out float hi, out float fill);
                    float breath = 0.90f + Mathf.PerlinNoise(j.Seed, now * 7f) * 0.20f;

                    // The stream is a CHAIN of the drawn stream, not one card of it, and this is the third answer to
                    // the same question. One card was right about the drawing and wrong about everything else. Blown up
                    // to the full eleven metres, the sheet's own curve hooks back and crosses its own tail, so the
                    // silhouette closes around a lens of background and stops being a stream at all - it reads as a
                    // paisley. The taper runs the wrong way too: the drawing carries its mass at the far end and its
                    // point at the root, so at full length the eye travels tip-to-nozzle and the fire looks INHALED.
                    // And thickness tied to length made an eleven-metre jet seven metres deep - four times the height
                    // of the man holding it. A flamethrower stream is about as thick as a man is wide.
                    //
                    // So: short overlapping copies laid along the aim, each about a man and a half tall and swelling
                    // toward the tip, each a few frames behind the one in front. None of them is long enough for the
                    // curve to double back, the swell runs the right way, and the thickness is an absolute size in
                    // metres instead of a fraction of the reach. It is the old chain's shape logic with the pack's
                    // drawing in place of the round fireball the chain used to be made of.
                    // The links have to FUSE. At a 43% overlap and three and a half frames apart they stayed five
                    // separate drawings: each outline closed before the next began, so the stream had a waist at every
                    // joint and necked almost to nothing between the third and fourth. Five contours in a row is a
                    // string of beads, and a string of beads is a worse read than one ugly shape, because at least the
                    // one shape was a single gesture the eye could travel. Past half their own length of overlap, and
                    // close enough in the book that neighbours are not on visibly different poses, the union of the
                    // silhouettes has no waist and the chain stops being countable.
                    const int Links = 4;   // three made every link a third of the run long: see the segment length below
                    bool flipHead = Mathf.PerlinNoise(j.Seed * 3.1f, 0f) < 0.5f;
                    float step = len / (Links - 0.5f);                      // the last link's centre lands ON the impact
                    // and the first one starts AT the mouth, not a fifth of the way out: the stream had begun
                    // in mid-air over empty mud with no nozzle and no operator under its near end.
                    Vector3 root = mouth - along * (step * 0.30f);
                    for (int i = 0; i < Links; i++)
                    {
                        float u = Links > 1 ? i / (Links - 1f) : 1f;
                        bool head = i == Links - 1;
                        // The LAST link is not part of the weave. Fusing every link at the same overlap cured the string
                        // of beads and then went straight past it: with five cards all inside one envelope, the right
                        // edge of the stream stopped being any card's drawn contour and became the CARD BOUNDARY - a
                        // hard vertical cut with a flat top, a rectangle with one corner rounded off. A gout of fire
                        // has a head, and the head is the loosest shape in it. So the tip link overlaps a little less,
                        // is turned off the axis and hangs slightly past the chain, which leaves its own silhouette -
                        // not a seam - to draw the end of the stream. SMALLER than the links behind it, though, and
                        // only slightly turned: at 1.15x and seventeen degrees it stopped being the stream's leading
                        // edge and became a blunt two-lobed paw tied to the end of it, convex on every side, with a
                        // visible notch where it met the chain. A head still has to be part of the line of action.
                        // Length scales WITH the link, not with the run. Dropping the link count stretched step, and
                        // because the thickness ramp is in absolute metres the root cards came out ten metres long and
                        // under one thick - flat slivers that vanished against the mud and took the near half of the
                        // stream with them. A short tight link at the nozzle growing to a long loose one at the head is
                        // the shape anyway; it just has to be said in the length as well as in the thickness.
                        // The near end needs MORE overlap, not less. At 1.35 steps long against a spacing of one
                        // step, the first links overlapped by only a quarter of their length and the chain tore open
                        // between the second and the third - measured, warm coverage collapsed 85% across one bin
                        // and the arc appeared to kink there, which read as the curve being applied unevenly when it
                        // was really a hole. Short links still, but never so short that two of them do not meet.
                        float segLen = step * Mathf.Lerp(2.35f, head ? 2.35f : 2.75f, u);   // still pinching at 1.95: measured 72% coverage lost in one bin
                        float cw = segLen / fill * (head ? 0.85f : 1f);
                                                // A fine tip, not a hairline: at 0.95 the first quarter of the stream measured under nine
                        // pixels thick over 240 of length, which reads as a thrown spear rather than as fuel under
                        // pressure. Fine is a proportion to the head, not an absolute thinness.
                        // LINEAR, not quadratic. Squaring the ramp dumped all of the growth into the far half, so
                        // with the terminus stacked on top of it the head measured seven and a half times the mouth -
                        // a tadpole. Fuel does widen downrange, but it widens all the way along.
                        // The MOUTH is what sets the head/mouth ratio, and raising the core's floor last round did nothing for it
                        // (measured 62 -> 60 px) because the silhouette at the nozzle is drawn by the ENVELOPE, not by
                        // the core hiding inside it. Widening the envelope's root and easing off its head is the only
                        // thing that moves the ratio: it had gone 4.36 -> 4.68 -> 5.26 against a 2.0-3.0 target while
                        // three separate attempts aimed at the core.
                        float thick = Mathf.Lerp(3.5f, 3.7f, Mathf.Min(1f, u * 1.05f)) * breath * (head ? 0.85f : 1f);
                        float ch = thick / Mathf.Max(0.05f, hi - lo);       // the drawing fills only part of its cell
                        Shapely(head ? FlipbookFx.Book.Head : FlipbookFx.Book.Jet, ref cw, ref ch);
                        // the arc: fuel leaves flat and the far end rises as it slows
                        // A SAG, not a ramp. Rising steadily over its whole run is what helium does; thickened petrol
                        // leaves the nozzle flat and fast, loses the argument with gravity through the middle of its
                        // flight, and only goes up again where it piles into something and burns. A dead-straight
                        // centreline climbing from nozzle to head was the single thing making the jet read as a decal
                        // laid across the picture rather than as fuel thrown through the air.
                        // Measured, the last version was FLAT: the middle third sat level with the nozzle to within a
                        // few pixels and then ramped, which reads as a horizontal girder with fire painted on it. A
                        // sag has to go BELOW the mouth before it comes up, and by enough to see.
                        // The sag has to be measured on the SCREEN, not in the world, and this is why two rounds of
                        // it read as a dead straight line however far the amplitude was pushed. The chord from the
                        // nozzle to the target already climbs the picture - the man is near the camera and firing
                        // away up-slope - so a world-space drop of a metre and a half was simply swallowed by a
                        // screen-space rise of far more. What the eye judges is the deviation from the CHORD, so the
                        // sag is applied perpendicular to it, in the card plane, and budgeted against the run's own
                        // on-screen length rather than in metres.
                        // The arc is hung off Arc(), and it is applied to the CORE chain as well. Measured on the
                        // last build the stream still bowed 13px the wrong way, and the reason was not the sign or
                        // the budget: the sag was only ever on the envelope, while the core - five opaque, bright
                        // links - ran dead straight underneath it and owned the warm centroid the eye actually reads.
                        // Half a curve and half a rod averages to a rod.
                        Vector3 at = root + along * (step * (i + 0.5f) + (head ? step * 0.10f : 0f)) + Arc(cam, u, len, along);
                        bool flip = (i & 1) == 1;
                        // Far enough apart in the BOOK to be different drawings. A stagger of a frame or two still
                        // showed the same cel five times in a row and the eye read the repeat instantly, which is the
                        // thing that makes a chain look like a chain; three and a half frames apart, no two links in
                        // the stream are on the same picture. The roll jitter is small and clamped for the same reason
                        // the links are short - let a card turn far off the travel direction and its own curve doubles
                        // back across the axis and closes the silhouette again.
                        // the head is turned well off the axis on purpose; the interior links only breathe
                        // per-link turn and size, not just per-link cel: mirroring alternate links was not enough
                        // on its own, because this drawing is near enough symmetrical that its mirror reads the same.
                        // The same hook at the same angle at the same size three times along a run is a printed
                        // pattern, and a printed pattern is the opposite of turbulence.
                        float vary = 1f + (Mathf.PerlinNoise(j.Seed + i * 5.3f, 0.5f) - 0.5f) * 0.30f;
                        cw *= vary; ch *= vary;
                        float jitter = (Mathf.PerlinNoise(j.Seed + i * 1.7f, now * 3f) - 0.5f) * 0.16f
                                     + (Mathf.PerlinNoise(j.Seed + i * 2.9f, 1.5f) - 0.5f) * 0.55f
                                     + (head ? 0.34f + (flipHead ? -0.10f : 0.10f) : 0f);   // nose-down out of the tangent, and never axis-aligned
                        // The head is a DIFFERENT BOOK. Every stream sheet in the pack is drawn as a curl, and a
                        // curl has a hole through the middle of it. Buried in a chain that never shows; on the
                        // leading mass it is fatal - clean terrain read straight through the centre of the hottest
                        // object in the picture, which makes the whole jet a ring of paint rather than a body of
                        // burning fuel. FireHead is an explosion sheet, measured at 0.91 solid through its own
                        // centroid where the stream books run 0.65 to 0.72: a closed bolus, which is what the front
                        // of a gout actually is.
                        books.Add(head ? FlipbookFx.Book.Head : FlipbookFx.Book.Jet, at, cw, RootEvery * 1.15f,
                                  flip ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                  // mirrored AND turned through half a circle flips a card about the aim; Mirror alone
                                  // would put the root of a rooted book at its FAR end
                                  roll: (flip ? roll + Mathf.PI : roll) + jitter,
                                  // The HEAD is fully opaque, and the thinning lives behind it. Carrying opacity down the run still left the
                                  // head the most transparent part of the stream, which is backwards: the head is where unburnt fuel is
                                  // thickest, and terrain reading through it makes the whole jet a coloured gel rather than a body.
                                                                    // Glow ramped along the run. The nozzle cel is the one authored into the hot band,
                                  // so raising the ceiling made the stream brightest exactly where the fuel is still
                                  // cold and has not caught - measured, mouth-to-head luminance went from 1.09 to
                                  // 1.41 the wrong way. A pressurised stream is dim at the mouth and peaks a third of
                                  // the way out, where combustion finishes.
                                  height: ch, alpha: (head ? 1f : 0.88f - 0.06f * u) * valve * drawnWeight,
                                  // This ramp was meant to make the stream dim at the mouth and brightest a third of the way out, and measured
                                  // on the render it does the opposite: mean luminance peaks in bin 3 near the nozzle at 160
                                  // and falls to 79-104 at the head, with the hot-core median sitting at 36% along. The
                                  // reason is the u * 2.2: it saturates at u = 0.45, so the whole downstream half is served
                                  // one flat value while the cards out there are thinner and more transparent than the ones
                                  // at the mouth. Ramping the whole length instead of the first half lets the head win.
                                  glow: glow * Mathf.Lerp(0.55f, 1.45f, Mathf.Min(1f, u * 1.15f)),
                                  velocity: j.ManVel,
                                  // Reversed, and this was a real bug rather than a matter of degree. Staggering by i
                                  // put the TIP on frame-11.6, clamped to zero - the very first cel of the book, where
                                  // the valve has barely opened and the drawing is at its thinnest and dimmest - while
                                  // the root sat on the fullest one. So the stream was brightest at the nozzle and
                                  // faded into the impact: the gesture pointed downrange and the value pointed back up
                                  // the barrel, which is the eye being told two opposite things at once. Counting from
                                  // the tip instead puts the most developed cel at the far end, where the fire is.
                                  // Floored at Open-2, never at 0. Counting back from the tip is right, but let a root
                                  // link reach the book's first cels and it is drawing the valve barely open - a thin
                                  // sliver that does not fill its card - so the near half of the stream came apart
                                  // into detached darts with mud showing between them. Every link has to be on a cel
                                  // where the drawing is a full stream; only WHICH full stream may differ.
                                  startFrame: Mathf.Max(Open - 2f, frame - (Links - 1 - i) * 2.0f));
                    }
                    // Licks torn off the far end. The widening is correct and deliberate - burning fuel slows, tumbles
                    // and spreads, so a flamethrower is the one kind of jet that gains mass downrange - but a widening
                    // shape whose contour never breaks is a poured slab rather than a stream. Real fire sheds pieces at
                    // the range where it is coming apart, and those detached scraps are what say "this is losing
                    // cohesion" instead of "this is a stripe of syrup". They are small, they are past the head, and
                    // they are deliberately NOT overlapped into the envelope.
                    for (int i = 0; i < 3; i++)
                    {
                        float lu = 0.72f + i * 0.13f;
                        float wob = (Mathf.PerlinNoise(j.Seed + i * 4.3f, now * 4f) - 0.5f);
                        // These were the ONE emitter in the jet that never went through Shapely, and they were the
                        // last thing in the shot still reading as stretched cards: at the old spread a lick could be
                        // 6.5 m wide against 1.7 m tall - a 3.8:1 card of a 1:1.4 drawing - rolled up to 54 degrees
                        // off the stream. Two of them landing on opposite wobbles drew a pair of matched horns
                        // sweeping out of the head, 1.6x the stream's own thickness above it, and the jet read as a
                        // crab claw. Measured, 9.5% of the fire's contour lay within 2 px RMS of a straight line
                        // over 120 px while every other fire in the build measured 0.0%, and the straightest window
                        // in the frame - 0.69 px - was that arm's lower edge.
                        //
                        // So: the same aspect discipline as everything else, half the roll, and the handedness comes
                        // off the seed rather than off i, because alternating parity is exactly what builds a
                        // symmetric pair out of neighbours.
                        float lw = Mathf.Lerp(2.6f, 0.8f, (lu - 0.72f) / 0.26f) * (0.6f + Mathf.Abs(wob) * 0.9f);
                        float lh = Mathf.Lerp(1.7f, 0.55f, (lu - 0.72f) / 0.26f);
                        Shapely(FlipbookFx.Book.Core, ref lw, ref lh);
                        bool lmir = ((Mathf.FloorToInt(j.Seed * 13f + i * 7.3f) & 1) == 0);
                        books.Add(FlipbookFx.Book.Core,
                                  mouth + along * (len * lu)
                                        + Vector3.up * (len * 0.03f + wob * 1.4f + 0.5f)   // some of them ABOVE the head
                                        + side * (wob * 1.8f),
                                  lw,
                                  RootEvery * 1.15f,
                                  lmir ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                  roll: roll + wob * 0.9f, height: lh,
                                  alpha: valve * drawnWeight, glow: glow * 1.35f,
                                  velocity: j.ManVel + Vector3.up * 0.7f,
                                  startFrame: 8f + Mathf.Repeat(now * 12f + i * 3f, 10f));
                    }

                    // THE CORE. A pressurised stream is not one thickness of fire: it is a dense opaque rod with a
                    // looser, thinner envelope boiling off it, and drawing only the envelope is what let the barbed
                    // wire read straight through the middle of the jet. So a second, shorter chain rides the same
                    // spine at about half the thickness, fully opaque, and it is a DIFFERENT DRAWING - FireCore, cut
                    // from the pack's other rooted stream. Measured against FireJet over their body frames it has the
                    // same root discipline but more cover and fewer holes, which is exactly what the inside of a jet
                    // wants. Being a different glyph also ends the complaint that would not die however much the links
                    // were jittered and mirrored: one drawing repeated five times along a run is a printed pattern, and
                    // no amount of turning it fixes that. Two drawings interleaved is turbulence.
                    const int CoreLinks = 9;   // shorter links, so the mouth one is not long enough to hit the clamp
                    float cStep = len / (CoreLinks - 0.35f);
                    for (int i = 0; i < CoreLinks; i++)
                    {
                        float u = CoreLinks > 1 ? i / (CoreLinks - 1f) : 1f;
                        float cSeg = cStep * 2.3f;
                        // Thicker. The envelope's sheets are curls and a curl has holes; sampling inside the stream's
                        // silhouette returned the cold background colour unchanged, which means the body was a wash
                        // rather than paint. The core is the only opaque thing in the jet, so it is the core that has
                        // to be wide enough to fill what the envelope leaves open.
                        // The core owns the mouth silhouette - it is the only opaque thing in the jet - so raising the
                        // ENVELOPE's floor and not this one left the near links pinned flat against the aspect clamp,
                        // and a card pinned at the clamp has a boundary measurably straighter than any brush in the
                        // build: 1.8 px RMS over 120, against 5-22 px for every hand-drawn contour beside it.
                        float cThick = Mathf.Lerp(2.60f, 3.40f, u) * breath;   // 1.70 still left a 41 px nozzle bin against a 306 px
                                                                              // head - measured 4.4:1, a tadpole. The mouth of a
                                                                              // pressurised jet is the one place the fuel is dense.
                        FlipbookFx.Geometry(FlipbookFx.Book.Core, out float cLo, out float cHi, out float cFill);
                        // The envelope stopped stamping when it got a second drawing, and the repeat simply moved into
                        // the core: its signature is a cream crescent hook, it is the brightest value in the picture,
                        // and three of them at one handedness and one size evenly spaced is a thing the eye locks onto
                        // instantly. Two drawings alternating is still a pattern if one of them has an unmistakable
                        // glyph, so the glyph itself has to be flipped and resized down the run.
                        bool cFlip = (i & 1) == 0;
                        float cVary = 1f + (Mathf.PerlinNoise(j.Seed + i * 7.1f, 3.5f) - 0.5f) * 0.30f;
                        float cW = cSeg / cFill * cVary, cH = cThick * cVary / Mathf.Max(0.05f, cHi - cLo);
                        Shapely(FlipbookFx.Book.Core, ref cW, ref cH);
                        books.Add(FlipbookFx.Book.Core,
                                  mouth + along * (cStep * (i + 0.5f)) + Arc(cam, u, len, along),
                                  cW, RootEvery * 1.15f,
                                  cFlip ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                  roll: (cFlip ? roll + Mathf.PI : roll)
                                      + (Mathf.PerlinNoise(j.Seed + i * 3.7f, 2.5f) - 0.5f) * 0.40f,
                                  height: cH,
                                  alpha: valve * drawnWeight, glow: glow, velocity: j.ManVel,
                                  startFrame: Mathf.Max(3f, 16f - (CoreLinks - 1 - i) * 2.2f));
                    }

                    // THE TERMINUS. Where the stream runs out - whether it met anything or not - the fuel stops being
                    // a jet and becomes fire going up, and that is a change of SHAPE, not more of the same shape. Until
                    // this there was nothing at the end of the line: the far end was simply the busiest part of the
                    // stream, at the same value and hue and alpha and made of the same drawing, so the jet read as
                    // passing through the frame rather than landing in it. Book.Bloom is a ground explosion - rooted on
                    // the floor of its cell, wider at the base than the stream is thick, throwing verticals - which is
                    // the shape of fuel piling up and turning the corner. It stands on the ground under the stream's
                    // end rather than hanging in the air at the end of it.
                    {
                        Vector3 foot = far;
                        if (ground != null) foot.y = ground(far.x, far.z);
                        // Taller than it is wide, and clamped to the book's RISE. Left free to run on, the drawing
                        // reached its own settled tail - the frames where a ground burst has fallen back and spread -
                        // and a terminus that spreads is a stain: the mass centre sat below and behind the stream's
                        // head, the lightest value in the picture was down on the mud, and the eye left the fire to
                        // look at a puddle. Only the throw frames say "the fuel hit this and went up".
                        Standing(FlipbookFx.Book.Bloom, foot.y, foot.y + Mathf.Lerp(3.2f, 5.2f, valve), out float bH, out float bY);
                        // Pulled toward the eye so it stands IN FRONT of the head and breaks its lower contour.
                        // Behind it, the whole rise was masked by the head card and the one beat that says the fuel
                        // landed was never seen at all - the clamp to the rise frames was correct and invisible.
                        Vector3 toEye = cam != null ? (cam.transform.position - far).normalized : Vector3.zero;
                        books.Add(FlipbookFx.Book.Bloom, new Vector3(far.x, bY, far.z) + toEye * 0.9f,
                                  Mathf.Lerp(1.9f, 2.8f, valve), RootEvery * 1.15f,
                                  (Mathf.FloorToInt(now * 12f) & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                  height: bH, alpha: valve * drawnWeight, glow: glow,
                                  startFrame: 1f + Mathf.Repeat(now * 12f, 7f));
                    }

                    // Where it lands, if it landed on anything: the pack's fan, rooted at the wall and climbing it.
                    // Burning fuel that hits a bank keeps its momentum - it peels UP AND BACK along the way it came,
                    // and what drips off falls. Thrown along the aim instead, the fan came off the wall evenly in every
                    // direction and read as a firework or a hit-spark decal rather than as fire arriving from somewhere.
                    if (struck)
                    {
                        Vector3 peel = (Vector3.up * 1.3f - j.Aim * 0.55f).normalized;
                        // sunk, so the bottom third of the fan is below the crest line. Sitting wholly above it, the
                        // splash was a small bonfire lit on top of a ridge; fire piling into a bank CLINGS to the face.
                        // Deeper than before: there was still open ground visible between the flame and the sandbag crest, and a
                        // fire with daylight under it is not burning the thing it is standing on. Its lower third belongs
                        // inside the bank's silhouette.
                        Vector3 sunk = far - Vector3.up * (reach * 0.16f) - j.Aim * (reach * 0.03f);
                        books.Add(FlipbookFx.Book.Fan, sunk, reach * 0.42f, RootEvery * 1.15f,
                                  (Mathf.FloorToInt(now * 12f) & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                  roll: FlipbookFx.ScreenRoll(cam, peel),
                                  height: reach * 0.42f, alpha: Coal * 0.8f * valve * drawnWeight, glow: glow,
                                  startFrame: 2f + Mathf.Repeat(now * 12f, 12f));
                        // and a smaller one, mirrored, lower, and a few frames out of phase. Two copies of one drawing at
                        // the same size standing side by side is a repeat, and a repeat reads as a sticker; at three
                        // fifths the height, sunk into the bank and never peaking at the same moment, it reads as depth.
                        books.Add(FlipbookFx.Book.Fan, sunk - Vector3.up * (reach * 0.10f) - j.Aim * (reach * 0.05f),
                                  reach * 0.26f, RootEvery * 1.15f,
                                  (Mathf.FloorToInt(now * 12f) & 1) == 0 ? FlipbookFx.Kind.None : FlipbookFx.Kind.Mirror,
                                  roll: FlipbookFx.ScreenRoll(cam, (Vector3.up * 0.7f - j.Aim * 1.1f).normalized),
                                  height: reach * 0.26f, alpha: Coal * 0.5f * valve * drawnWeight, glow: glow,
                                  startFrame: 2f + Mathf.Repeat(now * 12f + 5f, 12f));
                    }
                }

                // the chain, which is now either the fallback when the stream is pointed at the eye, or the loose fire
                // boiling off its far half when it is not
                if (chainWeight > 0.05f || true)
                {
                    int links = chainWeight > 0.05f ? JetLinks : 4;
                    float from = chainWeight > 0.05f ? 0.10f : 0.55f;
                    for (int i = 0; i < links; i++)
                    {
                        float u = links > 1 ? i / (links - 1f) : 1f;
                        float along01 = from + (0.90f - from) * u;
                        float wob = (Mathf.PerlinNoise(j.Seed + i * 0.37f, now * 5f) - 0.5f) * 2f * u;
                        Vector3 at = mouth + j.Aim * (reach * along01)
                                   + side * wob * 0.55f
                                   + Vector3.up * (u * u * 1.15f + wob * 0.25f);
                        float swell = along01 < 0.78f ? along01 : 0.78f - (along01 - 0.78f) * 0.9f;
                        float w = (0.80f + 2.15f * swell) * Cell;
                        float tip = 1f - Mathf.SmoothStep(0.72f, 1f, along01) * 0.6f;
                        float a = Coal * (0.95f - 0.30f * along01) * tip * valve
                                * (chainWeight > 0.05f ? chainWeight : 0.45f);
                        books.Add(FlipbookFx.Book.Fire, at, w, RootEvery * 1.15f,
                                  (i & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                                  height: w * (0.72f + 0.20f * along01),
                                  alpha: a, glow: glow,
                                  startFrame: 4f + Mathf.Repeat(now * 12f + i * 2.7f, 9f));
                    }
                }
            }

            // and the boil: loose fire torn off the end of the stream, which is the one thing a chain cannot do. It is
            // thrown from the outer half only, and its scatter grows with how far down the stream it came adrift, so
            // the jet stays tight where it leaves the weapon.
            while (now >= j.NextPuff)
            {
                // a quarter of the old rate while the drawn stream is carrying the body. Thirty of these over the top of
                // it was the crowd the stream was built to replace, and it filled in the negative space that makes the
                // drawing read; at the old rate they also cost more than everything else in the jet put together.
                j.NextPuff = (j.NextPuff <= 0f ? now : j.NextPuff) + PuffEvery * Mathf.Lerp(4f, 1f, chainWeight);
                float t = 0.55f + Mathf.Repeat((j.NextPuff - j.Started) / 0.19f, 1f) * 0.60f;   // past the end of the chain, so the tip frays outward
                Vector3 spread = new Vector3(Random.value - 0.5f, Random.value - 0.35f, Random.value - 0.5f) * 2.4f * t;
                float speed = (4.0f + Random.value * 3.0f) * valve;
                Vector3 vel = j.Aim * speed + spread + Vector3.up * (0.5f + Random.value * 0.8f);
                books.Add(FlipbookFx.Book.Fire, mouth + j.Aim * (Reach * valve * t), (0.85f + Random.value * 0.6f) * Cell,
                          0.45f + Random.value * 0.3f,
                          Random.value < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                          velocity: vel, grow: 1.15f + Random.value * 0.45f, roll: Lean() * 2f,
                          alpha: Coal * 0.7f, glow: glow, pop: 0.32f, startFrame: Body());
            }

            // where the stream is putting its fire down, and the smoke and light that come with it
            if (now >= j.NextLight)
            {
                j.NextLight = now + LightEvery;
                // Two stations, alternating beat by beat, rather than one light at mid-span. A gout of burning fuel
                // lights the ground it passes over along its whole length; with a single light the mud under the near
                // half of the arc stayed as cold as mud four hundred pixels away, and a stream that lights nothing it
                // crosses reads as a decal pasted on the sky plane. They alternate rather than both being laid every
                // beat because NightLights pools its lights and drops the dimmest - two competing every beat would
                // have starved the pools and the impact, which need their light more.
                float station = (Mathf.FloorToInt(now / LightEvery) & 1) == 0 ? 0.55f : 0.88f;   // the fire is at the far end, so the light is too
                Vector3 mid = mouth + j.Aim * (Reach * station * valve);
                float flick = 0.82f + Mathf.PerlinNoise(j.Seed + 11f, now * 7f) * 0.5f;
                SceneHooks.FireLight?.Invoke(mid + Vector3.up * 0.7f, Firelight, 9f * flick * valve, 6.5f, LightEvery * 2.3f, 0f);
                // the top of the jet turns over into smoke, the way burning fuel always does
                // smaller than the fire it comes off and late, the same rule the cook-off's smoke got: at the impact
                // it was the biggest shape in the frame, so a stream hitting a bank read as "small ember, large smoke"
                books.Add(FlipbookFx.Book.Smoke, far + Vector3.up * (0.6f + Random.value * 0.5f), 3.2f + Random.value * 1.5f, 1.9f + Random.value,
                          velocity: j.Aim * 1.6f + Vector3.up * (1.2f + Random.value), grow: 1.2f, alpha: 0.15f, pop: 0.4f, delay: 0.35f, startFrame: 3.2f + Random.value * 1.2f);
                if (Random.value < 0.6f) SceneHooks.Sparks?.Invoke(far + Vector3.up * 0.9f, 4);
            }
            // and what it sweeps over catches. Sampled along the stream rather than at its end, because a jet held on
            // a trench takes everyone between the man and the far end, not only whoever is standing at the tip. It
            // starts a third of the way out so the man working the weapon is never in his own fire.
            if (now >= j.NextCatch && Catch != null)
            {
                j.NextCatch = now + CatchEvery;
                for (int c = 0; c < 3; c++)
                {
                    float u = 0.34f + c * 0.33f;
                    Catch(mouth + j.Aim * (Reach * valve * u), 1.4f + 1.1f * u, 3.5f + Random.value * 2.5f);
                }
            }

            // and it leaves fuel burning where it swept
            if (now >= j.NextSpill)
            {
                j.NextSpill = (j.NextSpill <= 0f ? now : j.NextSpill) + SpillEvery * (0.75f + Random.value * 0.5f);
                Vector3 p = far + new Vector3(Random.value - 0.5f, 0f, Random.value - 0.5f) * 3.2f;
                p.y = ground != null ? ground(p.x, p.z) : far.y;
                Spill(p, 1.7f + Random.value * 1.1f, 4f + Random.value * 3f);
            }
        }

        /// <summary>
        /// How tall a card standing on something has to be, and where to hang it, so that the DRAWING - not the cell it
        /// is drawn in - runs from foot to top. A bottom-anchored card does not put fire at the point it is anchored to:
        /// every book leaves empty cell beneath its drawing (a quarter of it, in FireBall's case), which is why a man
        /// alight had bare boots and bare shins. Each book's own extent is measured by Tools/firebooks.py and carried on
        /// its Sheet row, because it is a fact about that drawing and not a number to taste.
        /// </summary>
        static void Standing(FlipbookFx.Book book, float foot, float top, out float height, out float hang)
        {
            FlipbookFx.Geometry(book, out float inkLow, out float inkHigh, out _);
            height = (top - foot) / (inkHigh - inkLow);
            hang = foot - inkLow * height;
        }

        /// <summary>
        /// How far a fire card is allowed to be turned. Almost none: the sheets are DRAWN with an up - tongues that
        /// taper upward, a heavy root, soot rising off the top - and spinning one to get variety throws that away and
        /// leaves a blob, which is what the ball riding a big fire had become and why it never matched the tongues
        /// under it. Variety comes from mirroring it, which costs the drawing nothing, and from a lean of a few
        /// degrees, which reads as the flame being pushed rather than as a card being rotated.
        /// </summary>
        static float Lean() { return (Random.value - 0.5f) * 0.28f; }

        /// <summary>
        /// Where a sustained fire enters its book. NOT anywhere in the 32 frames: the first five are the flame growing
        /// out of nothing and the last dozen are it dissipating into broad flat shapes, and a card that happens to be
        /// born in either spends its whole short life there - which is what made burning pools read as flat orange
        /// splats laid over the mud. Only the middle of the book is a fire that is simply burning.
        /// </summary>
        static float Body() { return 4f + Random.value * 9f; }

        /// <summary>
        /// Put a ground fire on the ground. A caller almost always has a place on the FIELD - a sim position, whose y
        /// is the sim's zero and not the height of the mud there - and handing that straight to Alight or Spill buries
        /// the fire: on a raised bank a three metre blaze at y=0 is nearly three metres under, and all that shows is
        /// its tip. It cannot be done at the call because the terrain is only known once Update has run, so it is done
        /// once, the first time the fire is stepped.
        /// </summary>
        Vector3 Stand(Vector3 at)
        {
            if (ground != null) at.y = ground(at.x, at.z);
            return at;
        }

        void StepPool(ref Pool p, float now, FlipbookFx books, float glow)
        {
            if (!p.Stood) { p.At = Stand(p.At); p.Stood = true; }
            float k = (now - p.Born) / p.Life;
            float ebb = 1f - Mathf.SmoothStep(0f, 1f, Mathf.Clamp01((k - 0.45f) / 0.55f));   // a pool burns down, it does not blink out
            if (now >= p.NextLight)
            {
                p.NextLight = now + FireLightEvery;
                float flick = 0.7f + Mathf.PerlinNoise(p.Seed, now * 6.5f) * 0.6f;
                SceneHooks.FireLight?.Invoke(p.At + Vector3.up * 0.8f, Firelight, FireLit * 0.55f * ebb * flick, 4f + p.Size, FireLightEvery * 1.15f, 0f);
            }
            if (now >= p.NextCatch && Catch != null)
            {
                p.NextCatch = now + CatchEvery * 2f;           // a puddle is slower to take a man than the stream is
                Catch(p.At, p.Size * 0.8f, 2.5f + Random.value * 2f);
            }
            if (now < p.Next) return;
            p.Next = (p.Next <= 0f ? now : p.Next) + Relight * (0.75f + Random.value * 0.5f);
            float w = p.Size * (0.75f + 0.45f * ebb);
            // the puddle itself is drawn flat and wide, with a blob lifting off it - which is what burning fuel on the
            // ground does, and what a ball of fire laid on its side never looked like
            Standing(FlipbookFx.Book.Pool, 0f, w * 0.62f * Cell, out float poolH, out float poolY);
            // Thin. This drawing lies flat on the mud and it is the one book in the set whose ink runs off the edge
            // of its cell, so at full opacity it is a hard-edged saturated lozenge - a sticker, and worse when one
            // lies across a sandbag top and a duckboard at once. Burning fuel on the ground is a FILM: you see the
            // ground through it, and its edge is where the fuel ran out rather than where the card stopped.
            books.Add(FlipbookFx.Book.Pool, p.At + Vector3.up * (poolY + 0.10f), w * Cell, Relight * Held,
                      FlipbookFx.Kind.Anchored | FlipbookFx.Kind.Upright | (Random.value < 0.5f ? FlipbookFx.Kind.Mirror : 0),
                      grow: 0.25f, alpha: Coal * 0.50f * ebb, glow: glow, height: poolH, pop: 0.45f, startFrame: Body());
            // a tongue standing up out of it, offset so the pool is never symmetrical
            if (Random.value < 0.45f)
                books.Add(FlipbookFx.Book.Pyre, p.At + new Vector3((Random.value - 0.5f) * w * 0.5f, 0.05f, (Random.value - 0.5f) * w * 0.5f),
                          w * (0.55f + Random.value * 0.3f) * Cell, Relight * Held,
                          FlipbookFx.Kind.Anchored | FlipbookFx.Kind.Upright | (Random.value < 0.5f ? FlipbookFx.Kind.Mirror : 0),
                          velocity: Vector3.up * 0.35f, alpha: Coal * 0.9f * ebb, glow: glow, startFrame: Body());
            if (Random.value < 0.30f * ebb) SceneHooks.Sparks?.Invoke(p.At + Vector3.up * (0.4f + Random.value * 0.4f), 2);
            if (Random.value < 0.35f * ebb)
                // CAPPED. Every smoke emitter here was told to be bigger than the frame so its disc edge could not be
                // recognised, and on a pool that rule ran away: a swept line of burning fuel makes wide pools, 2.2x a
                // wide pool is an enormous card, and with grow on top it ended up veiling a whole quarter of the
                // picture in flat grey - a dirty lens rather than smoke. Big enough to have no readable edge, not so
                // big it becomes the weather.
                books.Add(FlipbookFx.Book.Smoke, p.At + Vector3.up * (0.7f + Random.value * 0.5f), Mathf.Min(w * 2.2f, 5f), 2.0f + Random.value,
                          velocity: Vector3.up * (1.3f + Random.value), grow: 1.1f, alpha: 0.17f, pop: 0.5f, startFrame: 3.2f + Random.value * 1.2f);
        }

        void StepTorch(ref Torch t, float now, FlipbookFx books, System.Func<int, Vector3> drawn, System.Func<float, float, float> ground, float glow)
        {
            if (drawn == null) return;
            // drawn gives where he is DRAWN standing, feet included - see the contract on Update. It is not the same
            // as the mud under him: a man climbing a parapet or thrown up by a shell is drawn off the ground, and
            // grounding his fire here would leave it behind in the dirt at the one moment anyone is watching him.
            Vector3 at = drawn(t.Slot);
            float k = (now - t.Born) / t.Life;
            // he leaves a trail of burning fuel behind him wherever he runs, which is most of what makes this read
            if (now >= t.Next)
            {
                t.Next = (t.Next <= 0f ? now : t.Next) + Relight * (0.55f + Random.value * 0.4f);
                if (Random.value < 0.55f) Spill(at, 0.7f + Random.value * 0.5f, 2.6f + Random.value * 2f);
            }
            if (now >= t.NextLight)
            {
                t.NextLight = now + FireLightEvery;
                float flick = 0.75f + Mathf.PerlinNoise(t.Seed + 7f, now * 7f) * 0.5f;
                SceneHooks.FireLight?.Invoke(at + Vector3.up * 1.1f, Firelight, FireLit * 2.9f * flick, 5.5f, FireLightEvery * 1.15f, 0f);
            }
            // The fire ON him is laid on a clock of its own, and a fast one: a card stays where it was put and he is
            // running, so it has to be replaced before he has left it behind. It used to be laid EVERY FRAME, which
            // put two hundred cels of the same drawing over each other per man - the cost of a barrage for a fire that
            // read as a smudge, because two hundred half-transparent copies of a drawing are a fog and not a drawing.
            if (now < t.Fire) return;
            t.Fire = (t.Fire <= 0f ? now : t.Fire) + TorchEvery * (0.8f + Random.value * 0.4f);
            float w = 1.5f + Mathf.PerlinNoise(t.Seed, now * 6f) * 0.7f;
            bool mirror = Random.value < 0.5f;
            // the body of it: a flame wrapping him from his boots to over his head. Sized and sunk, not hung at his
            // feet, or the drawing starts at his waist and he walks about with his legs out of the fire.
            Standing(FlipbookFx.Book.Stand, 0f, 2.3f + Random.value * 0.4f, out float bodyH, out float bodyY);
            books.Add(FlipbookFx.Book.Stand, at + Vector3.up * bodyY, w * Cell, TorchEvery * 2.6f,
                      FlipbookFx.Kind.Anchored | FlipbookFx.Kind.Upright | (mirror ? FlipbookFx.Kind.Mirror : 0),
                      velocity: Vector3.up * 0.4f, height: bodyH, alpha: Coal, glow: glow, startFrame: Body());
            // and what tears off the top of him
            books.Add(FlipbookFx.Book.Fire, at + Vector3.up * (1.7f + Mathf.PerlinNoise(t.Seed + 3f, now * 8f) * 0.4f),
                      1.15f + Random.value * 0.4f, TorchEvery * 2.2f,
                      (!mirror ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None),
                      velocity: Vector3.up * 1.6f, grow: 0.7f, roll: Lean(), alpha: Coal * 0.95f, glow: glow, startFrame: Body());
            if (Random.value < 0.4f)
                books.Add(FlipbookFx.Book.Smoke, at + Vector3.up * (2.6f + Random.value * 0.6f), 3.0f + Random.value * 1.4f, 1.8f + Random.value,
                          velocity: Vector3.up * (1.8f + Random.value), grow: 1.2f, alpha: 0.16f * (0.4f + k), pop: 0.5f, startFrame: 3.2f + Random.value * 1.2f);
            if (Random.value < 0.5f) SceneHooks.Sparks?.Invoke(at + Vector3.up * 1.9f, 3);
        }

        void StepPyre(ref Pyre q, float now, FlipbookFx books, float glow)
        {
            if (!q.Stood) { q.At = Stand(q.At); q.Stood = true; }
            // it catches, it rages, it burns down. The catch is in SECONDS and the burning down is a fraction of the
            // life: a fire takes about the same second to take hold whether it has a minute in it or a quarter of an
            // hour, and reading the catch off the life meant a long fire spent its first minute invisible.
            float k = (now - q.Born) / q.Life;
            float ebb = Mathf.Clamp01((now - q.Born) / CatchSeconds) * (1f - Mathf.SmoothStep(0f, 1f, Mathf.Clamp01((k - 0.55f) / 0.45f)));
            if (now >= q.NextLight)
            {
                q.NextLight = now + FireLightEvery;
                float flick = 0.75f + Mathf.PerlinNoise(q.Seed, now * 5.5f) * 0.55f;
                SceneHooks.FireLight?.Invoke(q.At + Vector3.up * (q.Size * 0.55f + 0.7f), Firelight,
                                             FireLit * 3.0f * ebb * flick, 5f + q.Size * 1.1f, FireLightEvery * 1.15f, 0f);
            }
            if (now < q.Next) return;
            q.Next = (q.Next <= 0f ? now : q.Next) + Relight * (0.55f + Random.value * 0.4f);
            // a big fire is several fires: three tongues of different size and phase across its width, so it never
            // reads as one card scaled up, and one rolling ball riding the top of it where the heat is going
            int tongues = q.Size > 4f ? 4 : 3;   // one of them is the Pyre accent, so there is one more than there are flames
            // One of them leads. Three tongues of a size is a hedge, not a fire: a drawing of flame has a shape
            // hierarchy - one tall tongue carrying the silhouette and the others reading as its company - and without
            // it a big fire is a bush of identical licks. Which one leads walks along the fire about once a second,
            // slowly enough to read as the fire shifting its weight rather than as the cards being reshuffled.
            int hero = Mathf.Abs(Mathf.FloorToInt(now * 0.8f + q.Seed)) % tongues;
            for (int i = 0; i < tongues; i++)
            {
                float lead = i == hero ? 1.4f : 0.52f + Random.value * 0.12f;
                float across = (i + 0.5f) / tongues - 0.5f + (Random.value - 0.5f) * 0.10f;
                float h = q.Size * (0.60f + Random.value * 0.40f) * ebb * lead;
                // Stand is a flame drawn STANDING - a wide skirt at the bottom, tongues off the top - where Fire is a
                // ball of it rolling. A thing burning on the ground is the first shape, not the second.
                //
                // Every tongue is Stand now. The last one used to be Book.Pyre, kept as an accent for a different
                // silhouette, and that was a bad trade made before the sheets were measured: 36% of FireColumn's drawn
                // pixels are pure black, because it is a burst of SMOKE with a crescent of flame at its foot and not a
                // flame at all. Whatever the cel thresholds do with the rest, that third cannot be anything but dark,
                // and standing beside a lit fire it read as a big brown translucent slab leaning over it - taller than
                // the flame and the first thing the eye found. The smoke it was contributing is already drawn, properly
                // and cool, by Book.Smoke. Pyre survives as the pool's low tongue, where it is small and half-buried.
                var book = FlipbookFx.Book.Stand;
                Standing(book, 0f, h * Cell, out float tongueH, out float tongueY);
                books.Add(book,
                          q.At + new Vector3(across * q.Size * 1.15f, tongueY + 0.05f, (Random.value - 0.5f) * q.Size * 0.35f),
                          q.Size * (0.58f + Random.value * 0.26f) * Cell * Mathf.Lerp(0.82f, 1f, lead), Relight * Held,
                          // Handedness off the fire's OWN seed, not a coin. Two fires a few metres apart each flipped
                          // their own coins and landed the same way often enough to matter, and when they do the pair
                          // reads as one fire and a scaled copy of it rather than as two things burning.
                          FlipbookFx.Kind.Anchored | FlipbookFx.Kind.Upright
                              | (((i + Mathf.FloorToInt(q.Seed * 7f)) & 1) == 0 ? FlipbookFx.Kind.Mirror : 0),
                          velocity: Vector3.up * (0.5f + Random.value * 0.5f), height: tongueH,
                          // the hero leans across the others instead of standing parallel with them
                          roll: i == hero ? (Random.value < 0.5f ? -0.20f : 0.20f) : Lean(),
                          // the one that leads is the solid one; the others are thinner, so four of them stacked
                          // do not sum into a slab and the silhouette stays the lead tongue's
                          // and its phase off the seed too, so neighbours are never on the same cel of the loop
                          alpha: Coal * ebb * Mathf.Lerp(0.60f, 0.98f, lead), glow: glow,
                          startFrame: 4f + Mathf.Repeat(Random.value * 9f + q.Seed * 11f, 9f));
            }
            // The base of the fire is made of the SAME book as the rest of it. There was a Pool card here - the flat
            // puddle drawing - laid low and wide across the foot to break the ruled horizontal line the standing cards
            // made where their cells ended. It broke the line and did far more damage than the line ever did. Measured,
            // that sheet is the only fire book in the set whose ink runs off the side of its cell - a fifth of the left
            // and right edges are drawn right up to the boundary - so the card edge itself becomes a straight contour,
            // which is the one thing that cannot happen in a hand-drawn shape. Its silhouette is closed and near-convex
            // with round cream lobes punched in it, and the light shapes are broader than the dark webbing between
            // them, so the value inverts and it reads as cowhide rather than combustion. On a small fire it was most of
            // the drawing: the burning man stopped being recognisable as fire at all.
            //
            // It is an honest drawing of one thing only - a flat pool of burning fuel seen from above - and that is
            // where it stays (StepPool). A fire's skirt is the bottom of its own flame, so the line is broken here with
            // two more Stand cards instead: low, wide, splayed outward, sunk so their own bases are under the ground.
            for (int lick = 0; lick < 2; lick++)
            {
                float sw = q.Size * (0.70f + Random.value * 0.28f);
                Standing(FlipbookFx.Book.Stand, 0f, q.Size * (0.30f + Random.value * 0.12f), out float lickH, out float lickY);
                books.Add(FlipbookFx.Book.Stand,
                          q.At + new Vector3((lick == 0 ? -1f : 1f) * q.Size * (0.34f + Random.value * 0.18f),
                                             lickY - q.Size * 0.10f, (Random.value - 0.5f) * q.Size * 0.3f),
                          sw, Relight * Held,
                          FlipbookFx.Kind.Anchored | FlipbookFx.Kind.Upright | (lick == 0 ? FlipbookFx.Kind.Mirror : 0),
                          height: lickH, roll: (lick == 0 ? -0.34f : 0.34f) + Lean(),
                          alpha: Coal * 0.8f * ebb, glow: glow,
                          startFrame: 4f + Mathf.Repeat(Random.value * 9f + q.Seed * 13f + lick * 4f, 9f));
            }

            // low enough to overlap the tongue tips: held clear of them it read as a second, unrelated fire hanging
            // in the air above the first, because two closed silhouettes with background between them are two objects
            books.Add(FlipbookFx.Book.Fire, q.At + new Vector3((Random.value - 0.5f) * q.Size * 0.5f, q.Size * (0.55f + Random.value * 0.3f), (Random.value - 0.5f) * q.Size * 0.4f),
                      q.Size * (0.52f + Random.value * 0.24f),
                      Relight * Held, Random.value < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                      velocity: Vector3.up * (1.5f + Random.value), grow: 0.9f, roll: Lean(),
                      alpha: Coal * 0.7f * ebb, glow: glow, pop: 0.4f, startFrame: Body());
            // embers going up off it. A big fire that throws nothing is a picture of a fire; the specks leaving it are
            // what the eye reads as the thing being alive, and they cost almost nothing.
            if (Random.value < 0.55f)
                SceneHooks.Sparks?.Invoke(q.At + new Vector3((Random.value - 0.5f) * q.Size, q.Size * (0.6f + Random.value * 0.5f), (Random.value - 0.5f) * q.Size * 0.6f),
                                          2 + Mathf.RoundToInt(q.Size * ebb));
            // The column over it, which is how a big fire is read from across the field - and at night it is read from
            // nearby too, because a fire with no smoke over it is a light and not a fire. It is in two parts on
            // purpose. The low one is thick and sits inside the fire's own light, so it is the part that actually
            // READS: lit orange from underneath against the dark. The high one is thinner and wanders, and does its
            // work as a silhouette once it has climbed out of the light. One faint puff a second, which is what this
            // was, is invisible against a night sky - the smoke was there the whole time and simply never showed.
            //
            // Three times the size and a quarter of the opacity, which is the rule every smoke emitter here now follows.
            // The Smoke sheet's cells are drawn as soft DISCS: at any size that fits comfortably in shot you can count
            // the circles, and a stack of countable circles is a cartoon thought-bubble, not a plume. Bigger than the
            // frame there is no complete circle left to recognise. Faint, because at the opacity that made a disc
            // read, the smoke was taking the value centre of the picture off the fire - twice, in two different rounds,
            // in two different places. Smoke is the thing you see PAST. It is never the brightest or the biggest.
            if (Random.value < 0.85f)
                books.Add(FlipbookFx.Book.Smoke, q.At + Vector3.up * (q.Size * 0.95f) + new Vector3((Random.value - 0.5f) * q.Size * 0.4f, 0f, (Random.value - 0.5f) * q.Size * 0.3f),
                          q.Size * (1.80f + Random.value * 0.60f), 2.2f + Random.value,
                          velocity: Vector3.up * (1.9f + Random.value * 0.9f) + new Vector3(Random.value - 0.5f, 0f, Random.value - 0.5f) * 0.6f,
                          grow: 1.10f, alpha: 0.21f * ebb, pop: 0.40f, startFrame: 3.2f + Random.value * 1.2f);
            // the high one is silhouette, and silhouette is only worth paying for where it can be read: eight fires
            // going at once were putting up nearly two hundred smoke cards, and they are the largest quads drawn
            if (Random.value < 0.55f && CameraShake.DistanceToLook(q.At) < 70f)
                books.Add(FlipbookFx.Book.Smoke, q.At + Vector3.up * (q.Size * 1.9f), q.Size * (2.20f + Random.value * 0.80f), 3.4f + Random.value * 1.6f,
                          velocity: Vector3.up * (2.6f + Random.value * 1.3f) + new Vector3(Random.value - 0.5f, 0f, Random.value - 0.5f) * 1.4f,
                          grow: 1.30f, alpha: 0.16f * ebb, pop: 0.5f, startFrame: 3.2f + Random.value * 1.2f);
            if (Random.value < 0.5f) SceneHooks.Sparks?.Invoke(q.At + Vector3.up * (q.Size * 0.8f), 5);
        }
    }
}
