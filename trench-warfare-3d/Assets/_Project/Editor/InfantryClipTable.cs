// Phase: C1 (implemented) — which Mixamo (or made) file each controller Clip is baked from, and how. One entry per Clip;
// a Clip without one is baked from ProceduralSoldier.Sample of its fallback row, so the atlas always has every row.
// The files live in Art/Characters/Clips (the owner's Mixamo download of 2026-09-22 plus Tools/make_missing_clips.py;
// the manifest with every measurement is docs/reference/animation-clips.md). Loops sample n frames over the cycle
// (the last returns to the first); one-shots include their last frame and hold it. Cuts are seconds into the file;
// Rate 2 plays a file twice as fast (the bolt); StripYaw removes a turn the file carries that the controller does not
// want (the aim clips carry 26 to 40 degrees). Root travel is stripped as a linear trend on every clip (the sim moves
// the man, and a one-shot has to end where the next clip starts) except the deaths, which end in the fallen buffer
// where a man may fall a metre from where he stood.
using System.Collections.Generic;
using TW.Presentation;

namespace TW.Editor
{
    public struct ClipSource
    {
        public Clip Clip; public string File; public bool Loop; public int Fps; public float CutStart, CutEnd, Rate; public bool StripYaw, KeepRoot;
    }

    public static class InfantryClipTable
    {
        public const string Folder = "Assets/_Project/Art/Characters/Clips";

        static readonly List<ClipSource> list = new List<ClipSource>();
        public static IReadOnlyList<ClipSource> Entries { get { if (list.Count == 0) Build(); return list; } }

        static void L(Clip c, string file, int fps = 15, float cutStart = 0f, float cutEnd = 0f, float rate = 1f) => list.Add(new ClipSource { Clip = c, File = file, Loop = true, Fps = fps, CutStart = cutStart, CutEnd = cutEnd, Rate = rate });
        static void O(Clip c, string file, int fps = 15, float cutStart = 0f, float cutEnd = 0f, float rate = 1f, bool stripYaw = false) => list.Add(new ClipSource { Clip = c, File = file, Loop = false, Fps = fps, CutStart = cutStart, CutEnd = cutEnd, Rate = rate, StripYaw = stripYaw });
        static void D(Clip c, string file, int fps = 12, bool keepRoot = true) => list.Add(new ClipSource { Clip = c, File = file, Loop = false, Fps = fps, Rate = 1f, KeepRoot = keepRoot });

        static void Build()
        {
            // idle
            L(Clip.Idle, "Rifle Idle"); L(Clip.AimedIdle, "Rifle Aiming Idle"); L(Clip.ReadyIdle, "Rifle Idle (2)");
            L(Clip.KneelIdle, "Rifle Kneel Idle"); O(Clip.KneelAimedIdle, "Rifle Kneel To Aim", 15, 0.72f, 0.8f); L(Clip.ProneIdle, "Prone Idle"); L(Clip.StoopIdle, "Crouch Idle");
            O(Clip.FidgetLookAround, "Rifle Idle (3)", 12, 0f, 5.3f); O(Clip.FidgetRubEyes, "Rifle Rubbing Eyes"); O(Clip.FidgetCheckShoe, "Check Shoe", 12); O(Clip.FidgetCollar, "Rifle Idle (4)", 12); O(Clip.FidgetInspect, "Inspecting");
            // locomotion
            L(Clip.Walk, "Rifle Walk"); L(Clip.WalkAimed, "Walk With Rifle"); L(Clip.WalkWary, "Walk With Rifle (1)"); L(Clip.WalkBack, "Backwards Rifle Walk");
            L(Clip.WalkLeft, "Walk Left"); L(Clip.WalkRight, "Walk Right"); L(Clip.Run, "Rifle Run (1)", 20); L(Clip.RunBack, "Backwards Rifle Run", 20); L(Clip.Sprint, "Sprint Forward", 24);
            L(Clip.StoopWalk, "Rifle Crouch Walk"); L(Clip.StoopLow, "Walk Crouching Forward"); L(Clip.CrouchRun, "Crouched Run (1)", 20);
            L(Clip.Crawl, "Prone Crawl Forward"); L(Clip.CrawlBack, "Moving Backward In Prone Position"); L(Clip.Wade, "Wade Forward"); L(Clip.SideStep, "Rifle Side Step");
            L(Clip.MGCarry, "MG Carry Walk"); L(Clip.WireCross, "Wire Crossing");
            L(Clip.FireWalk, "Firing Rifle (2)"); L(Clip.FireRun, "Firing Rifle (3)", 20); L(Clip.FireSprint, "Firing Rifle (5)", 24); L(Clip.FireStoop, "Firing Rifle (4)");
            // fire
            O(Clip.FireStand, "Firing Rifle (1)", 24); O(Clip.FireSnap, "Firing Rifle", 30); O(Clip.FireKneel, "Fire Rifle", 24); O(Clip.FireProne, "Prone Firing Rifle", 24); L(Clip.FireMG, "Prone Firing Rifle (1)", 30);
            O(Clip.AimUp, "Rifle Down To Aim", 24, 0f, 0f, 1f, true); O(Clip.AimDown, "Rifle Aim To Down", 15, 0f, 0f, 1f, true); O(Clip.KneelAimUp, "Rifle Kneel To Aim", 24, 0f, 0f, 1f, true); O(Clip.KneelAimDown, "Rifle Aim To Kneel", 24, 0f, 0f, 1f, true);
            // actions
            O(Clip.ReloadStand, "Reloading", 12); O(Clip.ReloadStoop, "Reload", 12); O(Clip.ReloadProne, "Prone Reloading", 10); O(Clip.ReloadBolt, "Reloading", 20, 2.3f, 3.3f, 1.25f);
            O(Clip.Throw, "Toss Grenade", 12); O(Clip.MeleeStab, "Bayonet Stab", 15); O(Clip.MeleePunch, "Rifle Punch", 15); O(Clip.MeleeSmash, "Smash", 15); O(Clip.MeleeBlock, "Block With Rifle", 15);
            O(Clip.Sling, "Rifle Put Away", 12); O(Clip.Unsling, "Rifle Pull Out", 12);
            // reactions
            O(Clip.HitStand, "Hit Reaction (1)", 15); O(Clip.HitHeavy, "Hit Reaction", 15); O(Clip.HitWalk, "Walking Hit Reaction", 15); O(Clip.HitRun, "Hit Reaction (2)", 24); O(Clip.HitProne, "Rifle Prone Hit Reaction", 15);
            O(Clip.KneelFlinch, "Kneel Flinch", 24); O(Clip.ProneFlinch, "Prone Flinch", 24); O(Clip.Duck, "Dodging", 15); O(Clip.Shield, "Rifle Shielding Face", 12);
            O(Clip.DiveRoll, "Dive Roll", 15); O(Clip.ProneRoll, "Prone Roll", 15); O(Clip.Trip, "Fall Over", 15); O(Clip.GetUp, "Get Up From Prone", 15);
            O(Clip.MaskOn, "Mask Donning", 12); L(Clip.Burning, "Burning Run", 15); O(Clip.Stumble, "Stumble Running", 15);
            // trench and stance
            O(Clip.JumpDown, "Jumping Down", 15, 0f, 1.8f); O(Clip.ClimbOut, "Jump Up", 24); L(Clip.ClimbHold, "Jump Loop", 12); O(Clip.ClimbLand, "Jump Down", 24); L(Clip.ClimbLadder, "Ladder Climb");
            O(Clip.StandToKneel, "Rifle Stand To Kneel", 20); O(Clip.KneelToStand, "Rifle Kneel To Stand", 20); O(Clip.KneelToProne, "Rifle Kneel To Prone", 15); O(Clip.ProneToKneel, "Rifle Prone To Kneel", 15);
            O(Clip.StandToStoop, "Rifle Idle To Crouch", 15, 0f, 1.4f); O(Clip.StoopToStand, "Rifle Crouch Walk To Idle", 15, 1.4f, 2.93f); O(Clip.StoopToKneel, "Rifle Crouch Walk To Kneel", 15, 1.2f, 2.73f); O(Clip.KneelToStoop, "Rifle Crouch Idle To Walk", 15, 0f, 1.5f);
            O(Clip.TakeCover, "Taking Cover", 15); O(Clip.Emerge, "Emerging", 15);
            O(Clip.Turn90L, "Rifle Turn (4)", 15); O(Clip.Turn90R, "Rifle Turn (6)", 15); O(Clip.Turn180, "Rifle Turn (3)", 15);
            O(Clip.KneelTurn90L, "Crouching Turn 90 Left", 15); O(Clip.KneelTurn90R, "Crouching Turn 90 Right", 15);
            O(Clip.StoopTurn90L, "Rifle Crouch Turn (1)", 15); O(Clip.StoopTurn90R, "Rifle Crouch Turn (7)", 15); O(Clip.StoopTurn180, "Rifle Crouch Turn", 15);
            // deaths
            D(Clip.DeathFront, "Death From The Front"); D(Clip.DeathBack, "Death From The Back"); D(Clip.DeathRight, "Death From Right"); D(Clip.DeathLeft, "Rifle Death");
            D(Clip.DeathHeadshot, "Death From Front Headshot"); D(Clip.DeathWalking, "Walking To Dying", 12, false); D(Clip.DeathRunning, "Rifle Run To Dying", 12, false);
            D(Clip.DeathKneel, "Rifle Kneel Hit To Back"); D(Clip.DeathSquat, "Death Crouching Headshot Front"); D(Clip.DeathProne, "Prone Death"); D(Clip.DeathBlast, "Rifle Hit To Back", 15);
        }
    }
}
