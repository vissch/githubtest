// Phase: maintenance (2026-09-27) — which statics a scene load puts back, and which only the end of a Play session does.
// HudBootstrap builds the HUD in its own sceneLoaded handler, and ShellRouter calls SceneStatics.Reset from another;
// Unity does not promise their order. If Reset cleared the HUD's pointer mask, a HUD built first would lose it, and
// the camera, the debug panel and selection would treat clicks on the HUD as clicks on the field.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;

namespace TW.Tests
{
    public class SceneStaticsTests
    {
        [TearDown]
        public void Clean() => SceneStatics.ResetSession();

        [Test]
        public void A_Scene_Load_Keeps_What_The_New_Scenes_Hud_Registered()
        {
            System.Func<Vector2, bool> overHud = _ => true;
            System.Func<bool> wheel = () => true;
            HudBridge.PointerOverUi = overHud;
            HudBridge.WheelClaimed = wheel;
            SceneStatics.Reset();
            Assert.AreSame(overHud, HudBridge.PointerOverUi, "a scene load must not clear the HUD's pointer mask");
            Assert.AreSame(wheel, HudBridge.WheelClaimed, "a scene load must not clear the wheel claim");
        }

        // What Reset() DOES put back had no test at all: only the three statics nobody else owns are its
        // job, and a Reset that quietly stopped doing one of them (a debug bombardment left armed, the lightning's
        // time scale held, a modal keeping the keyboard) would have read as green here.
        [Test]
        public void A_Scene_Load_Puts_Back_The_Statics_Nobody_Owns()
        {
            SimHost.BombardmentOverride = 5f;
            Time.timeScale = 0.25f;
            InputFocus.Modal = true;
            InputFocus.Listening = true;
            SceneStatics.Reset();
            Assert.AreEqual(-1f, SimHost.BombardmentOverride, "a scene load disarms the debug bombardment");
            Assert.AreEqual(1f, Time.timeScale, "a scene load gives the time scale back, whatever held it");
            Assert.IsFalse(InputFocus.Modal, "a scene load closes the shell's modal");
            Assert.IsFalse(InputFocus.Listening, "a scene load stops the key-binding listen");
        }

        // Reset() runs the per-scene registry at its end (SceneStatics.cs:44), the line that keeps a destroyed
        // scene's cached view from being handed out in the next scene. Only a PlayMode test pinned it; a Reset that
        // stopped running the registry read as green in EditMode.
        [Test]
        public void A_Scene_Load_Runs_Every_Per_Scene_Registration()
        {
            int ran = 0;
            // The registration lives for the rest of the editor session (there is no unregister): it touches
            // nothing but this counter.
            SceneStatics.RegisterPerScene("SceneStaticsTests", () => ran++);
            SceneStatics.Reset();
            Assert.AreEqual(1, ran, "a scene load runs every per-scene registration");
        }

        [Test]
        public void The_End_Of_A_Session_Clears_Them()
        {
            HudBridge.PointerOverUi = _ => true;
            HudBridge.WheelClaimed = () => true;
            SceneStatics.ResetSession();
            Assert.IsNull(HudBridge.PointerOverUi);
            Assert.IsNull(HudBridge.WheelClaimed);
        }
    }
}
