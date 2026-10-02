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
