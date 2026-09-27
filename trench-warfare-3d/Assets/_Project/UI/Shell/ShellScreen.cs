// Phase: B6 (implemented) — one screen of the shell: a UXML picked from ShellAssets, bound to a router.
// Plain classes, not components, so an EditMode test can instantiate the UXML and Bind with a null router.
using UnityEngine.UIElements;

namespace TW.UI
{
    public abstract class ShellScreen
    {
        public VisualElement Root { get; private set; }
        protected ShellRouter Router { get; private set; }
        /// <summary>Gameplay input stops while this screen is up (the pause menu, settings, the debrief).</summary>
        public virtual bool Modal => true;
        /// <summary>The HUD hides behind this screen (the debrief, the main menu).</summary>
        public virtual bool HidesHud => false;

        public abstract VisualTreeAsset Tree(ShellAssets assets);

        public void Bind(VisualElement root, ShellRouter router)
        {
            Root = root; Router = router;
            OnBind();
            GameLogo.Fill(root);   // the logo wherever the screen marks a place for it
            root.Query<Button>().ForEach(b => b.focusable = false);
        }

        public void Unbind() { OnUnbind(); Root = null; Router = null; }

        protected abstract void OnBind();
        protected virtual void OnUnbind() { }
        public virtual void Tick() { }
        public virtual void OnEscape() => Router?.Pop();
        public virtual void OnCovered() { }
        public virtual void OnUncovered() { }

        protected Button Btn(string name, System.Action onClick)
        {
            var b = Root.Q<Button>(name);
            if (b != null && onClick != null) b.clicked += onClick;
            return b;
        }

        protected void SetText(string name, string text) { var l = Root.Q<Label>(name); if (l != null) l.text = text; }
    }
}
