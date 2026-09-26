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
            root.Query<Button>().ForEach(b => b.focusable = false);
        }

        public void Unbind() { OnUnbind(); Root = null; Router = null; }

        protected abstract void OnBind();
        protected virtual void OnUnbind() { }
        public virtual void Tick() { }
        public virtual void OnEscape() => Router?.Pop();
        public virtual void OnCovered() { }
        public virtual void OnUncovered() { }

        /// <summary>Destroy a Unity object a screen made (a thumbnail): Destroy in play, DestroyImmediate in the
        /// editor's EditMode tests, where Destroy logs an error.</summary>
        protected static void Discard(UnityEngine.Object o)
        {
            if (o == null) return;
            if (UnityEngine.Application.isPlaying) UnityEngine.Object.Destroy(o); else UnityEngine.Object.DestroyImmediate(o);
        }

        /// <summary>Go to a screen that links back here (the Home Front and the map): when it is already right under
        /// this one, pop back to it instead of stacking another copy.</summary>
        protected void SwapTo<T>(System.Func<T> make) where T : ShellScreen
        {
            if (Router == null) return;
            if (Router.Under is T) Router.Pop(); else Router.Push(make());
        }

        protected Button Btn(string name, System.Action onClick)
        {
            var b = Root.Q<Button>(name);
            if (b != null && onClick != null) b.clicked += onClick;
            return b;
        }

        protected void SetText(string name, string text) { var l = Root.Q<Label>(name); if (l != null) l.text = text; }
    }
}
