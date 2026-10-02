import re

WHY = 'The project-wide audio volume stays at 1: mute belongs in settings.json, never in the project asset.'


def run(ctx):
    """The game is muted through GameSettings -> AudioListener.volume at runtime; writing 0 into the PROJECT setting
    instead would mute every session and every shipped build for a reason that appears nowhere in C#, and five
    sessions share this editor. Unity can persist AudioListener.volume into this asset, so it is checked rather than
    trusted."""
    audio_asset = ctx.root / 'ProjectSettings/AudioManager.asset'
    if audio_asset.exists():
        m = re.search(r'^\s*m_Volume:\s*([0-9.]+)', audio_asset.read_text(encoding='utf-8', errors='replace'), re.M)
        if m and abs(float(m.group(1)) - 1.0) > 1e-6:
            return [f'ProjectSettings/AudioManager.asset: m_Volume is {m.group(1)}, not 1. '
                    'Mute belongs in settings.json (GameSettings.Audio.Master), never in the project asset: '
                    'this silences every session and every build. Set it back to 1.']
    return []
