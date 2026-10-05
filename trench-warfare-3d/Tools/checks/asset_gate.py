import importlib.util

WHY = 'Every model FBX matches its line in Tools/assetgate.json and breaks no hard rule without a waiver.'


def run(ctx):
    """A split's mistakes are silent in Unity (a part outside the Hull is never drawn, a lower LOD torn by its
    decimation, a pivot that moves between LODs), so Tools/assetgate.py measures the files themselves. Here it only
    hashes them and re-measures the ones that changed: the numbers must arrive in the commit that changes the art."""
    spec = importlib.util.spec_from_file_location('tw_assetgate', ctx.root / 'Tools' / 'assetgate.py')
    gate = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(gate)
    return ['assetgate: ' + e for e in gate.check(ctx.root)]
