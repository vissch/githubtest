"""Compare tw-perf/1 reports side by side. Usage: python cmp.py A.json B.json [C.json ...]"""
import json, sys

runs = [json.load(open(p)) for p in sys.argv[1:]]
labels = [r['run']['label'] for r in runs]
print('%-26s' % '', *['%24s' % l[:24] for l in labels])
def row(name, get):
    vals = []
    for r in runs:
        try: vals.append(get(r))
        except Exception: vals.append(None)
    print('%-26s' % name, *['%24s' % ('-' if v is None else v) for v in vals])
row('hash_start', lambda r: r['window']['hash_start'])
row('ticks', lambda r: '%d..%d' % (r['window']['tick_start'], r['window']['tick_end']))
row('alive start/end', lambda r: '%d/%d' % (r['window']['alive_start'], r['window']['alive_end']))
row('fps mean', lambda r: '%.1f' % r['window']['fps_mean'])
row('gc collections', lambda r: r['window']['gc_collections'])
row('hitches >33ms (cap 64)', lambda r: len(r['window']['hitches_over_33ms']))
for k in ['cpu_frame_ms', 'main_ms', 'main_ms_tick_frames', 'main_ms_idle_frames', 'render_ms', 'gpu_ms', 'draw_calls', 'setpass', 'gc_bytes', 'gc_count', 'vat_vertices', 'vat_shadows_on']:
    row(k + ' p50/p95/p99', lambda r, k=k: '%s/%s/%s' % tuple(('%.4g' % r['series'][k][q]) for q in ('p50', 'p95', 'p99')))
keys = sorted({k for r in runs for k in r['per_tick_ms']}, key=lambda k: -max(r['per_tick_ms'].get(k, 0) for r in runs))
print('per tick ms (all worlds):')
for k in keys:
    if max(r['per_tick_ms'].get(k, 0) for r in runs) < 0.05: continue
    row('  ' + k.replace('TW.Sim.Sys.', 'Sys.'), lambda r, k=k: '%.3f' % r['per_tick_ms'].get(k, float('nan')))
scripts = sorted({k for r in runs for k in r['series'] if k.startswith('script:')},
                 key=lambda k: -max((r['series'].get(k) or {}).get('mean', 0) for r in runs))
if scripts:
    print('per-frame script cost, mean ms (p95):')
    for k in scripts[:25]:
        row('  ' + k[7:][:40], lambda r, k=k: '-' if not r['series'].get(k) else '%.3f (%.2f)' % (r['series'][k]['mean'], r['series'][k]['p95']))
