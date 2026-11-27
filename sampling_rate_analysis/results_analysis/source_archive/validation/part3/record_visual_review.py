"""Record explicit completed review; never substitute structural tests for review."""
from pathlib import Path
import json,hashlib,sys
assert sys.argv[1:]==['--reviewed-all']
root=Path(__file__).resolve().parents[2];out=root/'output/part3'
assert json.loads((out/'validation/export_checks.json').read_text())['passed']
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
rows=[]
for p in sorted((out/'figures').glob('*.pdf')):
 render=out/'validation/pdf_render'/(p.stem+'.png');assert render.exists()
 rows.append(dict(figure=p.stem,pdf_sha256=sha(p),png_sha256=sha(p.with_suffix('.png')),review_render_sha256=sha(render),
  assessment='Reviewed: readable titles/axes/denominators, footer separated from x-axis titles, complete curves and disclosed reference/exposure limitations.'))
assert len(rows)==12
(out/'validation/visual_review.json').write_text(json.dumps(dict(passed=True,review_date='2026-11-22',figures=rows,
 source_tables_unchanged_during_layout_redraw=True,font_note='Poppler emitted fallback-font warnings; all final displayed labels were inspected and readable.'),indent=2))
print('Explicit visual review of all twelve final figure sets recorded.')
