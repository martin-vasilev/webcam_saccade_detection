"""Run with bundled PDF Python after rendering and visually reviewing all figures."""
from pathlib import Path
import json
from PIL import Image
from pypdf import PdfReader
root=Path(__file__).resolve().parents[2];out=root/'output/part3';records=[]
for p in sorted((out/'figures').glob('*.pdf')):
 r=PdfReader(p);assert len(r.pages)==1 and len(r.pages[0].images)==0
 assert len(r.pages[0].extract_text())>100
 with Image.open(p.with_suffix('.png')) as im:
  dpi=im.info.get('dpi');assert dpi and all(abs(d-300)<1 for d in dpi)
  records.append(dict(figure=p.stem,vector_pdf=True,pages=1,png_pixels=list(im.size),dpi=list(dpi)))
assert len(records)==12
(out/'validation/export_checks.json').write_text(json.dumps(dict(passed=True,figures=records),indent=2))
print('Twelve vector PDFs and corresponding 300-dpi PNGs PASS.')
