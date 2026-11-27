"""Part 3 verifier entry; tested full implementation and guarded cache are separate."""
from pathlib import Path
import runpy
runpy.run_path(str(Path(__file__).with_name("verify_cached.py")), run_name="__main__")
