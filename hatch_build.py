"""Hatchling build hook that stages the shared JSON data for editable installs.

A regular wheel gets ``hydroloom/data/*.json`` from ``force-include``. An
editable install imports ``hydroloom`` from ``src/hydroloom/``, so files
force-included into site-packages are never on the package path and
``importlib.resources.files("hydroloom.data")`` cannot find them. For editable
builds this hook copies each ``force-include`` target under ``hydroloom/data/``
into ``src/hydroloom/data/`` (gitignored). ``pixi run install`` reinstalls
editable, which reruns the copy after a JSON edit.
"""

import shutil
from pathlib import Path

from hatchling.builders.hooks.plugin.interface import BuildHookInterface


class CustomBuildHook(BuildHookInterface):
    """Copy force-included data files into the source tree for editable builds."""

    def initialize(self, version, build_data):
        """Stage the data files when building an editable wheel."""
        if self.target_name != "wheel" or version != "editable":
            return

        src_root = Path(self.root) / "src"
        for source, target in self.build_config.get_force_include().items():
            if Path(target).parts[:2] != ("hydroloom", "data"):
                continue
            dest = src_root / target
            dest.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, dest)
