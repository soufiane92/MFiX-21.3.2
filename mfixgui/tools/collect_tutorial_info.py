"""This file is will collect the .mfixguiinfo files data and save the info
to a single file for the gui. To use, run:

> make thumbnails

from the mfixgui directory"""

import json
import os
from mfixgui.tools import SCRIPT_DIRECTORY
from mfixgui.project import Project


def guess_solver(p):
    """ Attempt to derive solver type, after reading mfix file"""
    mmax = p.get_value("mmax", default=0)
    if mmax == 0:
        return "single"
    solids_models = set(
        p.get_value("solids_model", args=n, default="TFM").upper()
        for n in range(1, mmax + 1)
    )
    if solids_models == set(["TFM"]):
        return "tfm"
    if solids_models == set(["DEM"]):
        return "dem"
    if solids_models == set(["CGP"]):
        return "cgp"
    if solids_models == set(["PIC"]):
        return "pic"
    if solids_models == set(["TFM", "DEM"]):
        return "hybrid"
    return "single"


def make_info_dict():
    info_dict = {}
    for path, _, files in os.walk("."):
        for f in files:
            if f.endswith(".mfx") or f == "mfix.dat":
                name = os.path.basename(path)
                add_path_to_info_dict(info_dict, os.path.join(path, f))

    return info_dict


def add_path_to_info_dict(info_dict, path):
    if "build" in path:
        # make sure we don't look in the build dir
        return
    project = Project()
    project.parsemfixdat(path)
    print(path)
    d = info_dict[os.path.dirname(path).lower().replace('./','')] = {}
    d["solver"] = guess_solver(project)
    d["cutcell"] = project.get_value("cartesian_grid", default=False)
    d["chemistry"] = bool(project.reactions)
    d["description"] = project.get_value("description", default="")


def get_template_info():
    info = {}
    filename = os.path.join(SCRIPT_DIRECTORY, "tools", "template_data.json")
    if os.path.exists(filename):
        with open(filename, encoding="utf-8", errors="replace") as f:
            info = json.load(f)
    return info


def main():
    info_dict = make_info_dict()
    print("Found {} project files".format(len(info_dict)))
    with open(os.path.join(SCRIPT_DIRECTORY, "tools/template_data.json"), "w", encoding="utf-8") as f:
        json.dump(info_dict, f, sort_keys=True, indent=2)


if __name__ == "__main__":
    main()
