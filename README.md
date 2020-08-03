# master-classification
Repository for the research work on land cover fraction mapping with PROBA-V.

All code is under the GPLv3+ license, all included data and materials are under the CC-BY 4.0 license unless stated otherwise.

Repository structure is:

- `master` branch contains all text and small binaries of the research:
  - `src` contains the source code used to carry out the research. The scripts assume that they are run in the PROBA-V MEP/Terrascope platform.
    - `raster-based` is for raster-based processing of data done as part of the Master thesis at WUR. See `main.r` file for the explanation of how files are organised.
    - `pixel-based` contains the source code used to do pixel-based/point-based processing for the 2020 paper.
  - `data` contains some of the data used to carry out the research.
- `presentations` branch contains minified midterm and final presentation files.
- `thesis` branch contains the Master thesis proposal and thesis text in LaTeX format (not compiled).

More information on structure is in `readme.txt` or `readme.md` files in the respective directories.
