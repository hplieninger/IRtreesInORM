# Empirical Example

## Data

In the empirical example, data from the International Social Survey Programme were used, namely, from ISSP 2015 -- Work Orientations IV (ISSP Research Group, 2017).
The data are not part of this compendium in order to prevent copyright issues.
To do further analyses with these data, the following steps are needed:

- Please download the SPSS (*.sav) data file version 2.1.0 from doi: [10.4232/1.12848](http://dx.doi.org/10.4232/1.12848) and save it as `data-raw/issp15/ZA6770_v2-1-0.sav`.
- Run the two R scripts in `data_raw/issp15/`.

Then, the data should be save in `data/` and can be used for analyses via `data("issp15")`.

## IR-Tree Models

- In the file `01_irtree-model.R`, the data are prepared etc. and the relevant files are exported to the folder `mplus/`.
- Then, alle input files in `mplus/` were run using Mplus 7.4.
- Finally, results were analyzed using the script `02_irtree-res.R`.
  Therein, the plots provided in the paper are produced (see also `figures/`).
  These results can be obtained by running the R script, and they are also saved in `02_irtree-res.html` for convenience.
- Descriptive statistics were obtained using `11_descriptives.R`
  For example, the plot of the item response distribution across countries was produced (see also `figures/`).
