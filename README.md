
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IRtreesInORM

This is the research compendium that accompanies the paper **Developing
and Applying IR-Tree Models: Guidelines, Caveats, and an Extension to
Multiple Groups**. Herein, documentation, data, and code is provided for
both the simulation study and the empirical example. This compendium can
be obtained from GitHub or OSF. Rerunning the analyses, especially those
done in Mplus, should work out of the box. However, some code may work
only after installing the repository/package, and this can be done
using, for example, `devtools::install()`, `devtools::load_all()`, or
`remotes::install_github()`.

## Empirical Example

In the empirical example, data from the International Social Survey
Programme were used, namely, from ISSP 2015 – Work Orientations IV (ISSP
Research Group, 2017). The data are not part of this compendium in order
to prevent copyright issues. To do further analyses with these data, the
following steps are needed:

  - Please download the SPSS (\*.sav) data file version 2.1.0 from doi:
    [10.4232/1.12848](http://dx.doi.org/10.4232/1.12848) and save it as
    `data-raw/issp15/ZA6770_v2-1-0.sav`.
  - Run the two R scripts in `data_raw/issp15/`.
  - Further documentation is provided in the README file in
    `code/issp15/`.

## Simulation Study

The data were simulated using the R script
`code/simulation/01_generate-data.R`. A graphical overview of the
generated items and their parameters can also be found in
`figures/sim-study_items.pdf`. Further details can be found in the
README file in `code/simulation/`

## Mplus Code

Mplus code for single-group models as used in the simulation study can
be found in `code/simulation/mplus/`. Mplus code for multiple-group
models as used in the empirical example can be found in
`code/issp15/mplus/`.
