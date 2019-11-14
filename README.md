
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IRtreesInORM

This is the research compendium that accompanies the paper **Developing
and Applying IR-Tree Models: Guidelines, Caveats, and an Extension to
Multiple Groups**. Herein, documentation and code is provided for the
empirical example. Furthermore, a simulation study is reported. This
compendium can be obtained from GitHub or OSF.

This compendium in the form of an R package is avaible on GitHub and
OSF:

  - You can either access the files online, or you can download a copy
    of the compendium, for example, via
    `usethis::create_from_github("hplieninger/IRtreesInORM")`.
  - You may also install the R package to use the functions stored in
    `R/`.
      - You can install your local copy via `devtools::load_all()` or
        `devtools::install()`.
      - You can install directly from GitHub via
        `remotes::install_github("hplieninger/IRtreesInORM")`.

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

A simulation study was conducted to demonstrate that improper models
(i.e., using incorrect pseudoitems) can lead to serious consequences and
invalid conclusions. The data were simulated using the R script
`code/simulation/01_generate-data.R`. A graphical overview of the
generated items and their parameters can also be found in
`figures/sim-study_items.pdf`. Further details can be found in the
README file in `code/simulation/`. The results are reported in a
vignette stored in `doc/IRTreesInORM-Improper-Models.html`.

## Mplus Code

Mplus code for single-group models as used in the simulation study can
be found in `code/simulation/mplus/`. Mplus code for multiple-group
models as used in the empirical example can be found in
`code/issp15/mplus/`.
