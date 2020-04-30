Improper Models
================
Hansjörg Plieninger
2020-04-30

  - [Improper Models](#improper-models)
  - [Simulation Study](#simulation-study)
      - [Method](#method)
      - [Results](#results)
  - [Discussion](#discussion)
  - [References](#references)

This vignette contains a small simulation study, which was removed from
the final version of the paper by Plieninger (2020). Herein, it is
demonstrated that incorrect pseudo-items can lead to serious
consequences and invalid conclusions. The results match and explain
published results (LaHuis et al., 2019a, 2019b).

## Improper Models

<p>

The issue concerns parameter estimation on the basis of pseudo-items. As
described in the paper, an IR-tree model may be estimated in the form of
a multidimensional, binary IRT model after the original items have been
recoded into binary pseudo-items. This might lead to the misconception
that the pseudo-items can be defined independent of each other. For
example, a researcher may want to define the third pseudo-item
x\*<sub>m</sub> as (0, 0, 1, 0, 0) instead of (-, 0, 1, 0, -) in the MCN
model in order to reduce the number of missings. Or, one might want to
define the second pseudo-item x\*<sub>e</sub> as (1, 0, -, 0, 1) instead
of (1, 0, 0, 0, 1) for some reason. However, this is not legitimate
within the IR-tree framework: This leads to a model (as implied by the
pseudo-items) that no longer corresponds to the specified model
equations and tree diagram. Worse still, such changes can easily lead to
an improper model in the sense that the model equations do not sum to 1
and that no tree diagram exists that could illustrate its structure.
Figuratively speaking, such an improper model may make predictions such
as 50 % heads and 60 % tails. Strictly speaking, the equations implied
by the pseudo-items do not even comprise a probabilistic model, because
the definition of a discrete probability distribution requires that the
sum of the individual probabilities is 1.

</p>

Note that the estimation of an improper model may nevertheless converge
without problems, because the software “doesn’t know” that an IR-tree
model for polytomous data is specified rather than a multidimensional
IRT model with three seemingly independent, binary items.

Using an incorrect pseudo-item (i.e., model) will affect the empirical
results. However, it is not directly clear which outcomes will be
affected and to what extent. Therefore, a small simulation study was
conducted.

## Simulation Study

The aim of the simulation was very specific, namely, to investigate the
effects of a misspecified pseudo-item under known conditions. Generating
a very large sample allowed to analyze the data almost as if it were
population data without much uncertainty due to sampling error.
Furthermore, the data-generating model was kept as simple as possible
(e.g., by using only one instead of multiple target traits) in order to
isolate the most important aspects.

### Method

<p>

A data set comprised of 10,000 persons and 27 items was generated from a
population where the MCN model was the true model. Therein, the
variances of the latent dimensions were set to 1.00 (for *t*), 0.75 (for
*e*), and 0.50 (for *m*). Furthermore, the correlation was -.50 between
MRS and ERS, -.20 between target trait and MRS, and .20 between target
trait and ERS. With respect to the item parameters, a low, an
intermediate, and a high value was chosen for each of β<sub>m</sub> ,
β<sub>e</sub>, and β<sub>t</sub>. These were then fully crossed leading
to 3^3=27 combinations, one for each item. The chosen values match
estimates typically found in the IR-tree literature (e.g., Böckenholt,
2012; Plieninger & Meiser, 2014), and the code, the simulated data, and
further details can be found in the `/code/` folder of this repository.

</p>

Three IR-tree models were then fit to the generated data. First, the MCN
model should lead to the correct estimates and best model fit, since it
is the true, data-generating model. Second, the ECN model was used,
which should lead to inferior fit and estimates, since it is not the
correct model in this case. Third, the correct pseudo-item
x\*<sub>e</sub> (1, 0, 0, 0, 1) of the MCN model was replaced with an
incorrect pseudo-item (1, 0, -, 0, 1). This model will be called *MCN2*,
and it is an improper model in the sense described above. This MCN2
model was used by LaHuis et al. (2019a), and the present simulation
study will allow to examine the artifacts introduced by the incorrect
pseudo-item. The three models were fit using Mplus 7.4 (Muthén & Muthén,
2012).

### Results

``` r
# Import Mplus results

data("sim_res", package = "IRtreesInORM")

# Load true, data-generating parameters

data("sim_data", package = "IRtreesInORM")
thetas_true <- as.data.frame(sim_data$theta)
rm(sim_data)
```

#### Model Fit

As shown in the table below, the MCN model fit the data better than the
ECN model according to AIC and BIC, and this was expected given that it
was the true model. Importantly, the MCN2 model showed much, much
smaller values of AIC and BIC than the MCN model. This is an artifact,
because the MCN2 model is not a proper model, and it could not recover
the true parameters as good as the fit indices spuriously imply. Thus,
it is expected that the pseudo-items used by LaHuis et al. (2019a)
artificially increased the fit of the MCN2 model in their data.

<p>

In more detail, the (log-) likelihood of an IR-tree model is based on
Equation (5) and (6) (in Plieninger, 2020), and thus contains products
(sums) of the model parameters ξ or 1-ξ. However, in the MCN2 model, one
such term is removed from the likelihood of the MCN model, namely the
term 1-e<sub>ij</sub> for Category 3 (see Equation 9). Thus, the
deviance of the MCN2 model is necessarily smaller compared to the MCN
model artificially implying better fit. The amount of bias is a function
of the number of responses in Category 3 and of the size of
e<sub>ij</sub>, and bias would be 0 in the unlikely case of no midpoint
responses and/or all e<sub>ij</sub>=0.

</p>

``` r
sim_res %>%
    mutate(AIC = map_dbl(output, list("summaries", "AIC")),
           BIC = map_dbl(output, list("summaries", "BIC")),
           LL  = map_dbl(output, list("summaries", "LL")),
           par = map_dbl(output, list("summaries", "Parameters"))) %>% 
    mutate_at(vars(AIC:LL), round, -2) %>% 
    select(-output) %>% 
    knitr::kable(format = "markdown", format.args = list(big.mark = ','))
```

| Model |     AIC |     BIC |        LL | par |
| :---- | ------: | ------: | --------: | --: |
| MCN   | 701,100 | 701,700 | \-350,400 |  87 |
| MCN2  | 649,100 | 649,800 | \-324,500 |  87 |
| ECN   | 701,600 | 702,300 | \-350,700 |  87 |

#### Recovery of Person Parameters Theta

<p>

The correlation of true and estimated parameters (ideally equal to 1)
and the absolute bias (ideally equal to 0) is shown in the following
table for the person parameters. The results for the item parameters
showed the same pattern but are not reported for the sake of brevity. As
expected, the recovery of the true parameters was best for the
data-generating model, namely, the MCN model. The proper but
misspecified ECN model led to poorer recovery compared to the correct
MCN model, which was expected. Likewise, the improper MCN2 model also
led to inferior recovery, especially for θ<sub>e</sub> (which is
explained by the fact that the corresponding pseudo-item x\*<sub>e</sub>
is incorrectly defined in the MCN2 model). In more detail, the absolute
difference between the true and the estimated θ<sub>e</sub> parameters
was on average 0.12 larger in the MCN2 model compared to the MCN model,
which corresponds to almost 1/7 of a standard deviation. Importantly,
this bias was dependent on the number of midpoint responses: For
example, absolute bias was 0.30 for persons with at most 10 midpoint
responses, but was 0.44 for persons with more than 10 midpoint
responses.

</p>

``` r
# Extract estimated thetas

thetas <- sim_res %>%
    mutate(est = map(output, "savedata"),
           theta = map(est, ~select(.x, matches("theta\\d$"))))

# Add true thetas

thetas <- mutate(thetas, true = list(thetas_true))

# Comparison of estimated and true thetas across models

thetas %>%
    mutate(corT   = map2_dbl(theta, true, ~cor(.x$THETA1, .y$V3)),
           corE   = map2_dbl(theta, true, ~cor(.x$THETA2, .y$V2)),
           corM   = map2_dbl(theta, true, ~cor(.x$THETA3, .y$V1)),
           # biasT  = map2_dbl(theta, true, ~mean(.y$V3 - .x$THETA1)),
           # biasE  = map2_dbl(theta, true, ~mean(.y$V2 - .x$THETA2)),
           # biasM  = map2_dbl(theta, true, ~mean(.y$V1 - .x$THETA3)),
           abiasT = map2_dbl(theta, true, ~mean(abs(.y$V3 - .x$THETA1))),
           abiasE = map2_dbl(theta, true, ~mean(abs(.y$V2 - .x$THETA2))),
           abiasM = map2_dbl(theta, true, ~mean(abs(.y$V1 - .x$THETA3)))) %>%
    select(Model, starts_with("cor"), starts_with("abias")) %>% 
    knitr::kable(digits = 2, format = "markdown")
```

| Model | corT | corE | corM | abiasT | abiasE | abiasM |
| :---- | ---: | ---: | ---: | -----: | -----: | -----: |
| MCN   | 0.93 | 0.94 | 0.90 |   0.28 |   0.23 |   0.23 |
| MCN2  | 0.93 | 0.85 | 0.89 |   0.28 |   0.35 |   0.24 |
| ECN   | 0.93 | 0.90 | 0.85 |   0.28 |   0.29 |   0.31 |

``` r
# Note: cor = Correlation of true and estimated theta;
#       abias = Absolute bias;
#       T = Trait; E = ERS; M = MRS.
```

#### Recovery of Latent Variances and Correlations

A comparison of true and estimated latent variances and correlations
showed that the estimates from the MCN model closely resembled the true
values. In contrast, the estimates from the MCN2 model differed
substantially. For example, the true correlation between the ERS and MRS
dimension was set to -.50, it was correctly estimated in the MCN model,
and it equaled -.06 in the MCN2 model, which would lead to dramatically
different conclusions.

The true and estimated latent variances are depicted in the following
table.

``` r
sigma_res <- sim_res %>%
    mutate(est  = map(output, list("parameters", "unstandardized")),
           vars = map(est, ~filter(.x, paramHeader == "Variances")),
           vars = map(vars, ~select(.x, Var = est))) %>%
    mutate(std  = map(output, list("parameters", "stdyx.standardized")),
           cors = map(std, ~filter(.x, grepl("WITH", paramHeader))),
           cors = map(cors, ~select(.x, Cor = est)))

sigma_res %>% 
    select(Model, vars) %>% 
    unnest(cols = vars) %>% 
    mutate(Dim  = rep(c("T", "E", "M"), 3),
           True = rep(c(1, .75, .5), 3)) %>% 
    pivot_wider(names_from = Model, values_from = Var) %>% 
    knitr::kable(format = "markdown")
```

| Dim | True |  MCN | MCN2 |  ECN |
| :-- | ---: | ---: | ---: | ---: |
| T   | 1.00 | 1.01 | 1.01 | 1.01 |
| E   | 0.75 | 0.77 | 0.65 | 0.67 |
| M   | 0.50 | 0.50 | 0.47 | 0.61 |

Furthermore, the true and estimated latent correlations are depicted in
the following table.

``` r
sigma_res %>% 
    select(Model, cors) %>% 
    unnest(cols = cors) %>% 
    mutate(Dims = rep(c("T,E", "T,M", "E,M"), 3),
           True = rep(c(.2, -.2, -.5), 3)) %>% 
    pivot_wider(names_from = Model, values_from = Cor) %>% 
    knitr::kable(format = "markdown")
```

| Dims |  True |    MCN |   MCN2 |    ECN |
| :--- | ----: | -----: | -----: | -----: |
| T,E  |   0.2 |   0.21 |   0.16 |   0.16 |
| T,M  | \-0.2 | \-0.21 | \-0.20 | \-0.24 |
| E,M  | \-0.5 | \-0.50 | \-0.06 | \-0.55 |

## Discussion

In summary, the results of the present simulation study showed that
parameter estimates are affected when using incorrect pseudo-items.
These effects are most severe for parameters that directly depend on an
incorrect pseudo-item, but related parameters may be affected as well.
More importantly, model fit can also be disturbed, which may lead to
wrong conclusions. These results of the simulation study match and
explain related empirical results (LaHuis et al., 2019a, 2019b).

## References

<div id="refs" class="references">

<div id="ref-boeckenholt_modeling_2012">

Böckenholt, U. (2012). Modeling multiple response processes in judgment
and choice. *Psychological Methods*, *17*(4), 665–678.
<https://doi.org/10.1037/a0028111>

</div>

<div id="ref-lahuis_applying_2018">

LaHuis, D. M., Blackmore, C. E., Bryant-Lees, K. B., & Delgado, K.
(2019a). Applying item response trees to personality data in the
selection context. *Organizational Research Methods*, *22*(4),
1007–1018. <https://doi.org/10.1177/1094428118780310>

</div>

<div id="ref-lahuis_corrigendum_2019">

LaHuis, D. M., Blackmore, C. E., Bryant-Lees, K. B., & Delgado, K.
(2019b). Corrigendum: Applying item response trees to personality data
in the selection context. *Organizational Research Methods*, *22*(4),
1019–1020. <https://doi.org/10.1177/1094428119858467>

</div>

<div id="ref-muthen_mplus_74">

Muthén, L. K., & Muthén, B. O. (2012). *Mplus. Statistical analysis with
latent variables, Version 7.4*. Los Angeles, CA: Muthén & Muthén.

</div>

<div id="ref-plieninger_developing_2020">

Plieninger, H. (2020). Developing and applying ir-tree models:
Guidelines, caveats, and an extension to multiple groups.
*Organizational Research Methods*.
<https://doi.org/10.1177/1094428120911096>

</div>

<div id="ref-plieninger_validity_2014">

Plieninger, H., & Meiser, T. (2014). Validity of multiprocess IRT models
for separating content and response styles. *Educational and
Psychological Measurement*, *74*(5), 875–899.
<https://doi.org/10.1177/0013164413514998>

</div>

</div>
