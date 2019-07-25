devtools::load_all(".")

library("conflicted")
library("tidyverse")
conflict_prefer("filter", "dplyr")

# Import Mplus results ----------------------------------------------------

out_1 <- MplusAutomation::readModels(here("code/simulation/mplus"))

names(out_1) <- stringr::str_extract(names(out_1), "(?<=model_).+(?=_.+)")

out_2 <- enframe(out_1)

out_2 <- out_2 %>%
    slice(3, 1, 2) %>%
    mutate(AIC = map_dbl(value, list("summaries", "AIC")),
           BIC = map_dbl(value, list("summaries", "BIC")),
           LL  = map_dbl(value, list("summaries", "LL")),
           par = map_dbl(value, list("summaries", "Parameters")))

# save(out_2, file = here("code/simulation/sim-results.rda"))

# Model Fit ---------------------------------------------------------------

mutate_at(out_2, vars(AIC:LL), round, -2)

# Betas -------------------------------------------------------------------

load(here("code/simulation/betas.rda"))

# Extractor function
f1 <- possibly(function(x) {
    filter(x, paramHeader == "Thresholds") %>%
        separate(param, letters[1:3], sep = "_") %>%
        mutate(item = readr::parse_number(c)) %>%
        select(b, item, est) %>%
        pivot_wider(id_cols = item, names_from = b, values_from = est)
},
otherwise = NULL)

out_betas <- out_2 %>%
    filter(name != "grm") %>%
    mutate(est = map(value, list("parameters", "unstandardized")),
           thres = map(est, ~f1(.x))) %>%
    mutate(betas = list(betas))

### Comparison of estimated and true betas across models ###

out_betas %>%
    mutate(corM = map2_dbl(thres, betas, ~cor(.x$M, .y$beta_mrs)),
           corE = map2_dbl(thres, betas, ~cor(.x$E, .y$beta_ers)),
           corT = map2_dbl(thres, betas, ~cor(.x$T, .y$beta_trt)),
           abiasM = map2_dbl(thres, betas, ~mean(abs(.y$beta_mrs - .x$M))),
           abiasE = map2_dbl(thres, betas, ~mean(abs(.y$beta_ers - .x$E))),
           abiasT = map2_dbl(thres, betas, ~mean(abs(.y$beta_trt - .x$T)))) %>%
    select(-c(value:betas))

# Thetas ------------------------------------------------------------------

out_thetas <- out_2 %>%
    mutate(est = map(value, "savedata"),
           theta = map(est, ~select(.x, matches("theta\\d$"))))

### Comparison of estimated and true thetas across models ###

out_thetas2 <- out_thetas %>%
    mutate(true = list(as.data.frame(simdata$theta))) %>%
    mutate(corT  = map2_dbl(theta, true, ~cor(.x$THETA1, .y$V3)),
           corE  = map2_dbl(theta, true, ~cor(.x$THETA2, .y$V2)),
           corM  = map2_dbl(theta, true, ~cor(.x$THETA3, .y$V1)),
           biasT = map2_dbl(theta, true, ~mean(.y$V3 - .x$THETA1)),
           biasE = map2_dbl(theta, true, ~mean(.y$V2 - .x$THETA2)),
           biasM = map2_dbl(theta, true, ~mean(.y$V1 - .x$THETA3)),
           abiasT = map2_dbl(theta, true, ~mean(abs(.y$V3 - .x$THETA1))),
           abiasE = map2_dbl(theta, true, ~mean(abs(.y$V2 - .x$THETA2))),
           abiasM = map2_dbl(theta, true, ~mean(abs(.y$V1 - .x$THETA3)))) %>%
    select(-c(value, LL:true))
out_thetas2

out_thetas2 %>%
    select(name, starts_with("cor"), starts_with("abias"), AIC, BIC) %>%
    knitr::kable(format = "latex", digits = c(0, 2, 2, 2, 2, 2, 2, -2, -2),
                 booktabs = TRUE)


### Comparison of estimated thetas across models ###

out_thetas %>%
    select(name, theta) %>%
    pivot_wider(values_from = theta) %>%
    mutate(corT  = map2_dbl(mcn, app, ~cor(.x$THETA1, .y$THETA1)),
           corE  = map2_dbl(mcn, app, ~cor(.x$THETA2, .y$THETA2)),
           corM  = map2_dbl(mcn, app, ~cor(.x$THETA3, .y$THETA3)),
           corT2 = map2_dbl(mcn, ecn, ~cor(.x$THETA1, .y$THETA1)),
           corE2 = map2_dbl(mcn, ecn, ~cor(.x$THETA2, .y$THETA2)),
           corM2 = map2_dbl(mcn, ecn, ~cor(.x$THETA3, .y$THETA3)))

tmp1 <- out_thetas %>%
    mutate(true = list(as.data.frame(simdata$theta))) %>%
    filter(name == "app") %>%
    unnest(cols = c(theta, true), names_repair = "universal") %>%
    mutate(mid_responses = rowSums(simdata$dat == 2))

ggplot(tmp1, aes(THETA2, V2, color = mid_responses)) +
    geom_point(size = 3, alpha = .33) +
    scale_color_viridis_c() +
    geom_abline(slope = 1, intercept = 0)

ggplot(tmp1, aes(mid_responses, V2 - THETA2)) +
    geom_point(size = 3, alpha = .33) +
    stat_smooth()

tmp1 %>%
    mutate(fac = cut(mid_responses, breaks = c(-1, 10, 30))) %>%
    group_by(fac) %>%
    summarize(m = mean(abs(V2 - THETA2)),
              n = n()/nrow(simdata$dat))

# Variances ---------------------------------------------------------------

out_betas %>%
    select(-c(thres, betas)) %>%
    mutate(vars = map(est, ~filter(.x, paramHeader == "Variances")),
           vars = map(vars, ~select(.x, est))) %>%
    select(name, vars) %>%
    unnest(cols = vars)

# Correlations ------------------------------------------------------------

out_betas %>%
    select(name, value) %>%
    mutate(std = map(value, list("parameters", "stdyx.standardized")),
           covs = map(std, ~filter(.x, grepl("WITH", paramHeader))),
           covs = map(covs, ~select(.x, est))) %>%
    unnest(cols = covs)
