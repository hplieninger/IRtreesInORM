#' ---
#' output: html_document
#' ---

devtools::load_all(".")

# Sys.setenv(CLIPR_ALLOW = TRUE)

library("conflicted")
library("extrafont")
library("dplyr")
conflict_prefer("filter", "dplyr")
library("ggbeeswarm")

# Model definition --------------------------------------------------------

m1 <- "
# Comment
IRT:
t  BY v45@1, v46@1, v47@1;
e  BY v45@1, v46@1, v47@1;
m  BY v45@1, v46@1, v47@1;

# Subtree:
# t = t1 + t2

Equations:
1 = (1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m*(1-e)
4 = (1-m)*t*(1-e)
5 = t*e

# Processes:
# m, e, t

# Items:
# v45, v46, v47

Class:
Tree
"

m3 <- "
IRT:
t  BY v45@1, v46@1, v47@1;

# Items:
# v45, v46, v47

# Processes:
# t

Class:
GRM
"


# Venezuela ---------------------------------------------------------------

tmp1 <- MplusAutomation::readModels(here("code/issp15/mplus/mplus-tree-mcn-11.out"))

tmp1$summaries[c("AIC", "BIC")] %>%
    round(-1) %>%
    prettyNum(big.mark = ",") %>%
    paste(names(.), ., sep = " = ", collapse = " and ") %>%
    # clipr::write_clip() %>%
    identity()

res_1 <- ItemResponseTrees::extract_mplus_output(tmp1, m1)

### Item difficulties beta ###
round(colMeans(res_1$item$beta[, 2:4]), 2)
round(pnorm(-colMeans(res_1$item$beta[, 2:4])), 2)

### Latent variances/SDs ###
round(sqrt(diag(res_1$sigma)), 2)

round(pnorm(sqrt(res_1$sigma[2, 2]) - mean(res_1$item$beta$E)), 2)
round(pnorm(-sqrt(res_1$sigma[2, 2]) - mean(res_1$item$beta$E)), 2)

### Latent correlations ###
round(res_1$cormat, 2)

tmp2 <- MplusAutomation::readModels(here("code/issp15/mplus/mplus-grm-11.out"))

tmp2$summaries[c("AIC", "BIC")] %>%
    round(-1) %>%
    prettyNum(big.mark = ",") %>%
    paste(names(.), ., sep = " = ", collapse = " and ") %>%
    # clipr::write_clip() %>%
    identity()

res_2 <- ItemResponseTrees::extract_mplus_output(tmp2, model = m3)

### Comparison of theta_(target trait) across GRM and MCN ###

round(cor(res_2$person$personpar_est[, 1], res_1$person$personpar_est$T), 2)

plot(jitter(res_2$person$personpar_est[, 1], factor = 100),
     jitter(res_1$person$personpar_est$T, factor = 100))
abline(0, 1)

(res_2$person$personpar_est[, 1] - res_1$person$personpar_est$T) %>%
    abs %>%
    mean %>%
    round(2)

# Multiple-Group (11 Countries) -------------------------------------------


# __IR-Tree ---------------------------------------------------------------

mcn_mg_1 <- MplusAutomation::readModels(here("code/issp15/mplus/mplus-tree-mcn-01.out"))

mcn_mg_1$summaries[c("AIC", "BIC")] %>%
    round(-1) %>%
    prettyNum(big.mark = ",") %>%
    paste(names(.), ., sep = " = ", collapse = " and ") %>%
    # clipr::write_clip() %>%
    identity()

### Extract latent means and variances ###

res_mg_2 <- mcn_mg_1$parameters$unstandardized

tmp1 <- substr(levels(issp15$country)[which(levels(issp15$country) %in% ctrs)], 4, 100) %>%
    sub("Great Britain and/or United Kingdom", "United Kingdom", x = .)

dat_mcn_gg1 <- res_mg_2 %>%
    filter(paramHeader == "Means" | paramHeader == "Variances") %>%
    filter(param %in% c("M", "E", "T")) %>%
    mutate(country = factor(LatentClass, levels = 1:11, labels = tmp1)) %>%
    reshape2::dcast(country + param ~ paramHeader, value.var = "est") %>%
    rename(absMean = Means) %>%
    group_by(param) %>%
    mutate(Min = min(absMean)) %>%
    ungroup %>%
    mutate(Mean = absMean - Min,
           param = factor(tolower(param), levels = c("t", "e", "m")), Country = country,
           shape = factor(as.numeric(Country), levels = 1:11, labels = array(c(1:3, 7), dim = 11))) %>%
    identity()

dat_mcn_gg2 <- dat_mcn_gg1 %>%
    reshape2::melt(measure.vars = c("Mean", "Variances")) %>%
    mutate(variable = recode(variable, Mean = "Latent Mean", Variances = "Latent Variance"))

p1 <- ggplot(dat_mcn_gg2, aes(x = param, y = value, color = Country, shape = Country)) +
    # geom_jitter(height = 0, size = 3, width = .2) +
    geom_quasirandom(size = 3, width = .25, groupOnX = TRUE) +
    facet_wrap(~ variable, ncol = 2, scales = "free_y") +
    scale_color_viridis_d(direction = -1) +
    scale_shape_manual(values = array(c(15:18), dim = 11)) +
    labs(x = "Parameter", y = "Size") +
    lims(y = c(0, NA)) +
    # ggthemes::theme_igray() +
    theme(panel.grid.major.y = element_line(linetype = 2, size = .6, color = "gray90"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())

p1

x <- 6
cairo_pdf(here("figures/latent-means-variances-01.pdf"), width = x, height = x/1.618,
          family = "CMU Sans Serif", pointsize = 12)
p1
dev.off()

fs::file_copy(here("figures/latent-means-variances-01.pdf"),
              here("../figures/latent-means-variances-01.pdf"), overwrite = TRUE)

dat_mcn_gg1 %>%
    group_by(param) %>%
    summarize(m = median(Variances))

# __GRM -------------------------------------------------------------------

grm_mg_1 <- MplusAutomation::readModels(here("code/issp15/mplus/mplus-grm-01.out"))

grm_mg_1$summaries[c("AIC", "BIC")] %>%
    round(-1) %>%
    prettyNum(big.mark = ",") %>%
    paste(names(.), ., sep = " = ", collapse = " and ") %>%
    # clipr::write_clip() %>%
    identity()

### Extract latent means ###

grm_mg_2 <- grm_mg_1$parameters$unstandardized

tmp1 <- substr(levels(issp15$country)[which(levels(issp15$country) %in% ctrs)], 4, 100) %>%
    sub("Great Britain and/or United Kingdom", "United Kingdom", x = .)

dat_grm_gg1 <- grm_mg_2 %>%
    filter(paramHeader == "Means" | paramHeader == "Variances") %>%
    filter(param %in% c("M", "E", "T")) %>%
    mutate(country = factor(LatentClass, levels = 1:11, labels = tmp1)) %>%
    reshape2::dcast(country + param ~ paramHeader, value.var = "est") %>%
    rename(absMean = Means) %>%
    group_by(param) %>%
    mutate(Min = min(absMean)) %>%
    ungroup %>%
    mutate(Mean = absMean - Min,
           param = factor(tolower(param), levels = c("t", "e", "m")), Country = country,
           shape = factor(as.numeric(Country), levels = 1:11, labels = array(c(1:3, 7), dim = 11))) %>%
    identity()

dat_mdls_1 <- left_join(dat_grm_gg1, dat_mcn_gg1,
                        by = c("param", "Country" = "country"),
                        suffix = c("_grm", "_mcn"))

p2 <- ggplot(dat_mdls_1, aes(x = absMean_grm, y = absMean_mcn, color = Country,
                       shape = Country)) +
    geom_abline(intercept = 0, slope = 1, color = "gray90", size = .6,
                linetype = 2) +
    geom_point(size = 3) +
    scale_color_viridis_d(direction = -1) +
    scale_shape_manual(values = array(c(15:18), dim = 11)) +
    labs(x = "GRM", y = "IR-Tree Model") +
    lims(x = c(-.65, 1), y = c(-.65, 1)) +
    # theme_igray() +
    theme(panel.grid.major = element_line(linetype = 2, size = .6, color = "gray90"),
          panel.grid.minor = element_blank())

p2

x <- 6
cairo_pdf(here("figures/latent-means-scatter-01.pdf"), width = x, height = x/1.618,
          family = "CMU Sans Serif", pointsize = 12)
p2
dev.off()

fs::file_copy(here("figures/latent-means-scatter-01.pdf"),
              here("../figures/latent-means-scatter-01.pdf"), overwrite = TRUE)

cor(dat_mdls_1$absMean_grm, dat_mdls_1$absMean_mcn, method = "kendall")

# Slovenia ----------------------------------------------------------------

slo_grm_1 <- MplusAutomation::readModels(here("code/issp15/mplus/mplus-grm-21.out"))
slo_mcn_1 <- MplusAutomation::readModels(here("code/issp15/mplus/mplus-tree-mcn-21.out"))
slo_app_1 <- MplusAutomation::readModels(here("code/issp15/mplus/mplus-tree-app-21.out"))

list1 <- list(
    grm = slo_grm_1,
    mcn = slo_mcn_1,
    app = slo_app_1
)

purrr::map(list1, ~ .x[["summaries"]][c("AIC", "BIC")]) %>%
    purrr::map(round, -1) %>%
    purrr::map(prettyNum, big.mark = ",") %>%
    purrr::map(~ paste(names(.x), .x, sep = " = ", collapse = " and ")) %>%
    # clipr::write_clip() %>%
    identity()

sessioninfo::session_info()
