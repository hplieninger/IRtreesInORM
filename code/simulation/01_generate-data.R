devtools::load_all(".")
library("extrafont")

# Generate Data -----------------------------------------------------------

Sig <- matrix(nrow = 3, ncol = 3, byrow = TRUE,
              data = c( 1.0, -0.5, -0.2,
                       -0.5,  1.0,  0.2,
                       -0.2,  0.2,  1.0)) %>%
    MBESS::cor2cov(sd = sqrt(c(.5, .75, 1)))

beta_mrs <- c(.00, .25, .50)
beta_ers <- c(.50, .75, 1.0)
beta_trt <- c(-.25, .0, .25)

betas <- expand.grid(beta_mrs = beta_mrs,
                     beta_ers = beta_ers,
                     beta_trt = beta_trt)

i <- nrow(betas)
N <- 10000

set.seed(123)
sim_data <- sim_tree_data(items = i,
                    n = N,
                    betas = betas,
                    theta_vcov = Sig,
                    empirical = TRUE)

sim_data$probs <- NULL

# usethis::use_data(sim_data)

# Illustrate item parameters beta -----------------------------------------

dat01 <- apply(sim_data$dat, 2, function(x) prop.table(table(factor(x, 0:4, 1:5)))) %>%
    reshape2::melt()

dat01 %>%
    group_by(Var1) %>%
    summarize(m = list(enframe(summary(value)))) %>%
    mutate(m = map(m, ~mutate_at(.x, "value", as.numeric))) %>%
    unnest(cols = m) %>%
    filter(Var1 == 3)

f1 <- function(x) {
    formatC(x, digits = 2, format = "f", width = 5)
}

p1 <- ggplot(dat01, aes(Var1, value)) +
    geom_col() +
    facet_wrap(~Var2, nrow = 3) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .05)),
                       labels = f1) +
    labs(x = NULL, y = "Relative Frequency",
         title = "Simulation Study: Response Distribution of the 27 Items",
         subtitle = "Averaged Across all 10,000 Respondents") +
    theme(panel.grid = element_blank())

dat02 <- betas %>%
    tibble::rowid_to_column("item") %>%
    reshape2::melt(id.vars = "item") %>%
    dplyr::mutate(variable = factor(variable,
                                    c("beta_mrs", "beta_ers", "beta_trt"),
                                    c("m", "e", "t")))

p2 <- ggplot(dat02, aes(variable, value, color = variable)) +
    geom_point(size = 2) +
    facet_wrap(~item, nrow = 3) +
    scale_y_continuous(breaks = seq(-.25, 1, .25)) +
    scale_color_viridis_d() +
    labs(x = NULL, y = "Item Parameter beta",
         title = "Simulation Study: Item Parameters of the 27 Items",
         subtitle = "Each Item has three parameters, namely, beta_m, beta_e, and beta_t") +
    theme(panel.grid = element_blank(), legend.position = "none")

x <- 8
cairo_pdf(here("figures/sim-study_items.pdf"), width = x, height = x/16*9,
          family = "CMU Sans Serif", pointsize = 12, onefile = TRUE)
p1
p2
dev.off()

# Theoretical/expected response distribution for theta=0
#
# d0 <- sim_tree_data(items = i,
#                     thetas = array(0, dim = c(1, 3)),
#                     betas = betas)
# dat01 <- reshape2::melt(d0$probs)
# p1 <- ggplot(dat01, aes(Var3, value)) +
#     geom_col() +
#     facet_wrap(~Var2, nrow = 3) +
#     scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
#     labs(x = NULL, y = "Expected Probability",
#          title = "Simulation Study: Expected Response Distributions for the 27 Items",
#          subtitle = "For an Average Respondent With a Theta-Vector Equal to 0") +
#     theme(panel.grid = element_blank())

# Save data MCN model -----------------------------------------------------

dirx <- "code/simulation/mplus/"

dd_mcn <- do.call(cbind, rep(list(sim_data$dat), 3))
dd_mcn[, 1:i] <-
    apply(dd_mcn[, 1:i], 2,
          function(x)
              car::recode(x, "0:1 = 0; 2 = NA; 3:4 = 1"))
dd_mcn[, (i + 1):(i * 2)] <-
    apply(dd_mcn[, (i + 1):(i * 2)], 2,
          function(x)
              car::recode(x, "c(0, 4) = 1; 1:3 = 0"))
dd_mcn[, (2 * i + 1):(i * 3)] <-
    apply(dd_mcn[, (2 * i + 1):(i * 3)], 2,
          function(x)
              car::recode(x, "2=1; c(1, 3)=0; c(0, 4)=NA"))

write.table(dd_mcn, file = here(dirx, sprintf("data_MCN_N=%i_001.txt", N)),
            row.names = FALSE, col.names = FALSE, na = "*")

# Save data 'APP' model ---------------------------------------------------

dd_app <- do.call(cbind, rep(list(sim_data$dat), 3))
dd_app[, 1:i] <-
    apply(dd_app[, 1:i], 2,
          function(x)
              car::recode(x, "0:1 = 0; 2 = NA; 3:4 = 1"))
dd_app[, (i + 1):(i * 2)] <-
    apply(dd_app[, (i + 1):(i * 2)], 2,
          function(x)
              car::recode(x, "c(0, 4) = 1; c(1, 3) = 0; 2=NA"))
dd_app[, (2 * i + 1):(i * 3)] <-
    apply(dd_app[, (2 * i + 1):(i * 3)], 2,
          function(x)
              car::recode(x, "2=1; c(1, 3)=0; c(0, 4)=NA"))

write.table(dd_app, file = here(dirx, sprintf("data_APP_N=%i_001.txt", N)),
            row.names = FALSE, col.names = FALSE, na = "*")

# Save data GRM -----------------------------------------------------------

write.table(sim_data$dat, file = here(dirx, sprintf("data_raw_N=%i_001.txt", N)),
            row.names = FALSE, col.names = FALSE, na = "*")

# Save data ECN model -----------------------------------------------------

dd_ecn <- do.call(cbind, rep(list(sim_data$dat), 3))
dd_ecn[, 1:i] <-
    apply(dd_ecn[, 1:i], 2,
          function(x)
              car::recode(x, "0:1 = 0; 2 = NA; 3:4 = 1"))
dd_ecn[, (i + 1):(i * 2)] <-
    apply(dd_ecn[, (i + 1):(i * 2)], 2,
          function(x)
              car::recode(x, "c(0, 4) = 1; c(1, 3) = 0; 2=NA"))
dd_ecn[, (2 * i + 1):(i * 3)] <-
    apply(dd_ecn[, (2 * i + 1):(i * 3)], 2,
          function(x)
              car::recode(x, "2=1; c(0, 1, 3, 4)=0"))

write.table(dd_ecn, file = here(dirx, sprintf("data_ECN_N=%i_001.txt", N)),
            row.names = FALSE, col.names = FALSE, na = "*")

# paste("v", rep(c("t", "e", "m"), each = i), sep = "_") %>%
#     paste(1:i, sep = "_") %>%
#     paste(collapse = " ") %>%
#     strwrap(width = 90, indent = 14, exdent = 14) %>%
#     paste(collapse = "\n") %>%
#     cat
