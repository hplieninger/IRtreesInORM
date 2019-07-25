#' ---
#' output: html_document
#' ---

devtools::load_all(".")

library("conflicted")
library("extrafont")
library("dplyr")
conflict_prefer("filter", "dplyr")

tmp1 <- substr(levels(issp15$country)[which(levels(issp15$country) %in% ctrs)], 4, 100) %>%
    sub("Great Britain and/or United Kingdom", "United Kingdom", x = .)

dat_0 <- issp15 %>%
    filter(rowid %in% id_subset) %>%
    filter(country %in% ctrs) %>%
    select(rowid, v45:v47, country) %>%
    mutate_at(vars(v45:v47), . %>% as.numeric %>% {6 - .}) %>%
    droplevels %>%
    mutate(country = factor(as.numeric(country), levels = 1:11, labels = tmp1)) %>%
    identity()

# Descriptives ------------------------------------------------------------

dat_0 %>%
    group_by(country) %>%
    summarize(n = n()) %>%
    arrange(n)

dat_0 %>%
    select(v45:v47) %>%
    mutate_all(as.numeric) %>%
    {cor(.)} %>%
    round(2)

# Bar Plots ---------------------------------------------------------------

### Plot response distribution for each item ###

dat_1 <- dat_0 %>%
    reshape2::melt(id.vars = c("rowid", "country")) %>%
    group_by(country, variable) %>%
    summarize(y = table(value) %>% prop.table %>% list,
              x = table(value) %>% names %>% factor(levels = 1:5
                                                    # , labels = levels(issp15$v45)
                                                    ) %>% list) %>%
    tidyr::unnest(cols = c(y, x)) %>%
    mutate(y = as.numeric(y))

dat_2 <- dat_0 %>%
    reshape2::melt(id.vars = c("rowid", "country")) %>%
    group_by(country) %>%
    summarize(y = table(value) %>% prop.table %>% list,
              x = table(value) %>% names %>% factor(levels = 1:5) %>% list) %>%
    tidyr::unnest(cols = c(y, x))

p1 <- ggplot(dat_1, aes(x = x, y = y, fill = variable)) +
    geom_col(position = "dodge") +
    geom_col(color = alpha("black", .2),
             fill = "transparent", data = dat_2, mapping = aes(fill = NULL)) +
    facet_wrap(~ country, ncol = 3) +
    scale_fill_viridis_d() +
    scale_y_continuous(expand = expand_scale(add = c(0, .05))) +
    labs(fill = "Item", x = "", y = "Frequency") +
    # theme_igray() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color = "gray90"),
          panel.grid.minor.y = element_blank())

p1

x <- 6
cairo_pdf(here("figures/barplot-countries-01.pdf"), width = x,
          # height = x/1.618,
          height = x/4*3,
          family = "CMU Sans Serif", pointsize = 12)
p1
dev.off()

fs::file_copy(here("figures/barplot-countries-01.pdf"),
              here("../figures/barplot-countries-01.pdf"), overwrite = TRUE)
