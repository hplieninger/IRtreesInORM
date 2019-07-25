library("dplyr")
library("conflicted")
conflict_prefer("filter", "dplyr")

data("issp15")


# Random subsample of countries -------------------------------------------

set.seed(1234)
ctrs <- sample(levels(issp15$country), 10) %>%
    c(., "VE-Venezuela") %>%
    sort

# vapply(ctrs, grep, x = levels(issp15$country), 1L) %>%
#     sort %>%
#     # paste("country == ", ., collapse = " OR ") %>%
#     paste("country == ", ., collapse = " ") %>%
#     stringr::str_wrap(width = 90, indent = 20, exdent = 20) %>%
#     cat


# Random subsample of participants ----------------------------------------

set.seed(1234)
id_subset <- issp15 %>%
    select(v45:v47, country, rowid) %>%
    filter(., complete.cases(.)) %>%
    split(., .$country) %>%
    purrr::map_if(., ~ nrow(.x) >= 500, ~ sample_n(.x, 500)) %>%
    bind_rows() %>%
    pull(rowid)

# issp15 %>%
#     filter(rowid %in% id_subset) %>%
#     filter(country %in% ctrs)

usethis::use_data(ctrs, id_subset)
