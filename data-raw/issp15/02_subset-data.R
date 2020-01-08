library("dplyr")
library("conflicted")
conflict_prefer("filter", "dplyr")

data("issp15")

# The method for sampling random numbers in sample() was changed in R version 3.6.0.
# The subsamples drawn here were initially drawn using an earlier R version.
# Thus, to reproduce the old behavior, a different sampling method has to be
# used via RNGversion. See ?RNGkind for further details.
RNGversion("3.5.2")

# Duplicated cases --------------------------------------------------------

# The document "ZA1490_All_Overview_Duplicated Records.xlsx" marks the following
# six cases from Venezuela (two of which were included in my previous analyses)
# as duplicates:
# "The archive recommends deleting all the six affected cases."

dplcts <- "2015008620000053|2015008620000054|2015008620000123|2015008620000127|2015008620000148|2015008620000149"

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
    select(v45:v47, country, rowid, CASEID) %>%
    filter(., complete.cases(.)) %>%
    split(., .$country) %>%
    purrr::map_if(., ~ nrow(.x) >= 500, ~ sample_n(.x, 500)) %>%
    bind_rows() %>%
    filter(!grepl(dplcts, CASEID)) %>%
    pull(rowid)

# issp15 %>%
#     filter(rowid %in% id_subset) %>%
#     filter(country %in% ctrs)

usethis::use_data(ctrs, id_subset, overwrite = TRUE)

RNGversion(paste(R.Version()$major, R.Version()$minor, sep = "."))
