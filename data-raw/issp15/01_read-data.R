# ISSP Research Group (2017): International Social Survey Programme: Work
# Orientations IV - ISSP 2015. GESIS Data Archive, Cologne. ZA6770 Data file
# Version 2.1.0, doi:10.4232/1.12848

dat_0 <- haven::read_sav(here("data-raw/issp15/ZA6770_v2-1-0.sav"), user_na = TRUE)

issp15 <- dat_0 %>% 
    tibble::rowid_to_column() %>% 
    # mutate_at("CASEID", as.character) %>% 
    # select(v45:v47, country, V16) %>%
    # select(v53:v58, country, v16) %>%
    # select(v38:v39, country, v16) %>%
    # select(v69:v74, country, v16) %>%
    haven::zap_missing() %>%
    # haven::zap_label() %>% 
    # haven::zap_formats() %>% 
    haven::as_factor() %>% 
    dplyr::mutate_if(is.factor, forcats::fct_drop)

usethis::use_data(issp15, overwrite = TRUE)
