devtools::load_all(".")

library("conflicted")
library("dplyr")
conflict_prefer("filter", "dplyr")
library("ggbeeswarm")
# remotes::install_github("hplieninger/ItemResponseTrees")
library("ItemResponseTrees")

data("issp15")
data("id_subset")

dat_1 <- issp15 %>%
    filter(rowid %in% id_subset) %>%
    # filter(country %in% ctrs) %>%
    select(rowid, v45:v47, country) %>%
    mutate_at(vars(v45:v47), . %>% as.numeric %>% {6 - .})

# IR-Tree -----------------------------------------------------------------

m1 <- "
# MCN model

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
#
# Items:
# v45, v46, v47

Class:
Tree
"

# m2 <- "
# # ECN model
#
# IRT:
# t  BY v45@1, v46@1, v47@1;
# e  BY v45@1, v46@1, v47@1;
# m  BY v45@1, v46@1, v47@1;
#
# # Subtree:
# # t = t1 + t2
#
# Equations:
# 1 = (1-m)*(1-t)*e
# 2 = (1-m)*(1-t)*(1-e)
# 3 = m
# 4 = (1-m)*t*(1-e)
# 5 = (1-m)*t*e
#
# # Processes:
# # m, e, t
# #
# # Items:
# # v45, v46, v47
#
# Class:
# Tree
# "

m3 <- "
# GRModel

IRT
t  BY v45@1, v46@1, v47@1;

# Items:
# v45, v46, v47
#
# Processes:
# t

Class:
GRM
"

# model <- tree_model(m1)

dat_1 %>%
    mutate_at("country", as.numeric) %>%
    as.data.frame() %>%
    ItemResponseTrees::fit_tree_mplus(data = ., model = m1, file_name = "mplus-tree-boeck-01",
                                      dir = "code/issp15/mplus", run = F, overwrite = T)


# MCN2 Model --------------------------------------------------------------

### Export data for the improper MCN2 model ###

dat_improper <- do.call(cbind, rep(list(select(dat_1, v45:v47)), 3))

i <- 3

# Pseudoitem -010-
dat_improper[, 1:i] <-
    apply(dat_improper[, 1:i], 2,
          function(x)
              car::recode(x, "3=1; c(2, 4)=0; c(1, 5)=NA"))
names(dat_improper)[1:i] <- paste0("m_", names(dat_improper[, 1:i]))

# Pseudoitem 10-01
dat_improper[, (i + 1):(i * 2)] <-
    apply(dat_improper[, (i + 1):(i * 2)], 2,
          function(x)
              car::recode(x, "c(1, 5) = 1; c(2, 4) = 0; 3=NA"))
names(dat_improper)[(i + 1):(i * 2)] <- paste0("e_", names(dat_improper[, (i + 1):(i * 2)]))

# Pseudoitem 00-11
dat_improper[, (2 * i + 1):(i * 3)] <-
    apply(dat_improper[, (2 * i + 1):(i * 3)], 2,
          function(x)
              car::recode(x, "1:2 = 0; 3 = NA; 4:5 = 1"))
names(dat_improper)[(2 * i + 1):(i * 3)] <- paste0("t_", names(dat_improper[, (2 * i + 1):(i * 3)]))

tmp1 <- cbind(dat_improper, select(dat_1, rowid, v45:v47, country)) %>%
    mutate_at("country", as.numeric)

write.table(tmp1, file = here("code/issp15/mplus/mplus-improper-app-01.txt"),
            row.names = FALSE, col.names = FALSE, na = ".")
