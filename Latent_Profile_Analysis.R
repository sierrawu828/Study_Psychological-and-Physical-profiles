library(tidyLPA)
library(mclust)
library(tidyverse)
library(ggplot2)

set.seed(456)

raw_data <- read.csv("data.csv")
summary(raw_data)
LPA_data <- raw_data %>%
  select(PR,MVPA,LPA)
summary(LPA_data)
LPA_results <- estimate_profiles(df=LPA_data, n_profiles = 1:6, models = 3, nboot = 1000, package = "mclust")
LPA_fit <- get_fit(LPA_results)
View(LPA_fit)

#Lo-Mendell-Rubin likelihood ratio test
calc_lrt(17532, #Number of samples
         LPA_fit[["model_3_class_2"]][["model"]][["loglik"]], length(LPA_fit[["model_3_class_2"]][["estimates"]][["Parameter"]]), 2,
         LPA_fit[["model_3_class_3"]][["model"]][["loglik"]], length(LPA_fit[["model_3_class_3"]][["estimates"]][["Parameter"]]), 3)
calc_lrt(17532, 
         LPA_fit[["model_3_class_3"]][["model"]][["loglik"]], length(LPA_fit[["model_3_class_3"]][["estimates"]][["Parameter"]]), 3,
         LPA_fit[["model_3_class_4"]][["model"]][["loglik"]], length(LPA_fit[["model_3_class_4"]][["estimates"]][["Parameter"]]), 4)
calc_lrt(17532, 
         LPA_fit[["model_3_class_4"]][["model"]][["loglik"]], length(LPA_fit[["model_3_class_4"]][["estimates"]][["Parameter"]]), 4,
         LPA_fit[["model_3_class_5"]][["model"]][["loglik"]], length(LPA_fit[["model_3_class_5"]][["estimates"]][["Parameter"]]), 5)

LPA_fit$model_3_class_2$mode3$parameters$pro
LPA_fit$model_3_class_3$mode3$parameters$pro
LPA_fit$model_3_class_4$mode3$parameters$pro
LPA_fit$model_3_class_5$mode3$parameters$pro
LPA_fit$model_3_class_6$mode3$parameters$pro

p <- LPA_fit[[n]] %>% #Observing different solutions, e.g., n=2,3,4,5...
  plot_profiles(rawdata = F, sd = F, ci = F, add_line = T)
p + scale_x_continuous(breaks = 1:3, labels = c('PR','MVPA','LPA'))

LPA_fit_classes <- LPA_results$model_3_class_n$dff
LPA_est_cn <- get_estimates(LPA_results[[n]])
View(LPA_est_cn)

write.csv(LPA_fit,"lpa_fit_indices.csv")
write.csv(LPA_est_cn,"class(n)_estimates.csv")
write.csv(LPA_fit_classes,"classified_subjects.csv")