library(tidyverse)
library(fcrat)

df <- readRDS(file = "./simulations/data_sim/df_CentM_InclRates_LossComb_20240328.rds")
# note: test_level = 0.1


df

# mark true points
points <- tibble(comb_name = c("mean","median","mode",
                               "loss_mean-mode", "loss_mean-median","loss_median-mode",
                               "loss_mean-median-mode"),
                 theta1 = c(0,1,0, 0,  0.5,0.5, 0.333333333333333), 
                 theta2 = c(0,0,1, 0.5,0,  0.5, 0.333333333333333)) %>%
  mutate(truth=TRUE)

df <- df |> left_join(points) %>% filter(!is.na(truth))

# rename
df$comb_name <- as.factor(df$comb_name)
df$comb_name <- 
  factor(df$comb_name,
         levels= c("mean", "median", "mode", 
                   "loss_mean-median", "loss_mean-mode", "loss_median-mode", "loss_mean-median-mode", 
                   "forecaster_mean-median", "forecaster_mean-mode", "forecaster_median-mode", "forecaster_mean-median-mode"),
         labels = c("Mean Forecasts", "Median Forecasts", "Mode Forecasts",
                    "Mean-Median loss combination", "Mean-Mode loss combination", 
                    "Median-Mode loss combination", "Mean-Median-Mode loss combination",
                    "Mean and Median Forecasts", "Mean and Mode Forecasts",
                    "Median and Mode Forecasts", "Mean, Median and Mode Forecasts"))

 
size_print_inst1 <- df %>%
  dplyr::filter(inst=="(1,x)" & dgp=="Hom" & skew==0) %>%
  arrange(n, comb_name) %>%
  mutate(inclusion=100*inclusion) %>%
  pull(inclusion) %>%
  matrix(ncol=3, nrow=7) %>%
  format(nsmall=1, digits=1)

size_print_inst2 <- df %>%
  dplyr::filter(inst=="(1,x)" & dgp=="Hom" & skew==0.5) %>%
  arrange(n, comb_name) %>%
  mutate(inclusion=100*inclusion) %>%
  pull(inclusion) %>%
  matrix(ncol=3, nrow=7) %>%
  format(nsmall=1, digits=1)

size_print_inst3 <- df %>%
  dplyr::filter(inst=="(1,x)" & dgp=="AR-GARCH" & skew==0) %>%
  arrange(n, comb_name) %>%
  mutate(inclusion=100*inclusion) %>%
  pull(inclusion) %>%
  matrix(ncol=3, nrow=7) %>%
  format(nsmall=1, digits=1)

size_print_inst4 <- df %>%
  dplyr::filter(inst=="(1,x)" & dgp=="AR-GARCH" & skew==0.5) %>%
  arrange(n, comb_name) %>%
  mutate(inclusion=100*inclusion) %>%
  pull(inclusion) %>%
  matrix(ncol=3, nrow=7) %>%
  format(nsmall=1, digits=1)

help_doubledollar <- rep(" ", 7)

size_print <- cbind(chartr("_", " ", points$comb_name), help_doubledollar, help_doubledollar, size_print_inst1, help_doubledollar, size_print_inst2, help_doubledollar, size_print_inst3, help_doubledollar, size_print_inst4)

write.table(size_print, file=paste0("simulations/output/CentralityMeasures/size/size_dgpMain.txt"), row.names=FALSE, col.names=FALSE, sep = " & ", quote=FALSE, eol="\\\\\n")


