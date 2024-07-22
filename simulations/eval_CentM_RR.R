# Preparation -------------------------------------------------------------

library(tidyverse)
library(fcrat)


plot.mcs <- function(df, color_level = 0.945, 
                     color_limits = c(0,1.8),
                     # color_limits = c(.6,1.4),
                     ...)
{
  require(ggplot2)
  require("ggrepel")
  require(latex2exp)
  
  

  df$label <- NA
  df$label[df$theta1==0 & df$theta2==0]<-"mean"
  df$label[df$theta1==1 & df$theta2==0]<-"median"
  df$label[df$theta1==0 & df$theta2==1]<-"mode"
  
  
  # labels
  df$inclusion[df$inclusion > .99] <-.99
  df$inclusion_label <- as.character(round(df$inclusion,digits = 2)*100)
  
  df$inclusion_label <- ifelse(nchar(df$inclusion_label)==1,
                                paste0(" ",df$inclusion_label," "),
                                df$inclusion_label)
 
  df$inclusion[df$inclusion > .99] <-.99
  
  
  # mark true points
  points <- data.frame(comb_name = c("mean","median","mode","loss_mean-median-mode",
                                     "loss_mean-mode","loss_median-mode"),
                       theta1 = c(0,1,0,0.333333333333333,
                                  0,0.5), 
                       theta2=c(0,0,1,0.333333333333333,
                                0.5,0.5))
  
  points$included <- TRUE
  
  points$comb_name <- factor(points$comb_name, 
                             levels= c("mean", "median", "mode", 
                                       "loss_mean-median", "loss_mean-mode", "loss_median-mode", "loss_mean-median-mode", 
                                       "forecaster_mean-median", "forecaster_mean-mode", "forecaster_median-mode", "forecaster_mean-median-mode"),
                             labels = c("Mean Forecasts", "Median Forecasts", "Mode Forecasts",
                                        "Mean-Median loss combination", "Mean-Mode loss combination", 
                                        "Median-Mode loss combination", "Mean-Median-Mode loss combination",
                                        "Mean and Median Forecasts", "Mean and Mode Forecasts",
                                        "Median and Mode Forecasts", "Mean, Median and Mode Forecasts"))
  
  
  
  df <- df |> left_join(points)
  
  
  # rotate
  df$theta1n <- df$theta1 + 1/2 * df$theta2
  df$theta2n <-  sqrt(3)/2 * df$theta2
  df$theta1 <- df$theta1n
  df$theta2 <- df$theta2n
  
  #plot
  plot <- ggplot(data = df)+
    geom_label(aes(x=theta1,y=theta2,
                   color = is.na(included),
                   fill = inclusion,
                   label = inclusion_label),
               ...)
               
  
  
  plot <- plot +
    theme(
      text = element_text(),
      legend.position="right",
      axis.text.x=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())+
    scale_x_continuous(breaks=c(),labels = c(), name = "", limits = c(-.15,1.15))+
    scale_y_continuous(breaks=c(),labels = c(),name = "", limits = c(-.1,.95))+
    annotate("text", x=.5, y=.95, label="mode", color="black")+
    annotate("text", x=0, y=-.1, label="mean", color="black")+
    annotate("text", x=1, y=-.1, label="median", color="black")
  

  
  plot <- plot +
    scale_size_continuous(limits=c(0,1),range = c(4,4)) + guides(size=F,color=F)
  
  
  plot <- plot +
    scale_color_manual(values=c("FALSE"="red","TRUE"="black"))+
    scale_fill_gradient(low="white",high="black",limits=color_limits,na.value = "white",name = "Coverage rate")+
    facet_wrap(vars(comb_name),ncol=2)
  
  return(plot)
}



df <- readRDS(file = "./simulations/data_sim/df_CentM_InclRates_LossComb_20240328.rds")
# note: test_level = 0.1

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





# Power main paper:  ------------------------------------------------------

# + Loss combinations: main plot -----------------------------

comb_choice <-  c("Mean Forecasts", "Median Forecasts", "Mode Forecasts",
                  "Mean-Mode loss combination", "Median-Mode loss combination", "Mean-Median-Mode loss combination")

df |>  
  filter(comb_name %in% comb_choice,
         inst %in% c("(1,x)"),
         n %in% c(5000),
         dgp %in% c("Hom"),
         skew %in% c(0.5)) |> 
   plot.mcs(size=3.5,
            label.padding = unit(0.1, "lines")) + guides(fill=FALSE)

ggsave("simulations/output/CentralityMeasures/CoverageRates/Coverage_Hom_skew05.pdf",
       width = 8,height = 9)


# + Loss combinations: AR-GARCH -----------------------------

comb_choice <-  c("Mean Forecasts", "Median Forecasts", "Mode Forecasts",
                  "Mean-Mode loss combination", "Median-Mode loss combination", "Mean-Median-Mode loss combination")

df |>  
  filter(comb_name %in% comb_choice,
         inst %in% c("(1,x)"),
         n %in% c(5000),
         dgp %in% c("AR-GARCH"),
         skew %in% c(0.5)) |> 
  plot.mcs(size=3.5,
           label.padding = unit(0.1, "lines")) + guides(fill=FALSE)

ggsave("simulations/output/CentralityMeasures/CoverageRates/Coverage_AR-GARCH_skew05.pdf",
       width = 8,height = 9)


# + Loss combinations: symmetric  -----------------------------

comb_choice <-  c("Mean Forecasts", "Median Forecasts", "Mode Forecasts",
                  "Mean-Mode loss combination", "Median-Mode loss combination", "Mean-Median-Mode loss combination")

df |>  
  filter(comb_name %in% comb_choice,
         inst %in% c("(1,x)"),
         n %in% c(5000),
         dgp %in% c("Hom"),
         skew %in% c(0)) |> 
  plot.mcs(size=3.5,
           label.padding = unit(0.1, "lines")) + guides(fill=FALSE)

ggsave("simulations/output/CentralityMeasures/CoverageRates/Coverage_Hom_skew0.pdf",
       width = 8,height = 9)



# + Loss combinations: small sample -----------------------------

comb_choice <-  c("Mean Forecasts", "Median Forecasts", "Mode Forecasts",
                  "Mean-Mode loss combination", "Median-Mode loss combination", "Mean-Median-Mode loss combination")

df |>  
  filter(comb_name %in% comb_choice,
         inst %in% c("(1,x)"),
         n %in% c(500),
         dgp %in% c("Hom"),
         skew %in% c(0.5)) |> 
  plot.mcs(size=3.5,
           label.padding = unit(0.1, "lines")) + guides(fill=FALSE)

ggsave("simulations/output/CentralityMeasures/CoverageRates/Coverage_Hom_skew05_n500.pdf",
       width = 8,height = 9)






# + Loss combinations2 -----------------------------

comb_choice <-  c("Mean Forecasts", "Median Forecasts", "Mode Forecasts",
                  "Mean-Median-Mode loss combination")


df |>  
  filter(comb_name %in% comb_choice,
         inst %in% c("(1,x)"),
         n %in% c(5000),
         dgp %in% c("Hom"),
         skew %in% c(0.5)) |> 
  plot.mcs(size=3.5,
           label.padding = unit(0.1, "lines"))+guides(fill=FALSE)

ggsave("simulations/output/CentralityMeasures/CoverageRates/mcs_comb_loss2.pdf",
       width = 8,height = 6)






# Appendix ----------------------------------------------------------------


# + forecaster combination --------------------------------------------------

df <- readRDS(file = "./simulations/data_sim/df_CentM_InclRates_GammaComb_20240328.rds")

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



comb_choice <-c("Mean and Median Forecasts", "Mean and Mode Forecasts",
"Median and Mode Forecasts")

dfs <- df |>  
  filter(comb_name %in% comb_choice,
         inst %in% c("1", "(1,x)"),
         n %in% c(5000),
         dgp %in% c("Hom50"),
         skew %in% c(0.5)) 

dfs |> 
  mutate(
    inst = factor(inst, levels = c("1","(1,x)"))) |> 
  plot.mcs(size=3.5,
           label.padding = unit(0.1, "lines"))+guides(fill=FALSE)+
  facet_grid(comb_name~inst)

ggsave("simulations/output/CentralityMeasures/CoverageRates/mcs_comb_forec.pdf",
       width = 8,height = 8)




# + less ------------------------------------------------------------------

comb_choice <-c("Mean and Median Forecasts")

dfs <- df |>  
  filter(comb_name %in% comb_choice,
         inst %in% c("1", "(1,x)"),
         n %in% c(5000),
         dgp %in% c("Hom50"),
         skew %in% c(0.5)) 

dfs |> 
  mutate(comb_name = inst,
         comb_name = factor(comb_name, levels = c("1","(1,x)"))) |> 
  plot.mcs(size=3.5,
           label.padding = unit(0.1, "lines"))+guides(fill=FALSE)

ggsave("simulations/output/CentralityMeasures/CoverageRates/mcs_comb_forec2.pdf",
       width = 8,height = 3)



 

