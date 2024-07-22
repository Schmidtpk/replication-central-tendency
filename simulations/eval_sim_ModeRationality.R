library(tidyverse)
library(tibble)
library(fcrat)



# Load rejection rates
siglevel <- 0.05
df_eval_siglevels <- readRDS("./simulations/data_sim/ModeRat_rejrates_multiple_20240328.rds") %>%
  mutate(bw = bw_factor,
         bw_factor=factor(bw_factor))


## Classical size tables  ------------------------------------------------------
siglevel_names <- c("rejrate01", "rejrate05", "rejrate10")
inst_set <- unique(df_eval_siglevels$inst)
# kernel_set <- unique(df_eval_siglevels$kernel)
kernel_set <- "Gaussian"
dgp_list <- list("main" = c("Hom", "AR-GARCH"),
                 "supplement" = c("Het", "AR"))

n_skew <- length(unique(df_eval_siglevels$skew))

help_ss <- c("$100$", "$500$", "$2000$", "$5000$")
n_samplesize <- length(help_ss)
help_doubledollar <- rep(" ", n_samplesize)

for (siglevel_choice in siglevel_names) {
  for (index_kernel in 1:length(kernel_set)) {
    for (index_dgp in 1:length(dgp_list)) {
      dgp1 <- dgp_list[[index_dgp]][1]
      dgp2 <- dgp_list[[index_dgp]][2]
      
      # Choose the correct significance level
      df_eval_tmp <- df_eval_siglevels %>%
        rename(rejrate = {siglevel_choice})
      
      
      size_print1 <- df_eval_tmp %>%
        dplyr::filter(inst=="1" & dgp==dgp1 & bw_factor==1 & kappa == 0 &  kernel == kernel_set[index_kernel] & misspec=="bias") %>%
        arrange(skew, n) %>%
        mutate(rejrate=100*rejrate) %>%
        pull(rejrate) %>%
        matrix(ncol=n_skew, nrow=n_samplesize) %>%
        format(nsmall=1, digits=1)
      
      size_print2 <- df_eval_tmp %>%
        dplyr::filter(inst=="(1,x)" & dgp==dgp1 & bw_factor==1 & kappa == 0 &  kernel == kernel_set[index_kernel] & misspec=="bias") %>%
        arrange(skew, n) %>%
        mutate(rejrate=100*rejrate) %>%
        pull(rejrate) %>%
        matrix(ncol=n_skew, nrow=n_samplesize) %>%
        format(nsmall=1, digits=1)
      
      size_print3 <- df_eval_tmp %>%
        dplyr::filter(inst=="1" & dgp==dgp2 & bw_factor==1 & kappa == 0 &  kernel == kernel_set[index_kernel] & misspec=="bias") %>%
        arrange(skew, n) %>%
        mutate(rejrate=100*rejrate) %>%
        pull(rejrate) %>%
        matrix(ncol=n_skew, nrow=n_samplesize) %>%
        format(nsmall=1, digits=1)
      
      size_print4 <- df_eval_tmp %>%
        dplyr::filter(inst=="(1,x)" & dgp==dgp2 & bw_factor==1 & kappa == 0 &  kernel == kernel_set[index_kernel] & misspec=="bias") %>%
        arrange(skew, n) %>%
        mutate(rejrate=100*rejrate) %>%
        pull(rejrate) %>%
        matrix(ncol=n_skew, nrow=n_samplesize) %>%
        format(nsmall=1, digits=1)
      
      
      size_print <- cbind(help_ss, 
                          help_doubledollar, size_print1, help_doubledollar, size_print2,
                          help_doubledollar, size_print3, help_doubledollar, size_print4)
      
      write.table(size_print, file=paste0("simulations/output/ModeRationality/size/size_dgp",names(dgp_list[index_dgp]),"_",siglevel_choice,"_kernel",kernel_set[index_kernel],".txt"), 
                  row.names=FALSE, col.names=FALSE, sep = " & ", quote=FALSE, eol="\\\\\n")
    }
  }
}



# For the following, only consider rejection rates at the 5% level:
df_eval <- df_eval_siglevels %>%
  dplyr::select(-c(rejrate01, rejrate10)) %>%
  dplyr::rename(rejrate = rejrate05)


## Bandwidth size tables  ------------------------------------------------------
inst_set <- unique(df_eval$inst)
dgp_set <- unique(df_eval$dgp)
bw_set <- unique(df_eval$bw_factor)
skew_set <- unique(df_eval$skew)
kernel_set <- "Gaussian"

n_bw <- length(bw_set)


help_ss <- c("$100$", "$500$", "$2000$", "$5000$")
# help_ss <- c("$100$", "$500$")
n_samplesize <- length(help_ss)
help_doubledollar <- rep(" ", n_samplesize)

for (index_dgp in 1:length(dgp_set)) {
  for (index_kernel in 1:length(kernel_set)) {
    
    size_print_list <- list()
    for (index_skew in 1:length(skew_set)) {
      
      skew_choice <- skew_set[index_skew]
               
      size_print_list[[index_skew]] <- df_eval %>%
        dplyr::filter(inst=="(1,x)" & skew==skew_choice & dgp==dgp_set[index_dgp] & kappa == 0 &  kernel == kernel_set[index_kernel] & misspec=="bias") %>%
        arrange(bw_factor, n) %>%
        mutate(rejrate=100*rejrate) %>%
        pull(rejrate) %>%
        matrix(ncol=n_bw, nrow=n_samplesize) %>%
        format(nsmall=1, digits=1)
    }
    
    # Continue here if size tables are desired for the bandwidth choice!!!
    
      size_print <- cbind(help_ss, help_doubledollar, size_print_list[[1]], help_doubledollar, size_print_list[[3]], help_doubledollar, size_print_list[[4]])

    write.table(size_print, file=paste0("simulations/output/ModeRationality/size/size_bandwidth_dgp",dgp_set[index_dgp],"_kernel",kernel_set[index_kernel],".txt"), row.names=FALSE, col.names=FALSE, sep = " & ", quote=FALSE, eol="\\\\\n")
    
  }
}






##  Standard Power Plots for the Mode Rationality Test ------------------------------------------------------
plot_width <- 8
plot_height <-  4.5

DGP.labs <- c("iid", "Het", "AR", "AR-GARCH")
names(DGP.labs) <- c("iid", "Het", "AR", "AR-GARCH")

dgp_list <- list("main" = c("Hom", "AR-GARCH"),
                 "supplement" = c("Het", "AR"))

misspec_set <- unique(df_eval$misspec)
inst_set <- unique(df_eval$inst)


for (index_dgp in 1:length(dgp_list)) {
  dgps_choice <- dgp_list[[index_dgp]]
  
  for (index_misspec in 1:length(misspec_set)) {
    if (index_misspec == 1){kappa_points <- seq(-0.5,0.5,length.out=5)} 
    else {kappa_points <- seq(0,2,length.out=5)} 
    
    for (index_inst in 1:length(inst_set)) {
      siglevel_plot <- 0.05
      
      df_sub <- df_eval %>%
        dplyr::filter(kernel=="Gaussian",
                      dgp %in% dgps_choice,
                      misspec==misspec_set[index_misspec],
                      inst == inst_set[index_inst], 
                      bw_factor==1)  %>% 
        rename("skewness" = "skew")
      
      
      df_sub$n <- factor(df_sub$n, 
                         levels=c("100","500","2000","5000"), 
                         labels = c("T = 100", "T = 500", "T = 2000", "T = 5000"))
      df_sub$skewness <- as.factor(df_sub$skewness)
      df_sub$dgp <- as.factor(df_sub$dgp)
      df_sub$dgp <- factor(df_sub$dgp,  
                           levels = c("Hom", "Het", "AR", "AR-GARCH"),
                           labels = c("iid", "Het", "AR", "AR-GARCH"))
      
      p <- ggplot(df_sub, aes(x=kappa, y=rejrate)) +
        facet_grid(dgp ~ n, labeller = labeller(dgp = DGP.labs)) +
        theme_bw() +
        theme(panel.spacing.x = unit(4, "mm"),
              strip.background = element_rect(fill = NA,colour = "black"))+
        geom_hline(yintercept=c(siglevel), linewidth=0.5, color="grey40") +
        geom_hline(yintercept=c(0,1), color="black", linewidth=0.5) +
        geom_line(aes(color=skewness)) + 
        # scale_colour_brewer(palette="Set1") +
        theme(legend.position = "bottom") +
        labs(y = "rejection rate", x=expression(paste(kappa))) +
        scale_size_manual(values=5) +
        geom_point(data = subset(df_sub, kappa %in% kappa_points), 
                   aes(color=skewness, shape=skewness))
      
      p
      
      ggsave(paste0("simulations/output/ModeRationality/power/sim_RR_dgp",names(dgp_list[index_dgp]),"_",misspec_set[index_misspec],"_",inst_set[index_inst],".pdf"), 
           plot=p, width=plot_width, height=plot_height, units="in")
    }
  }
}







## Bandwidth Power Plots for the Mode Rationality Test ------------------------------------------------------
plot_width <- 8
plot_height <- 4.5

# significance levels
siglevel_set <- c(0.05)
m_siglevel <- length(siglevel_set)
siglevel_names <- c(5)
index_siglevel <- 1

DGP.labs <- c("iid", "Het", "AR", "AR-GARCH")
names(DGP.labs) <- c("iid", "Het", "AR", "AR-GARCH")

misspec_set <- unique(df_eval$misspec)
inst_set <- unique(df_eval$inst)

for (index_misspec in 1:length(misspec_set)) {
  if (index_misspec == 1){kappa_points <- seq(-0.5,0.5,length.out=5)} 
  else {kappa_points <- seq(0,2,length.out=5)} 
  
  for (index_inst in 1:length(inst_set)) {
    
    df_sub <- df_eval %>%
      dplyr::filter(kernel=="Gaussian",
                    dgp %in% c("Hom", "AR-GARCH"),
                    misspec==misspec_set[index_misspec],
                    inst == inst_set[index_inst], 
                    n %in% c(500))  %>% 
      rename("skewness" = "skew")
    
    df_sub$n <- factor(df_sub$n, 
                       levels=c("100","500","2000","5000"), 
                       labels = c("T = 100", "T = 500", "T = 2000", "T = 5000"))
    
    df_sub$skewness <- as.factor(df_sub$skewness)
    df_sub$skewness <- factor(df_sub$skewness, 
                       levels=c("0","0.1","0.25","0.5"), 
                       labels = c("gamma == 0", "gamma == 0.1", "gamma == 0.25", "gamma == 0.5"))
    
    df_sub$dgp <- factor(df_sub$dgp,  
                         levels = c("Hom", "Het", "AR", "AR-GARCH"),
                         labels = c("iid", "Het", "AR", "AR-GARCH"))
    
    p <- ggplot(df_sub, aes(x=kappa, y=rejrate)) +
      facet_grid(dgp ~ skewness, labeller = label_parsed) +
      theme_bw() +
      theme(panel.spacing.x = unit(4, "mm"),
            strip.background = element_rect(fill = NA,colour = "black"))+
      geom_hline(yintercept=c(siglevel_set[index_siglevel]), linewidth=0.5, color="grey40") +
      geom_hline(yintercept=c(0,1), color="black", linewidth=0.5) +
      geom_line(aes(color=bw_factor)) + 
      scale_color_manual(values=c("#F4A582", "#B2182B",  "black", "#2166AC", "#92C5DE")) +
      theme(legend.position = "bottom") +
      labs(y = "rejection rate", x=expression(paste(kappa))) +
      geom_point(data = subset(df_sub, kappa %in% kappa_points), 
                 aes(color=bw_factor, shape=bw_factor)) +
      labs(color='Bandwidth factor', shape='Bandwidth factor') 
    
    p
    
    ggsave(paste0("simulations/output/ModeRationality/power/sim_RR_bandwidth_",misspec_set[index_misspec],"_",inst_set[index_inst],".pdf"), 
           plot=p, width=plot_width, height=plot_height, units="in")
  }
}








## Kernel!!! Power Plots for the Mode Rationality Test ------------------------------------------------------
df_sub <- df_eval %>%
  dplyr::filter(dgp %in% c("Hom"),
                misspec=="bias",
                inst == "(1,x)",
                n %in% c(500,2000))  %>% 
  rename("skewness" = "skew")

# For the bias setup:
kappa_points <- seq(-0.5,0.5,length.out=5)


df_sub$n <- factor(df_sub$n, 
                   levels=c("100","500","2000","5000"), 
                   labels = c("T == 100", "T == 500", "T == 2000", "T == 5000"))

df_sub$skewness <- as.factor(df_sub$skewness)
df_sub$skewness <- factor(df_sub$skewness, 
                          levels=c("0","0.1","0.25","0.5"), 
                          labels = c("gamma == 0", "gamma == 0.1", "gamma == 0.25", "gamma == 0.5"))

df_sub$dgp <- as.factor(df_sub$dgp)
df_sub$dgp <- factor(df_sub$dgp, levels = c("Hom", "Het", "AR", "AR-GARCH"))

df_sub$kernel <- factor(df_sub$kernel, 
                   levels=c("BiSquare","Gaussian"), 
                   labels = c("Biweight","Gaussian"))


p <- ggplot(df_sub, aes(x=kappa, y=rejrate)) +
  facet_grid(n ~ skewness, labeller = label_parsed) +
  theme_bw() +
  theme(panel.spacing.x = unit(4, "mm"),
        strip.background = element_rect(fill = NA,colour = "black"))+
  geom_hline(yintercept=c(siglevel_set[index_siglevel]), linewidth=0.5, color="grey40") +
  geom_hline(yintercept=c(0,1), color="black", linewidth=0.5) +
  geom_line(aes(color=bw_factor, linetype=kernel)) + 
  scale_color_manual(values=c("#F4A582", "#B2182B",  "black", "#2166AC", "#92C5DE")) +
  theme(legend.position = "bottom") +
  labs(y = "rejection rate", x=expression(paste(kappa))) +
  geom_point(data = subset(df_sub, kappa %in% kappa_points), 
             aes(color=bw_factor, shape=bw_factor)) +
  labs(color='Bandwidth factor', shape='Bandwidth factor') 

p

ggsave(paste0("simulations/output/ModeRationality/power/sim_RR_kernels.pdf"), 
       plot=p, width=plot_width, height=plot_height, units="in")

