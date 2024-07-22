library(tidyverse)
library(tibble)
library(reshape2)
library(sn)
library(foreach)
library(doParallel)
library(abind)
library(fcrat)

source('./simulations/functions_DGP.R', echo=FALSE)

################################################################################################
# Mode Forecast Rationality Test under H0

# Set options
MC_n <- 2000  
n_set <- c(100,500,2000,5000)


vcov_method = iid

skewness_coef <- c(0, 0.1, 0.25, 0.5)
m_skew <- length(skewness_coef)

m_kappa <- 41
kappa_set_bias <- seq(-0.5,0.5,length.out=m_kappa)
kappa_set_noise <- seq(0,2,length.out=m_kappa)
kappa_descr <- 1:m_kappa

instr_descr <- c("1", "(1,x)")
m_instr <- length(instr_descr)


kernel_descr <- c("Gaussian", "BiSquare")
m_kernel <- length(kernel_descr)


# DGPs
DGP_list <- list(
  "Hom" = list(type="regression", theta=c(1,1,1,1, 1,0), sigma_seq=c(1,1)),
  "Het" = list(type="regression", theta=c(1,1,1,1, 1,0), sigma_seq=c(0.5,2)),
  "AR" = list(type="AR-GARCH", theta= c(0.5,1,0,0), sigma_seq=c(1,1)),
  "AR-GARCH" = list(type="AR-GARCH", theta= c(0.5,0.1,0.8,0.1), sigma_seq=c(1,1))
)
m_dgp <- length(DGP_list)

misspec_descr <- c("bias", "noise")
m_misspec <- length(misspec_descr)

bw_factor_list <- c(0.5, 0.75, 1, 1.5, 2)

# the last skewness entry of coef_CP is modified later on anyways in the loop
coef_CP <- c(0, 1, 0.25)


# Make Cluster
cl <- makeCluster(min(parallel::detectCores()-1, MC_n) )
registerDoParallel(cl)
start_time <- Sys.time()

# Parallel for loop
df_ModeRationality <- foreach(
  i = 1:MC_n,
  .combine=rbind,
  .packages=c("tidyverse", "tibble", "sn", "fcrat"),
  .errorhandling="pass"
)%dopar%{
  
  source('./simulations/functions_DGP.R', echo=TRUE)
  set.seed(i) # set seed for reproducibility
  res_df <- tibble()
  
  for (index_n in 1:length(n_set)){
    n <- n_set[index_n]
    for (index_skew in 1:m_skew) {
      coef_CP[3] <- skewness_coef[index_skew]
      for (index_DGP in 1:m_dgp) {
        DGP_choice <- DGP_list[[index_DGP]]
        
        # Simulate
        dat <- dgp_sn_LossComb(n=n,
                               loss_comb=NA,
                               dgp_type=DGP_choice$type,
                               theta=DGP_choice$theta,
                               sigma_het=seq(DGP_choice$sigma_seq[1], DGP_choice$sigma_seq[2], length.out=n),
                               coef_CP=coef_CP)
        
        n_LargeSample <- 10^5
        dat_LargeSample <- dgp_sn_LossComb(n=n_LargeSample,
                                           loss_comb=NA,
                                           dgp_type=DGP_choice$type,
                                           theta=DGP_choice$theta,
                                           sigma_het=seq(DGP_choice$sigma_seq[1], DGP_choice$sigma_seq[2], length.out=n_LargeSample),
                                           coef_CP=coef_CP)
        
        # Calculate what the sd of the FC is!
        sd_FC <- sd(dat_LargeSample$Y - dat_LargeSample$mode)
        
        for (index_misspec in 1:m_misspec){
          for (index_kappa in 1:m_kappa){
            if (misspec_descr[index_misspec] == "bias") {
              kappa <- kappa_set_bias[index_kappa]
              FC <- dat$mode + kappa*sd_FC
            } else if (misspec_descr[index_misspec] == "noise") {
              kappa <- kappa_set_noise[index_kappa]
              FC <- dat$mode + rnorm(n, mean=0, sd=kappa*sd_FC)
            } else {
              warning("No correct misspecification description")
            }
            
            for (index_inst in 1:m_instr) {
              if (DGP_choice$type == "regression") {
                # Select Instruments for the Cross Sectional DGPs
                if (index_inst == 1) {
                  inst_matrix <- matrix(1,n,1)
                } else if (index_inst == 2) {
                  inst_matrix <- cbind(1, FC)
                } else if (index_inst == 3) {
                  inst_matrix <- cbind(1, FC, dat$W1)
                } else {
                  inst_matrix <- matrix(1,n,1)
                  warning("No correct insturment choice for the Cross Sectional DGP! Test test uses a constant only instead")
                }
              } else if (DGP_choice$type == "AR-GARCH") {
                
                # Select Instruments for the Time Series DGPs
                if (index_inst == 1) {
                  inst_matrix <- matrix(1,n,1)
                } else if (index_inst == 2) {
                  inst_matrix <- cbind(1, FC)
                } else if (index_inst == 3) {
                  inst_matrix <- cbind(1, FC, dat$lag2_Y)
                } else {
                  inst_matrix <- matrix(1,n,1)
                  warning("No correct insturment choice for the Time Series DGP! Test test uses a constant only instead")
                }
              }
              
              for (bw_factor in bw_factor_list){
                bw_PlugIn0 <- fcrat::bw_rule(Y=dat$Y, X=FC)
                
                for (kernel_choice in kernel_descr){
                  if(kernel_choice == "Gaussian"){
                    id_fct_choice = id_modegaussian
                    bw_PlugIn <- bw_PlugIn0
                  }
                  else if (kernel_choice == "BiSquare"){
                    id_fct_choice = id_mode_BiSquare
                    bw_PlugIn <- bw_PlugIn0 * 2.5
                  } 
                  else {
                    stop("Wrong kernel choice")
                  }
                  
                  # mode rationality test
                  res_ModeRat <- tryCatch(rationality.test(Y=dat$Y, X=FC, id.fct = id_fct_choice, instruments = inst_matrix, vcov = vcov_method, bw=bw_factor*bw_PlugIn), 
                                          error=function(e) NULL)
                  
                  # Save results
                  if (!is.null(res_ModeRat)) {
                    pval_df_tmp <- tibble(i_MC=i,
                                          n=n,
                                          kappa=kappa,
                                          inst=instr_descr[index_inst],
                                          dgp=names(DGP_list)[index_DGP],
                                          misspec=misspec_descr[index_misspec],
                                          skew=skewness_coef[index_skew],
                                          kernel=kernel_choice,
                                          bw_factor=bw_factor,
                                          pval=as.numeric(res_ModeRat$pval))
                    
                    res_df <- bind_rows(res_df,
                                        pval_df_tmp)
                    
                  } else {
                    warning( paste0("Mode test failed for index_n=", index_n, "index_skew=", index_skew, ", index_DGP=", index_DGP, "index_misspec=", index_misspec , "index_kappa=", index_kappa, ", index_inst=", index_inst) )
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  res_df
}
stopCluster(cl)
(run_time <- Sys.time() - start_time)

# Save raw simulation results
saveRDS(df_ModeRationality, file = "./simulations/data_sim/ModeRat_20240328.rds")

# Transform to rejection rates
df_eval <- df_ModeRationality %>%
  group_by(n, kappa, inst, dgp, misspec, skew, kernel, bw_factor) %>%
  summarize(rejrate01 = mean(pval <= 0.01, na.rm=TRUE),
            rejrate05 = mean(pval <= 0.05, na.rm=TRUE),
            rejrate10 = mean(pval <= 0.10, na.rm=TRUE),
            MC_rep=n()) %>%
  mutate(bw = bw_factor,
         bw_factor=factor(bw_factor))

# Save rejection rates
saveRDS(df_eval, "./simulations/data_sim/ModeRat_rejrates_multiple_20240328.rds")
