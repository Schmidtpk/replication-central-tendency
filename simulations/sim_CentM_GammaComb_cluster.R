library(tidyverse)
library(tibble)
library(reshape2)
library(sn)
library(foreach)
library(doParallel)
library(abind)
library(fcrat)

source('./simulations/functions_DGP.R', echo=FALSE)


# # setting: regime inst functional -----------------------------------------
MCrep_par <- 2000 
n_set <- c(500,2000,5000)
n_theta_values <- 13
vcov_method <- iid


# Forecast combination types
FC_combination_tbl_full <- tibble(
  type = c("functional", "functional", "functional", 
           "functional", "functional",  "functional", 
           "loss", "loss", "loss", "loss",
           "forecaster",  "forecaster", "forecaster", "forecaster"),
  name =  c("mean", "median", "mode", 
            "functional_mean-median-mode", "functional_mean-median", "functional_mean-mode", 
            "loss_mean-median-mode", "loss_mean-median", "loss_mean-mode", "loss_median-mode", 
            "forecaster_mean-median-mode", "forecaster_mean-median", "forecaster_mean-mode", "forecaster_median-mode"),
  mean =   c(1,0,0, 1/3,1/2,1/2,  1/3,1/2,1/2,0,  1/3,1/2,1/2,0),
  median = c(0,1,0, 1/3,1/2,0  ,  1/3,1/2,0,1/2,  1/3,1/2,0,1/2),
  mode =   c(0,0,1, 1/3,0  ,1/2,  1/3,0,1/2,1/2,  1/3,0  ,1/2,1/2)
)


# Only select cases without loss combination
FC_combination_tbl <- FC_combination_tbl_full[c(11:14),]
m_combination <- nrow(FC_combination_tbl)



# DGPs
DGP_list <- list(
  "Hom01" = list(type="regression", theta=c(1,1,1,1, 1,0), sigma_seq=c(1,1)),
  "Hom05" = list(type="regression", theta=c(1,1,1,1, 5,0), sigma_seq=c(1,1)),
  "Hom10" = list(type="regression", theta=c(1,1,1,1, 10,0), sigma_seq=c(1,1)),
  "Hom20" = list(type="regression", theta=c(1,1,1,1, 20,0), sigma_seq=c(1,1)),
  "Hom50" = list(type="regression", theta=c(1,1,1,1, 50,0), sigma_seq=c(1,1)),
  "AR-GARCH_05" = list(type="AR-GARCH", theta= c(0.5,0.1,0.8,0.1), sigma_seq=c(1,1)),
  "AR-GARCH_02" = list(type="AR-GARCH", theta= c(0.2,0.1,0.8,0.1), sigma_seq=c(1,1)),
  "AR-GARCH_01" = list(type="AR-GARCH", theta= c(0.1,0.1,0.8,0.1), sigma_seq=c(1,1)),
  "AR-GARCH_005" = list(type="AR-GARCH", theta= c(0.05,0.1,0.8,0.1), sigma_seq=c(1,1)),
  "AR-GARCH_00" = list(type="AR-GARCH", theta= c(0,0.1,0.8,0.1), sigma_seq=c(1,1))
)
m_dgp <- length(DGP_list)


# Skewness coefficients
skewness_list <- c(0, 0.25, 0.5)
m_skew <- length(skewness_list)


# Instruments
instr_descr <- c("1", "(1,x)")
inst_list <- c(list(c("const")),
               list(c("1","X")))
m_instr <- length(inst_list)


# Kernels
kernel_descr <- c("Gaussian")
m_kernel <- length(kernel_descr)


# the last skewness entry of coef_CP is modified later on anyways in the loop
coef_CP <- c(0, 1, 0.25)


start_time <- Sys.time()
### replicate outside of the clusters:
# for (j_seq in 1:MCrep_seq) {
#   
# Generate cluster
cl <- makeCluster(min(parallel:::detectCores()-1, MCrep_par) )
registerDoParallel(cl)

# Parallel for loop
df_ConvexComb <- foreach(
  i = 1:MCrep_par,
  .combine=rbind,
  .packages=c("tidyverse", "tibble", "reshape2", "sn", "abind", "fcrat"),
  .errorhandling="pass"
)%dopar%{
  
  source('./simulations/functions_DGP.R', echo=FALSE)
  # set.seed(j_seq*MCrep_seq + i) # set seed for reproducibility
  
  set.seed(i)
  df_pval <- tibble()
  
  for (index_n in 1:length(n_set)){
    n <- n_set[index_n]
    
    for (index_DGP in 1:m_dgp) {
      DGP_choice <- DGP_list[[index_DGP]]
      
      for (index_skew in 1:m_skew) {
        coef_CP[3] <- skewness_list[index_skew]
        
        for (index_convcomb in 1:m_combination){
          type_comb <- FC_combination_tbl$type[index_convcomb]
          name_comb <- FC_combination_tbl$name[index_convcomb]
          comb_weights <- FC_combination_tbl[index_convcomb, c("mean", "median", "mode")] %>% as.numeric()
          
          # Set loss_weights to NA unless (type_comb=="loss") to avoid unnessecary computation time in the DGP function
          if (type_comb=="loss") {loss_weights <- comb_weights} else {loss_weights <- NA}
          
          # Simulate
          dat <- dgp_sn_LossComb(n=n,
                                 loss_comb=loss_weights,
                                 dgp_type=DGP_choice$type,
                                 theta=DGP_choice$theta,
                                 sigma_het=seq(DGP_choice$sigma_seq[1], DGP_choice$sigma_seq[2], length.out=n),
                                 coef_CP=coef_CP,
                                 M = M_losscomb,
                                 burn_in = 500,
                                 iter.loss=3)
          
          FC_mean <- dat$mean
          FC_median <- dat$median
          FC_mode <- dat$mode
          
          # Distinguish combination types
          if (type_comb=="functional"){
            FC <- as.numeric(comb_weights %*% rbind(FC_mean, FC_median, FC_mode))
            
          } else if (type_comb=="forecaster"){
            # Select for each time point either the mean, median or mode forecast depending on the selection probabilities in "comb_weights"
            selection_matrix <- matrix(0, nrow=3, ncol=n)
            for (jj in 1:n){
              selection_matrix[sample(x=1:3, size=1, replace=TRUE, prob=comb_weights), jj] = 1
            }
            FC <- colSums(selection_matrix * rbind(FC_mean, FC_median, FC_mode))
            
          } else if (type_comb=="loss"){
            
            FC <- dat$FC_thetacomb
            
          } else {
            error("Error: No valid combination method selected!")
          }
          
          
          for (index_inst in 1:m_instr) {
            inst_matrix <- inst_list[[index_inst]]
            inst_name <- instr_descr[index_inst]
            
            
            
            # test --------------------------------------------------------------------
            # Apply the test_convex function...
            res_convex <- tryCatch(fcrat::test_convex(data = data.frame(x=FC,
                                                                        y=dat$Y,
                                                                        sigma=sqrt(dat$sigma2)),
                                                      instruments=inst_matrix,
                                                      number_values = n_theta_values,
                                                      vcov = vcov_method), error=function(e) NULL)
            
            if (!is.null(res_convex)) {
              # res_convex_pval[ , index_n, index_skew, index_DGP, index_convcomb, index_inst, 1] <- as.numeric(res_convex$data[,3])
              df_pval <- dplyr::bind_rows(df_pval, 
                                          tibble(i_MC=i, 
                                                 # j_seq=j_seq,
                                                 n=n, 
                                                 dgp=names(DGP_list)[index_DGP],
                                                 skew=coef_CP[3],
                                                 comb_type=type_comb,
                                                 comb_name=name_comb,
                                                 comb_weight_mean=comb_weights[1],
                                                 comb_weight_median=comb_weights[2],
                                                 comb_weight_mode=comb_weights[3],
                                                 index_functional=index_convcomb, 
                                                 inst=inst_name, 
                                                 index_convex_comb=1:length(as.numeric(res_convex$data[,3])),
                                                 pval=as.numeric(res_convex$data[,3])))
            } else {
              warning( paste0("Convex test failed for index_n=", index_n, ", index_DGP=", index_DGP, ", index_convcomb=", index_convcomb, ", index_inst=", index_inst) )
            }
          }
        }
      }
    }
  }
  # }
  
  df_pval
}
stopCluster(cl)

# transform the 55 convex combination numbers to theta1 and theta2 values
# hlp is a variable in order to get the mapping of the counting numbers 1-55 on theta1 and theta2
hlp <- fcrat::test_convex(x=GDP$FC.first, y=GDP$Obs.first, instruments=list(c("1","X")), 
                          number_values = 13)
df_ConvexComb["theta1"] <- hlp$dat[df_ConvexComb$index_convex_comb,1]
df_ConvexComb["theta2"] <- hlp$dat[df_ConvexComb$index_convex_comb,2]

# Save full simulation results
saveRDS(df_ConvexComb, 
        paste0(file = "./simulations/data_sim/df_CentM_Full_GammaComb_20240328.rds"))


# Save aggregated simulation results
test_level <- .1

df_ConvexComb_plot <- df_ConvexComb |> 
  group_by(theta1,theta2,n,inst,dgp,skew,comb_name) |> 
  summarise(inclusion=mean(pval>test_level, na.rm=TRUE))

saveRDS(df_ConvexComb_plot, 
        file = "./simulations/data_sim/df_CentM_InclRates_GammaComb_20240328.rds")


(run_time <- Sys.time() - start_time)
