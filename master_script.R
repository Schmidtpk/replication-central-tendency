### master_script.R
# executes the scripts in the correct order

# checks packages and Rversion with instructions to install necessary packages
source("packages.R")

# runs simulation
# do not run - requires parallel computation or long waiting times
if(FALSE){
  source("simulations/sim_CentM_GammaComb_cluster.R")
  source("simulations/sim_CentM_LossComb_cluster.R")
  source("simulations/sim_ModeRat_cluster.R")
}

# generates output from simulations
source("simulations/eval_CentM_RR_size.R")
source("simulations/eval_CentM_RR.R")
source("simulations/eval_sim_ModeRationality.R")

# runs application
source("applications/main applications.R")

# runs robustness check with clustered covariance
source("applications/sce application with clustered covariance.R")