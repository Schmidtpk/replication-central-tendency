
# The function 'dgp_AR_GARCH_sn_LossComb' simulates outcomes and forecasts from a 
# convex combination of mean / median / (asymptotic) mode loss functions 
dgp_sn_LossComb <- function(n=500, 
                            loss_comb=c(1/3,1/3,1/3), 
                            dgp_type="AR-GARCH",
                            theta=c(0.5,0.1,0.8,0.1), 
                            sigma_het=seq(0.5,2,length.out=n),
                            coef_CP=c(0, 1, 0.5),  
                            M = 2000, 
                            burn_in = 500,
                            iter.loss=3) {
  
  
  # skewed normal parameters
  coef_DP <- cp2dp(coef_CP,family = "SN")
  
  if (dgp_type %in% c("regression")){
    W1 <- rnorm(n, mean = 1, sd = 1)
    W2 <- rnorm(n, mean = -1, sd = 1)
    W3 <- rnorm(n, mean = 2, sd = sqrt(0.1))
    W <- cbind(1,W1,W2,W3)
    
    # Other possibility to determine sigma_het
    location <- as.numeric(theta[1:4] %*% t(W))
    scale <- (theta[5] + theta[6]*abs(W1)) * sigma_het
      
    # skewed normally distributed errors
    u <- rsn(n=n, dp=coef_DP)
    Y <- as.numeric(location + scale * u)
    
    # True mean, median and mode forecasts
    FC_mean <- as.numeric(location + scale*coef_CP[1])
    FC_median <- as.numeric(location + scale*qsn(0.5, dp=coef_DP))
    FC_mode <- as.numeric(location + scale*modeSECdistr(dp=coef_DP, family="SN", object=NULL))
    
    return.df <- tibble(Y=Y, mean=FC_mean, median=FC_median,  mode=FC_mode, 
                        sigma2=scale^2, W1=W1, W2=W2, W3=W3)
  }
  
  
  # Simulate AR-GARCH
  else if (dgp_type %in% c("AR", "AR-GARCH")){
    u <- rsn(n=n+burn_in+1, dp=coef_DP)
    
    sigma2 <- numeric(n+burn_in+1)
    sigma2[1] <- theta[2]/(1-theta[3]-theta[4])
    Y <- numeric(n+burn_in+1)
    Y[1] <- 0
    for (t in 1:(n+burn_in)) {
      sigma2[t+1] <- theta[2] + theta[3]*sigma2[t] + theta[4]*sigma2[t]*u[t]^2
      Y[t+1] <- theta[1]*Y[t] + sqrt(sigma2[t+1])*u[t+1]
    }
    
    # True mean, median and mode forecasts
    lag_Y <- lag(Y)
    FC_mean <-  theta[1]*lag_Y + sqrt(sigma2) * rep(coef_CP[1], n+burn_in+1)
    FC_median <- theta[1]*lag_Y + sqrt(sigma2) * rep(qsn(0.5, dp=coef_DP), n+burn_in+1)
    FC_mode <- theta[1]*lag_Y + sqrt(sigma2) * rep(modeSECdistr(dp=coef_DP, family="SN", object=NULL), n+burn_in+1)
    
    # Truncate everything
    Yinital <- tail(Y,n+1)
    sigma2inital <- tail(sigma2,n+1)
    
    sigma2 <- tail(sigma2,n)
    lag_Y <- tail(lag_Y,n)
    lag2_Y <- tail(lag(Y,2),n)
    Y <- tail(Y,n)
    
    FC_mean <- tail(FC_mean,n)
    FC_median <- tail(FC_median,n)
    FC_mode <- tail(FC_mode,n)
    
    return.df <- tibble(Y=Y, mean=FC_mean, median=FC_median,  mode=FC_mode, 
                        sigma2 = tail(sigma2,n), lag2_Y)
  } else {
    error("No correct dgp_type specified.")
  }
  
  
  ######### Centrality Forecasts by Minimizing Convex Combinations of Loss Functions
  if( any(!is.na(loss_comb))){
    # initial X0 as convex combination of mean, median and mode (functionals)
    X0 <- cbind(FC_mean, FC_median, FC_mode) %*% loss_comb
    
    # Iterate iter.loss (e.g., 3) times
    for (j in 1:iter.loss){
      bw <- fcrat::bw_rule(Y=Y, X=as.numeric(X0))
      
      ids_mean <- fcrat::id_mean(Y=Y, X=X0)
      ids_median <- fcrat::id_median(Y=Y, X=X0)
      ids_mode <- fcrat::id_modegaussian(Y=Y, X=X0, bw=bw)
      
      w0_mean <- 1/as.numeric(sqrt(fcrat::iid(ids_mean)$cov))
      w0_median <- 1/as.numeric(sqrt(fcrat::iid(ids_median)$cov))
      w0_mode <- 1/as.numeric(sqrt(fcrat::iid(ids_mode)$cov))
      
      loss_ConvexComb <- function(X,Y,weights){
        mean( weights[1] * w0_mean * 0.5*(Y-X)^2 + 
                weights[2] * w0_median * abs(Y-X) + 
                weights[3] * w0_mode * (-dnorm((X-Y)/bw)) ) 
      }
      
      X0_optim <- rep(NA, n)
      
      # Optimize at each time step
      for (tt in 1:(length(Y))){
        if (dgp_type %in% c("regression")){
          resid <- rsn(M, dp=coef_DP)
          location_tt <- as.numeric(theta[1:4] %*% (W[tt,]))
          scale_tt <- (theta[5] + theta[6]*abs(W1[tt])) * sigma_het
          Ytt_sim <- location_tt + scale_tt * as.numeric(resid)
        } else if (dgp_type %in% c("GARCH", "AR-GARCH")){
          resid <- rsn(M, dp=coef_DP)
          Ytt_sim <- theta[1]*Yinital[tt] + sqrt(sigma2inital[tt+1]) * as.numeric(resid)
        }
        
        X0tt <- X0[tt]
        X0_optim[tt] <- optim(X0tt, loss_ConvexComb, method="Brent", lower=min(Ytt_sim), upper=max(Ytt_sim), 
                              Y=Ytt_sim, weights=c(loss_comb[1], loss_comb[2], loss_comb[3]))$par
      }
      
      X0 <- X0_optim
    }
    
    X0
    return.df <- return.df %>% mutate(FC_thetacomb=X0)
    
  }
  
  ########
  return(return.df)
}





# ### 
# n <- 1000
# 
# test1 <- dgp_sn_LossComb(n=n,
#                          loss_comb=c(1/3,1/3,1/3),
#                          dgp_type="regression",
#                          theta=c(1,1,1,1),
#                          sigma_het=seq(0.5,2,length.out=n),
#                          coef_CP=c(0, 1, 0.5),
#                          M = 1000,
#                          burn_in = 500,
#                          iter.loss=3)
# 
# 
# test2 <- dgp_sn_LossComb(n=n,
#                          loss_comb=c(1/3,1/3,1/3),
#                          dgp_type="AR-GARCH",
#                          theta=c(0.5,0.1,0.8,0.1),
#                          sigma_het=seq(0.5,2,length.out=n),
#                          coef_CP=c(0, 1, 0.5),
#                          M = 1000,
#                          burn_in = 500,
#                          iter.loss=3)


 

