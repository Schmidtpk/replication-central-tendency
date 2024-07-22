if(!"fcrat" %in% installed.packages()){
  stop("Need to install the package fcrat from github:",
       '\n 
       You can use 
       \n
       #install.packages("devtools") \n
       devtools::install_github("Schmidtpk/fcrat") \n\n
       to install the package or check out the github page at https://github.com/Schmidtpk/fcrat.')
}

# necessary CRAN packages
list.of.packages <- c("PointFore","tidyverse","lubridate","xtable","sandwich","parallel","reshape2","sn","foreach","doParallel","abind","tibble")


if(any(!list.of.packages %in% installed.packages())){
  stop("Need to install the following packages:", list.of.packages[!list.of.packages %in% installed.packages()], 
       "\n 
       You can use 
       \n
       install.packages(list.of.packages[!list.of.packages %in% installed.packages()]) \n
       to install the most recent CRAN versions.")
}

if(R.Version()$version.string!="R version 4.3.3 (2024-02-29 ucrt)"){
  warning("Replication package was run on R version: R version 4.3.3 (2024-02-29 ucrt)")
}


warning("If you want to use the package version at runtime, use the groundhog-based code below based on date '2024-03-28'.
        \n \n
        library(groundhog) \n
        groundhog.library(list.of.packages, date =  '2024-03-28')")

