# adaptation from main file "submission revision.R" with clustered standard errors for SCE
library(fcrat)
library(tidyverse)

join_different_results <- function(res, yname = NULL, xname=NULL)
{
  
  res_final <- NULL
  for(i in 1:length(res))
  {
    res.cur <- res[[i]]

    if(!is.null(yname))
      res.cur$data$yname = yname[i]
    
    if(!is.null(xname))
      res.cur$data$xname = xname[i]
    
    if(is.null(res_final))
      res_final <- res.cur
    else
      res_final$data <- rbind(res_final$data,res.cur$data)
  }
  
  if(!is.null(yname))
    res_final$data$yname <- factor(res_final$data$yname,levels = unique(yname))
  
  if(!is.null(xname))
    res_final$data$xname <- factor(res_final$data$xname,levels = unique(xname))
  
  return(res_final)
}


# helper function for clustered covariance estimation
cluster <- function(ids,...)
{ 
  if(exists("cluster_groups"))
    warning("Use cluster_groups variable from global environment.")
  else
    stop("Variable cluster_groups not defined in global environment.")
  
  if(is.matrix(ids))
  {
    n <- nrow(ids)
  } else
  {
    n <- length(ids)
  }
  list(cov= sandwich::meatCL(lm(ids~1), type = "HC0",cluster = cluster_groups,...), name = "cluster")
}




# SCE ---------------------------------------------------------------------
### compute round
df <- sce |> 
  filter(filter1&filter3)|> 
  group_by(userid) |> 
  arrange(date, .by_group=TRUE) |> 
  mutate(n = n(),
         i = row_number())

df <- df |> 
  mutate(
    catround=ifelse(i==2,'second',
                    ifelse(n==1,'once','first')),
    month = lubridate::month(rdate)) |> 
  ungroup() |> 
  filter(!is.na(catround),
         !is.na(expectation),!is.na(income)) |> 
  as.data.frame() |> 
  mutate(change=expectation-lagincome) 

instr_matrix_description <- c("1","X")

dat.cur.filtered <- df



# + main plot ------------------------------------------------------------------

# old
plot(test_convex(dat.cur.filtered, vcov, fcrat::iid,
                  instruments =  list(
                    c("1"),c("1","X")
                  ) ))


# clustered by wave
cluster_groups <- dat.cur.filtered$date
plot(test_convex(dat.cur.filtered, vcov = cluster,
                 instruments =  list(
                   c("1"),c("1","X")
                 ) ))
ggsave("./applications/plots/revision/Rsce_newC.pdf",
      width=6, height = 3)


# clustered by individual
cluster_groups <- dat.cur.filtered$userid
plot(test_convex(dat.cur.filtered, vcov = cluster,
                         instruments =  list(
                           c("1"),c("1","X")
                         ) ))

# clustered by wave+individual
cluster_groups <- dat.cur.filtered%>%select(date,userid)
plot(test_convex(dat.cur.filtered, vcov = cluster,
                         instruments =  list(
                           c("1"),c("1","X")
                         ) ))

# + by lag income -------------------------------------------------------

var.cur <- "lagincome"

quant1 <- quantile(dat.cur.filtered[,var.cur],probs = 1/3,na.rm = TRUE)
quant2 <- quantile(dat.cur.filtered[,var.cur],probs = 2/3,na.rm = TRUE)

cat("\n", quant1, " ",quant2)
cat("\n", var.cur,": lower",mean(dat.cur.filtered[,var.cur]<quant1,na.rm=TRUE), " upper: ", mean(dat.cur.filtered[,var.cur]>quant2,na.rm=T))

nas.cur <- is.na(dat.cur.filtered[,var.cur])

dim(dat.cur.filtered[dat.cur.filtered[,var.cur]<quant1 & !nas.cur,])
dim(dat.cur.filtered[dat.cur.filtered[,var.cur]>=quant1 & dat.cur.filtered[,var.cur]<quant2 & !nas.cur,])
dim(dat.cur.filtered[dat.cur.filtered[,var.cur]>=quant2 & !nas.cur,])

dat.list <- list()
dat.list[[1]] <- dat.cur.filtered[dat.cur.filtered[,var.cur]<quant1& !nas.cur,]
dat.list[[2]] <- dat.cur.filtered[dat.cur.filtered[,var.cur]>=quant1 & dat.cur.filtered[,var.cur]<quant2 & !nas.cur,]
dat.list[[3]] <- dat.cur.filtered[dat.cur.filtered[,var.cur]>=quant2 & !nas.cur,]

res.list <- list()
for(i in 1:length(dat.list)){
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description)
}
plot(
  join_different_results(
    res.list,
    xname = c("low","middle","high")))+facet_grid(1~xname)


res.list <- list()
for(i in 1:length(dat.list)){
  cluster_groups <- dat.list[[i]]$date
  res.list[[i]]<- test_convex(dat.list[[i]], 
                         instruments =  instr_matrix_description,
                         vcov = cluster)
}
plot(
  join_different_results(
    res.list,
    xname = c("low","middle","high")))+facet_grid(1~xname)
ggsave(paste0("./applications/plots/revision/Rsce_by_terciles_",var.cur,"C.pdf"),
      width=9, height = 3)

# + income vs age ------------------------------------------------------

dats <- dat.cur.filtered %>% filter(!is.na(age_cat) & !is.na(ss_highincome))
dats$ss_age <- dats$age_cat=="Under 40"
table(dats$ss_age,dats$ss_highincome,dnn = c("ss_age","income"))


dat.list <- list()
dat.list[[1]] <- dats %>% filter(ss_highincome==TRUE,ss_age==TRUE)
dat.list[[2]] <- dats %>% filter(ss_highincome==TRUE,ss_age==FALSE)
dat.list[[3]] <- dats %>% filter(ss_highincome==F,ss_age==TRUE)
dat.list[[4]] <- dats %>% filter(ss_highincome==F,ss_age==F)

# non clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description)
}
res.cur <- join_different_results(res.list,
                                  xname = c("high income","high income", "low income", "low income"),
                                  yname = c("under 40","over 40","under 40","over 40")
)
res.cur$data$xname <- factor(res.cur$data$xname, levels=c("low income","high income"))
plot(res.cur)+facet_grid(xname~yname)


# clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  cluster_groups <- dat.list[[i]]$date
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description,
                              vcov = cluster)
}
res.cur <- join_different_results(res.list,
  xname = c("high income","high income", "low income", "low income"),
  yname = c("under 40","over 40","under 40","over 40")
)
res.cur$data$xname <- factor(res.cur$data$xname, levels=c("low income","high income"))
plot(res.cur)+facet_grid(xname~yname)
ggsave("./applications/plots/revision/Rscelag_2by2_income_agebC.pdf",
      width=6.5, height = 5)




# + income vs privat ------------------------------------------------------

dats <- dat.cur.filtered %>% filter(!is.na(ss_privat) & !is.na(ss_highincome))

table(dats$privat,dats$ss_highincome,dnn = c("private","income"))


dat.list <- list()
dat.list[[1]] <- dats %>% filter(ss_highincome==TRUE,ss_privat==TRUE)
dat.list[[2]] <- dats %>% filter(ss_highincome==TRUE,ss_privat==FALSE)
dat.list[[3]] <- dats %>% filter(ss_highincome==F,ss_privat==TRUE)
dat.list[[4]] <- dats %>% filter(ss_highincome==F,ss_privat==F)

# non clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description)
}
res.cur <- join_different_results(res.list,
                                  xname = c("high income","high income", "low income", "low income"),
                                  yname = c("private","non private","private","non private")
)
res.cur$data$xname <- factor(res.cur$data$xname, levels=c("low income","high income"))
plot(res.cur)+facet_grid(xname~yname) 




# clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  cluster_groups <- dat.list[[i]]$date
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description,
                              vcov = cluster)
}
res.cur <- join_different_results(res.list,
                                  xname = c("high income","high income", "low income", "low income"),
                                  yname = c("private","non private","private","non private")
)
res.cur$data$xname <- factor(res.cur$data$xname, levels=c("low income","high income"))
plot(res.cur)+facet_grid(xname~yname)
ggsave("./applications/plots/revision/Rscelag_2by2_income_privatC.pdf",
      width=6.5, height = 5)






# + income vs offer ---------------------------------------------------------

dats <- dat.cur.filtered %>% filter(!is.na(ss_jobofferspast) & !is.na(ss_highincome))

table(dats$ss_jobofferspast,dats$ss_highincome,dnn = c("job offers","income"))

dat.list <- list()
dat.list[[1]] <- dats %>% filter(ss_highincome==TRUE,ss_jobofferspast==TRUE)
dat.list[[2]] <- dats %>% filter(ss_highincome==TRUE,ss_jobofferspast==FALSE)
dat.list[[3]] <- dats %>% filter(ss_highincome==F,ss_jobofferspast==TRUE)
dat.list[[4]] <- dats %>% filter(ss_highincome==F,ss_jobofferspast==F)

# non clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description)
}
res.cur <- join_different_results(res.list,
                                  xname = c("high income","high income", "low income", "low income"),
                                  yname = c("offer","no offer","offer","no offer")
)
res.cur$data$xname <- factor(res.cur$data$xname, levels=c("low income","high income"))
plot(res.cur)+facet_grid(xname~yname)



# clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  cluster_groups <- dat.list[[i]]$date
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description,
                              vcov = cluster)
}
res.cur <- join_different_results(res.list,  
                                  xname = c("high income","high income", "low income", "low income"),
                                  yname = c("offer","no offer","offer","no offer")
)
res.cur$data$xname <- factor(res.cur$data$xname, levels=c("low income","high income"))
plot(res.cur)+facet_grid(xname~yname)
ggsave("./applications/plots/revision/Rscelag_2by2_income_offerpastC.pdf",
      width=6.5, height = 5)







# + round -------------------------------------------------------------------

dat.list <- list()
dat.list[[1]] <- df |> filter(catround%in%c('once','first'))
dat.list[[2]] <- df |> filter(catround%in%c('second'))


# no cluster
res.list <- list()
for(i in 1:length(dat.list)){
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description)
}
res.cur <- join_different_results(res.list,  
                                  xname = c("first","second"))
plot(res.cur)+facet_grid(1~xname)

# clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  cluster_groups <- dat.list[[i]]$date
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description,
                              vcov = cluster)
}
res.cur <- join_different_results(res.list,  
                                  xname = c("first","second"))
plot(res.cur)+facet_grid(1~xname)
ggsave("./applications/plots/revision/Rsce_by_roundC.pdf",
       width=6, height = 3)



# + income vs round ---------------------------------------------------------

dfs <- df

dfs.inc.round <- dfs |> mutate(ss1= ss_highincome,
                               ss2= catround %in% c('once','first'))
ss1.name <- c('high income','low income')
ss2.name <- c('first','second')


table(dfs.inc.round |> select(ss1,ss2))

dat.list <- list()
dat.list[[1]] <- dfs.inc.round %>% filter(ss1==F,ss2==TRUE)
dat.list[[2]] <- dfs.inc.round %>% filter(ss1==F,ss2==FALSE)
dat.list[[3]] <- dfs.inc.round %>% filter(ss1==T,ss2==TRUE)
dat.list[[4]] <- dfs.inc.round %>% filter(ss1==T, ss2==FALSE)
  

# non clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description)
}
res.cur <- join_different_results(
  res.list,
  xname = c(ss1.name[2],ss1.name[2], ss1.name[1], ss1.name[1]),
  yname = c(ss2.name[1],ss2.name[2], ss2.name[1], ss2.name[2])
)
plot(res.cur)+facet_grid(xname~yname)


# clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  cluster_groups <- dat.list[[i]]$date
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description,
                              vcov = cluster)
}
res.cur <- join_different_results(
  res.list,
  xname = c(ss1.name[2],ss1.name[2], ss1.name[1], ss1.name[1]),
  yname = c(ss2.name[1],ss2.name[2], ss2.name[1], ss2.name[2])
)
plot(res.cur)+facet_grid(xname~yname)
ggsave("./applications/plots/revision/Rscelag_2by2_income_roundC.pdf",width=6.5, height = 5)


# + income vs abs(error) ----------------------------------------------------

dfs <- df
dfs$t <- as.numeric(factor(rank(dfs$rdate)))
dfs <- dfs |> 
  group_by(userid) |> 
  mutate(
    lagerror = lag(income-expectation,order_by = t),
    abserror = abs(lagerror)/lag(expectation,order_by = t)
  ) |> ungroup() |> 
  filter(!is.na(lagerror),!is.na(income),!is.na(expectation),!is.na(abserror)) |> 
  ungroup() |> as.data.frame()



dfs.inc.aerr <- dfs |> mutate(ss2= abserror<=median(abserror),
                              ss1= ss_highincome)
ss2.name <- c('small error','large error')
ss1.name <- c('low income','high income')


table(dfs.inc.aerr |> select(ss1,ss2))

dat.list <- list()
dat.list[[1]] <- dfs.inc.aerr %>% filter(ss1==F,ss2==TRUE)
dat.list[[2]] <- dfs.inc.aerr %>% filter(ss1==F,ss2==FALSE)
dat.list[[3]] <- dfs.inc.aerr %>% filter(ss1==T,ss2==TRUE)
dat.list[[4]] <- dfs.inc.aerr %>% filter(ss1==T, ss2==FALSE)



# non clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description)
}
res.cur <- join_different_results(
  res.list,
  xname = c(ss1.name[1],ss1.name[1], ss1.name[2], ss1.name[2]),
  yname = c(ss2.name[1],ss2.name[2], ss2.name[1], ss2.name[2])
)
plot(res.cur)+facet_grid(xname~yname)


# clustered se
res.list <- list()
for(i in 1:length(dat.list)){
  cluster_groups <- dat.list[[i]]$date
  res.list[[i]]<- test_convex(dat.list[[i]], 
                              instruments =  instr_matrix_description,
                              vcov = cluster)
}
res.cur <- join_different_results(
  res.list,
  xname = c(ss1.name[1],ss1.name[1], ss1.name[2], ss1.name[2]),
  yname = c(ss2.name[1],ss2.name[2], ss2.name[1], ss2.name[2])
)
plot(res.cur)+facet_grid(xname~yname)
ggsave("./applications/plots/revision/Rscelag_2by2_income_abserrorC.pdf",width=6.5, height = 5)





