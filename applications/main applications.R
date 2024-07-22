library(fcrat)
library(tidyverse)

# function helper for plotting
# - res: list of data.frame with estimation results
# - yname: list of names plotted on y axis
# - xname: list of names plotted on x axis
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


# SCE ---------------------------------------------------------------------
# data ---------------------------------------------------------------------

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

df.main <- df

# + different instruments ---------------------------------------------------

instr_matrix_description <- list(c("1"), 
                                 c("1","X"),
                                 c("1","X","job_offers_binary"),
                                 c("1","X","gov"),
                                 c("1","X","privat"),
                                 c("1","X","lagincome")
)

res_inst <- fcrat::test_central(y = df.main$income, 
                         x = df.main$expectation, 
                         other_data = df.main,
                         instruments =  instr_matrix_description)

rownames(res_inst)<-res_inst$instruments
xtable::xtable(res_inst[,c("mean","median","mode")])


# + main plot ------------------------------------------------------------------

plot(test_convex(df.main, 
                  instruments =  list(
                    c("1"),c("1","X")
                  ) ))
ggsave("./applications/plots/revision/Rsce_new.pdf",
       width=6, height = 3)




# + by lag income -------------------------------------------------------

instr_matrix_description <- c("1","X")
var.cur <- "lagincome"

quant1 <- quantile(df.main[,var.cur],probs = 1/3,na.rm = TRUE)
quant2 <- quantile(df.main[,var.cur],probs = 2/3,na.rm = TRUE)

cat("\n", quant1, " ",quant2)
cat("\n", var.cur,": lower",mean(df.main[,var.cur]<quant1,na.rm=TRUE), " upper: ", mean(df.main[,var.cur]>quant2,na.rm=T))

nas.cur <- is.na(df.main[,var.cur])

dim(df.main[df.main[,var.cur]<quant1 & !nas.cur,])
dim(df.main[df.main[,var.cur]>=quant1 & df.main[,var.cur]<quant2 & !nas.cur,])
dim(df.main[df.main[,var.cur]>=quant2 & !nas.cur,])


plot(
  join_different_results(
    list(
      test_convex(df.main[df.main[,var.cur]<quant1& !nas.cur,], 
                   instruments =  instr_matrix_description),
      test_convex(df.main[df.main[,var.cur]>=quant1 & df.main[,var.cur]<quant2 & !nas.cur,], 
                   instruments =  instr_matrix_description),
      test_convex(df.main[df.main[,var.cur]>=quant2 & !nas.cur,], 
                   instruments =  instr_matrix_description)),
    xname = c("low","middle","high")))+facet_grid(1~xname)
ggsave(paste0("./applications/plots/revision/Rsce_by_terciles_",var.cur,".pdf"),
       width=9, height = 3)


# + income vs age ------------------------------------------------------

dats <- df.main %>% filter(!is.na(age_cat) & !is.na(ss_highincome))
dats$ss_age <- dats$age_cat=="Under 40"


table(dats$ss_age,dats$ss_highincome,dnn = c("ss_age","income"))

res.cur <- join_different_results(
  list(
    test_convex(dats %>% filter(ss_highincome==TRUE,ss_age==TRUE), 
                instruments =  instr_matrix_description),
    test_convex(dats %>% filter(ss_highincome==TRUE,ss_age==FALSE), 
                instruments =  instr_matrix_description),
    
    test_convex(dats %>% filter(ss_highincome==F,ss_age==TRUE), 
                instruments =  instr_matrix_description),
    test_convex(dats %>% filter(ss_highincome==F, ss_age==FALSE), 
                instruments =  instr_matrix_description)),
  xname = c("high income","high income", "low income", "low income"),
  yname = c("under 40","over 40","under 40","over 40")
)
res.cur$data$xname <- factor(res.cur$data$xname, levels=c("low income","high income"))
plot(res.cur)+facet_grid(xname~yname)
ggsave("./applications/plots/revision/Rscelag_2by2_income_ageb.pdf",
       width=6.5, height = 5)


# + income vs privat ------------------------------------------------------

dats <- df.main %>% filter(!is.na(ss_privat) & !is.na(ss_highincome))

table(dats$privat,dats$ss_highincome,dnn = c("private","income"))

res.cur <- join_different_results(
  list(
    test_convex(dats %>% filter(ss_highincome==TRUE,ss_privat==TRUE), 
                 instruments =  instr_matrix_description),
    test_convex(dats %>% filter(ss_highincome==TRUE,ss_privat==FALSE), 
                 instruments =  instr_matrix_description),
    
    test_convex(dats %>% filter(ss_highincome==F,ss_privat==TRUE), 
                 instruments =  instr_matrix_description),
    test_convex(dats %>% filter(ss_highincome==F, ss_privat==FALSE), 
                 instruments =  instr_matrix_description)),
  xname = c("high income","high income", "low income", "low income"),
  yname = c("private","non private","private","non private")
)
res.cur$data$xname <- factor(res.cur$data$xname, levels=c("low income","high income"))
plot(res.cur)+facet_grid(xname~yname)

ggsave("./applications/plots/revision/Rscelag_2by2_income_privat.pdf",
       width=6.5, height = 5)



# + income vs offer ---------------------------------------------------------

dats <- df.main %>% filter(!is.na(ss_jobofferspast) & !is.na(ss_highincome))

table(dats$ss_jobofferspast,dats$ss_highincome,dnn = c("job offers","income"))

res.cur <- join_different_results(
  list(
    test_convex(dats %>% filter(ss_highincome==TRUE,ss_jobofferspast==TRUE), 
                 instruments =  instr_matrix_description),
    test_convex(dats %>% filter(ss_highincome==TRUE,ss_jobofferspast==FALSE), 
                 instruments =  instr_matrix_description),
    
    test_convex(dats %>% filter(ss_highincome==F,ss_jobofferspast==TRUE), 
                 instruments =  instr_matrix_description),
    test_convex(dats %>% filter(ss_highincome==F, ss_jobofferspast==FALSE), 
                 instruments =  instr_matrix_description)),
  xname = c("high income","high income", "low income", "low income"),
  yname = c("offer","no offer","offer","no offer")
)
res.cur$data$xname <- factor(res.cur$data$xname, levels=c("low income","high income"))
plot(res.cur)+facet_grid(xname~yname)

ggsave("./applications/plots/revision/Rscelag_2by2_income_offerpast.pdf",
       width=6.5, height = 5)




# + round -------------------------------------------------------------------

plot(
  join_different_results(
    list(
      test_convex(df |> filter(catround%in%c('once','first')), 
                  instruments =  instr_matrix_description),
      test_convex(df |> filter(catround=='second'),
                  instruments =  instr_matrix_description)),
    xname = c("first","second")))+facet_grid(1~xname)
ggsave(paste0("./applications/plots/revision/Rsce_by_round.pdf"),
       width=6, height = 3)


# + income vs round ---------------------------------------------------------

dfs <- df


dfs.inc.round <- dfs |> mutate(ss1= ss_highincome,
                               ss2= catround %in% c('once','first'))
ss1.name <- c('high income','low income')
ss2.name <- c('first','second')


table(dfs.inc.round |> select(ss1,ss2))


res.cur <- join_different_results(
  list(
    test_convex(dfs.inc.round %>% filter(ss1==F,ss2==TRUE), 
                instruments =  instr_matrix_description),
    test_convex(dfs.inc.round %>% filter(ss1==F,ss2==FALSE), 
                instruments =  instr_matrix_description),
    
    test_convex(dfs.inc.round %>% filter(ss1==T,ss2==TRUE), 
                instruments =  instr_matrix_description),
    test_convex(dfs.inc.round %>% filter(ss1==T, ss2==FALSE), 
                instruments =  instr_matrix_description)),
  xname = c(ss1.name[2],ss1.name[2], ss1.name[1], ss1.name[1]),
  yname = c(ss2.name[1],ss2.name[2], ss2.name[1], ss2.name[2])
)
plot(res.cur)+facet_grid(xname~yname)
ggsave("./applications/plots/revision/Rscelag_2by2_income_round.pdf",width=6.5, height = 5)


# + income vs abs(error) ----------------------------------------------------

dfs <- df

# compute wave as numeric value
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


res.cur <- join_different_results(
  list(
    test_convex(dfs.inc.aerr %>% filter(ss1==F,ss2==TRUE), 
                instruments =  instr_matrix_description),
    test_convex(dfs.inc.aerr %>% filter(ss1==F,ss2==FALSE), 
                instruments =  instr_matrix_description),
    test_convex(dfs.inc.aerr %>% filter(ss1==T,ss2==TRUE), 
                instruments =  instr_matrix_description),
    test_convex(dfs.inc.aerr %>% filter(ss1==T,ss2==FALSE), 
                instruments =  instr_matrix_description)),
  xname = c(ss1.name[1],ss1.name[1], ss1.name[2],ss1.name[2]),
  yname = c(ss2.name[1], ss2.name[2],ss2.name[1], ss2.name[2])
)
plot(res.cur)+facet_grid(xname~yname)

ggsave("./applications/plots/revision/Rscelag_2by2_income_abserror.pdf",width=6.5, height = 5)





# exchange rate -----------------------------------------------------------

exchange1 <- exchange[exchange$to=="USD",]
exchange2 <- exchange[exchange$to=="JPY",]
exchange3 <- exchange[exchange$to=="AUD",]

dim(exchange1)
dim(exchange2)
dim(exchange3)

range(exchange1$date)
range(exchange2$date)
range(exchange3$date)

res_test <- list(
  test_convex(x = exchange1$fc, y = exchange1$realization, 
              instruments =  list(c("1","lag(X-Y)"))),
  test_convex(x = exchange2$fc, y = exchange2$realization, 
              instruments =  list(c("1","lag(X-Y)"))),
  test_convex(x = exchange3$fc, y = exchange3$realization, 
              instruments =  list(c("1","lag(X-Y)")))
)

plot(join_different_results(res = res_test,
                            xname = c("USD/EUR","JPY/EUR","AUD/EUR")))+ 
  theme(legend.position="bottom")+
  labs(size = "p-value")+facet_grid(1~xname)

ggsave("./applications/plots/revision/Rexchange_all_innovation.pdf",
       width = 9, height=3)



# GDP ---------------------------------------------------------------------

if(!identical(sort(GDP$date),GDP$date))
  stop("Data not sorted right")

dim(GDP)

inst.cur <- c("1","X")
plot(join_different_results(list(
  test_convex(x = GDP$FC.middle,y =  GDP$Obs.first,
              instruments =  inst.cur),
  test_convex(x = GDP$FC.middle,y =  GDP$Obs.second,
              instruments =  inst.cur),
  test_convex(x = GDP$FC.middle, y= GDP$Obs.recent,
              instruments =  inst.cur)),
  xname = c("first vintage", "second vintage", "most recent vintage")))+facet_grid(1~xname)
ggsave("./applications/plots/revision/Rgdp_all.pdf", width = 9,height = 3)











# Table -------------------------------------------------------------------

const2 <- function (stateVariable, theta, ...) 
{
  return(return(theta))
}


var.cur <- "lagincome"
quant1 <- quantile(df[,var.cur],probs = 1/3,na.rm = TRUE)
quant2 <- quantile(df[,var.cur],probs = 2/3,na.rm = TRUE)


eps <- 0.025

exchange1 <- exchange[exchange$to=="USD",]
exchange2 <- exchange[exchange$to=="JPY",]
exchange3 <- exchange[exchange$to=="AUD",]


# helper function for rounding
myround <- function(x,digits=2){format(round(x, digits=digits), nsmall = digits) }

# generate list of data samples with names
samples <- list(
  #full sample
  list(name='sce', 
       dat = df),
  
  # by income
  list(name='sce (high income)', 
       dat = df |> filter(lagincome>=quant2)),
  list(name='sce (middle income)', 
       dat = df |> filter(lagincome<quant2,lagincome>=quant1)),
  list(name='sce (low income)', 
       dat = df |> filter(lagincome<quant1)),
  
  # cross splits
  list(name='sce (under 40, low inc.)', 
       dat = df |> filter(age_cat=="Under 40",ss_highincome==F)),
  list(name='sce (under 40, high inc.)', 
       dat = df |> filter(age_cat=="Under 40",ss_highincome==T)),
  list(name='sce (over 40, low inc.)', 
       dat = df |> filter(age_cat!="Under 40",ss_highincome==F)),
  list(name='sce (over 40, high inc.)', 
       dat = df |> filter(age_cat!="Under 40",ss_highincome==T)),
  list(name='sce (offer, low inc.)',
       dat = df |> filter(ss_jobofferspast==T,ss_highincome==F)),
  list(name='sce (offer, high inc.)',
       dat = df |> filter(ss_jobofferspast==T,ss_highincome==T)),
  list(name='sce (no offer, low inc.)',
       dat = df |> filter(ss_jobofferspast==F,ss_highincome==F)),
  list(name='sce (no offer, high inc.)',
       dat = df |> filter(ss_jobofferspast==F,ss_highincome==T)),
  
  # rounds
  list(name='sce (second)', 
       dat = df |> filter(catround=='second')),
  list(name='sce (first)', 
       dat = df |> filter(catround%in% c('first','once'))),
  
  #income vs round
  list(name='sce (first, low inc.)', 
       dat = dfs.inc.round %>% filter(ss1==F,ss2==TRUE)),
  list(name='sce (first, high inc.)', 
       dat = dfs.inc.round %>% filter(ss1==T,ss2==TRUE)),
  list(name='sce (second, low inc.)', 
       dat = dfs.inc.round %>% filter(ss1==F,ss2==F)),
  list(name='sce (second, high inc.)', 
       dat = dfs.inc.round %>% filter(ss1==T,ss2==F)),
  
  #income vs abserror
  list(name='sce (large shock, low inc.)', 
       dat = dfs.inc.aerr %>% filter(ss1==F,ss2==TRUE)),
  list(name='sce (large shock, high inc.)', 
       dat = dfs.inc.aerr %>% filter(ss1==T,ss2==TRUE)),
  list(name='sce (small shock, low inc.)', 
       dat = dfs.inc.aerr %>% filter(ss1==F,ss2==F)),
  list(name='sce (small shock, high inc.)', 
       dat = dfs.inc.aerr %>% filter(ss1==T,ss2==F)),
  
  
  list(name='GDP',
       dat = GDP |> rename(expectation = FC.middle,income = Obs.second) |> select(expectation,income)),
  list(name='Exchange (USD)',
       dat = exchange1 |> rename(expectation = fc,income = realization    ) |> select(expectation,income)),
  list(name='Exchange (JPY)',
       dat = exchange2 |> rename(expectation = fc,income = realization    ) |> select(expectation,income)),
  list(name='Exchange (AUD)',
       dat = exchange3 |> rename(expectation = fc,income = realization    ) |> select(expectation,income))
)


# generate list of functions on data samples with names
functions <- list(
  list(name='n',
       foo = function(x) x%>%ungroup() |> filter(!is.na(income),!is.na(expectation)) |> summarise(n=n()) |> pull(n)),
  list(name='mean',
       foo = function(x) myround(as.numeric(test_central(y = x$income,x = x$expectation,instruments = inst1)$mean),digits = 2)),
  list(name='median',
       foo = function(x) myround(as.numeric(test_central(y = x$income,x = x$expectation,instruments = inst1)$median),digits = 2)),
  list(name='mode',
       foo = function(x) myround(as.numeric(test_central(y = x$income,x = x$expectation,instruments = inst1)$mode),digits = 2)),
  list(name='EKT (quantiles)',
       foo = function(x) {
         res.cur <- PointFore::estimate.functional(model = const2,type='cue',
                                                   theta0 = 0.5, vcov='iid',
                                                   PointFore::quantiles, 
                                                   Y = x$income,X = x$expectation,instruments = inst2)
         return(paste0(
           myround(summary(res.cur)$Jtest$test[2],digits = 2),
           ' [',paste0(myround(confint(res.cur$gmm,level = .9)$test,digits=2),collapse = ', '),']'))}),
  list(name='EKT (expectiles)',
       foo = function(x) {
         res.cur <- PointFore::estimate.functional(model = const2,type='cue',
                                                   theta0 = 0.5, vcov='iid',
                                                   PointFore::expectiles, 
                                                   Y = x$income,X = x$expectation,instruments = inst2)
         return(paste0(
           myround(summary(res.cur)$Jtest$test[2],digits = 2),
           ' [',paste0(myround(confint(res.cur$gmm,level = .9)$test,digits=2),collapse = ', '),']'))})
 )


# generate table
table <- data.frame(sample =rep(NA,length(samples)*length(functions)), val =NA, name=NA)
i <- 1
for(sample in samples){
  for(foo in functions){
    
    if(grepl('xchange',sample$name)){
      inst1 <- c("1","lag(X-Y)")
      inst2 <- c('lag(X-Y)')
    } else {
      inst1 <- c('1','X')
      inst2 <- c('X')
    }
    
    
    res.cur <- foo$foo(sample$dat)
    table$val[i]<-res.cur
    table$name[i] <- foo$name
    table$sample[i]<-sample$name
    i <- i+1
  }
}

# print table
table |>
  pivot_wider(names_from = name,values_from = val) |> 
  xtable::xtable() |> print(include.rownames=FALSE)












