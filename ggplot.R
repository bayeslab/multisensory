# M2 = glmer(Response01 ~ VisCondition+c.MarchScore+MusicExp
#           +VisCondition:c.MarchScore+MusicExp:c.MarchScore
#           +VisCondition:c.MarchScore:MusicExp
#           +(c.MarchScore+VisCondition+VisCondition:c.MarchScore|ID)
#           ,family=binomial(link="logit"),data=task2,
#           control=glmerControl(optimizer  = 'bobyqa',optCtrl=list(maxfun=1e6)))

c.MarchScore <- unique(task2$MarchScore)
VisCondition <- unique(task2$VisCondition)
MusicExp <- unique(task2$MusicExp)
assorted.csv <- expand.grid(c.MarchScore=c.MarchScore, VisCondition=VisCondition,
                            MusicExp=MusicExp,KEEP.OUT.ATTRS = TRUE)

predictLog1 <- predict(M2, newdata = assorted.csv, newparams = NULL, re.form = NA,
                       terms = NULL, allow.new.levels = FALSE, na.action = na.pass)

myFun <- function(model){
  predict(model, newdata = assorted.csv, newparams = NULL, re.form = NA, terms = NULL, 
          allow.new.levels = FALSE, na.action = na.pass)
}

#start.time1 <- Sys.time()
#bootLog1 <- bootMer(M2, FUN = myFun , nsim = 8, seed = NULL, use.u = FALSE, re.form=NA, 
#                    type = c("parametric"), verbose = TRUE, .progress = "none",
#                    PBargs = list(), parallel = c("multicore"),
#                    ncpus = 8, cl = NULL)
#end.time1 <- Sys.time()
#time.taken1 <- end.time1 - start.time1

start.time2 <- Sys.time()
# preferably use this one with higher number of simulations
bootLog2 <- bootMer(M2, FUN = myFun , nsim = 1000, seed = NULL, use.u = FALSE, re.form=NA, 
                    type = c("parametric"), verbose = TRUE, .progress = "none",
                    PBargs = list(), #parallel = c("multicore"),
                    ncpus = getOption("boot.ncpus", 1L), cl = NULL)
end.time2 <- Sys.time()
time.taken2 <- end.time2 - start.time2

# Using the 1000 sim bootLog for plot

qs <- apply(bootLog2$t, MARGIN = 2, FUN = quantile)
lwr <- qs["25%",]
upr <- qs["75%",]
results <- cbind(assorted.csv, mle = bootLog2$t0, lwr = lwr, upr = upr)
library(ggplot2)
library(dplyr)
sim1000plot <- results %>% ggplot(aes(x=c.MarchScore, y=mle, fill=VisCondition, color=VisCondition))+
  geom_line()+geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.1,size=0)+facet_wrap(~MusicExp)
