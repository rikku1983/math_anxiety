setwd("C:/Users/sunli/Desktop/Feiya/2019 math anxiety/")
d0 <- read.csv('PISA2012_USA_recode.csv', header=T)
vmap <- read.csv('math_variables.csv', header = T, stringsAsFactors = F)
vmap$group <- sapply(strsplit(vmap$label, ' - '), '[', 1)
vmap$item <- sapply(strsplit(vmap$label, ' - '), '[', 2)
vmap$group[grep('Plausible',vmap$label)] <- 'Math grades'
t1 <- vmap[vmap$group!='Maths Self-Efficacy	',c('name', 'group', 'item')]
names(t1) <- c('item ID','Group','item')
write.csv(t1, 'results/t1_item_used.csv', row.names = F)

my <- vmap$name[vmap$group=='Math grades']
mav <- vmap$name[vmap$group=='Maths Anxiety']
mscv <- vmap$name[vmap$group=='Maths Self-Concept']
mbv <- vmap$name[vmap$group=='Maths Behaviour']
miv <- vmap$name[vmap$group=='Maths Interest']
msev <- vmap$name[vmap$group=='Maths Self-Efficacy']

d1 <- d0[, c(mav, mscv, miv, mbv, my)]
sapply(d1, function(x)sum(is.na(x)))
sapply(d1, function(x)sum(x %in% c(7,8,9)))
# Only works for numeric column
for(i in colnames(d1)){
  v0 <- d1[,i]
  ms <- vmap$missing[vmap$name==i]
  ms <- sapply(strsplit(ms, ','), as.numeric)
  v0[v0 %in% ms] <- NA
  d1[,i] <- v0
}       
sum(complete.cases(d1))

################### CFA
library('lavaan')
cfam<- paste0(c(paste0('anxiety =~ ', paste(mav, collapse = ' + ')), 
           paste0('interest =~ ', paste(miv, collapse = ' + ')), 
           paste0('selfcon =~ ', paste(mscv, collapse = ' + ')),
           paste0('behave =~ ', paste(mbv, collapse = ' + '))), 
           collapse = '\n')
cat(cfam)
cfaf <- cfa(cfam, data=d1)
parameterEstimates(cfaf)
summary(cfaf, fit.measures=TRUE, standardized=TRUE)
resm <- residuals(cfaf, type = "cor")$cov
write.csv(resm, 'results/CFA_residuals.csv')
# plot
library(reshape2)
mresm <- melt(resm)
levels(mresm$Var1) <- c(mav, miv, mscv, mbv)
levels(mresm$Var2) <- c(mav, miv, mscv, mbv)
p1 <- ggplot(data=mresm, aes(x=Var1, y=Var2, fill=value)) +
  geom_raster() +
  scale_fill_gradient2() +
  labs(x='',y='') +
  theme(axis.text.x = element_text(angle = 90))
pdf('results/CFA_residuals.pdf', width=9, height = 7)
p1
dev.off()

# Model has CFI and TLI both > 0.9, indicating a good fit
# ST49Q02	Maths Behaviour - Help Friends with Maths, has residual correlation with all other items

################### Use mean of each construct
d1c <- d1[complete.cases(d1[,c(mav,mscv,miv)]),]
d1c$a <- apply(d1c[, mav],1,function(x)mean(x))  # the higher, the stronger of axiety
d1c$sc <- apply(d1c[, mscv],1,function(x)mean(x)) # the higher the stronger of self concept
d1c$i <- apply(d1c[, miv],1,function(x)mean(x)) # the higher, the stronger of interest
d1c$y <- apply(d1c[, my],1,function(x)mean(x)) 

library(tidyLPA)
library(tidyverse)
ms <- d1c[,c('a','i','sc')] %>%
  estimate_profiles(2:6,
                    variances = c("equal","varying", "equal","varying"),
                    covariances = c("zero","zero", "equal","varying"),
                    package="mclust")
ms2 <- d1c[,c('a','i','sc')] %>%
  estimate_profiles(2:6,
                    variances = c("equal","varying", "equal", "varying","equal","varying"),
                    covariances = c("zero","zero", "equal", "equal","varying","varying"),
                    package="MplusAutomation")

df1 <- as.data.frame(print(ms))
df2 <- as.data.frame(print(ms2))
dfmodels <- df2[df2$Model != 6 & df2$Classes != 6, ]
write.csv(dfmodels, 'lfa_model_select.csv', row.names = F)


ms4 <- d1c[,c('a','i','sc')] %>%
  estimate_profiles(4,
                    variances = c("varying"),
                    covariances = c("zero"),
                    package="mclust")



plot_profiles(ms4, rawdata=F)
d2c <- cbind(d1c$y,get_data(ms4))
names(d2c)[1] <- 'y'
str(d2c)
d2c$Class <- factor(d2c$Class, levels = 1:4)
str(d2c)
library(ggplot2)
ggplot(d2c, aes(x=Class, y=y)) + geom_boxplot()

af <- aov(y~Class, data=d2c)
TukeyHSD(x=af, 'Class', conf.level=0.95)

d3 <- cbind(d2c, d1c$b)
names(d3)[12] <- 'b'
hist(d3$b)
ggplot(d3, aes(x=Class, y=b)) + geom_boxplot()
ggplot(d3,aes(x=y, y=b)) +geom_point() + geom_smooth(method='loess')


# library(poLCA)
# ###############
# data(election)
# f <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~1
# nes1 <- poLCA(f,election,nclass=1) # log-likelihood: -18647.31
# nes2 <- poLCA(f,election,nclass=2) # log-likelihood: -17344.92
# nes3 <- poLCA(f,election,nclass=3) # log-likelihood: -16714.66
# nes3 <- poLCA(f,election,nclass=4) # log-likelihood: -16367.29
# nes3 <- poLCA(f,election,nclass=5) # log-likelihood: -16081.57
# f2a <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
#              MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~PARTY
# nes2a <- poLCA(f2a,election,nclass=3,nrep=5)    # log-likelihood: -16222.32 
# pidmat <- cbind(1,c(1:7))
# exb <- exp(pidmat %*% nes2a$coeff)
# matplot(c(1:7),(cbind(1,exb)/(1+rowSums(exb))),ylim=c(0,1),type="l",
#         main="Party ID as a predictor of candidate affinity class",
#         xlab="Party ID: strong Democratic (1) to strong Republican (7)",
#         ylab="Probability of latent class membership",lwd=2,col=1)
# text(5.9,0.35,"Other")
# text(5.4,0.7,"Bush affinity")
# text(1.8,0.6,"Gore affinity")

################
# f <- cbind(ST29Q01,ST29Q03,ST29Q04,ST37Q01,ST37Q02,ST37Q03,ST37Q04,ST37Q05,ST37Q06,ST37Q07,ST37Q08,ST42Q01,ST42Q02,ST42Q03,ST42Q04,ST42Q05,ST42Q06,ST42Q07,ST42Q08,ST42Q09,ST42Q10)~1
# lcas <- data.frame(nc=1:10, loglike=NA, aic=NA, bic=NA)
# for(nc in 1:10){
#   m <- poLCA(f, d1c,nclass = nc, nrep=5)
#   lcas[lcas$nc==nc, 2:4] <- c(m$llik, m$aic, m$bic)
# }
# library(ggplot2)
# ggplot(lcas, aes(x=nc)) + 
#   geom_path(aes(y=scale(loglike)), col='red')+
#   geom_path(aes(y=scale(aic)), col='green')+
#   geom_path(aes(y=scale(bic)), col='blue')





d1c[,c('a','i','sc','se')] %>%
  estimate_profiles(6) %>%
  plot_profiles()

# model 1
d1c[,c('a','i','sc')] %>%
  estimate_profiles(3:7,
                    variances = c("equal"),
                    covariances = c("zero"))


# model 2
d1c[,c('a','i','sc')] %>%
  estimate_profiles(3:7,
                    variances = c("varying"),
                    covariances = c("zero"))


# model 3  
d1c[,c('a','i','sc')] %>%
  estimate_profiles(3:7,
                    variances = c("equal"),
                    covariances = c("equal"))

# model 4
d1c[,c('a','i','sc')] %>%
  estimate_profiles(3:7,
                    variances = c("varying"),
                    covariances = c("equal"),
                    package="MplusAutomation")
# model 5
d1c[,c('a','i','sc')] %>%
  estimate_profiles(3:7,
                    variances = c("equal"),
                    covariances = c("varying"),
                    package="MplusAutomation")
# model 6
d1c[,c('a','i','sc')] %>%
  estimate_profiles(3:7,
                    variances = c("varying"),
                    covariances = c("varying"),
                    package="MplusAutomation")

# The best model is model 2. 
d1c[,c('a','i','sc')] %>%
  estimate_profiles(3:7,
                    variances = c("varying"),
                    covariances = c("zero"),
                    package="MplusAutomation")

d1c[,c('a','i','sc')] %>%
  estimate_profiles(3:7,
                    variances = c("varying"),
                    covariances = c("zero"),
                    package="mclust")
ms <- d1c[,c('a','i','sc')] %>%
  estimate_profiles(2:5,
                    variances = c("varying"),
                    covariances = c("zero"),
                    package="mclust")
m4 <- d1c[,c('a','i','sc')] %>%
  estimate_profiles(4,
                    variances = c("varying"),
                    covariances = c("zero"),
                    package="mclust")
plot_profiles(m4,rawdata = F)

d4 <- get_data(m4)
df1 <- cbind(d1c, d4)




