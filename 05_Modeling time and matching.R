########################################################################################################################
# (5) ANCESTRY CULTURE AND FERTILITY (Modeling time and matching)
# R script written by Jose Luis Estevez (Vaestoliitto)
# Date: Oct 17th, 2023
########################################################################################################################

# CLEAN THE ENVIRONMENT
rm(list=ls())

# REQUIRED PACKAGES
library(data.table);library(cem);library(survival);library(ggplot2);library(ggpubr)

# DATA LOADING
load('sample.women.RData')

########################################################################################################################

# CREATE THE GROUP TO COMPARE THE NATIVE WITH...
indivyear[,X := factor(paste(generation,broadbackground,sep=': '),
                       levels=c('Native: Finland',
                                '1.5G migrant: Northwest Europe','2G migrant: Northwest Europe','2.5G migrant: Northwest Europe',
                                '1.5G migrant: South & East Europe','2G migrant: South & East Europe','2.5G migrant: South & East Europe',
                                '1.5G migrant: Former USSR','2G migrant: Former USSR','2.5G migrant: Former USSR',
                                '1.5G migrant: Middle East & North Africa','2G migrant: Middle East & North Africa','2.5G migrant: Middle East & North Africa',
                                '1.5G migrant: Sub-Saharan Africa','2G migrant: Sub-Saharan Africa','2.5G migrant: Sub-Saharan Africa',
                                '1.5G migrant: South & Southeast Asia','2G migrant: South & Southeast Asia','2.5G migrant: South & Southeast Asia',
                                '1.5G migrant: East Asia, Americas & Pacific','2G migrant: East Asia, Americas & Pacific','2.5G migrant: East Asia, Americas & Pacific')
                       )]
indivyear[,summary(X)]

########################################################################################################################

# COVARIATE TRANSFORMATION: AGE AND BIRTH COHORT
# centered AGE
indivyear[,std.age := scale(end,center=TRUE,scale=FALSE)] # centered, not scale
# BIRTH COHORT as factor (groups of 5 years width)
indivyear[,birthcohort := factor(ifelse(syntyv %in% 1980:1984,'1980-84',
                                        ifelse(syntyv %in% 1985:1989,'1985-89',
                                               ifelse(syntyv %in% 1990:1994,'1990-94','1995-99'))),
                                 levels=c('1985-89','1980-84','1990-94','1995-99'))]

# MODELLING TIME (FINDING BEST POLYNOMIAL VERSION TO MODEL TIME)
mg <- glm(formula = mom ~ as.factor(end), # general
          data=indivyear,family=binomial(link='logit')) 
m0 <- glm(formula = mom ~ 1,
          data=indivyear,family=binomial(link='logit')) # constant
m1 <- glm(formula = mom ~ std.age,
          data=indivyear,family=binomial(link='logit')) # linear
m2 <- glm(formula = mom ~ poly(std.age,2),
          data=indivyear,family=binomial(link='logit')) # quadratic
m3 <- glm(formula = mom ~ poly(std.age,3),
          data=indivyear,family=binomial(link='logit')) # cubic
m4 <- glm(formula = mom ~ poly(std.age,4),
          data=indivyear,family=binomial(link='logit')) # fourth-power polynomial
m5 <- glm(formula = mom ~ poly(std.age,5),
          data=indivyear,family=binomial(link='logit')) # fifth-power polynomial
m6 <- glm(formula = mom ~ poly(std.age,6),
          data=indivyear,family=binomial(link='logit')) # sixth-power polynomial
m7 <- glm(formula = mom ~ poly(std.age,7),
          data=indivyear,family=binomial(link='logit')) # seventh-power polynomial
m8 <- glm(formula = mom ~ poly(std.age,8),
          data=indivyear,family=binomial(link='logit')) # eighth-power polynomial
m9 <- glm(formula = mom ~ poly(std.age,9),
          data=indivyear,family=binomial(link='logit')) # ninth-power polynomial

# Summary table
sum.table <- data.table(time = c('Constant','Linear','Quadratic','Cubic','Fourth order','Fifth order','Sixth order',
                                 'Seventh order','Eighth order','Ninth order','General'),
                        `n parameters` = c(1:10,length(mg$coefficients)),
                        deviance = c(deviance(m0),deviance(m1),deviance(m2),deviance(m3),deviance(m4),deviance(m5),deviance(m6),
                                     deviance(m7),deviance(m8),deviance(m9),deviance(mg)),
                        dev.diff.prev = NA,
                        dev.diff.gen = NA,
                        AIC = c(AIC(m0),AIC(m1),AIC(m2),AIC(m3),AIC(m4),AIC(m5),AIC(m6),AIC(m7),AIC(m8),AIC(m9),AIC(mg)),
                        BIC = c(BIC(m0),BIC(m1),BIC(m2),BIC(m3),BIC(m4),BIC(m5),BIC(m6),BIC(m7),BIC(m8),BIC(m9),BIC(mg))
)
sum.table$dev.diff.prev[2:10] <- round(sum.table$deviance[1:9] - sum.table$deviance[2:10],0)
sum.table$dev.diff.gen[1:10] <- round(sum.table$deviance[1:10] - sum.table$deviance[11],0)

sum.table # Seems like a 8th degree polynomial is the best option, but 5th can suffice

########################################################################################################################

# COARSENED EXACT MATCHING

tr <- indivyear[,which(broadbackground != 'Finland')] # Non-native
cr <- indivyear[,which(broadbackground == 'Finland')] # Natives
ntr <- length(tr)
ncr <- length(cr)

# 1) MATCHING VARIABLES 
indivyear[,age := as.factor(end)] # Age
indivyear[,birthyear := as.factor(syntyv)] # Year of birth
indivyear[,region := as.factor(mkunta)] # Region in Finland
indivyear[,summary(habitat)] # habitat

# Remove those containing NAs in these variables
indivyear <- indivyear[!is.na(age) & !is.na(birthyear) & !is.na(region) & !is.na(habitat)]

# 2) IMBALANCE
indivyear[,groups := ifelse(broadbackground != 'Finland',1,0)] # Variable for grouping
imbalance(group=indivyear$groups,data=indivyear[,.(age,birthyear,region,habitat)])

# 3) CEM
set.seed(0708)
mat <- cem(treatment="groups",
           data=indivyear[,.(age,birthyear,region,habitat,groups)],drop='groups',
           eval.imbalance = TRUE,keep.all = TRUE)
mat

# 4) K-TO-K
set.seed(0708)
mat2 <- k2k(obj=mat,data=indivyear[,.(age,birthyear,region,habitat,groups)],method=NULL,1)
mat2

# Weights addition
indivyear[,weights := mat$w]
indivyear[,weights2 := mat2$w]

########################################################################################################################

# Save image
save(indivyear,file="cem.sample.RData")

########################################################################################################################

# MATCHING AND SAMPLING

# Exclude women with a least one unmatched observations
exclID <- indivyear[weights == 0,unique(shnro)]
indivyear2 <- indivyear[!(shnro %in% exclID)]

indivyear[X == 'Native: Finland',length(unique(shnro))] # 575,938 full
indivyear2[X == 'Native: Finland',length(unique(shnro))] # 509,446 reduced

# Now, take a sample of only 5,000 unique women for the ref. category
inclID <- indivyear2[X == 'Native: Finland',unique(shnro)]
set.seed(0708)
inclID <- sample(inclID,size=5000,replace=FALSE)

# Final sample after matching and sampling
nat <- indivyear[X == 'Native: Finland',] # native-born
nat <- nat[shnro %in% inclID] # only the 5,000 selected
indivyear <- indivyear[X != 'Native: Finland',] # migrant descendants
indivyear <- rbind(indivyear,nat) # Put back together

# Remove unnecessary objects
rm(exclID);rm(inclID);rm(indivyear2);rm(nat)

#############################

# For plotting, let's cover only from 15 to 30
indivyearplot <- indivyear[end <= 30]

# EXTRACT RISK SETS AND EVENTS PER GROUP (Natives, 1.5G, 2G, and 2.5G migrants - the last three by background too)
ts.nat <- survfit(Surv(start,end,mom)~1,conf.type='log',conf.int=.95,type='kaplan-meier',error='greenwood',
                  data=indivyearplot[generation == 'Native'])
ts.mig <- survfit(Surv(start,end,mom)~broadbackground,conf.type='log',conf.int=.95,type='kaplan-meier',error='greenwood',
                  data=indivyearplot[generation == '1.5G migrant'])
ts.2nd <- survfit(Surv(start,end,mom)~broadbackground,conf.type='log',conf.int=.95,type='kaplan-meier',error='greenwood',
                  data=indivyearplot[generation == '2G migrant'])
ts.mix <- survfit(Surv(start,end,mom)~broadbackground,conf.type='log',conf.int=.95,type='kaplan-meier',error='greenwood',
                  data=indivyearplot[generation == '2.5G migrant'])

# Put results into a data set
hazarddata <- data.table(
  generation = c(rep('Native',times=16*7),rep('1.5G migrant',times=16*7),rep('2G migrant',times=16*7),rep('2.5G migrant',times=16*7)),
  broadbackground = c(rep(indivyear[,levels(broadbackground)[-1]],each=16,times=4)),
  time = c(rep(ts.nat$time,7),ts.mig$time,ts.2nd$time,ts.mix$time),
  risk = c(rep(ts.nat$n.risk,7),ts.mig$n.risk,ts.2nd$n.risk,ts.mix$n.risk),
  mom = c(rep(ts.nat$n.event,7),ts.mig$n.event,ts.2nd$n.event,ts.mix$n.event),
  survival = c(rep(ts.nat$surv,7),ts.mig$surv,ts.2nd$surv,ts.mix$surv), # survival
  lower = c(rep(ts.nat$lower,7),ts.mig$lower,ts.2nd$lower,ts.mix$lower), # lower bond SE
  upper = c(rep(ts.nat$upper,7),ts.mig$upper,ts.2nd$upper,ts.mix$upper) # lower bond SE
)

hazarddata[,hazard := mom/risk] # hazards: event / risk set
hazarddata[,se.h := sqrt(hazard*(1-hazard)/risk)] # SE of the hazard

# Visualization
hazarddata[,broadbackground := factor(broadbackground,
                                      c('Finland','Northwest Europe','South & East Europe','Former USSR',
                                        'Middle East & North Africa','Sub-Saharan Africa',
                                        'South & Southeast Asia','East Asia, Americas & Pacific'))]
hazarddata[,generation := factor(generation,
                                 c('Native','1.5G migrant','2G migrant','2.5G migrant'))]


# Estimated hazard probability
p1 <- ggplot(data=hazarddata,
             aes(time,hazard,color=generation,group=generation)) +
  #geom_pointrange(aes(ymin=(hazard-se.h*qnorm(.975)),ymax=(hazard+se.h*qnorm(.975)))) +
  geom_smooth(method='loess',se=FALSE) +
  geom_line(linetype='dashed') +
  facet_wrap(~broadbackground,nrow=1) +
  theme_bw() +
  scale_color_manual(values=c('Native'='black','1.5G migrant'='firebrick2','2G migrant'='darkgoldenrod','2.5G migrant'='dodgerblue')) +
  xlab("") + ylab("Estimated hazard probability") + labs(group='',color='') +
  theme(legend.position = 'top',legend.justification = 'center',
        strip.background = element_rect(fill='black'),strip.text=element_text(color='white'))

# Estimated survival probability
p2 <- ggplot(data=hazarddata,
             aes(time,survival,color=generation,group=generation)) +
  geom_line() +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  facet_wrap(~broadbackground,nrow=1) +
  theme_bw() +
  scale_color_manual(values=c('Native'='black','1.5G migrant'='firebrick2','2G migrant'='darkgoldenrod','2.5G migrant'='dodgerblue')) +
  xlab("Age at first child") + ylab("Estimated survival probability") + labs(group='',color='') +
  theme(legend.position = 'top',legend.justification = 'center',
        strip.background = element_rect(fill='black'),strip.text=element_text(color='white'))

jpeg(filename="fig.curves.cem.jpeg",width=15,height=8,units='in',res=500)
ggarrange(p1,p2,
          labels=c('',''),ncol=1,common.legend = TRUE)
dev.off()

########################################################################################################################

# EVOLUTION OF THE RISK SET 

indivyear[,event := paste('Motherhood =',mom,sep=' ')] 

# Education, by origin
jpeg(filename="fig.riskset.edu.jpeg",width=17,height=8,units='in',res=500)
ggplot(data=indivyear,aes(x=end,fill=education)) +
  geom_bar(color='black',position='stack') + # position='fill'
  facet_wrap(event~broadbackground,nrow=2,scales='free_y') +
  theme_bw() +
  scale_fill_brewer(palette = 'Oranges') +
  xlab("Age") + ylab("Count") + labs(fill='') +
  theme(legend.position = 'top',legend.justification = 'center',
        strip.background = element_rect(fill='black'),strip.text=element_text(color='white'))
dev.off()

# Partnership status, by origin
jpeg(filename="fig.riskset.part.jpeg",width=17,height=8,units='in',res=500)
ggplot(data=indivyear,aes(x=end,fill=partnerstatus)) +
  geom_bar(color='black',position='stack') + # position='fill'
  facet_wrap(event~broadbackground,nrow=2,scales='free_y') +
  theme_bw() +
  scale_fill_brewer(palette = 'Oranges') +
  xlab("Age") + ylab("Count") + labs(fill='') +
  theme(legend.position = 'top',legend.justification = 'center',
        strip.background = element_rect(fill='black'),strip.text=element_text(color='white'))
dev.off()

########################################################################################################################