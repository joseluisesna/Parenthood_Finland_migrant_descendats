########################################################################################################################
# (6) ANCESTRY CULTURE AND FERTILITY (Data modeling)
# R script written by Jose Luis Estevez (Vaestoliitto)
# Date: Oct 19th, 2023
########################################################################################################################

# CLEAN THE ENVIRONMENT
rm(list=ls())

# REQUIRED PACKAGES
library(data.table);library(khb);library(ggplot2);library(sjPlot);library(stargazer);library(splines);library(sjPlot)
library(ggpubr)

# DATA LOADING
load('cem.sample.RData')

########################################################################################################################

# VARIABLES WITHOUT SPACES OR UNDERSCORES (Needed for the KHB analyses)
indivyear[,X := factor(X,labels=c('FIN',
                                  '1.5NWE','2NWE','2.5NWE',
                                  '1.5SEE','2SEE','2.5SEE',
                                  '1.5USSR','2USSR','2.5USSR',
                                  '1.5MENA','2MENA','2.5MENA',
                                  '1.5AFR','2AFR','2.5AFR',
                                  '1.5SEA','2SEA','2.5SEA',
                                  '1.5EAAP','2EAAP','2.5EAAP'))]

indivyear[,partnerstatus := factor(partnerstatus,labels=c('never','single','cohab','marry'))]
indivyear[,partnerstatus := relevel(partnerstatus,ref='cohab')] # ref category: cohabitation

indivyear[,education := factor(education,labels=c('In.edu','low','inter','high'))]
# Let's use intermediate education as reference category
indivyear[,education := relevel(education,ref='inter')]

########################################################################################################################

# MODELS

# MODEL 0 (no weights)
model0 <- glm(formula = mom ~ X
              + poly(start,5) 
              + birthcohort,
              data=indivyear,family=binomial(link='logit'))
summary(model0)
round(exp(coefficients(model0)),2) # exp(coeff)
# Show model
stargazer(model0,type='text')
AIC(model0);BIC(model0) # GOF

######################

# MATCHING AND SAMPLING

# Exclude women with a least one unmatched observations
exclID <- indivyear[weights == 0,unique(shnro)]
indivyear2 <- indivyear[!(shnro %in% exclID)]

indivyear[X == 'FIN',length(unique(shnro))] # 575,938 full
indivyear2[X == 'FIN',length(unique(shnro))] # 509,446 reduced

# Now, take a sample of only 5,000 unique women for the ref. category
inclID <- indivyear2[X == 'FIN',unique(shnro)]
set.seed(0708)
inclID <- sample(inclID,size=5000,replace=FALSE)

# Final sample after matching and sampling
nat <- indivyear[X == 'FIN',] # native-born
nat <- nat[shnro %in% inclID] # only the 5,000 selected
indivyear <- indivyear[X != 'FIN',] # migrant descendants
indivyear <- rbind(indivyear,nat) # Put back together

######################

# LAGGING COVARIATES: EDUCATION AND PARTNERSHIP STATUS

# Use as predictor the variable a t-1
laggeddata <- indivyear[,.(shnro,end,education,partnerstatus)]
laggeddata[,end := end + 1] # add one

indivyear <- indivyear[,-c(13,90)] # remove partnership status and education at time t
indivyear <- merge(indivyear,laggeddata,by=c('shnro','end'),all.x=TRUE) # and replace with information at time t-1

# Remove unnecessary objects
rm(exclID);rm(inclID);rm(indivyear2);rm(nat);rm(laggeddata)

######################

# To enable comparison, remove when NA in any of the covariates
indivyear <- indivyear[!is.na(partnerstatus) & !is.na(education)]
# Number of transitions
indivyear[,sum(mom)]

# MODEL 1
model1 <- glm(formula = mom ~ X
              + poly(start,5) 
              + birthcohort,
              data=indivyear,family=binomial(link='logit'))
summary(model1)
round(exp(coefficients(model1)),2) # exp(coeff)
stargazer(model1,type='text')
AIC(model1);BIC(model1) # GOF

# MODEL 2
model2 <- glm(formula = mom ~ X
              + poly(start,5)              
              + birthcohort
              + education + partnerstatus,
              data=indivyear,family=binomial(link='logit'))
summary(model2)
round(exp(coefficients(model2)),2) # exp(coeff)
stargazer(model1,model2,type='text')
AIC(model2);BIC(model2) # GOF

# MODEL 3 (with interaction education x partnership status)
model3 <- glm(formula = mom ~ X
              + poly(start,5)              
              + birthcohort
              + education*partnerstatus,
              data=indivyear,family=binomial(link='logit'))
summary(model3)
round(exp(coefficients(model3)),2) # exp(coeff)
stargazer(model1,model2,model3,type='text')
AIC(model3);BIC(model3) # GOF

# MODEL 4 (with interaction age and background)
model4 <- glm(formula = mom ~ X*bs(start,degree=3)              
              + birthcohort
              + education + partnerstatus,
              data=indivyear,family=binomial(link='logit'))
summary(model4)
round(exp(coefficients(model4)),2) # exp(coeff)
stargazer(model1,model2,model4,type='text')
AIC(model4);BIC(model4) # GOF

# Save these data and model for plotting
save(indivyear,file='modeling.data.RData')
save(model4,file="with.splines.RData")

########################################################################################################################

# MEDIATION ANALYSES (KHB)

# Remove unnecessary objects
rm(list=setdiff(ls(),c("model1","model2")))

# BOTH EDUCATION AND PARTNER STATUS
km12 <- khb(model1,model2) # Mediating effect of education and partner status
print(km12,disentangle=TRUE)
print(km12,keyvar='X')

# See adjusted model
print(km12,type='models',digits=4)
round(exp(km12$adjusted$coefficients),2)

# Let's plot the indirect effects of the KHB model
khb.ie <- khb.se <-list()

for(i in 1:21){
  khb.ie[[paste(i)]] <- km12$key[[i]]$detail[-1,'Estimate'] # No need of the grand indirect effect
  khb.se[[paste(i)]] <- km12$key[[i]]$detail[-1,'Std. Error'] # No need of the grand indirect effect either
}

khb.plot <- data.table(coeff = unlist(khb.ie),
                       se = unlist(khb.se),
                       origin = rep(1:21,each=6),
                       gen = factor(rep(1:3,times=7,each=6),labels=c('1.5G migrant','2G migrant','2.5G migrant')),
                       type = rep(1:6,times=21))

# Labels to variables
khb.plot[,origin := factor(origin,levels=1:21,
                           labels = c('Northwest Europe (1.5G)','Northwest Europe (2G)','Northwest Europe (2.5G)',
                                      'South & East Europe (1.5G)','South & East Europe (2G)','South & East Europe (2.5G)',
                                      'Former USSR (1.5G)','Former USSR (2G)','Former USSR (2.5G)',
                                      'Middle East & North Africa (1.5G)','Middle East & North Africa (2G)','Middle East & North Africa (2.5G)',
                                      'Sub-Saharan Africa (1.5G)','Sub-Saharan Africa (2G)','Sub-Saharan Africa (2.5G)',
                                      'South & Southeast Asia (1.5G)','South & Southeast Asia (2G)','South & Southeast Asia (2.5G)',
                                      'East Asia, Americas & Pacific (1.5G)','East Asia, Americas & Pacific (2G)','East Asia, Americas & Pacific (2.5G)'))]
khb.plot[,type := factor(type,levels=1:6,
                         labels = c('In education','Low/Unknown education','High education',
                                    'Never partnered','Previous partner','Marriage'))]

# For colors, based on significance and direction of the effect
khb.plot[,sig := ifelse(coeff - se*qnorm(.975) > 0,'positive',
                        ifelse(coeff + se*qnorm(.975) < 0,'negative','no effect'))]

# Visualization
jpeg(filename="fig.ind.effects.khb.jpeg",width=10,height=9,units='in',res=500)
ggplot(data = khb.plot,
       aes(x=coeff,
           y=factor(origin,levels=rev(levels(origin))),
           shape=gen,
           color=sig)) +
  geom_vline(xintercept = 0,color='black',linetype='dashed') +
  geom_pointrange(aes(xmin=coeff - se*qnorm(.975),xmax=coeff + se*qnorm(.975))) +
  facet_wrap(~type,ncol=3,scales='free_x') +
  theme_bw() +
  scale_shape_manual(values=c('1.5G migrant'=21,'2G migrant'=22,'2.5G migrant'=24)) +
  scale_color_manual(values=c('no effect'='grey','positive'='blue','negative'='red')) +
  xlab('Log-Odds') + ylab('') + labs(group='',shape='',color='') +
  theme(legend.position = 'top',legend.justification = 'center',
        strip.background = element_rect(fill='black'),strip.text=element_text(color='white'))
dev.off()

########################################################################################################################

# Save output
save(km12,file="khb.output.RData")

########################################################################################################################