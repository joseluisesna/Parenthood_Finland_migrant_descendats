########################################################################################################################
# (6) ANCESTRY CULTURE AND FERTILITY (Data modeling)
# R script written by Jose Luis Estevez (Vaestoliitto)
# Date: Oct 19th, 2023
########################################################################################################################

# CLEAN THE ENVIRONMENT
rm(list=ls())

# REQUIRED PACKAGES
library(data.table);library(khb);library(ggplot2)

# DATA LOADING
load('cem.sample.women.RData')

########################################################################################################################

# VARIABLES WITHOUT SPACES OR UNDERSCORES (Needed for the KHB analyses)
indivyear[,X := factor(X,labels=c('FIN','1.5NWE','2.5NWE','1.5SEE','2.5SEE','1.5USSR','2.5USSR',
                                  '1.5MENA','2.5MENA','1.5AFR','2.5AFR','1.5SEA','2.5SEA','1.5EAAP','2.5EAAP'))]

indivyear[,partnerstatus := factor(partnerstatus,labels=c('never','single','cohab','marry'))]
indivyear[,partnerstatus := relevel(partnerstatus,ref='cohab')] # ref category: cohabitation

indivyear[,education := factor(education,labels=c('In.edu','low','inter','high'))]

########################################################################################################################

# MODELS

# MODEL 0 (no weights)
model0 <- glm(formula = mom ~ X
              + std.age + I(std.age^2) + I(std.age^3) + I(std.age^4) + I(std.age^5) 
              + birthcohort,
              data=indivyear,family=binomial(link='logit'))
summary(model0)
round(exp(coefficients(model0)),2) # exp(coeff)
AIC(model0);BIC(model0) # GOF

######################

# CEM SAMPLE (K-TO-K)
indivyear <-indivyear[weights2 == 1]

# MODEL 1
model1 <- glm(formula = mom ~ X
              + std.age + I(std.age^2) + I(std.age^3) + I(std.age^4) + I(std.age^5) 
              + birthcohort,
              data=indivyear,family=binomial(link='logit'))
summary(model1)
round(exp(coefficients(model1)),2) # exp(coeff)
AIC(model1);BIC(model1) # GOF

# MODEL 2
model2 <- glm(formula = mom ~ X
              + std.age + I(std.age^2) + I(std.age^3) + I(std.age^4) + I(std.age^5) 
              + birthcohort
              + education,
              data=indivyear,family=binomial(link='logit'))
summary(model2)
round(exp(coefficients(model2)),2) # exp(coeff)
AIC(model2);BIC(model2) # GOF

# MODEL 3
model3 <- glm(formula = mom ~ X
              + std.age + I(std.age^2) + I(std.age^3) + I(std.age^4) + I(std.age^5)              
              + birthcohort
              + partnerstatus,
              data=indivyear,family=binomial(link='logit'))
summary(model3)
round(exp(coefficients(model3)),2) # exp(coeff)
AIC(model3);BIC(model3) # GOF

# MODEL 4
model4 <- glm(formula = mom ~ X
              + std.age + I(std.age^2) + I(std.age^3) + I(std.age^4) + I(std.age^5)              
              + birthcohort
              + education + partnerstatus,
              data=indivyear,family=binomial(link='logit'))
summary(model4)
round(exp(coefficients(model4)),2) # exp(coeff)
AIC(model4);BIC(model4) # GOF

########################################################################################################################

# MEDIATION ANALYSES (KHB)

# Increase memory for this procedure
Sys.setenv(R_MAX_VSIZE=10000)

# BOTH EDUCATION AND PARTNER STATUS
km14 <- khb(model1,model4) # Mediating effect of education and partner status
print(km14,disentangle=TRUE)
print(km14,keyvar='X')

# See adjusted model
print(km14,type='models')
round(exp(km14$adjusted$coefficients),2)

# Let's plot the indirect effects of the KHB model
khb.ie <- khb.se <-list()

for(i in 1:14){
  khb.ie[[paste(i)]] <- km14$key[[i]]$detail[-1,'Estimate'] # No need of the grand indirect effect
  khb.se[[paste(i)]] <- km14$key[[i]]$detail[-1,'Std. Error'] # No need of the grand indirect effect either
}

khb.plot <- data.table(coeff = unlist(khb.ie),
                       se = unlist(khb.se),
                       origin = rep(1:14,each=6),
                       type = rep(1:6,times=14))

# Labels to variables
khb.plot[,origin := factor(origin,levels=1:14,
                           labels = c('Northwest Europe (1.5G+2G)','Northwest Europe (2.5G)',
                                      'South & East Europe (1.5G+2G)','South & East Europe (2.5G)',
                                      'Former USSR (1.5G+2G)','Former USSR (2.5G)',
                                      'Middle East & North Africa (1.5G+2G)','Middle East & North Africa (2.5G)',
                                      'Sub-Saharan Africa (1.5G+2G)','Sub-Saharan Africa (2.5G)',
                                      'South & Southeast Asia (1.5G+2G)','South & Southeast Asia (2.5G)',
                                      'East Asia, Americas & Pacific (1.5G+2G)','East Asia, Americas & Pacific (2.5G)'))]
khb.plot[,type := factor(type,levels=1:6,
                         labels = c('Low/Unknown education','Intermediate education','High education',
                                    'Never partnered','Previous partner','Marriage'))]

# Visualization
jpeg(filename="fig.ind.effects.khb.jpeg",width=15,height=8,units='in',res=500)
ggplot(data = khb.plot,
       aes(x=exp(coeff),
           y=factor(origin,levels=rev(levels(origin))))) +
  geom_vline(xintercept = 1,color='red') +
  geom_pointrange(aes(xmin=exp(coeff - se*qnorm(.975)),xmax=exp(coeff + se*qnorm(.975)))) +
  scale_x_log10() +
  facet_wrap(~type,ncol=3,scales='fixed') +
  theme_light() +
  xlab('Indirect effect (logarithmic scale)') + ylab('') +
  theme(strip.background = element_rect(fill='black'))
dev.off()

########################################################################################################################

# Save output
save(km14,file="khb.output.RData")

########################################################################################################################