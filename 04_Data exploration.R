########################################################################################################################
# (4) ANCESTRY CULTURE AND FERTILITY (Data exploration)
# R script written by Jose Luis Estevez (Vaestoliitto)
# Date: Oct 9th, 2023
########################################################################################################################

# CLEAN THE ENVIRONMENT
rm(list=ls())

# REQUIRED PACKAGES
library(data.table);library(survival);library(ggplot2);library(ggpubr)

# DATA LOADING
load('individuals.background.RData')

########################################################################################################################

# SAMPLE SELECTION (WOMEN, BORN BETWEEN 1980-99, NATIVE, 1.5G, 2G, OR 2.5G, AND WITH NO CHILD PRIOR TO AGE 15)

# Only women
indiv <- indiv[sukup == 2] 
# Let's exclude 1G migrants, returnees, and people of uncertain background
indiv <- indiv[migratorystatus %in% c('Native','2G migrant','2.5G migrant','1.5G migrant')]
# And exclude those whose origin is uncertain
indiv <- indiv[!is.na(broadbackground)]
# Remove those who become parents before 15
indiv <- indiv[is.na(parenthoodage) | parenthoodage >= 15]

# I will replace "broadbackground" with "broadbackground2" for simplicity
indiv[,broadbackground := broadbackground2]
indiv[,table(broadbackground,migratorystatus)] # check

# There are some problematic cases I will take out
indiv <- indiv[!(broadbackground == 'Finland' & migratorystatus != 'Native')]

########################################################################################################################

# DESCRIPTIVES 

# Migratory status
indiv[!is.na(broadbackground2),table(migratorystatus,syntyp2)]

# (Ancestral) origin
indiv[migratorystatus %in% c('2G migrant','2.5G migrant','1.5G migrant') & broadbackground2 != 'Finland',
      table(broadbackground2)]

# Ancestral origin by migratory status
# Visualization
indiv[,migratorystatus := relevel(migratorystatus,ref='1.5G migrant')] # relevel
ggplot(data=indiv[migratorystatus != 'Native'],aes(x=migratorystatus,fill=broadbackground)) +
  geom_bar(color='black',position='dodge') +
  theme_light() +
  xlab("") + ylab("Count") + labs(fill='') +
  theme(legend.position = 'top',legend.justification = 'center')

# Let's put together 1.5G and 2.G vs 2.5G
indiv[,generation := factor(ifelse(migratorystatus == 'Native','Native',
                                   ifelse(migratorystatus %in% c('1.5G migrant','2G migrant'),'1.5G+2G migrant',
                                          '2.5G migrant')),
                            levels = c('Native','1.5G+2G migrant','2.5G migrant'))]

jpeg(filename="fig.sample.jpeg",width=8,height=6,units='in',res=500)
ggplot(data=indiv[migratorystatus != 'Native'],aes(x=generation,fill=broadbackground)) +
  geom_bar(color='black',position='dodge') +
  theme_light() +
  xlab("") + ylab("Count") + labs(fill='') +
  theme(legend.position = 'top',legend.justification = 'center')
dev.off()

# Which countries primarily composed our interest categories?
indiv[generation != 'Native' & broadbackground2 == 'Northwest Europe',
      prop.table(table(background))*100]
indiv[generation != 'Native' & broadbackground2 == 'South & East Europe',
      prop.table(table(background))*100]
indiv[generation != 'Native' & broadbackground2 == 'Former USSR',
      prop.table(table(background))*100]
indiv[generation != 'Native' & broadbackground2 == 'Sub-Saharan Africa',
      prop.table(table(background))*100]
indiv[generation != 'Native' & broadbackground2 == 'Middle East & North Africa',
      prop.table(table(background))*100]
indiv[generation != 'Native' & broadbackground2 == 'South & Southeast Asia',
      prop.table(table(background))*100]
indiv[generation != 'Native' & broadbackground2 == 'East Asia, Americas & Pacific',
      prop.table(table(background))*100]

########################################################################################################################

# DATA IN PERSON-YEAR FORM

# For every individual in our sample, we need their period between 15 and parenthood (or truncation)
# The event of interest is parenthood
indiv[,mom := ifelse(!is.na(indiv$parenthoodage),1,0)]
# The exit is either dead, transition to parenthood, or end of the observation period
indiv[,exit := mapply(min,kuolv,parenthoodyear,vuosi,na.rm=TRUE)]
# We cover from 15 until exit
indiv[,end := exit - syntyv] # Year of exit minus year of birth
# Person-year format
indivyear <- survSplit(data=indiv[,.(shnro,syntyv,mom,exit,end,migratorystatus,generation,broadbackground,country,firstcohabage,
                                     adoptparenthoodage,smkunta)], # variables needed
                       cut=14:max(indiv$end), # from the year the turned 15 until exit
                       end='end',event='mom',start='start')

# Remove observations starting at zero 
indivyear <- as.data.table(indivyear) # data.table format
indivyear <- indivyear[end >= 15] 

#######################

# EXTRACT RISK SETS AND EVENTS PER GROUP (Natives, 2.5G migrants and 1.5G-2G migrants - the last two by background too)
ts.nat <- survfit(Surv(start,end,mom)~1,conf.type='none'
                  ,data=indivyear[generation == 'Native'])
ts.mig <- survfit(Surv(start,end,mom)~broadbackground,conf.type='none',
                   data=indivyear[generation == '1.5G+2G migrant'])
ts.mix <- survfit(Surv(start,end,mom)~broadbackground,conf.type='none',
                   data=indivyear[generation == '2.5G migrant'])

# Put results into a data set
hazarddata <- data.table(
  generation = c(rep('Native',times=25*7),rep('1.5G+2G migrant',times=25*7),rep('2.5G migrant',times=25*7)),
  broadbackground = c(rep(indiv[,levels(broadbackground)[-1]],each=25,times=3)),
  time = c(rep(ts.nat$time,7),ts.mig$time,ts.mix$time),
  risk = c(rep(ts.nat$n.risk,7),ts.mig$n.risk,ts.mix$n.risk),
  mom = c(rep(ts.nat$n.event,7),ts.mig$n.event,ts.mix$n.event),
  survival = c(rep(ts.nat$surv,7),ts.mig$surv,ts.mix$surv), # survival
  se = c(rep(ts.nat$std.err,7),ts.mig$std.err,ts.mix$std.err) # SE
)

hazarddata[,hazard := mom/risk] # hazards: event / risk set
hazarddata[,se.h := sqrt(hazard*(1-hazard)/risk)] # SE of the hazard

# Visualization
hazarddata[,broadbackground := factor(broadbackground,
                                      c('Finland','Northwest Europe','South & East Europe','Former USSR',
                                        'Middle East & North Africa','Sub-Saharan Africa',
                                        'South & Southeast Asia','East Asia, Americas & Pacific'))]

# Estimated hazard probability
p1 <- ggplot(data=hazarddata[time <= 30],
             aes(time,hazard,color=generation,group=generation)) +
  geom_line() +
  geom_pointrange(aes(ymin=(hazard-se.h*qnorm(.975)),ymax=(hazard+se.h*qnorm(.975)))) +
  facet_wrap(~broadbackground,nrow=1,scales='free_y') +
  theme_light() +
  scale_color_manual(values=c('Native'='black','1.5G+2G migrant'='firebrick2','2.5G migrant'='dodgerblue')) +
  xlab("") + ylab("Estimated hazard probability") + labs(group='',color='') +
  theme(legend.position = 'top',legend.justification = 'center',strip.background = element_rect(fill='black'))

# Estimated survival probability
p2 <- ggplot(data=hazarddata[time <= 30],
             aes(time,survival,color=generation,group=generation)) +
  geom_line() +
  geom_pointrange(aes(ymin=(survival-se*qnorm(.975)),ymax=(survival+se*qnorm(.975)))) +
  facet_wrap(~broadbackground,nrow=1,scales='free_y') +
  theme_light() +
  scale_color_manual(values=c('Native'='black','1.5G+2G migrant'='firebrick2','2.5G migrant'='dodgerblue')) +
  xlab("Age at first child") + ylab("Estimated survival probability") + labs(group='',color='') +
  theme(legend.position = 'top',legend.justification = 'center',strip.background = element_rect(fill='black'))

jpeg(filename="fig.curves.jpeg",width=15,height=8,units='in',res=500)
ggarrange(p1,p2,
          labels=c('',''),ncol=1,common.legend = TRUE)
dev.off()

########################################################################################################################

# DATA ENRICHMENT (COVARIATES)

# 1) PARTNERSHIP STATUS

# We can use partnership data to distinguish the period before and after first cohabitation or marriage
indivyear[,partnerstatus := ifelse(is.na(firstcohabage),'Never partnered (yet)',
                                   ifelse(!is.na(firstcohabage) & end < firstcohabage,'Never partnered (yet)',
                                          'Cohabitation'))]
indivyear[,round(prop.table(table(end,partnerstatus),1),3)] # check
# As expected, observations in a partnership at ages 15-17 are rare. It is after 18 that values start increasing

#######################

# Now, we can use time-varying data to further distinguish if the individual is married, in cohabitation, or neither
# Data loading
load('personyear.RData')
# Let's reduce to observations from those in the sample
data <- data[shnro %in% indiv[,shnro]]

# We can connect with the time varying data (personyear)
data[,end := vuosi-syntyv] # the age of the person at observations (need for linkage)
indivyear <- merge(indivyear[,-'syntyv'],data, # syntyv is in both datasets
                   by=c('shnro','end'),all.x=TRUE)
rm(data)

#######################

# How many observations did we miss in the registers?
indivyear[is.na(vuosi)] # 23,635 out of 4,656,457 (the lost is minimal, fortunately)
indivyear <- indivyear[!is.na(vuosi)] # Remember person-year observations never observed

# Now, we can use the information in spuhnro (if no partner that observation)
indivyear[partnerstatus == 'Cohabitation' & is.na(spuhnro)]$partnerstatus <- 'Previous partner'

# Finally, we can distinguish cohabitation from marriage by using the date of marriage with the current partner
indivyear[,marriageyear := substr(vihkipvm,1,4)] # I only need the year, not month and day
indivyear[partnerstatus == 'Cohabitation' & vuosi >= marriageyear]$partnerstatus <- 'Marriage'

# Change levels of this new variable
indivyear[,partnerstatus := factor(partnerstatus,
                                   levels=c('Never partnered (yet)','Previous partner','Cohabitation','Marriage'))]

# Check
indivyear[,table(partnerstatus)]
indivyear[,round(prop.table(table(end,partnerstatus),1),3)]
# Compare with categories in FOLK: unmarried (1), married or separated (2), divorced (4), widowed (5)
indivyear[,table(partnerstatus,sivs)] # minor disagreements

# If somebody characterized as 'never partnered (yet)' figures as married or separated (2), divorced (4), widowed (5),
# I changed the category to 'Previous partner'
indivyear[partnerstatus == 'Never partnered (yet)' & sivs %in% 2:5]$partnerstatus <- 'Previous partner'

indivyear[,table(partnerstatus,sivs)]
indivyear[,round(prop.table(table(partnerstatus,sivs),1),3)] 

#######################

# 2) EDUCATION LEVEL

indivyear[,table(end,ututku_aste,useNA='always')] # NAs abound in younger ages
# Turn into Low, intermediate and high
indivyear[,education := factor(ifelse(is.na(ututku_aste),'Low/Unknown status',
                               ifelse(ututku_aste %in% 3:5,'Intermediate','High')),
                               levels=c('In education','Low/Unknown status','Intermediate','High'))]
# For years 15-16, all individuals are still in education
indivyear[end %in% 15:16]$education <- 'In education'
# For individuals of 17 or above, if their status (sose) is student (60), we assign them 'Still in education'
indivyear[end > 16,table(end,sose == 60)] # 60 means student
indivyear[end > 16 & sose == 60]$education <- 'In education'

# Check
indivyear[,table(end,education,useNA = 'always')]
indivyear[,round(prop.table(table(end,education),1),2)]

#######################

# 3) HABITAT: URBAN-RURAL DIVIDE FOR FERTILITY

# Two variables can be useful: maka and kuntaryhm
indivyear[,table(maka,useNA = 'always')]
indivyear[,table(kuntaryhm,useNA = 'always')] # urban, semi-urban, rural
indivyear[,table(maka,kuntaryhm,useNA = 'always')] # Combining the two variables, only 196 cases missing

# Habitat
indivyear[,habitat := factor(ifelse(is.na(kuntaryhm),NA,
                                    ifelse(kuntaryhm == 1,'Urban',
                                    ifelse(kuntaryhm == 2,'Semi-urban','Rural'))),
                             levels=c('Rural','Semi-urban','Urban'))]
indivyear[,table(habitat,kuntaryhm,useNA = 'always')]  # check
# Now, let's complement the NAs with the information in maka
indivyear[is.na(habitat) & maka %in% c('K1','K2')]$habitat <- 'Urban'
indivyear[is.na(habitat) & maka %in% c('K3')]$habitat <- 'Semi-urban'
indivyear[is.na(habitat) & maka %in% c('M4','M5','M6','M7')]$habitat <- 'Rural'

# See distribution of different groups by type of habitat
indivyear[,round(prop.table(table(broadbackground,habitat),1),3)]

#######################

# 4) LANGUAGE

indivyear[,round(prop.table(table(broadbackground,kieli_k,useNA='always'),1),3)]
# As factor
indivyear[,language := factor(kieli_k,levels=1:3,labels=c('Finnish','Swedish','Other'))]

#######################

# 5) UNEMPLOYMENT
indivyear[,summary(as.factor(tyke))] # Many people do not have unemployment months or is NA
indivyear[,plot(as.factor(tyke))] # shape of the distribution

indivyear[,unemployment := factor(ifelse(is.na(tyke),'0',
                                         ifelse(tyke == 0,'0',
                                                ifelse(tyke %in% 1:3,'1-3',
                                                       ifelse(tyke %in% 4:6,'4-6','7-12')))),
                                  levels=c('0','1-3','4-6','7-12'))]
indivyear[,summary(unemployment)]

########################################################################################################################

# Save image
save(indivyear,file='sample.women.RData')

########################################################################################################################