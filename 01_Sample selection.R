########################################################################################################################
# (1) ANCESTRY CULTURE AND FERTILITY (Sample selection)
# R script written by Jose Luis Estevez (Vaestoliitto)
# Date: Sep 21st, 2023
########################################################################################################################

# CLEAN THE ENVIRONMENT
rm(list=ls())

# REQUIRED PACKAGES
library(data.table)

########################################################################################################################

# FOLK DATA (1995-2019)

data2 <- data <- list()

data[['1995']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1995_all_ids.RDS"))
data[['1996']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1996_all_ids.RDS"))
data[['1997']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1997_all_ids.RDS"))
data[['1998']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1998_all_ids.RDS"))
data[['1999']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1999_all_ids.RDS"))
data[['2000']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2000_all_ids.RDS"))
data[['2001']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2001_all_ids.RDS"))
data[['2002']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2002_all_ids.RDS"))
data[['2003']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2003_all_ids.RDS"))
data[['2004']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2004_all_ids.RDS"))
data[['2005']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2005_all_ids.RDS"))
data[['2006']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2006_all_ids.RDS"))
data[['2007']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2007_all_ids.RDS"))

# SAMPLE SELECTION
for(i in seq_along(data)){
  # birth cohort 1980-99, only observations when they are 15 to 45 years of age
  data[[i]] <- data[[i]][(syntyv %in% 1980:1999) & (vuosi - syntyv) %in% 15:45]
}

data2[['2008']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2008_all_ids.RDS"))
data2[['2009']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2009_all_ids.RDS"))
data2[['2010']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2010_all_ids.RDS"))
data2[['2011']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2011_all_ids.RDS"))
data2[['2012']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2012_all_ids.RDS"))
data2[['2013']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2013_all_ids.RDS"))
data2[['2014']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2014_all_ids.RDS"))
data2[['2015']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2015_all_ids.RDS"))
data2[['2016']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2016_all_ids.RDS"))
data2[['2017']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2017_all_ids.RDS"))
data2[['2018']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2018_all_ids.RDS"))
data2[['2019']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2019_all_ids.RDS"))

# SAMPLE SELECTION
for(i in seq_along(data2)){
  data2[[i]] <- data2[[i]][(syntyv %in% 1980:1999) & (vuosi - syntyv) %in% 15:45]
}

# Put data altogether
data <- append(data,data2)
rm(data2)

########################################################################################################################

# SAMPLE

# Count individuals rather than person-year observations
indiv <- do.call(rbind,data)
indiv <- indiv[order(vuosi,decreasing = TRUE)][!duplicated(shnro)] # pick only the last observation of each person

# See summary by background and whether or not born in Finland, according to the classification in Statistics Finland
indiv[,summary(as.factor(syntyp2))]
# Broken by sex
indiv[,table(sukup,as.factor(syntyp2))]

########################################################################################################################

# PARENT-CHILD DATA (1987-2019)

# Data loading
parentchild <- fread("W:/Nordforsk/shared/data/u1708_a_folkllvv_19872021_f.csv")
parentchild <- parentchild[,-'V1'] # this column is some noise

# First, we need to connect individuals with their parents' IDs
# I will start by restricting these data to only individuals born between 1980 and 1999
forparents <- parentchild[syntyv %in% 1980:1999]
# For each person we can have several observations (this is person-year data). So, let's keep the last
forparents <- forparents[order(vuosi,decreasing = TRUE)][!duplicated(shnro)]
# We found data for 90.1% of all individuals in our sample
nrow(forparents)/nrow(indiv)

# Let's correct blank spaces "" with NAs
forparents[shnro_m == "",]$shnro_m <- NA
forparents[shnro_f == "",]$shnro_f <- NA
forparents[shnro_am == "",]$shnro_am <- NA
forparents[shnro_af == "",]$shnro_af <- NA
forparents[shnro_s1 == "",]$shnro_s1 <- NA
forparents[shnro_s2 == "",]$shnro_s2 <- NA

# Let's merge the datasets indiv and forparents
indiv <- merge(indiv,forparents[,-'vuosi'],by=c('shnro','syntyv'),all.x=TRUE)

# As expected, most NAs corresponds to individuals born abroad, especially for those with foreign background (22)
indiv[,prop.table(table(is.na(shnro_m),as.factor(syntyp2)),2)*100] # Mother info missing
indiv[,prop.table(table(is.na(shnro_f),as.factor(syntyp2)),2)*100] # Father info missing

#######################

# We can use the parent-child data to also identify the children of those in our sample and their age at first birth
# Restrict parent-child data to only observations when either father or mother is in our sample
forchildren <- parentchild[(shnro_m %in% indiv[,shnro]) 
                           | (shnro_f %in% indiv[,shnro])]
# Let's keep one observation per child (the last): FOR 601,137 children
forchildren <- forchildren[order(vuosi,decreasing = TRUE)][!duplicated(shnro)]

# When were these children
mean(forchildren$syntyv)
range(forchildren$syntyv)
hist(forchildren$syntyv)

# Now that we have all children let's retrieve the number of children and age at first birth
# For this, it will be easier to divide by sex (father and mother)
moms <- forchildren[shnro_m %in% indiv[sukup==2,shnro]]
dads <- forchildren[shnro_f %in% indiv[sukup==1,shnro]]

# Number of children (quantum)
quantmoms <- moms[,length(shnro),by=shnro_m][order(V1)]
quantdads <- dads[,length(shnro),by=shnro_f][order(V1)]
names(quantmoms) <- names(quantdads) <- c('shnro','children2019')
# Merge information with indiv
indiv <- merge(indiv,rbind(quantmoms,quantdads),by='shnro',all.x=TRUE)
indiv[is.na(children2019)]$children2019 <- 0 # turn NAs to zeroes

# Sample by sex (sukup) and number of children in 2020
indiv[,table(children2019,sukup)]

# Finally, let's retrieve the age at first birth
agemoms <- moms[,min(syntyv),by=shnro_m][order(V1)]
agedads <- dads[,min(syntyv),by=shnro_f][order(V1)]
names(agemoms) <- names(agedads) <- c('shnro','parenthoodyear')
# Merge information with indiv
indiv <- merge(indiv,rbind(agemoms,agedads),by='shnro',all.x=TRUE)

# By substracting year of birth from year at first birth, we  can get the age at first birth
indiv[,parenthoodage := parenthoodyear - syntyv]

# Visualization of age at first birth by sex of the parent
par(mfrow=c(1,2))
hist(indiv[sukup == 2]$parenthoodage,breaks=20,main='Age at first birt for mothers') # women
hist(indiv[sukup == 1]$parenthoodage,breaks=20,main='Age at first birt for fathers') # men

# Many are not parents (yet), we can see this by sex
indiv[,prop.table(table(children2019==0,sukup),2)*100] 

#######################

# ADOPTIVE CHILDREN
adopchildren <- parentchild[(shnro_am %in% indiv[,shnro]) 
                            | (shnro_af %in% indiv[,shnro])]
# Let's keep one observation per adopted child (the first): 949 children
adopchildren <- adopchildren[order(vuosi,decreasing = FALSE)][!duplicated(shnro)]
# Parent's ID: 1,018 cases
adopted <- unique(c(adopchildren[,shnro_am],adopchildren[,shnro_af]))

# Let's retrieve the year of the first observation when someone is considered an adoptive parent
adopmomyear <- adopchildren[order(vuosi,decreasing=FALSE)][!duplicated(shnro_am)][,.(vuosi,shnro_am)]
adopdadyear <- adopchildren[order(vuosi,decreasing=FALSE)][!duplicated(shnro_af)][,.(vuosi,shnro_af)]
# Let's put this together
names(adopmomyear) <- names(adopdadyear) <- c('adoptparenthoodyear','shnro')
adopparentyear <- rbind(adopmomyear,adopdadyear)
# And merge in the indiv dataset
indiv <- merge(indiv,adopparentyear,by='shnro',all.x=TRUE) 

# By substracting year of birth, we  can get the age when become an adoptive parent
indiv[,adoptparenthoodage := adoptparenthoodyear - syntyv]

# Visualization of age at adoptive parenthood
par(mfrow=c(1,2))
hist(indiv[sukup == 2]$adoptparenthoodage,breaks=12,main='Age when child adopted for women') # women
hist(indiv[sukup == 1]$adoptparenthoodage,breaks=12,main='Age when child adopted for men') # men

# How many did not have a child of their own (yet) when adopting
indiv[!is.na(adoptparenthoodage)] # We have 908 cases of adoptive parents

indiv[!is.na(adoptparenthoodage) & # they adopted AND
        (is.na(parenthoodage) | # either they never became parents OR
           (!is.na(parenthoodage) & (parenthoodage > adoptparenthoodage)) )] # They became biological parents later
# 591 cases (65.1%) became adoptive parents before having a child of their own (if they ever did)

########################################################################################################################

# COHABITATION DATA (1987-2019)

# Data loading
aslii <- list()
aslii[['1']] <- fread("D:/ready-made/FOLK_aslii_8800a/folk_19872000_tua_aslii21tot_1.csv")
aslii[['2']] <- fread("D:/ready-made/FOLK_aslii_0110a/folk_20012010_tua_aslii21tot_1.csv")
aslii[['3']] <- fread("D:/ready-made/FOLK_aslii_11a/folk_20112020_tua_aslii21tot_1.csv")
# From the data, we need 2 things: partners per year for indiv, and information on split ups for indiv's parents

# PARTNERSHIP STATUS
# Put cohabitation data altogether
aslii <- do.call(rbind,aslii)

# Create new dataset (cohab) for only observations from the individuals in the sample
cohab <- aslii[shnro %in% indiv[,shnro]]

# I don't think we can simply merge the data as one person can have two (or more) different partners the same year, but let's check
cohab[,personyear := paste(shnro,vuosi)] 
cohab[,length(unique(personyear))] / nrow(cohab) 
# there's a unique observation per person and year; we can merge without problems
cohab <- cohab[,-'personyear']
data <- do.call(rbind,data)
data <- merge(data,cohab,by=c('shnro','vuosi'),all.x=TRUE)

# How many of our person-year observations are in partnership with someone
nrow(data[!is.na(spuhnro),]) # 6,985,785
data[,prop.table(table(!is.na(spuhnro)))*100] # 33.6% of the total
data[,prop.table(table(sukup,!is.na(spuhnro)),1)*100] # Larger prop. among women (38.4%) than men (29.1%)

# How many individuals in our sample have cohabitated, when did they started cohabitating with somebody
firstcohab <- data[!is.na(spuhnro)][order(vuosi,decreasing = FALSE)][!duplicated(shnro),.(shnro,vuosi)]
names(firstcohab) <- c('shnro','firstcohabyear') # first cohabitation year

indiv <- merge(indiv,firstcohab,by='shnro',all.x = TRUE)
indiv[,firstcohabage := firstcohabyear - syntyv] # first cohabitation age

# What proportion of individuals cohabitated by sex
indiv[,prop.table(table(sukup,!is.na(firstcohabage)),1)*100] # 73.8% of women, 61.7% of men

# Visualization
par(mfrow=c(1,2))
hist(indiv[sukup == 2]$firstcohabage,breaks=20,main='Age when first partnership for women') # women
hist(indiv[sukup == 1]$firstcohabage,breaks=20,main='Age when first partnership for men') # men

#######################

# Save images
save(indiv,file='individuals.RData')
save(data,file='personyear.RData')
save(forchildren,file='children.RData') # In case, we move to recurrent events

########################################################################################################################