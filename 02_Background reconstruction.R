########################################################################################################################
# (2) ANCESTRY CULTURE AND FERTILITY (Background reconstruction)
# R script written by Jose Luis Estevez (Vaestoliitto)
# Date: Oct 4th, 2023
########################################################################################################################

# CLEAN THE ENVIRONMENT
rm(list=ls())

# REQUIRED PACKAGES
library(data.table);library(dplyr)

# DATA LOADING
load('individuals.RData')
load('personyear.RData')
load('children.RData')

########################################################################################################################

# SAMPLE OF INDIVIDUALS WHOSE MIGRATORY BACKGROUND NEEDS TO BE RETRIEVED

# all people in our sample, their parents, their partners, and their children's other parent
IDindiv <- indiv[,shnro] # indiv
IDparent <- unique(c(indiv[,shnro_m],indiv[,shnro_f],indiv[,shnro_am],indiv[,shnro_af])) # parents (also adoptive)
IDpartner <- unique(data[,spuhnro]) 
IDpartner <- IDpartner[!(IDpartner %in% IDindiv)] # partners 
IDotherparent <- unique(c(forchildren[,shnro_m],forchildren[,shnro_f]))
IDotherparent <- IDotherparent[!(IDotherparent %in% c(IDindiv,IDpartner))] # children's other parent

# There must be many overlaps, let put them all together
IDs <- unique(c(IDindiv,IDparent,IDpartner,IDotherparent)) # A sample of 2,967,290 unique individuals
# Let's make this a data table
IDs <- data.table(shnro = IDs)

########################################################################################################################

# MUUTT DATASET ON MOBILITY

# Data loading
muutt <- fread("W:/Nordforsk/shared/data/u1708_a_folkmuutto_1987_2021.csv")
muutt <- muutt[shnro %in% IDs[,shnro]] # Remove data pertaining to other people different from those we need

# Remember that values 991, 997, 998 and 999 are forms of missing, so let's re-categorize them
muutt[kansa1_m %in% 991:999]$kansa1_m <- NA
muutt[kansa2_m %in% 991:999]$kansa2_m <- NA
muutt[svaltio_m %in% 991:999]$svaltio_m <- NA

# We can retrieve the migratory background: first and second nationality, and country of birth
bg1 <- muutt[,.(shnro,vuosi,kansa1_m)] # first nationality
bg2 <- muutt[,.(shnro,vuosi,kansa2_m)] # second nationality
bg3 <- muutt[,.(shnro,vuosi,svaltio_m)] # country of birth
# Remove NAs
bg1 <- na.omit(bg1)
bg2 <- na.omit(bg2)
bg3 <- na.omit(bg3)

# Take the oldest observation per person for nationality (sometimes people adopt the Finnish nationality over time)
bg1 <- bg1[order(vuosi,decreasing = FALSE)][!duplicated(shnro)][,.(shnro,kansa1_m)]
bg2 <- bg2[order(vuosi,decreasing = FALSE)][!duplicated(shnro)][,.(shnro,kansa2_m)]
# But the newest observation for country of birth (sometimes registers correct this information)
bg3 <- bg3[order(vuosi,decreasing = TRUE)][!duplicated(shnro)][,.(shnro,svaltio_m)]

# Let's put all the information together
IDs <- merge(IDs,bg1,by='shnro',all.x = TRUE)
IDs <- merge(IDs,bg2,by='shnro',all.x = TRUE)
IDs <- merge(IDs,bg3,by='shnro',all.x = TRUE)

# Remove first two rows (NAs and '')
IDs <- IDs[-c(1:2),]
names(IDs) <- c('shnro','nation1','nation2','birthcountry') # rename variables
backgrounds <- IDs

# We managed to find the country of birth of ...
backgrounds[!is.na(birthcountry),length(shnro)] # 2,403,361 unique individuals
backgrounds[!is.na(birthcountry),length(shnro)] / nrow(backgrounds) # 81.0% 
# Of the sample to be analyzed
backgrounds[shnro %in% indiv[,shnro] & !is.na(birthcountry),length(shnro)] # 1,404,443 
backgrounds[shnro %in% indiv[,shnro] & !is.na(birthcountry),length(shnro)] / nrow(indiv) # 96.8%

########################################################################################################################

# COMPLEMENT DATA IN MUUTT WITH DATA IN FOLK

# Let's check consistency across registers (FOLK and MUUTT)
indiv <- merge(indiv,backgrounds,by='shnro',all.x=TRUE)
indiv[,table(syntyp2,birthcountry == '246',useNA = 'always')] # Only 159 mismatches

# I corrected these cases using information on their nationality
indiv[syntyp2 %in% c(12,22) & birthcountry == 246]$nation1
sum(indiv[syntyp2 %in% c(12,22) & birthcountry == 246]$nation1 != 246) # 84 cases can be corrected
indiv[syntyp2 %in% c(12,22) & birthcountry == 246]$birthcountry <- indiv[syntyp2 %in% c(12,22) & birthcountry == 246]$nation1
indiv[,table(syntyp2,birthcountry == '246',useNA = 'always')] # These reduce the mismatches to 75

# INPUT FINLAND AS COUNTRY OF BIRTH WHEN FOLK CATEGORIZE THE PERSON ACCORDINGLY
# Let's input Finland as country of birth for those with birthcountry missing and values 11 or 21 in FOLK
indiv[is.na(birthcountry) & syntyp2 %in% c(11,21)]$birthcountry <- 246
indiv[,table(syntyp2,birthcountry == '246',useNA = 'always')] # 4,404 cases still missing

# INPUT NATIONALTY AS COUNTRY OF BIRTH WHEN BORN ABROAD (12, 22) AND COUNTRY OF BIRTH IS MISSING
indiv[is.na(birthcountry) & nation1 != 246,sum(!is.na(nation1))] # 3,673 individuals with this info available
indiv[is.na(birthcountry) & nation1 != 246]$birthcountry <- indiv[is.na(birthcountry) & nation1 != 246]$nation1
indiv[,table(syntyp2,birthcountry == '246',useNA = 'always')] 

# Save this information also in the object backgrounds
backgrounds <- rbind(indiv[,.(shnro,nation1,nation2,birthcountry)],
                     backgrounds[!(shnro %in% indiv[,shnro])])

# As a result of this imputation, the missing cases reduced substantively 
backgrounds[shnro %in% indiv[,shnro] & !is.na(birthcountry),length(shnro)] #  we know country of birth of 1,450,673 
backgrounds[shnro %in% indiv[,shnro] & !is.na(birthcountry),length(shnro)] / nrow(indiv) # 99.95%

########################################################################################################################

# COMPLEMENT DATA FOR PARENTS AND PARTNERS USING FOLK AS WELL

# Let's omit those already in our sample and look at parents and partners only
backgrounds2 <- backgrounds[!(shnro %in% indiv[,shnro])]
# In how many cases we have no information on country of birth?
nrow(backgrounds2[is.na(birthcountry)]) / nrow(backgrounds2) # around 34.1%

#######################

# FOLK DATA (1987-2019)

data3 <- data2 <- data <- list()

data[['1987']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1987_all_ids.RDS"))
data[['1988']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1988_all_ids.RDS"))
data[['1989']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1989_all_ids.RDS"))
data[['1990']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1990_all_ids.RDS"))
data[['1991']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1991_all_ids.RDS"))
data[['1992']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1992_all_ids.RDS"))
data[['1993']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1993_all_ids.RDS"))
data[['1994']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1994_all_ids.RDS"))
data[['1995']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1995_all_ids.RDS"))
data[['1996']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1996_all_ids.RDS"))
data[['1997']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1997_all_ids.RDS"))

# SAMPLE SELECTION
for(i in seq_along(data)){
  # only observations from parents and partners (those in backgrounds2)
  data[[i]] <- data[[i]][shnro %in% backgrounds2[,shnro]]
}

data2[['1998']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1998_all_ids.RDS"))
data2[['1999']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_1999_all_ids.RDS"))
data2[['2000']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2000_all_ids.RDS"))
data2[['2001']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2001_all_ids.RDS"))
data2[['2002']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2002_all_ids.RDS"))
data2[['2003']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2003_all_ids.RDS"))
data2[['2004']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2004_all_ids.RDS"))
data2[['2005']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2005_all_ids.RDS"))
data2[['2006']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2006_all_ids.RDS"))
data2[['2007']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2007_all_ids.RDS"))
data2[['2008']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2008_all_ids.RDS"))
data2[['2009']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2009_all_ids.RDS"))

# SAMPLE SELECTION
for(i in seq_along(data2)){
  data2[[i]] <- data2[[i]][shnro %in% backgrounds2[,shnro]]
}

data3[['2010']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2010_all_ids.RDS"))
data3[['2011']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2011_all_ids.RDS"))
data3[['2012']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2012_all_ids.RDS"))
data3[['2013']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2013_all_ids.RDS"))
data3[['2014']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2014_all_ids.RDS"))
data3[['2015']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2015_all_ids.RDS"))
data3[['2016']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2016_all_ids.RDS"))
data3[['2017']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2017_all_ids.RDS"))
data3[['2018']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2018_all_ids.RDS"))
data3[['2019']] <- data.table(readRDS("W:/Nordforsk/shared/data/Basic_yearwise_info/basic_2019_all_ids.RDS"))

# SAMPLE SELECTION
for(i in seq_along(data3)){
  data3[[i]] <- data3[[i]][shnro %in% backgrounds2[,shnro]]
}

# Put data altogether
parentpartnerdata <- append(data,append(data2,data3))
rm(data);rm(data2);rm(data3)
parentpartnerdata <- do.call(rbind,parentpartnerdata) # omit sub-folders

# Save image (I will keep this object for retrieving other variables later)
save(parentpartnerdata,file='personyear.parentpartner.RData')

#######################

# For the moment I only need the origin as registered in FOLK
folkorig <- parentpartnerdata[,c('shnro','vuosi','syntyp2')]
folkorig <- na.omit(folkorig) # in case there is missing data
# Remove observations from the same individuals, keep only the last one
folkorig <- folkorig[order(vuosi,decreasing = TRUE)][!duplicated(shnro)]

# Now, let's merge the data
backgrounds2 <- merge(backgrounds2,folkorig[,c('shnro','syntyp2')],by='shnro',all.x = TRUE)
rm(folkorig)

# Now, we can complement the data for parents and partners
# Check for discrepancies between registers first
backgrounds2[,table(syntyp2,birthcountry == '246',useNA = 'always')] # 36 mismatches

# For those categorized as Finnish in MUUTT but FOLK says born abroad, I checked their nationality
backgrounds2[syntyp2 %in% c(12,22) & birthcountry == 246]$nation1
sum(backgrounds2[syntyp2 %in% c(12,22) & birthcountry == 246]$nation1 != 246) # 6 cases can be corrected
backgrounds2[syntyp2 %in% c(12,22) & birthcountry == 246]$birthcountry <- backgrounds2[syntyp2 %in% c(12,22) & birthcountry == 246]$nation1
backgrounds2[,table(syntyp2,birthcountry == '246',useNA = 'always')] # 31 mismatches

# How about the other 4 people missclassified?
backgrounds2[syntyp2 == 11 & birthcountry != '246']
backgrounds2[syntyp2 == 21 & birthcountry != '246'] 
# All them are born abroad

# INPUT FINLAND AS COUNTRY OF BIRTH WHEN FOLK CATEGORIZE THE PERSON ACCORDINGLY
# Let's input Finland as country of birth for those with birthcountry missing and values 11 or 21 in FOLK
backgrounds2[is.na(birthcountry) & syntyp2 %in% c(11,21)]$birthcountry <- 246
backgrounds2[,table(syntyp2,birthcountry == '246',useNA = 'always')] # 11,763 cases still missing

# INPUT NATIONALTY AS COUNTRY OF BIRTH WHEN BORN ABROAD (12, 22) AND COUNTRY OF BIRTH IS MISSING
backgrounds2[is.na(birthcountry) & nation1 != 246,sum(!is.na(nation1))] # 2,579 individuals with this info available
backgrounds2[is.na(birthcountry) & nation1 != 246]$birthcountry <- backgrounds2[is.na(birthcountry) & nation1 != 246]$nation1
backgrounds2[,table(syntyp2,birthcountry == '246',useNA = 'always')] 

# As a result of this imputation, the missing cases reduced substantively 
backgrounds2[!is.na(birthcountry),length(shnro)] #  we know country of birth of 1,506,700 
backgrounds2[!is.na(birthcountry),length(shnro)] / nrow(backgrounds2) # 99.39%

# Now, let's merge this with the data from people in the main sample
backgrounds <- backgrounds[shnro %in% indiv[,shnro]]
backgrounds <- rbind(backgrounds,backgrounds2[,-'syntyp2'])
rm(backgrounds2)

########################################################################################################################

# COUNTRY NAMES INSTEAD OF CODES

# Increase memory for this procedure
Sys.setenv(R_MAX_VSIZE=1000)

backgrounds <- backgrounds %>%
  mutate(country = case_when(
    birthcountry == '4' ~ 'Afghanistan',
    birthcountry == '8' ~ 'Albania',
    birthcountry == '12' ~ 'Algeria',
    birthcountry == '16' ~ 'American Samoa',
    birthcountry == '20' ~ 'Andorra',
    birthcountry == '24' ~ 'Angola',
    birthcountry == '660' ~ 'Anguilla',
    birthcountry == '10' ~ 'Antarctica',
    birthcountry == '28' ~ 'Antigua and Barbuda',
    birthcountry == '32' ~ 'Argentina',
    birthcountry == '51' ~ 'Armenia',
    birthcountry == '533' ~ 'Aruba',
    birthcountry == '36' ~ 'Australia',
    birthcountry == '40' ~ 'Austria',
    birthcountry == '31' ~ 'Azerbaijan',
    birthcountry == '44' ~ 'Bahamas',
    birthcountry == '48' ~ 'Bahrain',
    birthcountry == '50' ~ 'Bangladesh',
    birthcountry == '52' ~ 'Barbados',
    birthcountry == '112' ~ 'Belarus',
    birthcountry == '56' ~ 'Belgium',
    birthcountry == '84' ~ 'Belize',
    birthcountry == '204' ~ 'Benin',
    birthcountry == '60' ~ 'Bermuda',
    birthcountry == '64' ~ 'Bhutan',
    birthcountry == '68' ~ 'Bolivia',
    birthcountry == '535' ~ 'Bonaire, Sint Eustatius and Saba',
    birthcountry == '70' ~ 'Bosnia and Herzegovina',
    birthcountry == '72' ~ 'Botswana',
    birthcountry == '74' ~ 'Bouvet Island',
    birthcountry == '76' ~ 'Brazil',
    birthcountry == '86' ~ 'British Indian Ocean Territory',
    birthcountry == '96' ~ 'Brunei Darussalam',
    birthcountry == '100' ~ 'Bulgaria',
    birthcountry == '854' ~ 'Burkina Faso',
    birthcountry == '108' ~ 'Burundi',
    birthcountry == '116' ~ 'Cambodia',
    birthcountry == '120' ~ 'Cameroon',
    birthcountry == '124' ~ 'Canada',
    birthcountry == '132' ~ 'Cabo Verde',
    birthcountry == '136' ~ 'Cayman Islands',
    birthcountry == '140' ~ 'Central African Republic',
    birthcountry == '148' ~ 'Chad',
    birthcountry == '152' ~ 'Chile',
    birthcountry == '156' ~ 'China',
    birthcountry == '162' ~ 'Christmas Island',
    birthcountry == '166' ~ 'Cocos (Keeling) Islands',
    birthcountry == '170' ~ 'Colombia',
    birthcountry == '174' ~ 'Comoros',
    birthcountry == '178' ~ 'Congo (Congo-Brazzaville)',
    birthcountry == '180' ~ 'Congo, The Democratic Republic of',
    birthcountry == '184' ~ 'Cook Islands',
    birthcountry == '188' ~ 'Costa Rica',
    birthcountry == '384' ~ 'Ivory Coast',
    birthcountry == '191' ~ 'Croatia',
    birthcountry == '192' ~ 'Cuba',
    birthcountry == '531' ~ 'Curacao',
    birthcountry == '196' ~ 'Cyprus',
    birthcountry == '203' ~ 'Czechia',
    birthcountry == '208' ~ 'Denmark',
    birthcountry == '262' ~ 'Djibouti',
    birthcountry == '212' ~ 'Dominica',
    birthcountry == '214' ~ 'Dominican Republic',
    birthcountry == '626' ~ 'East Timor',
    birthcountry == '218' ~ 'Ecuador',
    birthcountry == '818' ~ 'Egypt',
    birthcountry == '222' ~ 'El Salvador',
    birthcountry == '226' ~ 'Equatorial Guinea',
    birthcountry == '232' ~ 'Eritrea',
    birthcountry == '233' ~ 'Estonia',
    birthcountry == '231' ~ 'Ethiopia',
    birthcountry == '238' ~ 'Falkland Islands (Malvinas)',
    birthcountry == '234' ~ 'Faroe Islands',
    birthcountry == '242' ~ 'Fiji',
    birthcountry == '246' ~ 'Finland',
    birthcountry == '250' ~ 'France',
    birthcountry == '254' ~ 'French Guiana',
    birthcountry == '258' ~ 'French Polynesia',
    birthcountry == '260' ~ 'French Southern Territories',
    birthcountry == '266' ~ 'Gabon',
    birthcountry == '270' ~ 'Gambia',
    birthcountry == '268' ~ 'Georgia',
    birthcountry == '276' ~ 'Germany',
    birthcountry == '288' ~ 'Ghana',
    birthcountry == '292' ~ 'Gibraltar',
    birthcountry == '300' ~ 'Greece',
    birthcountry == '304' ~ 'Greenland',
    birthcountry == '308' ~ 'Grenada',
    birthcountry == '312' ~ 'Guadeloupe',
    birthcountry == '316' ~ 'Guam',
    birthcountry == '320' ~ 'Guatemala',
    birthcountry == '831' ~ 'Guernsey',
    birthcountry == '324' ~ 'Guinea',
    birthcountry == '624' ~ 'Guinea-Bissau',
    birthcountry == '328' ~ 'Guyana',
    birthcountry == '332' ~ 'Haiti',
    birthcountry == '334' ~ 'Heard Island and McDonald Island',
    birthcountry == '336' ~ 'Holy See (Vatican City State)',
    birthcountry == '340' ~ 'Honduras',
    birthcountry == '344' ~ 'Hong Kong',
    birthcountry == '348' ~ 'Hungary',
    birthcountry == '352' ~ 'Iceland',
    birthcountry == '356' ~ 'India',
    birthcountry == '360' ~ 'Indonesia',
    birthcountry == '364' ~ 'Iran',
    birthcountry == '368' ~ 'Iraq',
    birthcountry == '372' ~ 'Ireland',
    birthcountry == '833' ~ 'Isle of Man',
    birthcountry == '376' ~ 'Israel',
    birthcountry == '380' ~ 'Italy',
    birthcountry == '388' ~ 'Jamaica',
    birthcountry == '392' ~ 'Japan',
    birthcountry == '832' ~ 'Jersey',
    birthcountry == '400' ~ 'Jordan',
    birthcountry == '398' ~ 'Kazakhstan',
    birthcountry == '404' ~ 'Kenya',
    birthcountry == '296' ~ 'Kiribati',
    birthcountry == '408' ~ 'North Korea',
    birthcountry == '410' ~ 'South Korea',
    birthcountry == '414' ~ 'Kuwait',
    birthcountry == '417' ~ 'Kyrgyzstan',
    birthcountry == '418' ~ 'Laos',
    birthcountry == '428' ~ 'Latvia',
    birthcountry == '422' ~ 'Lebanon',
    birthcountry == '426' ~ 'Lesotho',
    birthcountry == '430' ~ 'Liberia',
    birthcountry == '434' ~ 'Libya',
    birthcountry == '438' ~ 'Liechtenstein',
    birthcountry == '440' ~ 'Lithuania',
    birthcountry == '442' ~ 'Luxenbourg',
    birthcountry == '446' ~ 'Macao',
    birthcountry == '807' ~ 'North Macedonia',
    birthcountry == '450' ~ 'Madagascar',
    birthcountry == '454' ~ 'Malawi',
    birthcountry == '458' ~ 'Malaysia',
    birthcountry == '462' ~ 'Maldives',
    birthcountry == '466' ~ 'Mali',
    birthcountry == '470' ~ 'Malta',
    birthcountry == '584' ~ 'Marshall Islands',
    birthcountry == '474' ~ 'Martinique',
    birthcountry == '478' ~ 'Mauritania',
    birthcountry == '480' ~ 'Mauritius',
    birthcountry == '175' ~ 'Mayotte',
    birthcountry == '484' ~ 'Mexico',
    birthcountry == '583' ~ 'Micronesia',
    birthcountry == '498' ~ 'Moldova',
    birthcountry == '492' ~ 'Monaco',
    birthcountry == '496' ~ 'Mongolia',
    birthcountry == '499' ~ 'Montenegro',
    birthcountry == '500' ~ 'Montserrat',
    birthcountry == '504' ~ 'Morocco',
    birthcountry == '508' ~ 'Mozambique',
    birthcountry == '104' ~ 'Myanmar',
    birthcountry == '516' ~ 'Namibia',
    birthcountry == '520' ~ 'Nauru',
    birthcountry == '524' ~ 'Nepal',
    birthcountry == '528' ~ 'Netherlands',
    birthcountry == '540' ~ 'New Caledonia',
    birthcountry == '554' ~ 'New Zealand',
    birthcountry == '558' ~ 'Nicaragua',
    birthcountry == '562' ~ 'Niger',
    birthcountry == '566' ~ 'Nigeria',
    birthcountry == '570' ~ 'Niue',
    birthcountry == '574' ~ 'Norfolk Island',
    birthcountry == '580' ~ 'Northern Mariana Islands',
    birthcountry == '578' ~ 'Norway',
    birthcountry == '512' ~ 'Oman',
    birthcountry == '586' ~ 'Pakistan',
    birthcountry == '585' ~ 'Palau',
    birthcountry == '275' ~ 'Palestine',
    birthcountry == '591' ~ 'Panama',
    birthcountry == '598' ~ 'Papua New Guinea',
    birthcountry == '600' ~ 'Paraguay',
    birthcountry == '604' ~ 'Peru',
    birthcountry == '608' ~ 'Philippines',
    birthcountry == '612' ~ 'Pitcairn',
    birthcountry == '616' ~ 'Poland',
    birthcountry == '620' ~ 'Portugal',
    birthcountry == '630' ~ 'Puerto Rico',
    birthcountry == '634' ~ 'Qatar',
    birthcountry == '638' ~ 'Reunion',
    birthcountry == '642' ~ 'Romania',
    birthcountry == '643' ~ 'Russian Federation',
    birthcountry == '646' ~ 'Rwanda',
    birthcountry == '652' ~ 'Saint Barthelemy',
    birthcountry == '654' ~ 'Saint Helena',
    birthcountry == '659' ~ 'Saint Kitts and Nevis',
    birthcountry == '662' ~ 'Saint Lucia',
    birthcountry == '663' ~ 'Saint Martin',
    birthcountry == '666' ~ 'Saint Pierre and Miquelon',
    birthcountry == '670' ~ 'Saint Vincent and the Grenadines',
    birthcountry == '882' ~ 'Samoa',
    birthcountry == '674' ~ 'San Marino',
    birthcountry == '678' ~ 'Sao Tome and Principe',
    birthcountry == '682' ~ 'Saudi Arabia',
    birthcountry == '686' ~ 'Senegal',
    birthcountry == '688' ~ 'Serbia',
    birthcountry == '690' ~ 'Seychelles',
    birthcountry == '694' ~ 'Sierra Leone',
    birthcountry == '702' ~ 'Singapore',
    birthcountry == '534' ~ 'Sint Maarten',
    birthcountry == '703' ~ 'Slovakia',
    birthcountry == '705' ~ 'Slovenia',
    birthcountry == '90' ~ 'Solomon Islands',
    birthcountry == '706' ~ 'Somalia',
    birthcountry == '710' ~ 'South Africa',
    birthcountry == '239' ~ 'South Georgia and the Sandwich Islands',
    birthcountry == '728' ~ 'South Sudan',
    birthcountry == '724' ~ 'Spain',
    birthcountry == '144' ~ 'Sri Lanka',
    birthcountry == '729' ~ 'Sudan',
    birthcountry == '740' ~ 'Suriname',
    birthcountry == '744' ~ 'Svalbard and Jan Mayen',
    birthcountry == '748' ~ 'Eswatini',
    birthcountry == '752' ~ 'Sweden',
    birthcountry == '756' ~ 'Switzerland',
    birthcountry == '760' ~ 'Syria',
    birthcountry == '158' ~ 'Taiwan',
    birthcountry == '762' ~ 'Tajikistan',
    birthcountry == '834' ~ 'Tanzania',
    birthcountry == '764' ~ 'Thailand',
    birthcountry == '768' ~ 'Togo',
    birthcountry == '772' ~ 'Tokelau',
    birthcountry == '776' ~ 'Tonga',
    birthcountry == '780' ~ 'Trinidad and Tobago',
    birthcountry == '788' ~ 'Tunisia',
    birthcountry == '792' ~ 'Turkey',
    birthcountry == '795' ~ 'Turkmenistan',
    birthcountry == '796' ~ 'Turks and Caicos Islands',
    birthcountry == '798' ~ 'Tuvalu',
    birthcountry == '800' ~ 'Uganda',
    birthcountry == '804' ~ 'Ukraine',
    birthcountry == '784' ~ 'United Arab Emirates',
    birthcountry == '826' ~ 'United Kingdom',
    birthcountry == '840' ~ 'United States',
    birthcountry == '581' ~ 'United States Minor Outlying Islands',
    birthcountry == '858' ~ 'Uruguay',
    birthcountry == '860' ~ 'Uzbekistan',
    birthcountry == '876' ~ 'Wallis and Futuna',
    birthcountry == '548' ~ 'Vanuatu',
    birthcountry == '862' ~ 'Venezuela',
    birthcountry == '732' ~ 'Western Sahara',
    birthcountry == '704' ~ 'Vietnam',
    birthcountry == '92' ~ 'Virgin Islands, British',
    birthcountry == '850' ~ 'Virgin Islands, United States',
    birthcountry == '887' ~ 'Yemen',
    birthcountry == '894' ~ 'Zambia',
    birthcountry == '716' ~ 'Zimbabwe',
    birthcountry == '248' ~ 'Aland Islands',
    # Former codes
    birthcountry == '736' ~ 'Former Sudan',
    birthcountry == '886' ~ 'Former South Yemen',
    birthcountry == '200' ~ 'Former Czechoslovakia',
    birthcountry == '810' ~ 'Former Soviet Union',
    birthcountry == '890' ~ 'Former Yugoslavia',
    birthcountry == '891' ~ 'Former Serbia and Montenegro',
    birthcountry == '991' ~ 'Without citizenship',
    TRUE ~ 'Unknown'
  ))

# Let's turn Unknown to NA
backgrounds[country == 'Unknown']$country <- NA

# Most common backgrounds
head(backgrounds[,as.data.table(table(country))][order(N,decreasing = TRUE)],11)

# Most common backgrounds (only individuals directly analyzed)
head(backgrounds[shnro %in% indiv[,shnro],
                 as.data.table(table(country))][order(N,decreasing = TRUE)],11)

#######################

# Save images
save(backgrounds,file='backgrounds.RData')

########################################################################################################################