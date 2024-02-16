########################################################################################################################
# (3) ANCESTRY CULTURE AND FERTILITY (Variable construction)
# R script written by Jose Luis Estevez (Vaestoliitto)
# Date: Oct 5th, 2023
########################################################################################################################

# CLEAN THE ENVIRONMENT
rm(list=ls())

# REQUIRED PACKAGES
library(data.table);library(dplyr)

# DATA LOADING
load('individuals.RData')
load('backgrounds.RData')

########################################################################################################################

# CLASSIFICATION OF INDIVIDUALS BASED ON MIGRATORY STATUS AS NATIVES, MIGRANTS, 2ND GENERATION ETC.

# Let's retrieve the individual's country of origin first
backgrounds <- backgrounds[,c('shnro','country')]
indiv <- merge(indiv,backgrounds,by='shnro',all.x = TRUE)

# Now let's find out who is second generation migrant using info about parents' country of birth and nationality
bg_m <- bg_f <- backgrounds
names(bg_m) <- paste(names(bg_m),'_m',sep='')
names(bg_f) <- paste(names(bg_f),'_f',sep='')

# Let's merge this information in the object indiv
indiv <- merge(indiv,bg_m,by='shnro_m',all.x = TRUE)
indiv <- merge(indiv,bg_f,by='shnro_f',all.x = TRUE)

# If we look at the parents' country of birth
indiv[country == 'Finland',table(country_m != 'Finland',useNA = 'always')] # 14,511 have their mum born abroad
indiv[country == 'Finland',table(country_f != 'Finland',useNA = 'always')] # 16,564 have their dad born abroad
indiv[country == 'Finland',table((country_f != 'Finland' | country_m != 'Finland'),useNA = 'always')] # 26,733 have a foreign parent
indiv[country == 'Finland',table((country_f != 'Finland' & country_m != 'Finland'),useNA = 'always')] # 4,342 have both foreign parents

########################

# New variable: MIGRATORY STATUS
indiv[,migratorystatus := NA]
indiv[,migratorystatus := factor(migratorystatus,
                                 levels=c('Native','1G migrant','2G migrant','2.5G migrant','Returnee'))]

# 3 categories (MIGRANTS, 2G MIGRANTS, AND RETURNEES) can be inferred from the FOLK classification
indiv[syntyp2 == 22]$migratorystatus <- '1G migrant'
indiv[syntyp2 == 12]$migratorystatus <- 'Returnee'
indiv[syntyp2 == 21]$migratorystatus <- '2G migrant'

# The tricky part is to distinguish 2.5G migrants from natives
# If born in Finland and at least one foreign parent: '2.5G migrant'
indiv[is.na(migratorystatus) & country == 'Finland' & (country_m != 'Finland' | country_f != 'Finland')]$migratorystatus <- '2.5G migrant'
# If both parents, then '2G migrant'
indiv[is.na(migratorystatus) & country == 'Finland' & country_m != 'Finland' & country_f != 'Finland']$migratorystatus <- '2G migrant'
# If born in Finland and no trace of migration from parents, then 'Native'
indiv[is.na(migratorystatus) & country == 'Finland' & !(country_m != 'Finland' | country_f != 'Finland')]$migratorystatus <- 'Native'

# Check output
indiv[,table(migratorystatus,useNA='always')] 
# Check overlap with FOLK categories
indiv[,table(migratorystatus,syntyp2,useNA='always')] 
# Check by sex
indiv[,table(migratorystatus,sukup,useNA='always')]

########################################################################################################################

# CLASSIFICATION OF INDIVIDUALS BASED ON (ANCESTRAL) BACKGROUND

indiv[,background := NA]
indiv[,background := as.character(background)]

# Natives and retunees have Finland as background
indiv[migratorystatus %in% c('Native','Returnee')]$background <- 'Finland'
# Migrants receive their own birth country
indiv[migratorystatus == '1G migrant']$background <- indiv[migratorystatus == '1G migrant']$country

# 2.5G migrants receive the background of the non-native parent
# 2G migrants give priority to the mother's birth country (or use the one available)
indiv[migratorystatus %in% c('2.5G migrant','2G migrant')]$background <- 
  indiv[migratorystatus %in% c('2.5G migrant','2G migrant')]$country_m
indiv[migratorystatus %in% c('2.5G migrant','2G migrant')
      & (is.na(background) | background == 'Finland')]$background <-
  indiv[migratorystatus %in% c('2.5G migrant','2G migrant') & (is.na(background) | background == 'Finland')]$country_f

# Check output
indiv[,table(background,migratorystatus,useNA='always')]

# How many cases of mixed parents do we have?
indiv[migratorystatus == '2G migrant' & (country_m != country_f),
      c('shnro','background','country_m','country_f')] # 1,137 cases

# MOST COMMON BACKGROUNDS
# Most common backgrounds among 2G nd 2.5G migrants
head(indiv[migratorystatus %in% c('2G migrant','2.5G migrant'),
           as.data.table(table(background))][order(N,decreasing = TRUE)],12)
# Most common backgrounds among 1G migrants
head(indiv[migratorystatus %in% c('1G migrant'),
           as.data.table(table(background))][order(N,decreasing = TRUE)],12)

########################################################################################################################

# 1.5G MIGRANTS (AGE AT ARRIVAL)

# Let's find out who, among the 1G migrants, their age at arrival in Finland
migrants <- indiv[migratorystatus == '1G migrant']

# Load MUUTT data again
muutt <- fread("W:/Nordforsk/shared/data/u1708_a_folkmuutto_1987_2021.csv")
muutt <- muutt[shnro %in% migrants[,shnro]] # select only observations from migrants

# Select only immigration
immig <- muutt[muuttolaji == 41]
immig <- immig[order(vuosi,decreasing = FALSE)][!duplicated(shnro)] # the first movement registered
immig <- immig[,c('shnro','vuosi')] # keep only name and year of immigration
names(immig) <- c('shnro','year_arrival')

# Merge data
migrants <- merge(migrants,immig,by='shnro',all.x=TRUE)
migrants[,age_arrival := year_arrival - syntyv] # Age of arrival

# Visualization
hist(migrants[,age_arrival])

# Reclassification as 1.5 migrants
migrants[age_arrival <= 15]$migratorystatus <- '1.5G migrant'
migrants[,table(migratorystatus)] # 28,468 cases of 1.5G migrants

########################

# DATA OF ARRIVAL MISSING: CHECK FOLK
# How about migration before 1987? (We miss movement data during those years)
hist(migrants[is.na(age_arrival)]$syntyv) # Many missing cases among those born in the 1980s

# For these I used  FOLK data. If the first observation happened before they turned 15, they were in Finland already
load('personyear.RData')

# keep only observations from migrants with date of arrival missing
data <- data[shnro %in% migrants[is.na(age_arrival),shnro]]
# the earliest observation available
data <- data[order(vuosi,decreasing = FALSE)][!duplicated(shnro)]
data <- data[,c('shnro','vuosi')]
names(data) <- c('shnro','inf_year_arrival')

# Merge data
migrants <- merge(migrants,data,by='shnro',all.x = TRUE)
migrants[,inf_age_arrival := inf_year_arrival - syntyv] # Age of arrival

# Visualization
hist(migrants[,inf_age_arrival])

# Reclassification as 1.5 migrants
migrants[inf_age_arrival <= 15]$migratorystatus <- '1.5G migrant'
migrants[,table(migratorystatus)] # 29,636 cases of 1.5G migrants

########################

# LET'S PUT THE DATA TOGETHER
indiv$inf_age_arrival <- indiv$inf_year_arrival <- indiv$age_arrival <- indiv$year_arrival <- NA
indiv <- indiv[is.na(migratorystatus) | migratorystatus != '1G migrant']
indiv <- rbind(indiv,migrants)

########################################################################################################################

# Check output
indiv[,table(migratorystatus,useNA='always')] 
# Check overlap with FOLK categories
indiv[,table(migratorystatus,syntyp2,useNA='always')] 
# Show only those whose (ancestry) background we know
indiv[!is.na(background),table(migratorystatus,syntyp2,useNA='always')] 

# MOST COMMON BACKGROUNDS
# Most common backgrounds among 2G, 2.5G, and 1.5G migrants
head(indiv[migratorystatus %in% c('2G migrant','2.5G migrant','1.5G migrant'),
           as.data.table(table(background))][order(N,decreasing = TRUE)],15) 

########################################################################################################################

# THE FINAL STEP IS TO CLASSIFY THE MANY COUNTRIES INTO BROADER CATEGORIES

# Increase memory for this procedure
Sys.setenv(R_MAX_VSIZE=1000)

indiv <- indiv %>%
  mutate(broadbackground = case_when(
    background == 'Afghanistan' ~ 'MENA',
    background == 'Albania' ~ 'South East Europe',
    background == 'Algeria' ~ 'MENA',
    background == 'American Samoa' ~ 'Other',
    background == 'Andorra' ~ 'South Europe',
    background == 'Angola' ~ 'Africa',
    background == 'Anguilla' ~ 'Americas',
    background == 'Antarctica' ~ 'Other',
    background == 'Antigua and Barbuda' ~ 'Americas',
    background == 'Argentina' ~ 'Americas',
    background == 'Armenia' ~ 'East Europe-Central Asia',
    background == 'Aruba' ~ 'Americas',
    background == 'Australia' ~ 'US-CA-AU-NZ',
    background == 'Austria' ~ 'West Europe',
    background == 'Azerbaijan' ~ 'East Europe-Central Asia',
    background == 'Bahamas' ~ 'Americas',
    background == 'Bahrain' ~ 'MENA',
    background == 'Bangladesh' ~ 'South Asia',
    background == 'Barbados' ~ 'Americas',
    background == 'Belarus' ~ 'East Europe-Central Asia',
    background == 'Belgium' ~ 'West Europe',
    background == 'Belize' ~ 'Americas',
    background == 'Benin' ~ 'Africa',
    background == 'Bermuda' ~ 'Americas',
    background == 'Bhutan' ~ 'South Asia',
    background == 'Bolivia' ~ 'Americas',
    background == 'Bonaire, Sint Eustatius and Saba' ~ 'Americas',
    background == 'Bosnia and Herzegovina' ~ 'South East Europe',
    background == 'Botswana' ~ 'Africa',
    background == 'Bouvet Island' ~ 'Other',
    background == 'Brazil' ~ 'Americas',
    background == 'British Indian Ocean Territory' ~ 'Other',
    background == 'Brunei Darussalam' ~ 'South East Asia',
    background == 'Bulgaria' ~ 'South East Europe',
    background == 'Burkina Faso' ~ 'Africa',
    background == 'Burundi' ~ 'Africa',
    background == 'Cambodia' ~ 'South East Asia',
    background == 'Cameroon' ~ 'Africa',
    background == 'Canada' ~ 'US-CA-AU-NZ',
    background == 'Cabo Verde' ~ 'Africa',
    background == 'Cayman Islands' ~ 'Americas',
    background == 'Central African Republic' ~ 'Africa',
    background == 'Chad' ~ 'Africa',
    background == 'Chile' ~ 'Americas',
    background == 'China' ~ 'East Asia',
    background == 'Christmas Island' ~ 'Other',
    background == 'Cocos (Keeling) Islands' ~ 'Other',
    background == 'Colombia' ~ 'Americas',
    background == 'Comoros' ~ 'Africa',
    background == 'Congo (Congo-Brazzaville)' ~ 'Africa',
    background == 'Congo, The Democratic Republic of' ~ 'Africa',
    background == 'Cook Islands' ~ 'Other',
    background == 'Costa Rica' ~ 'Americas',
    background == 'Ivory Coast' ~ 'Africa',
    background == 'Croatia' ~ 'South East Europe',
    background == 'Cuba' ~ 'Americas',
    background == 'Curacao' ~ 'Americas',
    background == 'Cyprus' ~ 'South Europe',
    background == 'Czechia' ~ 'South East Europe',
    background == 'Denmark' ~ 'Nordic',
    background == 'Djibouti' ~ 'Africa',
    background == 'Dominica' ~ 'Americas',
    background == 'Dominican Republic' ~ 'Americas',
    background == 'East Timor' ~ 'South East Asia',
    background == 'Ecuador' ~ 'Americas',
    background == 'Egypt' ~ 'MENA',
    background == 'El Salvador' ~ 'Americas',
    background == 'Equatorial Guinea' ~ 'Africa',
    background == 'Eritrea' ~ 'Africa',
    background == 'Estonia' ~ 'East Europe-Central Asia',
    background == 'Ethiopia' ~ 'Africa',
    background == 'Falkland Islands (Malvinas)' ~ 'Other',
    background == 'Faroe Islands' ~ 'Nordic',
    background == 'Fiji' ~ 'Other',
    background == 'Finland' ~ 'Finland',
    background == 'France' ~ 'West Europe',
    background == 'French Guiana' ~ 'Americas',
    background == 'French Polynesia' ~ 'Other',
    background == 'French Southern Territories' ~ 'Other',
    background == 'Gabon' ~ 'Africa',
    background == 'Gambia' ~ 'Africa',
    background == 'Georgia' ~ 'East Europe-Central Asia',
    background == 'Germany' ~ 'West Europe',
    background == 'Ghana' ~ 'Africa',
    background == 'Gibraltar' ~ 'Other',
    background == 'Greece' ~ 'South Europe',
    background == 'Greenland' ~ 'Nordic',
    background == 'Grenada' ~ 'Americas',
    background == 'Guadeloupe' ~ 'Americas',
    background == 'Guam' ~ 'Other',
    background == 'Guatemala' ~ 'Americas',
    background == 'Guernsey' ~ 'West Europe',
    background == 'Guinea' ~ 'Africa',
    background == 'Guinea-Bissau' ~ 'Africa',
    background == 'Guyana' ~ 'Americas',
    background == 'Haiti' ~ 'Americas',
    background == 'Heard Island and McDonald Island' ~ 'Other',
    background == 'Holy See (Vatican City State)' ~ 'South Europe',
    background == 'Honduras' ~ 'Americas',
    background == 'Hong Kong' ~ 'East Asia',
    background == 'Hungary' ~ 'South East Europe',
    background == 'Iceland' ~ 'Nordic',
    background == 'India' ~ 'South Asia',
    background == 'Indonesia' ~ 'South East Asia',
    background == 'Iran' ~ 'MENA',
    background == 'Iraq' ~ 'MENA',
    background == 'Ireland' ~ 'West Europe',
    background == 'Isle of Man' ~ 'West Europe',
    background == 'Israel' ~ 'MENA',
    background == 'Italy' ~ 'South Europe',
    background == 'Jamaica' ~ 'Americas',
    background == 'Japan' ~ 'East Asia',
    background == 'Jersey' ~ 'West Europe',
    background == 'Jordan' ~ 'MENA',
    background == 'Kazakhstan' ~ 'East Europe-Central Asia',
    background == 'Kenya' ~ 'Africa',
    background == 'Kiribati' ~ 'Other',
    background == 'North Korea' ~ 'East Asia',
    background == 'South Korea' ~ 'East Asia',
    background == 'Kuwait' ~ 'MENA',
    background == 'Kyrgyzstan' ~ 'East Europe-Central Asia',
    background == 'Laos' ~ 'South East Asia',
    background == 'Latvia' ~ 'East Europe-Central Asia',
    background == 'Lebanon' ~ 'MENA',
    background == 'Lesotho' ~ 'Africa',
    background == 'Liberia' ~ 'Africa',
    background == 'Libya' ~ 'MENA',
    background == 'Liechtenstein' ~ 'West Europe',
    background == 'Lithuania' ~ 'East Europe-Central Asia',
    background == 'Luxenbourg' ~ 'West Europe',
    background == 'Macao' ~ 'East Asia',
    background == 'North Macedonia' ~ 'South East Europe',
    background == 'Madagascar' ~ 'Africa',
    background == 'Malawi' ~ 'Africa',
    background == 'Malaysia' ~ 'South East Asia',
    background == 'Maldives' ~ 'Other',
    background == 'Mali' ~ 'Africa',
    background == 'Malta' ~ 'South Europe',
    background == 'Marshall Islands' ~ 'Other',
    background == 'Martinique' ~ 'Americas',
    background == 'Mauritania' ~ 'Africa',
    background == 'Mauritius' ~ 'Africa',
    background == 'Mayotte' ~ 'Other',
    background == 'Mexico' ~ 'Americas',
    background == 'Micronesia' ~ 'Other',
    background == 'Moldova' ~ 'East Europe-Central Asia',
    background == 'Monaco' ~ 'South Europe',
    background == 'Mongolia' ~ 'East Europe-Central Asia',
    background == 'Montenegro' ~ 'South East Europe',
    background == 'Montserrat' ~ 'Americas',
    background == 'Morocco' ~ 'MENA',
    background == 'Mozambique' ~ 'Africa',
    background == 'Myanmar' ~ 'South East Asia',
    background == 'Namibia' ~ 'Africa',
    background == 'Nauru' ~ 'Other',
    background == 'Nepal' ~ 'South Asia',
    background == 'Netherlands' ~ 'West Europe',
    background == 'New Caledonia' ~ 'Other',
    background == 'New Zealand' ~ 'US-CA-AU-NZ',
    background == 'Nicaragua' ~ 'Americas',
    background == 'Niger' ~ 'Africa',
    background == 'Nigeria' ~ 'Africa',
    background == 'Niue' ~ 'Other',
    background == 'Norfolk Island' ~ 'Other',
    background == 'Northern Mariana Islands' ~ 'Other',
    background == 'Norway' ~ 'Nordic',
    background == 'Oman' ~ 'MENA',
    background == 'Pakistan' ~ 'South Asia',
    background == 'Palau' ~ 'Other',
    background == 'Palestine' ~ 'MENA',
    background == 'Panama' ~ 'Americas',
    background == 'Papua New Guinea' ~ 'South East Asia',
    background == 'Paraguay' ~ 'Americas',
    background == 'Peru' ~ 'Americas',
    background == 'Philippines' ~ 'South East Asia',
    background == 'Pitcairn' ~ 'Other',
    background == 'Poland' ~ 'South East Europe',
    background == 'Portugal' ~ 'South Europe',
    background == 'Puerto Rico' ~ 'Americas',
    background == 'Qatar' ~ 'MENA',
    background == 'Reunion' ~ 'Other',
    background == 'Romania' ~ 'South East Europe',
    background == 'Russian Federation' ~ 'East Europe-Central Asia',
    background == 'Rwanda' ~ 'Africa',
    background == 'Saint Barthelemy' ~ 'Americas',
    background == 'Saint Helena' ~ 'Other',
    background == 'Saint Kitts and Nevis' ~ 'Americas',
    background == 'Saint Lucia' ~ 'Americas',
    background == 'Saint Martin' ~ 'Americas',
    background == 'Saint Pierre and Miquelon' ~ 'Other',
    background == 'Saint Vincent and the Grenadines' ~ 'Americas',
    background == 'Samoa' ~ 'Other',
    background == 'San Marino' ~ 'South Europe',
    background == 'Sao Tome and Principe' ~ 'Africa',
    background == 'Saudi Arabia' ~ 'MENA',
    background == 'Senegal' ~ 'Africa',
    background == 'Serbia' ~ 'South East Europe',
    background == 'Seychelles' ~ 'Other',
    background == 'Sierra Leone' ~ 'Africa',
    background == 'Singapore' ~ 'South East Asia',
    background == 'Sint Maarten' ~ 'Americas',
    background == 'Slovakia' ~ 'South East Europe',
    background == 'Slovenia' ~ 'South East Europe',
    background == 'Solomon Islands' ~ 'Other',
    background == 'Somalia' ~ 'Africa',
    background == 'South Africa' ~ 'Africa',
    background == 'South Georgia and the Sandwich Islands' ~ 'Other',
    background == 'South Sudan' ~ 'Africa',
    background == 'Spain' ~ 'South Europe',
    background == 'Sri Lanka' ~ 'South Asia',
    background == 'Sudan' ~ 'Africa',
    background == 'Suriname' ~ 'Americas',
    background == 'Svalbard and Jan Mayen' ~ 'Other',
    background == 'Eswatini' ~ 'Africa',
    background == 'Sweden' ~ 'Nordic',
    background == 'Switzerland' ~ 'West Europe',
    background == 'Syria' ~ 'MENA',
    background == 'Taiwan' ~ 'East Asia',
    background == 'Tajikistan' ~ 'East Europe-Central Asia',
    background == 'Tanzania' ~ 'Africa',
    background == 'Thailand' ~ 'South East Asia',
    background == 'Togo' ~ 'Africa',
    background == 'Tokelau' ~ 'Other',
    background == 'Tonga' ~ 'Other',
    background == 'Trinidad and Tobago' ~ 'Americas',
    background == 'Tunisia' ~ 'MENA',
    background == 'Turkey' ~ 'MENA',
    background == 'Turkmenistan' ~ 'East Europe-Central Asia',
    background == 'Turks and Caicos Islands' ~ 'Americas',
    background == 'Tuvalu' ~ 'Other',
    background == 'Uganda' ~ 'Africa',
    background == 'Ukraine' ~ 'East Europe-Central Asia',
    background == 'United Arab Emirates' ~ 'MENA',
    background == 'United Kingdom' ~ 'West Europe',
    background == 'United States' ~ 'US-CA-AU-NZ',
    background == 'United States Minor Outlying Islands' ~ 'Other',
    background == 'Uruguay' ~ 'Americas',
    background == 'Uzbekistan' ~ 'East Europe-Central Asia',
    background == 'Wallis and Futuna' ~ 'Other',
    background == 'Vanuatu' ~ 'Other',
    background == 'Venezuela' ~ 'Americas',
    background == 'Western Sahara' ~ 'MENA',
    background == 'Vietnam' ~ 'South East Asia',
    background == 'Virgin Islands, British' ~ 'Americas',
    background == 'Virgin Islands, United States' ~ 'Americas',
    background == 'Yemen' ~ 'MENA',
    background == 'Zambia' ~ 'Africa',
    background == 'Zimbabwe' ~ 'Africa',
    background == 'Aland Islands' ~ 'Finland',
    background == 'Former Sudan' ~ 'Africa',
    background == 'Former South Yemen' ~ 'MENA',
    background == 'Former Czechoslovakia' ~ 'South East Europe',
    background == 'Former Soviet Union' ~ 'East Europe-Central Asia',
    background == 'Former Yugoslavia' ~ 'South East Europe',
    background == 'Former Serbia and Montenegro' ~ 'South East Europe',
    background == 'Without citizenship' ~ '',
    TRUE ~ 'Unknown'
  ))

# Redefine unknown as NA
indiv[broadbackground == 'Unknown']$broadbackground <- NA

# Distribution in these categories
indiv[migratorystatus %in% c('2G migrant','2.5G migrant','1.5G migrant') & broadbackground != 'Finland',
      table(broadbackground)]

#######################

# IT SEEMS SOME CATEGORIES ARE TOO SMALL
indiv <- indiv %>%
  mutate(broadbackground2 = case_when(
    broadbackground == 'Finland' ~ 'Finland',
    broadbackground == 'Nordic' ~ 'Northwest Europe',
    broadbackground == 'West Europe' ~ 'Northwest Europe',
    broadbackground == 'South Europe' ~ 'South & East Europe',
    broadbackground == 'South East Europe' ~ 'South & East Europe',
    broadbackground == 'East Europe-Central Asia' ~ 'Former USSR',
    broadbackground == 'MENA' ~ 'Middle East & North Africa',
    broadbackground == 'Africa' ~ 'Sub-Saharan Africa',
    broadbackground == 'South Asia' ~ 'South & Southeast Asia',
    broadbackground == 'South East Asia' ~ 'South & Southeast Asia',
    broadbackground == 'East Asia' ~ 'East Asia, Americas & Pacific',
    broadbackground == 'US-CA-AU-NZ' ~ 'East Asia, Americas & Pacific',
    broadbackground == 'Americas' ~ 'East Asia, Americas & Pacific',
    broadbackground == 'Other' ~ 'East Asia, Americas & Pacific',
    TRUE ~ 'Unknown'
  ))

# Redefine unknown as NA
indiv[broadbackground2 == 'Unknown']$broadbackground2 <- NA

# The reference category will be Finland
indiv[,broadbackground2 := factor(broadbackground2,
                                 levels=c('Finland','Northwest Europe','South & East Europe','Former USSR',
                                          'Middle East & North Africa','Sub-Saharan Africa',
                                          'South & Southeast Asia','East Asia, Americas & Pacific'))]

# Distribution in these categories
indiv[migratorystatus %in% c('2G migrant','2.5G migrant','1.5G migrant') & broadbackground2 != 'Finland',
      table(broadbackground2)]

#######################

# Save images
save(indiv,file='individuals.background.RData')

########################################################################################################################