setwd("")

library(data.table)
library(bit64)

#read age, incom, poverty rate, and education level files from ACS

mypath ="/ACS_tract_bg"

age_files <- list.files(path = mypath, pattern = "2_SEX BY AGE.csv", full.names = TRUE, recursive = TRUE)
income_files <- list.files(path = mypath, pattern = "59_HOUSEHOLD*", full.names = TRUE, recursive = TRUE)
poverty_files <- list.files(path = mypath, pattern = "48_POVERTY*", full.names = TRUE, recursive = TRUE)
edu_files <- list.files(path = mypath, pattern = "43_SEX*", full.names = TRUE, recursive = TRUE)

age_cbg = data.table()
income_cbg = data.table()
poverty_cbg = data.table()
edu_cbg = data.table()

for (i in 1:51) {
  
  print(i)
  #####################
  age = fread(age_files[i])
  age = age[,c(7:55,242)]# columns needed
  age$`Geography ID` = substr(age$`Geography ID`,8,10000)
  age = age[nchar(age$`Geography ID`) == 12] #block group level
  age[,3:25] = age[,3:25]+age[,27:49] # male + female
  age = age[,c(1,3:25,50)]
  
  age = age[`Geography ID` %in% nhts_acs$HHFIPS_BG]
  
  age_cbg = rbind(age_cbg,age)
  
  ##################
  income = fread(income_files[i])
  income = income[,c(7:23,205)]
  income$`Geography ID` = substr(income$`Geography ID`,8,10000)
  income = income[nchar(income$`Geography ID`) == 12]
  
  income = income[`Geography ID` %in% nhts_acs$HHFIPS_BG]
  
  income_cbg = rbind(income_cbg,income)

  ###############################
  poverty = fread(poverty_files[i])
  poverty = poverty[,c(7:65,243)]
  poverty$`Geography ID` = substr(poverty$`Geography ID`,8,10000)
  poverty = poverty[nchar(poverty$`Geography ID`) == 11]#tract level
  
  poverty = poverty[`Geography ID` %in% nhts_acs$HHFIPS_TRACT]
  
  poverty_cbg = rbind(poverty_cbg,poverty)
  
  ##############################
  edu = fread(edu_files[i])
  edu = edu[,c(7:89,249)]
  edu$`Geography ID` = substr(edu$`Geography ID`,8,10000)
  edu = edu[nchar(edu$`Geography ID`) == 11]
  
  edu = edu[`Geography ID` %in% nhts_acs$HHFIPS_TRACT]
  
  edu_cbg = rbind(edu_cbg,edu)
  
}

##############################
#merge age&income&poverty rate&education level
cbg_age_inc_pov_edu = merge(age_cbg, income_cbg, by = 'Geography ID')
cbg_age_inc_pov_edu = merge(cbg_age_inc_pov_edu, poverty_cbg, by = 'Geography ID')
cbg_age_inc_pov_edu = merge(cbg_age_inc_pov_edu, edu_cbg, by = 'Geography ID')


#combine age intervals based on needs
cbg_age_inc_pov_edu$cbg_age_pct_under_17 = (cbg_age_inc_pov_edu$B01001_003 + cbg_age_inc_pov_edu$B01001_004 + cbg_age_inc_pov_edu$B01001_005 + cbg_age_inc_pov_edu$B01001_006)/cbg_age_inc_pov_edu$B01001_001
cbg_age_inc_pov_edu$cbg_age_pct_18_24 = (cbg_age_inc_pov_edu$B01001_007 + cbg_age_inc_pov_edu$B01001_008 + cbg_age_inc_pov_edu$B01001_009 + cbg_age_inc_pov_edu$B01001_010)/cbg_age_inc_pov_edu$B01001_001
cbg_age_inc_pov_edu$cbg_age_pct_25_34 = (cbg_age_inc_pov_edu$B01001_011 + cbg_age_inc_pov_edu$B01001_012)/cbg_age_inc_pov_edu$B01001_001
cbg_age_inc_pov_edu$cbg_age_pct_35_54 = (cbg_age_inc_pov_edu$B01001_013 + cbg_age_inc_pov_edu$B01001_014 + cbg_age_inc_pov_edu$B01001_015 + cbg_age_inc_pov_edu$B01001_016)/cbg_age_inc_pov_edu$B01001_001
cbg_age_inc_pov_edu$cbg_age_pct_55_64 = (cbg_age_inc_pov_edu$B01001_017 + cbg_age_inc_pov_edu$B01001_018 + cbg_age_inc_pov_edu$B01001_019)/cbg_age_inc_pov_edu$B01001_001
cbg_age_inc_pov_edu$cbg_age_pct_65_over = (cbg_age_inc_pov_edu$B01001_020 + cbg_age_inc_pov_edu$B01001_021 + cbg_age_inc_pov_edu$B01001_022 + cbg_age_inc_pov_edu$B01001_023 + cbg_age_inc_pov_edu$B01001_024 + cbg_age_inc_pov_edu$B01001_025)/cbg_age_inc_pov_edu$B01001_001


#combine income intervals based on needs
cbg_age_inc_pov_edu$cbg_inc_pct_less_25k = (cbg_age_inc_pov_edu$B19001_002 + cbg_age_inc_pov_edu$B19001_003 + 
                                              cbg_age_inc_pov_edu$B19001_004 + cbg_age_inc_pov_edu$B19001_005)/cbg_age_inc_pov_edu$B19001_001
cbg_age_inc_pov_edu$cbg_inc_pct_25k_50k = (cbg_age_inc_pov_edu$B19001_006 + cbg_age_inc_pov_edu$B19001_007 + cbg_age_inc_pov_edu$B19001_008 + 
                                             cbg_age_inc_pov_edu$B19001_009 + cbg_age_inc_pov_edu$B19001_010)/cbg_age_inc_pov_edu$B19001_001

bg_age_inc_pov_edu$cbg_inc_pct_50k_75k = (cbg_age_inc_pov_edu$B19001_011 + cbg_age_inc_pov_edu$B19001_012)/cbg_age_inc_pov_edu$B19001_001
cbg_age_inc_pov_edu$cbg_inc_pct_75k_125k = (cbg_age_inc_pov_edu$B19001_013 + cbg_age_inc_pov_edu$B19001_014)/cbg_age_inc_pov_edu$B19001_001
cbg_age_inc_pov_edu$cbg_inc_pct_125k_more = (cbg_age_inc_pov_edu$B19001_015 + cbg_age_inc_pov_edu$B19001_016 + cbg_age_inc_pov_edu$B19001_017)/cbg_age_inc_pov_edu$B19001_001


#poverty rate: Population whose income in the past 12 months below poverty level
cbg_age_inc_pov_edu$ct_pov_rate = cbg_age_inc_pov_edu$B17001_002/cbg_age_inc_pov_edu$B17001_001


#education level: High school graduate and less, Some college no degree&Associate's degree, Bachelor's degree and higher
#original data are separated by different ages. combine them

cbg_age_inc_pov_edu$ct_edu_pct_less_HS = (  cbg_age_inc_pov_edu$B15001_004 + cbg_age_inc_pov_edu$B15001_005 + cbg_age_inc_pov_edu$B15001_006 + 
                                              cbg_age_inc_pov_edu$B15001_012 + cbg_age_inc_pov_edu$B15001_013 + cbg_age_inc_pov_edu$B15001_014 +
                                              cbg_age_inc_pov_edu$B15001_020 + cbg_age_inc_pov_edu$B15001_021 + cbg_age_inc_pov_edu$B15001_022 +
                                              cbg_age_inc_pov_edu$B15001_028 + cbg_age_inc_pov_edu$B15001_029 + cbg_age_inc_pov_edu$B15001_030 +
                                              cbg_age_inc_pov_edu$B15001_036 + cbg_age_inc_pov_edu$B15001_037 + cbg_age_inc_pov_edu$B15001_038  
                                            )/cbg_age_inc_pov_edu$B15001_001

cbg_age_inc_pov_edu$ct_edu_pct_SCND_AD = (  cbg_age_inc_pov_edu$B15001_007 + cbg_age_inc_pov_edu$B15001_008 + 
                                              cbg_age_inc_pov_edu$B15001_015 + cbg_age_inc_pov_edu$B15001_016 + 
                                              cbg_age_inc_pov_edu$B15001_023 + cbg_age_inc_pov_edu$B15001_024 + 
                                              cbg_age_inc_pov_edu$B15001_031 + cbg_age_inc_pov_edu$B15001_032 + 
                                              cbg_age_inc_pov_edu$B15001_039 + cbg_age_inc_pov_edu$B15001_040
                                            )/cbg_age_inc_pov_edu$B15001_001

cbg_age_inc_pov_edu$ct_edu_pct_BD_more = (    cbg_age_inc_pov_edu$B15001_009 + cbg_age_inc_pov_edu$B15001_010 + 
                                                cbg_age_inc_pov_edu$B15001_017 + cbg_age_inc_pov_edu$B15001_018 + 
                                                cbg_age_inc_pov_edu$B15001_025 + cbg_age_inc_pov_edu$B15001_026 + 
                                                cbg_age_inc_pov_edu$B15001_033 + cbg_age_inc_pov_edu$B15001_034 + 
                                                cbg_age_inc_pov_edu$B15001_041 + cbg_age_inc_pov_edu$B15001_042
                                              )/cbg_age_inc_pov_edu$B15001_001

acs_aggregate = cbg_age_inc_pov_edu[, c('Geography ID',	'cbg_inc_pct_less_25k',	'cbg_inc_pct_25k_50k',	'cbg_inc_pct_50k_75k',	'cbg_inc_pct_75k_125k',	
                                        'cbg_inc_pct_125k_more',	'cbg_age_pct_under_17',	'cbg_age_pct_18_24',	'cbg_age_pct_25_34',	'cbg_age_pct_35_54',	
                                        'cbg_age_pct_55_64',	'cbg_age_pct_65_over',	'ct_pov_rate',	'ct_edu_pct_less_HS',	'ct_edu_pct_SCND_AD',
                                        'ct_edu_pct_BD_more')]

#some variable without BG level information, use tract level instead
acs_aggregate_bg = acs_aggregate[nchar(acs_aggregate$`Geography ID`) == 12]
acs_aggregate_bg$TTFIPS = substr(acs_aggregate_bg$`Geography ID`,1,11)
acs_aggregate_bg = merge(acs_aggregate_bg[,c(1:12,17)], acs_aggregate[,c(1,13,14,15,16)], by.x = 'TTFIPS', by.y = 'Geography ID', all.x = TRUE)

names(acs_aggregate_bg)[2] = "BGFIPS"
acs_aggregate_bg = acs_aggregate_bg[,2:17]

#save for feature engineering for MDLD
fwrite(acs_aggregate_bg,"/ACS_20215Y_BG_age5_inc5_pov_edu.csv")

##############################
#read nhts person & trip data
nhts_2017_person = fread("/nhts2017_person.csv")
nhts_2017_trip = fread("/nhts2017_trip.csv")

#merge the geoid at cbg level

hhid_cbg = fread("/hhctbg.csv")

hhid_cbg$HHSTFIPS = formatC(hhid_cbg$HHSTFIPS, width = 2, format = "d", flag = "0")
hhid_cbg$HHCNTYFP = formatC(hhid_cbg$HHCNTYFP, width = 3, format = "d", flag = "0")
hhid_cbg$HHCT = formatC(hhid_cbg$HHCT, width = 6, format = "d", flag = "0")
hhid_cbg$HHBG = as.character(hhid_cbg$HHBG)
hhid_cbg$HHFIPS_BG = paste0(hhid_cbg$HHSTFIPS, hhid_cbg$HHCNTYFP, hhid_cbg$HHCT, hhid_cbg$HHBG)

nhts_acs = merge(nhts_2017_person, hhid_cbg[,c(1,6)], by = 'HOUSEID', all.x = TRUE)
nhts_acs$HHFIPS_TRACT = substr(nhts_acs$HHFIPS_BG,1,11)

#merge nhts with acs
nhts_acs = merge(nhts_acs, acs_aggregate_bg, by.x = 'HHFIPS_BG', by.y = 'Geography ID', all.x = TRUE)

#exclude people under 18
nhts_acs = nhts_acs[R_AGE >= 18]

#target feature 1: age group
nhts_acs$R_AGE_group = -1
nhts_acs[R_AGE >=18 & R_AGE <=24]$R_AGE_group = 0
nhts_acs[R_AGE >=25 & R_AGE <=34]$R_AGE_group = 1
nhts_acs[R_AGE >=35 & R_AGE <=54]$R_AGE_group = 2
nhts_acs[R_AGE >=55 & R_AGE <=64]$R_AGE_group = 3
nhts_acs[R_AGE >=65]$R_AGE_group = 4

#target feature 2: income group
nhts_acs$HH_INC_group = nhts_acs$HHFAMINC
nhts_acs[HHFAMINC <0]$HH_INC_group = -1
nhts_acs[HHFAMINC >=1 & HHFAMINC <=3]$HH_INC_group = 0
nhts_acs[HHFAMINC >=4 & HHFAMINC <=5]$HH_INC_group = 1
nhts_acs[HHFAMINC ==6]$HH_INC_group = 2
nhts_acs[HHFAMINC >=7 & HHFAMINC <=8]$HH_INC_group = 3
nhts_acs[HHFAMINC >=9]$HH_INC_group = 4

#generate person id
nhts_acs$ID = as.character(nhts_acs$HOUSEID*100 +nhts_acs$PERSONID)
nhts_2017_trip$ID = as.integer64(nhts_2017_trip$HOUSEID*100 +nhts_2017_trip$PERSONID)

#convert some raw value to percentile
nhts_acs$DISTTOWK_pctl = nhts_acs$DISTTOWK17
nhts_acs$GCDWORK_pctl = nhts_acs$GCDWORK

worker=nhts_acs[DISTTOWK17>=0]$DISTTOWK17
worker1=nhts_acs[GCDWORK>=0]$GCDWORK

for (i in which(nhts_acs$DISTTOWK17>=0)) {
  if(i %% 1000==0) {print(i)}
  nhts_acs$DISTTOWK_pctl[i] =length(which(worker < nhts_acs$DISTTOWK_pctl[i]))/length(worker)
}

for (i in which(nhts_acs$GCDWORK>=0)) {
  if(i %% 1000==0) {print(i)}
  nhts_acs$GCDWORK_pctl[i] =length(which(worker1 < nhts_acs$GCDWORK_pctl[i]))/length(worker1)
}
###

#convert some string to code
nhts_acs[HBHUR == "C"]$HBHUR = 1
nhts_acs[HBHUR == "R"]$HBHUR = 2
nhts_acs[HBHUR == "S"]$HBHUR = 3
nhts_acs[HBHUR == "T"]$HBHUR = 4
nhts_acs[HBHUR == "U"]$HBHUR = 5
nhts_acs$HBHUR = as.integer(nhts_acs$HBHUR)

nhts_acs$HHCBSA = nhts_acs$HH_CBSA
nhts_acs[HH_CBSA == "XXXXX"]$HHCBSA = -1
nhts_acs$HHCBSA = as.integer(nhts_acs$HHCBSA)

nhts_acs$PUBUSE = 0
nhts_acs[PTUSED > 0]$PUBUSE = 1

#select input attributes
socialdemo_input = nhts_acs[,c("ID", "HHFIPS_BG", "HHFIPS_TRACT", "HH_INC_group", "R_AGE_group", "CDIVMSAR", "CENSUS_D", 
                               "CENSUS_R", "CNTTDTR", "DISTTOWK17", "FLEXTIME", "GCDWORK", "HBHTNRNT", "HBHUR", "HBPPOPDN",
                               "HBRESDN", "HHCBSA", "HHSTFIPS", "HTEEMPDN", "HTHTNRNT", "HTPPOPDN", "HTRESDN","MSACAT", "MSASIZE", 
                               "NBIKETRP", "NOCONG", "NWALKTRP", "PRMACT", "PTUSED", "PUBTIME", "PUBUSE","RAIL", "TIMETOWK", 
                               "URBAN", "URBANSIZE","URBRUR", "USEPUBTR", "WKFMHMXX", "WKFTPT", "WKRMHM", "WKSTFIPS", "WORKER", 
                               "WRK_HOME", "WRKTRANS", "YEARMILE", "cbg_inc_pct_less_25k","cbg_inc_pct_25k_50k",
                               "cbg_inc_pct_50k_75k","cbg_inc_pct_75k_125k", "cbg_inc_pct_125k_more", 
                               "cbg_age_pct_under_17", "cbg_age_pct_18_24", "cbg_age_pct_25_34", "cbg_age_pct_35_54",
                               "cbg_age_pct_55_64","cbg_age_pct_65_over", "ct_poverty_rate", "ct_edu_pct_less_HS", 
                               "ct_edu_pct_SCND_AD", "ct_edu_pct_BD_more")]

#add features from trip file

#night trips and early morning trips
nightid=as.integer64(unique(nhts_2017_trip[STRTTIME >= 2100 | STRTTIME <= 500 ]$ID))
morningid=as.integer64(unique(nhts_2017_trip[STRTTIME <= 700 & STRTTIME >= 500 ]$ID))
socialdemo_input$Nighttrip = 0
socialdemo_input$Morningtrip = 0
socialdemo_input[ID %in% nightid]$Nighttrip = 1
socialdemo_input[ID %in% morningid]$Morningtrip = 1

# time of the earliest trips and latest trips, average trip distance
socialdemo_input$earliesttrip = -1
socialdemo_input$latesttrip = -1
socialdemo_input$avg_tripdis_miles = -1

for (i in 1:length(socialdemo_input$ID)) {
  if(i %% 1000==0) {print(i)}
  trip=nhts_2017_trip[ID==socialdemo_input$ID[i]]
  socialdemo_input$earliesttrip[i]=min(trip$STRTTIME)
  socialdemo_input$latesttrip[i]=max(trip$ENDTIME)
  trip = trip[TRPMILES>=0]
  socialdemo_input$avg_tripdis_miles[i] = mean(trip$TRPMILES)
}

socialdemo_input[earliesttrip==Inf]$earliesttrip = -1
socialdemo_input[latesttrip==-Inf]$latesttrip = -1

fwrite(socialdemo_input,"/socialdemo_input.csv")

