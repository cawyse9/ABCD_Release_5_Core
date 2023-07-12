#####################################################################################################################################
#
#  make table of the core demographic variables (age, sex, race, parent educ, SEP, PA, puberty, site, screentime, sleep)
#  ABCD Data release 5.0 June 2023 - caw
#
#####################################################################################################################################

setwd("C:/Users/cwyse/OneDrive - Maynooth University/General/Research/ABCD/June Data Package CAW")
install.packages( 'https://raw.github.com/CDC-DNPAO/CDCAnthro/master/cdcanthro_0.1.1.tar.gz', type='source', repos=NULL )#package for BMIz score, install from github, CDC growth tables
library(cdcanthro)
library(tidyverse)
library(data.table)
library(psych)
library(skimr)


# master table starts with ID only - saved as csv file on server ABCD folder
ABCD_all_ID <- read.csv("C:/Users/cwyse/OneDrive - Maynooth University/General/Research/ABCD/June Data Package CAW/ABCD_all_ID.csv")

# rename this dataframe to start your own dataset
core <- ABCD_all_ID
core <- distinct(core) 

# recursive list of files in release 5.0 folder - all tables
allfiles <- list.files("C:/Users/cwyse/OneDrive - Maynooth University/General/Research/ABCD/June Data Package CAW/Release 5.0", full.names = T, recursive = T, pattern = ".*.csv")

# function that can retrieve variables from the tables store in ABDC release 5.0 folder on server and copy them to another table, either "core" line 24 above, or merge to existing ABCD table
# define the table and variable you need here, var can be a vector

table_to_find <- "abcd_y_lt.csv"  # the table to find
var_to_find <- "site_id_l"        # the variable to find
mydata <- core                    # the dataframe to merge the new variables

getvar <- function(table_to_find, var_to_find, mydata){

    # open the file and get the variable and merge with master by ID and event
    myfile <- list.files("C:/Users/cwyse/OneDrive - Maynooth University/General/Research/ABCD/June Data Package CAW/Release 5.0", full.name=T, recursive = TRUE, pattern=table_to_find)
    myfile <- fread (myfile)
    
    ABCDdata <<- myfile %>%
      select(src_subject_id, eventname, all_of(var_to_find)) %>% #select the variable we want
      right_join(mydata)%>%   #join by ID and event
      distinct()   #remove duplicates %>%
      
    return(ABCDdata) # returns a table called ABCD data with the ID number and merged data
}

#get site id
getvar(table_to_find = "abcd_y_lt.csv",
       var_to_find = "site_id_l",
       mydata = core)

#get sex
getvar(table_to_find = "ph_p_pds",
       var_to_find = "pubertal_sex_p",
       mydata = ABCDdata)

#get income status, race and income
getvar(table_to_find = "abcd_p_demo",
       var_to_find = c("demo_comb_income_v2_l",
                       "demo_comb_income_v2",
                       "race_ethnicity",
                       "demo_prnt_ed_v2_2yr_l",
                       "demo_prtnr_ed_v2_2yr_l",
                       "demo_prnt_marital_v2",
                       "demo_race_a_p___10",
                      "demo_race_a_p___11",
                      "demo_race_a_p___12",
                      "demo_race_a_p___13",
                      "demo_race_a_p___14",
                      "demo_race_a_p___15",
                      "demo_race_a_p___16",
                      "demo_race_a_p___17",
                      "demo_race_a_p___18",
                      "demo_race_a_p___19",
                      "demo_race_a_p___20",
                      "demo_race_a_p___21",
                      "demo_race_a_p___22",
                      "demo_race_a_p___23",
                      "demo_race_a_p___24",
                      "demo_race_a_p___25",
                      "demo_race_a_p___77",
                      "demo_race_a_p___99",
                      "demo_prnt_race_a_v2___10",
                      "demo_prnt_race_a_v2___11",
                      "demo_prnt_race_a_v2___12",
                      "demo_prnt_race_a_v2___13",
                      "demo_prnt_race_a_v2___14",
                      "demo_prnt_race_a_v2___15",
                      "demo_prnt_race_a_v2___16",
                      "demo_prnt_race_a_v2___17",
                      "demo_prnt_race_a_v2___18",
                      "demo_prnt_race_a_v2___19",
                      "demo_prnt_race_a_v2___20",
                      "demo_prnt_race_a_v2___21",
                      "demo_prnt_race_a_v2___22",
                      "demo_prnt_race_a_v2___23",
                      "demo_prnt_race_a_v2___24",
                      "demo_prnt_race_a_v2___25",
                      "demo_prnt_race_a_v2___77",
                      "demo_prnt_race_a_v2___99"),
      mydata = ABCDdata)

#get height (inch) and weight (lb)
getvar(table_to_find = "ph_y_anthro",
       var_to_find = c("anthroheightcalc","anthroweightcalc"),
       mydata = ABCDdata)
 
#get age and date of interview
getvar(table_to_find = "abcd_y_lt",
       var_to_find = c("interview_date","interview_age"),
       mydata = ABCDdata)

#get physical activity (days physically active > 1h/day in last 7days, Youth Risk Behaviour Survey)
getvar(table_to_find = "ph_y_yrb",
       var_to_find = c("physical_activity1_y"),
       mydata = ABCDdata)

#get sleep duration and disturbance (Sleep Disturbance Scale for Children)
getvar(table_to_find = "ph_p_sds",
       var_to_find = c("sleepdisturb1_p","sds_p_ss_total", "sds_p_ss_total_nm"),
       mydata = ABCDdata)

#get screentime hour and minutes on weekday and weekend days (Parent Screen Time Survey)
getvar(table_to_find = "nt_p_stq",
       var_to_find = 	c("screentime_1_wkdy_hrs_p",
	                      "screentime_1_wkdy_min_p",
	                      "screentime_1_wknd_hrs_p",
	                      "screentime_1_wknd_min_p"),
       mydata = ABCDdata)

# area deprivation index sum and percentiles addr1 [addr3 seems to be all NA but can't find what the difference is between 1 and 3]
getvar(table_to_find = "led_l_adi",
       var_to_find =   c("reshist_addr1_adi_wsum",
                        "reshist_addr1_adi_perc"),
       	mydata = ABCDdata)

#reshist_addr1_adi_wsum   Residential history derived - Area Deprivation Index: scaled weighted sum based on Kind et al., Annals of Internal Medicine, 2014 1
#reshist_addr1_adi_perc   Residential history derived - Area Deprivation Index: national percentiles, higher means higher value of ADI 1

#get pubertal development variables
getvar(table_to_find = "ph_p_pds",
       var_to_find = c("pds_p_ss_male_category",
                       "pds_p_ss_female_category"),
       mydata = ABCDdata)     
                       
getvar(table_to_find = "ph_y_pds",
       var_to_find = c("pds_y_ss_male_category",
                       "pds_y_ss_female_category"),  
       mydata = ABCDdata)

#get relatedness variables
getvar(table_to_find = "abcd_y_lt",
       var_to_find = c("rel_family_id",
                       "rel_birth_id"),  
       mydata = ABCDdata)

#get household size variables
getvar(table_to_find = "abcd_p_demo",
       var_to_find = c("demo_roster_v2"),  
       mydata = ABCDdata)

#################################################################################################
## define core variables that need labeling or processing as in release 4.0 (ABCD-study github)
#################################################################################################

#-----------------------              sex               ----------------------
# pubertal_sex_p 1 = Male Masculino; 2 = Female Femenino
ABCDdata$sex <- factor(as.numeric(ABCDdata$pubertal_sex_p),
                       levels = 1:2,
                       labels = c("male", "female"))



#-----------------------         household income     ----------------------

# Household income
    #there are two combined income variables, v2_l is the longitudinal variable but difference is unclear, we we use _l but need to check this
    #ABCDdata$demo_comb_income_v2
    #ABCDdata$demo_comb_income_v2_l

#these are the levels in the original variable (ABCDdata$demo_comb_income_v2_l)
# 1 = Less than $5,000 
# 2 = $5,000 through $11,999 
# 3 = $12,000 through $15,999 
# 4 = $16,000 through $24,999 
# 5 = $25,000 through $34,999 
# 6 = $35,000 through $49,999 
# 7 = $50,000 through $74,999 
# 8 = $75,000 through $99,999 
# 9 = $100,000 through $199,999 
# 10 = $200,000 and greater 
# 999, Don't know
# 777, Refuse to answer

#make a more condensed version
# 1 <50k
# 2 >=50K & <100K
# 3 >=100K
# 777 NA
# 999 NA

ABCDdata$household.income.3level <- ABCDdata$demo_comb_income_v2_l
ABCDdata$household.income.10level <- ABCDdata$demo_comb_income_v2_l

ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "1"] <- 1 # "[<50K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "2"] <- 1 # "[<50K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "3"] <- 1 # "[<50K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "4"] <- 1 # "[<50K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "5"] <- 1 # "[<50K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "6"] <- 1 # "[<50K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "7"] <- 2 # "[>=50K & <100K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "8"] <- 2 # "[>=50K & <100K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "9"] <- 3 # "[>=100K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "10"] <- 3 # "[>=100K]"
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "777"] <- NA
ABCDdata$household.income.3level[ABCDdata$demo_comb_income_v2_l == "999"] <- NA
ABCDdata$household.income.3level[ABCDdata$household.income.3level %in% c(NA, "999", "777")] <- NA

ABCDdata$household.income.3level <- factor(ABCDdata$household.income.3level,
                             levels = 1:3,
                             labels = c("<50k", ">=50k & <100k", ">100k")
                             )
#change codes to NA
ABCDdata$household.income.10level[ABCDdata$demo_comb_income_v2_l == "777"] <- NA
ABCDdata$household.income.10level[ABCDdata$demo_comb_income_v2_l == "999"] <- NA
ABCDdata$household.income.10level[ABCDdata$household.income.3level %in% c(NA, "999", "777")] <- NA

#define levels
ABCDdata$household.income.10level <- factor(ABCDdata$household.income.10level,
                                  levels = 1:10,
                                  labels =  c("< $5k",
                                              "$5-12k",
                                              "$12-16k",
                                              "$16-25k",
                                              "$25-35k",
                                              "$35-50k",
                                              "$50-75k",
                                              "$75-100k",
                                              "$100-200k",
                                              ">$200k")
                                  )
                                  # NA  999, Don't know
                                  # NA  777, Refuse to answer



#-----------------------         parent education     ----------------------

#release 5 variables
ABCDdata$demo_prnt_ed_v2_2yr_l
ABCDdata$demo_prtnr_ed_v2_2yr_l

#code taken from ABCD github release 4 demographic variables

# #ABCD categories
# 0 = Never attended/Kindergarten only 
# 1 = 1st grade 1
# 2 = 2nd grade 2
# 3 = 3rd grade 3
# 4 = 4th grade 4
# 5 = 5th grade 5
# 6 = 6th grade 6
# 7 = 7th grade 7
# 8 = 8th grade 8
# 9 = 9th grade 9
# 10 = 10th grade 10 
# 11 = 11th grade 11 
# 12 = 12th grade, no diploma 
# 13 = High school graduate  
# 14 = GED or equivalent 
# 16 = Associate degree: Occupational, Technical, or Vocational  
# 17 = Associate degree: Academic Program 
# 18 = Bachelor's degree (ex. BA, AB, BS, BBS) 
# 19 = Master's degree (ex. MA, MS, MEng, MEd, MBA) 
# 20 = Professional School degree (ex. MD, DDS, DVN, JD) 
# 21 = Doctoral degree (ex. PhD, EdD) 
# 22 = Less than 1 year of college credit/post-secondary education (or less than 10 classes)  
# 23 = One year or more of college credit, no degree 
# 777 = Refused to answer 
# 999 = Don't Know 

#recode the raw variables
ABCDdata$educ_parent <- ABCDdata$demo_prnt_ed_v2_2yr_l
ABCDdata$educ_partner <- ABCDdata$demo_prtnr_ed_v2_2yr_l

#get the max of both parent and partner education to make one highest_educ variable
ABCDdata$household.educ <- pmax(as.numeric(as.character(ABCDdata$educ_parent)), as.numeric(as.character(ABCDdata$educ_partner)),na.rm = T)

#now take the max of both parents and categorise the education into 5 levels correspond to the numbers published by the American Community Survey (ACS).  Code taken from ABCD github
  idx <- which(ABCDdata$household.educ %in% 0:12, arr.ind = TRUE)
ABCDdata$household.educ[idx] <- 1 # "< HS Diploma"

  idx <- which(ABCDdata$household.educ %in% 13:14, arr.ind = TRUE)
ABCDdata$household.educ[idx] <- 2 # "HS Diploma/GED"

  idx <- which(ABCDdata$household.educ %in% 15:17, arr.ind = TRUE)
ABCDdata$household.educ[idx] <- 3 # "Some College"  

idx <- which(ABCDdata$household.educ %in% 22:23, arr.ind = TRUE)
ABCDdata$household.educ[idx] <- 3 # "Some College"  #note this is modified as 22 and 23 were not listed in code for release 5

  idx <- which(ABCDdata$household.educ == 18, arr.ind = TRUE)
ABCDdata$household.educ[idx] <- 4 # "Bachelor"

  idx <- which(ABCDdata$household.educ %in% 19:21, arr.ind = TRUE)
ABCDdata$household.educ[idx] <- 5 # "Post Graduate Degree"

ABCDdata$household.educ[which(ABCDdata$household.educ == "999")] <- NA
ABCDdata$household.educ[which(ABCDdata$household.educ == "777")] <- NA

ABCDdata$household.educ <- factor(
  ABCDdata$household.educ,
  levels = 1:5,
  labels = c(
    "< HS Diploma",
    "HS Diploma/GED",
    "Some College",
    "Bachelor",
    "Post Graduate Degree"
  )
)


#-----------------------  marital status of household     ----------------------

#release 5 variable
ABCDdata$demo_prnt_marital_v2

# 1 = Married 
# 2 = Widowed 
# 3 = Divorced  
# 4 = Separated  
# 5 = Never married  
# 6 = Living with partner  
# 777 = Refused to answer 

ABCDdata$married <- rep(NA, length(ABCDdata$demo_prnt_marital_v2))
ABCDdata$married[ABCDdata$demo_prnt_marital_v2 == 1] <- 1
ABCDdata$married[ABCDdata$demo_prnt_marital_v2 %in% 2:6] <- 0
ABCDdata$married <- factor(ABCDdata$married, levels = 0:1, labels = c("No", "Yes"))

# Add another variable that also includes couples that just live together.
ABCDdata$married.or.livingtogether <- rep(NA, length(ABCDdata$demo_prnt_marital_v2))
ABCDdata$married.or.livingtogether[ABCDdata$demo_prnt_marital_v2 %in% c(1, 6)] <- 1
ABCDdata$married.or.livingtogether[ABCDdata$demo_prnt_marital_v2 %in% 2:5] <- 0
ABCDdata$married.or.livingtogether <- factor(ABCDdata$married.or.livingtogether,
                                          levels = 0:1,
                                          labels = c("No", "Yes")
)

#-----------------------              BMI             ----------------------


# Height and weight variables from release 5 
ABCDdata$anthroheightcalc
ABCDdata$anthroweightcalc

#Standing Height Average (inches)
#Average Measured Weight (lbs)

#convert height in inches to cm
ABCDdata$height.cm <- (ABCDdata$anthroheightcalc)*2.54

#convert weight in lbs to kg 
ABCDdata$weight.kg <- (ABCDdata$anthroweightcalc)*0.453592

#rename age
ABCDdata$age <- ABCDdata$interview_age

#calculate BMI using code given for release 4.0 from ABCD-study github page
ABCDdata$anthro_bmi_calc <- as.numeric(as.character(ABCDdata$anthroweightcalc)) / as.numeric(as.character(ABCDdata$anthroheightcalc))^2 * 703

#ABCDdata$anthro_bmi_calc[which(ABCDdata$anthro_bmi_calc > 36 | ABCDdata$anthro_bmi_calc < 11)] <- NA

#this package (cdcanthro) used to calculate z-score (bmiz) for age z-scores based on 2000 CDC growth charts (non-obese children) and extended BMIz (obese children)

bmi_results <- cdcanthro(ABCDdata,
                age = age,
                wt = weight.kg,
                ht = height.cm,
                bmi = anthro_bmi_calc,
                all = FALSE)

#pastez-score back into ABCDdata frame
ABCDdata$BMIz <- bmi_results$bmiz

# reset unrealistic values;
# https://www.cdc.gov/nccdphp/dnpao/growthcharts/who/examples/example4_pop_cdc_bmi.htm



#-----------------------             race                  ----------------------

#_______________________________________________________________________
#
#What race do you consider the child to be? Please check all that apply. 
#______________________________________________________________________

# 10 White 
# 11 Black/African American 
# 12 American Indian Native American 
# 13 Alaska Native 
# 14 Native Hawaiian 
# 15 Guamanian 
# 16 Samoan 
# 17 Other Pacific Islander 
# 18 Asian Indian 
# 19 Chinese 
# 20 Filipino 
# 21 Japanese 
# 22 Korean 
# 23 Vietnamese 
# 24 Other Asian 
# 25 Other Race 
# 77 Refuse To Answer 
# 99 Don't Know 

#make vector of race variables about child (Parent completed)
dat_nms = c("src_subject_id",
            "demo_ethn_p", 
            "demo_race_a_p___10", #white
            "demo_race_a_p___11", #black
            "demo_race_a_p___12", #American Indian Native American 
            "demo_race_a_p___13", #Alaska Native 
            "demo_race_a_p___14", #Native Hawaiian 
            "demo_race_a_p___15", #Guamanian 
            "demo_race_a_p___16", #Samoan 
            "demo_race_a_p___17", #Other Pacific Islander
            "demo_race_a_p___18", #Asian Indian 
            "demo_race_a_p___19", #Chinese
            "demo_race_a_p___20", #Filipino 
            "demo_race_a_p___21", #Japanese 
            "demo_race_a_p___22", #Korean 
            "demo_race_a_p___23", #Vietnamese 
            "demo_race_a_p___24", #Other Asian 
            "demo_race_a_p___25", #Other Race 
            "demo_race_a_p___77", #Refuse To Answer 
            "demo_race_a_p___99") #Don't Know 

#get subject ID col
ind_dat = which(names(ABCDdata)==dat_nms[1])

#get col number of others
for(j in 2:length(dat_nms)){
  ind_dat = c(ind_dat,which(names(ABCDdata)==dat_nms[j]))
}

#match the names
#names(ABCDdata)[ind_dat]
#dat = data.table(ABCDdata[ABCDdata$eventname=="baseline_year_1_arm_1",ind_dat])

#filter baseline and only race variables to new dataframe, dat
dat <-ABCDdata %>%
        filter(eventname=="baseline_year_1_arm_1") %>%
        select(any_of(dat_nms)) 
       
# White
dat$white <- with(dat, ifelse(demo_race_a_p___10 == "1", 1, 0))   #white

# Black
dat$black <- with(dat, ifelse(demo_race_a_p___11 == "1", 1, 0))   #black

# Asian
dat$asian <- 0
dat$asian <- with(dat, ifelse(demo_race_a_p___18 == "1" | #Asian Indian 
                              demo_race_a_p___19 == "1" | #Chinese
                              demo_race_a_p___20 == "1" | #Filipino 
                              demo_race_a_p___21 == "1" | #Japanese 
                              demo_race_a_p___22 == "1" | #Korean 
                              demo_race_a_p___23 == "1" | #Vietnamese 
                              demo_race_a_p___24 == "1" , #Other Asian 
                              1, 0))

# AIAN: American Indian and Alaska Native
dat$aian <- 0
dat$aian <- with(dat, ifelse(demo_race_a_p___12 == "1" | #American Indian Native American 
                              demo_race_a_p___13 == "1" , #Alaska Native
                              1, 0))  

#NHPI: Native Hawaiian and Other Pacific
dat$NHPI <- 0
dat$NHPI <- with(dat, ifelse(demo_race_a_p___14 == "1" | #Native Hawaiian 
                             demo_race_a_p___15 == "1" | #Guamanian 
                             demo_race_a_p___16 == "1" | #Samoan 
                             demo_race_a_p___17 == "1" | #Other Pacific Islander
                              1, 0))  
# Other
dat$other <- 0
dat$other <- with(dat, ifelse (demo_race_a_p___25 == "1", 1, 0))  

# Mixed
dat[, mixed:= (white + black + asian + aian + NHPI + other)]
dat[, table(mixed, useNA = "if")]
dat[ mixed <= 1, mixed:= 0]
dat[ mixed > 1, mixed:= 1]

dat[, table(asian, useNA = "if")]

# Make a Race variable with 4 levels - note that someone could be in both race and mixed category - but that is what was coded in release 4
dat$race.4level <- NA
dat$race.4level[dat$white == "1"] <- "1"
dat$race.4level[dat$black == "1"] <- "2"
dat$race.4level[dat$asian == "1"] <- "3"
dat$race.4level[dat$aian == "1"] <- "4"
dat$race.4level[dat$NHPI == "1"] <- "4"
dat$race.4level[dat$other == "1"] <- "4"
dat$race.4level[dat$mixed == "1"] <- "4"

dat$race.4level<- factor(dat$race.4level,
                         levels = 1:4,
                         labels = c("White","Black","Asian","Other/Mixed"))

table(dat$race.4level)

dat$race.eth[dat$race.eth==1] = "White"
dat$race.eth[dat$race.eth==2] = "Black"
dat$race.eth[dat$race.eth==3] ="Asian"
dat$race.eth[dat$race.eth==4] = "Other/Mixed"
dat[, table(race.4level, useNA = "if") ]

# Race 6 level
dat[white==1,race.6level:=1]
dat[black==1,race.6level:=2]
dat[asian==1,race.6level:=3]
dat[aian==1,race.6level:=4]
dat[NHPI==1,race.6level:=4]
dat[other==1,race.6level:=5]
dat[mixed==1,race.6level:=6]
dat[, table(race.6level,useNA="if") ]

dat$race.6level<- factor(dat$race.6level,
                         levels=1:6,
                         labels= c("White","Black","Asian","AIAN/NHPI","Other","Mixed"))
dat$race.eth[dat$race.6level==1] ="White"
dat$race.eth[dat$race.6level==2] ="Black"
dat$race.eth[dat$race.6level==3] ="Asian"
dat$race.eth[dat$race.6level==4] ="AIAN/NHPI"
dat$race.eth[dat$race.6level==5] ="Other"
dat$race.eth[dat$race.6level==6] ="Mixed"
dat[, table(race.6level,useNA="if") ]

#variables to transfer to ABCDdata from dat dataframe
# dat$race.6level
# dat$race.4level

dat <- dat %>% 
  rename("race.6level.y" = "race.6level")

dat <- dat %>% 
  rename("race.4level.y" = "race.4level")

ABCDdata <- dat %>%
  select(src_subject_id, race.6level.y, race.4level.y) %>%
  left_join(ABCDdata, by = "src_subject_id")

#_______________________________________________________________________
#
# What race do you consider yourself to be? Please check all that apply. 
#______________________________________________________________________

# demo_prnt_race_a_v2___10
# demo_prnt_race_a_v2___11
# demo_prnt_race_a_v2___12
# demo_prnt_race_a_v2___13
# demo_prnt_race_a_v2___14
# demo_prnt_race_a_v2___15
# demo_prnt_race_a_v2___16
# demo_prnt_race_a_v2___17
# demo_prnt_race_a_v2___18
# demo_prnt_race_a_v2___19
# demo_prnt_race_a_v2___20
# demo_prnt_race_a_v2___21
# demo_prnt_race_a_v2___22
# demo_prnt_race_a_v2___23
# demo_prnt_race_a_v2___24
# demo_prnt_race_a_v2___25
# demo_prnt_race_a_v2___77
# demo_prnt_race_a_v2___99

# 10 White 
# 11 Black/African American 
# 12 American Indian Native American
# 13 Alaska Native 
# 14 Native Hawaiian 
# 15 Guamanian 
# 16 Samoan 
# 17 Other Pacific Islander
# 18 Asian Indian 
# 19 Chinese 
# 20 Filipino 
# 21 Japanese 
# 22 Korean 
# 23 Vietnamese 
# 24 Other Asian 
# 25 Other Race 
# 77 Refuse To Answer 
# 99 Don't Know 


#make vector of race variables about child (Parent completed)
dat_nms_prnt = c("src_subject_id",
            "demo_ethn_p", 
            "demo_prnt_race_a_v2___10", #white
            "demo_prnt_race_a_v2___11", #black
            "demo_prnt_race_a_v2___12", #American Indian Native American 
            "demo_prnt_race_a_v2___13", #Alaska Native 
            "demo_prnt_race_a_v2___14", #Native Hawaiian 
            "demo_prnt_race_a_v2___15", #Guamanian 
            "demo_prnt_race_a_v2___16", #Samoan 
            "demo_prnt_race_a_v2___17", #Other Pacific Islander
            "demo_prnt_race_a_v2___18", #Asian Indian 
            "demo_prnt_race_a_v2___19", #Chinese
            "demo_prnt_race_a_v2___20", #Filipino 
            "demo_prnt_race_a_v2___21", #Japanese 
            "demo_prnt_race_a_v2___22", #Korean 
            "demo_prnt_race_a_v2___23", #Vietnamese 
            "demo_prnt_race_a_v2___24", #Other Asian 
            "demo_prnt_race_a_v2___25", #Other Race 
            "demo_prnt_race_a_v2___77", #Refuse To Answer 
            "demo_prnt_race_a_v2___99") #Don't Know 

#get subject ID col
ind_dat = which(names(ABCDdata)==dat_nms_prnt[1])

#get col number of others
for(j in 2:length(dat_nms_prnt)){
  ind_dat = c(ind_dat,which(names(ABCDdata)==dat_nms_prnt[j]))
}

#match the names
#names(ABCDdata)[ind_dat]
#dat = data.table(ABCDdata[ABCDdata$eventname=="baseline_year_1_arm_1",ind_dat])

#filter baseline and only race variables to new dataframe, dat
dat_prnt <-ABCDdata %>%
  filter(eventname=="baseline_year_1_arm_1") %>%
  select(any_of(dat_nms_prnt)) 

# White
dat_prnt$white <- with(dat_prnt, ifelse(demo_prnt_race_a_v2___10 == "1", 1, 0))   #white

# Black
dat_prnt$black <- with(dat_prnt, ifelse(demo_prnt_race_a_v2___11 == "1", 1, 0))   #black

# Asian
dat_prnt$asian <- 0
dat_prnt$asian <- with(dat_prnt, ifelse(demo_prnt_race_a_v2___18 == "1" | #Asian Indian 
                                demo_prnt_race_a_v2___19 == "1" | #Chinese
                                demo_prnt_race_a_v2___20 == "1" | #Filipino 
                                demo_prnt_race_a_v2___21 == "1" | #Japanese 
                                demo_prnt_race_a_v2___22 == "1" | #Korean 
                                demo_prnt_race_a_v2___23 == "1" | #Vietnamese 
                                demo_prnt_race_a_v2___24 == "1" , #Other Asian 
                              1, 0))

# AIAN: American Indian and Alaska Native
dat_prnt$aian <- 0
dat_prnt$aian <- with(dat_prnt, ifelse(demo_prnt_race_a_v2___12 == "1" | #American Indian Native American 
                               demo_prnt_race_a_v2___13 == "1" , #Alaska Native
                             1, 0))  

#NHPI: Native Hawaiian and Other Pacific
dat_prnt$NHPI <- 0
dat_prnt$NHPI <- with(dat_prnt, ifelse(demo_prnt_race_a_v2___14 == "1" | #Native Hawaiian 
                               demo_prnt_race_a_v2___15 == "1" | #Guamanian 
                               demo_prnt_race_a_v2___16 == "1" | #Samoan 
                               demo_prnt_race_a_v2___17 == "1" | #Other Pacific Islander
                               1, 0))  
# Other
dat_prnt$other <- 0
dat_prnt$other <- with(dat_prnt, ifelse (demo_prnt_race_a_v2___25 == "1", 1, 0))  

# Mixed
dat_prnt[, mixed:= (white + black + asian + aian + NHPI + other)]
dat_prnt[, table(mixed, useNA = "if")]
dat_prnt[ mixed <= 1, mixed:= 0]
dat_prnt[ mixed > 1, mixed:= 1]

dat_prnt[, table(mixed, useNA = "if")]

# Make a Race variable with 4 levels - note that someone could be in both race and mixed category - but that is what was coded in release 4
dat_prnt$race.4level.prnt <- NA
dat_prnt$race.4level.prnt[dat_prnt$white == "1"] <- "1"
dat_prnt$race.4level.prnt[dat_prnt$black == "1"] <- "2"
dat_prnt$race.4level.prnt[dat_prnt$asian == "1"] <- "3"
dat_prnt$race.4level.prnt[dat_prnt$aian == "1"] <- "4"
dat_prnt$race.4level.prnt[dat_prnt$NHPI == "1"] <- "4"
dat_prnt$race.4level.prnt[dat_prnt$other == "1"] <- "4"
dat_prnt$race.4level.prnt[dat_prnt$mixed == "1"] <- "4"

dat_prnt$race.4level.prnt<- factor(dat_prnt$race.4level,
                         levels = 1:4,
                         labels = c("White","Black","Asian","Other/Mixed"))

table(dat_prnt$race.4level.prnt)

dat_prnt$race.eth.prnt[dat_prnt$race.eth.prnt==1] = "White"
dat_prnt$race.eth.prnt[dat_prnt$race.eth.prnt==2] = "Black"
dat_prnt$race.eth.prnt[dat_prnt$race.eth.prnt==3] ="Asian"
dat_prnt$race.eth.prnt[dat_prnt$race.eth.prnt==4] = "Other/Mixed"
dat_prnt[, table(race.4level.prnt, useNA = "if") ]

# Race 6 level
dat_prnt[white==1,race.6level.prnt:=1]
dat_prnt[black==1,race.6level.prnt:=2]
dat_prnt[asian==1,race.6level.prnt:=3]
dat_prnt[aian==1,race.6level.prnt:=4]
dat_prnt[NHPI==1,race.6level.prnt:=4]
dat_prnt[other==1,race.6level.prnt:=5]
dat_prnt[mixed==1,race.6level.prnt:=6]
dat_prnt[, table(race.6level.prnt,useNA="if") ]

dat_prnt$race.6level.prnt<- factor(dat_prnt$race.6level.prnt,
                         levels=1:6,
                         labels= c("White","Black","Asian","AIAN/NHPI","Other","Mixed"))
table(dat_prnt$race.6level.prnt)
# 
# dat_prnt$race.6level.prnt[dat_prnt$race.6level.prnt==1] ="White"
# dat_prnt$race.6level.prnt[dat_prnt$race.6level.prnt==2] ="Black"
# dat_prnt$race.6level.prnt[dat_prnt$race.6level.prnt==3] ="Asian"
# dat_prnt$race.6level.prnt[dat_prnt$race.6level.prnt==4] ="AIAN/NHPI"
# dat_prnt$race.6level.prnt[dat_prnt$race.6level.prnt==5] ="Other"
# dat_prnt$race.6level.prnt[dat_prnt$race.6level.prnt==6] ="Mixed"
# dat_prnt[, table(race.6level.prnt,useNA="if") ]

#variables to transfer to ABCDdata from dat_prnt dataframe
# dat_prnt$race.6level.prnt
# dat_prnt$race.4level.prnt

#rename 
# dat_prnt$race.6level.p
# dat_prnt$race.4level.p

dat_prnt <- dat_prnt %>% 
  rename("race.6level.p" = "race.6level.prnt")

dat_prnt <- dat_prnt %>% 
  rename("race.4level.p" = "race.4level.prnt")

## select cols of interest and join to ABDCdata
ABCDdata <- dat_prnt %>%
  select(src_subject_id, race.6level.p, race.4level.p) %>%
  left_join(ABCDdata, by = "src_subject_id")

#-----------------------             sleep                  ----------------------

# Sleep Disturbance Scale for Children
# Bruni, O., Ottaviano, S., et al. (1996) The Sleep Disturbance Scale for Children (SDSC). Construction and validation of an instrument to evaluate sleep disturbances in childhood and adolescence. J Sleep Res 5(4): 251-261.		

# Sleep duration
# How many hours of sleep does your child get on most nights? Consider each question pertaining to the PAST 6 MONTHS of the child's life		
# 1 = 9-11 hours
# 2 = 8-9 hours 
# 3 = 7-8 hours 
# 4 = 5-7 hours
# 5 = Less than 5 hours

ABCDdata$sleepdisturb1_p <- factor(ABCDdata$sleepdisturb1_p,
                                   levels=1:5,
                                   labels= c( "9-11h",
                                              "8-9h", 
                                              "7-8h",
                                              "5-7h",
                                              "<5h")
                                     )
# sleep disturbance (Sum of 6 Factors): sds_p_ss_dims + sds_p_ss_sbd + sds_p_ss_da + sds_p_ss_swtd + sds_p_ss_does + sds_p_ss_shy

# Validation: All items must be answered so set total missing > 0 to NA
ABCDdata$sds_p_ss_total[(ABCDdata$sds_p_ss_total_nm !=0)]<-NA

#change from character to integer
ABCDdata$sds_p_ss_total <- as.integer(ABCDdata$sds_p_ss_total)

		
#-----------------------          screen time                  ----------------------

#total screen time on typical day - add minutes and hours for weekend and weekdays = TIME on a computer, cellphone, tablet, or other electronic device
#Parent Screen Time Survey

ABCDdata$screentime_total_p <-  ABCDdata$screentime_1_wkdy_hrs_p +
                                (ABCDdata$screentime_1_wkdy_min_p/60) +
                                ABCDdata$screentime_1_wknd_hrs_p +
                                (ABCDdata$screentime_1_wknd_min_p/60) 
#-----------------------    pubertal development    ----------------------
#Pubertal Development Scale 

#ABCD variables (see dictionary for scoring)
# pds_p_ss_male_category
#	pds_p_ss_female_category
# pds_y_ss_male_category
# pds_y_ss_female_category

# Labels
#1 = pre puberty
#2 = early puberty
#3 = mid puberty
#4 = late puberty
#5 = post puberty

#parent and youth answered questions and separate score in ABCD.  We derived total score = sum parent and youth, not average (Petrican et al., 2021) as fits inital classification better without half points

pds_total_py_male <- as.integer(ABCDdata$pds_y_ss_male_category + ABCDdata$pds_p_ss_male_category)
pds_total_py_female <- as.integer(ABCDdata$pds_y_ss_female_category + ABCDdata$pds_p_ss_female_category)

ABCDdata$pds_total_py_male <- factor(pds_total_py_male,
                            levels = c(1:5),
                            labels = c("pre puberty",
                                        "early puberty",
                                        "mid puberty",
                                        "late puberty",
                                        "post puberty")
                            )

ABCDdata$pds_total_py_female <- factor(pds_total_py_female,
                            levels = c(1:5),
                            labels = c("pre puberty",
                                       "early puberty",
                                       "mid puberty",
                                       "late puberty",
                                       "post puberty")
                            )
(pds_total_py_female)


table(ABCDdata$pds_total_py_female)
table(ABCDdata$pds_total_py_male)

#-------------------------------------------------------------------------------------------------

names (ABCDdata)

#select variable for core dataset and save to csv

ABCD5_core <- as.data.frame(ABCDdata[,c('src_subject_id',
  'interview_date',
  'eventname',
  'site_id_l',
  'age',
  'sex',
  'reshist_addr1_adi_wsum',
  'reshist_addr1_adi_perc',
  'household.income.3level',
  'household.income.10level',
  'race.6level.p',
  'race.4level.p',
  'race.6level.y',
  'race.4level.y',
  'race_ethnicity',
  'household.educ',
  'married',
  'married.or.livingtogether',
  'rel_family_id',
  'rel_birth_id',
  'demo_roster_v2',
  'sleepdisturb1_p',
  'sds_p_ss_total',
  'anthro_bmi_calc',
  'BMIz',
  'physical_activity1_y',
  'screentime_total_p',
  'pds_total_py_male',
  'pds_total_py_female')])

                                     
   
write.csv(ABCD5_core, file="ABCD5_core_variables_060723.csv")



