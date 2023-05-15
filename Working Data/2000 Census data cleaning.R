# This file process 2000 0.95% census data and transform the dataset for further research
# Output: 2000 migration data and individual controls 2.0
# Upadate: with more specific control and more detailed labeling

# Prep work ----
# rm(list=ls())
setwd("/Users/jiaxiangchen/Desktop/ECON32212 Applied Dissertation B/Empirics")
# Load packages
library(haven)
library(tidyverse)
library(readr)
library(expss)
library(writexl)
library(foreign)
# Load 2000 census data
cens00 <- read_dta("Raw Data/Census Data/2000年全国0.95%%抽样调查/2000Census-rd.dta")
summary(cens00)


# Raw data ----
str(cens00)
dim(cens00)
na_count <-sapply(cens00, function(y) sum(length(which(is.na(y)))))


# Clean data ----
# Select key variables
data_2000 <- cens00 %>% 
        select(province, distric, # Current residence
               h1,h16, h18,h19,h20,h21, # Housing, charactristics
               r3, r5, # Gender, ethnicity
               r62, r63, r7, # Hukou
               r9, r101, r102, r103, r11,r12, # Migrate within 5 years?
               r14, r151, # Literacy, education
               r17, r18, r19, r20, r211, r212, r22, # Employment
               r23, r241, r253, r254, # Marriage, children
               age)

## Recoding the variable -----
na_count2 <-sapply(data_2000, function(y) sum(length(which(is.na(y)))))
na_count2

### 1. Housing charactristcs ----
# province
var_lab(data_2000$province) <- "Current residence province?"
# district
var_lab(data_2000$distric) <- "Current residence province district?"
# h1: household
data_2000 <- rename(data_2000, "household" = "h1")
var_lab(data_2000$household) <- ""
# h16: kitchen
data_2000 <- rename(data_2000, "kitchen" = "h16")
var_lab(data_2000$kitchen) <- "Is there a kithchen in the residence?"
val_lab(data_2000$kitchen) <- num_lab("1 Independent
                                       2 Share
                                       3 None")
table(data_2000$kitchen)
# h18: water_tap
data_2000 <- rename(data_2000, "water_tap" = "h18")
var_lab(data_2000$water_tap) <- "Is there a water tap in the residence?"
val_lab(data_2000$water_tap) <- num_lab("1 Yes
                                       2 No")
table(data_2000$water_tap)
# h19: shower
data_2000 <- rename(data_2000, "shower" = "h19")
var_lab(data_2000$shower) <- "Is there a shower in the residence?"
data_2000$shower <- ifelse(data_2000$shower > 0 & data_2000$shower < 4, 1, data_2000$shower)
data_2000$shower <- ifelse(data_2000$shower == 4, 2, data_2000$shower)
val_lab(data_2000$shower) <- num_lab("1 Yes
                                       2 No")
table(data_2000$shower)
# h20: toilet
data_2000 <- rename(data_2000, "toilet" = "h20")
var_lab(data_2000$toilet) <- "Is there a toilet in the residence?"
data_2000$toilet <- ifelse(data_2000$toilet == 1|data_2000$toilet == 3, 1, data_2000$toilet)
data_2000$toilet <- ifelse(data_2000$toilet == 2|data_2000$toilet == 4, 2, data_2000$toilet)
data_2000$toilet <- ifelse(data_2000$toilet == 5, 3, data_2000$toilet)
val_lab(data_2000$toilet) <- num_lab("1 Independent
                                       2 Share
                                       3 None")
table(data_2000$toilet)
# h21: res_sc
data_2000 <- rename(data_2000, "res_sc" = "h21")
var_lab(data_2000$res_sc) <- "Source of residence"
data_2000$res_sc <- ifelse(data_2000$res_sc ==2|data_2000$res_sc ==3|data_2000$res_sc ==4, 2, data_2000$res_sc)
data_2000$res_sc <- ifelse(data_2000$res_sc ==5|data_2000$res_sc ==6, 3, data_2000$res_sc)
data_2000$res_sc <- ifelse(data_2000$res_sc ==7, 4, data_2000$res_sc)
val_lab(data_2000$res_sc) <- num_lab("1 Self-built 
                                     2 Self-owned
                                     3 Rent
                                     4 Other")
table(data_2000$res_sc)


### 2. Gender, ethnicity ----
# r3: Gender
data_2000 <- rename(data_2000, "gender" = "r3")
var_lab(data_2000$gender) <- c("")
val_lab(data_2000$gender) <- num_lab("1 Male
                                     2 Female")
table(data_2000$gender)

# r5: ethnicity
data_2000 <- rename(data_2000, "ethnicity" = "r5")
data_2000$ethnicity <- ifelse(data_2000$ethnicity  < 2, 1, 2)
val_lab(data_2000$ethnicity) <- num_lab("1 Han
                                     2 Non-Han")
table(data_2000$ethnicity)

### 3. Hukou ----
# r62 hukou_stat
data_2000 <- rename(data_2000, "hukou_stat" = "r62")
var_lab(data_2000$hukou_stat) <- "Hukou status"

# r63 hukou_prov
data_2000 <- rename(data_2000, "hukou_prov" = "r63")
var_lab(data_2000$hukou_prov) <- "Hukou province"

# r62 hukou_typ
data_2000 <- rename(data_2000, "hukou_typ" = "r7")
var_lab(data_2000$hukou_typ) <- "Hukou type"
val_lab(data_2000$hukou_typ) <- num_lab("1 Ag
                                     2 Non-Ag")


### 4. Migration----
# r9 mig
data_2000 <- rename(data_2000, "mig" = "r9")
var_lab(data_2000$mig) <- "When move to the current residence?"
data_2000$mig <- ifelse(data_2000$mig == 1, "Never", data_2000$mig)
data_2000$mig <- ifelse(data_2000$mig == 2 |data_2000$mig == 3,  "Before 1996", data_2000$mig)
data_2000$mig <- ifelse(data_2000$mig == 4, "1996", data_2000$mig)
data_2000$mig <- ifelse(data_2000$mig == 5, "1997", data_2000$mig)
data_2000$mig <- ifelse(data_2000$mig == 6, "1998", data_2000$mig)
data_2000$mig <- ifelse(data_2000$mig == 7, "1999", data_2000$mig)
data_2000$mig <- ifelse(data_2000$mig == 8, "2000", data_2000$mig)
table(data_2000$mig)

# r101: mig_local
data_2000 <- rename(data_2000, "mig_local" = "r101")
var_lab(data_2000$mig_local) <- "Migrate from local?"
val_lab(data_2000$mig_local) <- num_lab("1 Local
                                     2 Non-Local")

# r102: org_prov
data_2000 <- rename(data_2000, "org_prov" = "r102")
var_lab(data_2000$org_prov) <- "Origin province"

# r103: org_distric
data_2000 <- rename(data_2000, "org_distric" = "r103")
var_lab(data_2000$org_distric) <- "Orgin district"

# r11: org_typ
data_2000 <- rename(data_2000, "org_typ" = "r11")
var_lab(data_2000$org_typ) <- "Origin type"
data_2000$org_typ <- ifelse(data_2000$org_typ == 1 |data_2000$org_typ == 3,  1, data_2000$org_typ)
data_2000$org_typ <- ifelse(data_2000$org_typ == 2 |data_2000$org_typ == 4,  2, data_2000$org_typ)
val_lab(data_2000$org_typ) <- num_lab("1 Rural
                                     2 Urban")

# r12: mig_reason
data_2000 <- rename(data_2000, "mig_reason" = "r12")
var_lab(data_2000$mig_reason) <- "Migration reason"
val_lab(data_2000$mig_reason) <- num_lab("1 work and business
                                         2 job transfer
                                         3 job assignment
                                         4 education and training
                                         5 relocation
                                         6 marriage
                                         7 migrate with family
                                         8 go to relative and friend
                                         9 other
                                         ")
table(data_2000$mig_reason)

### 5. Literacy, Education ----
# r14: literacy
data_2000 <- rename(data_2000, "literacy" = "r14")
var_lab(data_2000$literacy) <- "literacy"
val_lab(data_2000$literacy) <- num_lab("1 Literate
                                         2 Non-literate")

# r151: educ
data_2000 <- rename(data_2000, "educ" = "r151")
var_lab(data_2000$educ) <- "education Level"
val_lab(data_2000$educ) <- num_lab("1 No schooling
        2 Literacy class
        3 Primiray school
        4 Middle school
        5 High school
        6 Technical secondary school
        7 Tehnical college
        8 University
        9 Graduate school")
        
table(data_2000$educ)

### 6. Employment ----
# r17: employ
data_2000 <- rename(data_2000, "employ" = "r17")
var_lab(data_2000$employ) <- c("employment status last week")
val_lab(data_2000$employ) <- num_lab("1 Working
                                     2 Holiday or sesonal break
                                     3 Unemployed")
table(data_2000$employ)

# r18: wrk_t
data_2000 <- rename(data_2000, "wrk_t" = "r18")
var_lab(data_2000$wrk_t) <- c("How long did u work in the last week?")
val_lab(data_2000$wrk_t) <- num_lab("1 One day
                                    2 Two days
                                   3 Three days
                                   4 Four days
                                   5 Five days
                                   6 Six days
                                   7 Seven days")
table(data_2000$wrk_t)

# r19: industry
data_2000 <- rename(data_2000, "industry" = "r19")
var_lab(data_2000$industry) <- c("")
table(data_2000$industry)

# r20: job
data_2000 <- rename(data_2000, "job" = "r20")
var_lab(data_2000$job) <- c("")
table(data_2000$job)

# r211: unemp_stat
data_2000 <- rename(data_2000, "unemp_stat" = "r211")
var_lab(data_2000$unemp_stat) <- c("Unemployment status")
val_lab(data_2000$unemp_stat) <- num_lab("1 Student
                                    2 Housework
                                   3 Retired
                                   4 Lost working ability
                                   5 Searching for first job
                                   6 Searching for next job
                                   7 Other")
table(data_2000$unemp_stat)

# r212: last_job
data_2000 <- rename(data_2000, "last_job" = "r212")
var_lab(data_2000$last_job) <- c("Main job before unemployed")
table(data_2000$last_job)

# r22: liv_dep
data_2000 <- rename(data_2000, "liv_dep" = "r22")
var_lab(data_2000$liv_dep) <- c("Main living dependency")
val_lab(data_2000$liv_dep) <- num_lab("1 Pension
                                      2 Basic living allowence
                                      3 Family support
                                      4 Finical income
                                      5 Insurance
                                      6 Other")


### 7. Marriage, Children ----
# r23: marriage
data_2000 <- rename(data_2000, "marriage" = "r23")
var_lab(data_2000$marriage) <- c("Marriage status")
data_2000$marriage <- ifelse(data_2000$marriage ==2 & data_2000$marriage ==3, 2, data_2000$marriage)
data_2000$marriage <- ifelse(data_2000$marriage == 4, 3, data_2000$marriage)
data_2000$marriage <- ifelse(data_2000$marriage == 5, 4, data_2000$marriage)
val_lab(data_2000$marriage) <- num_lab("1 Single
                                       2 Married
                                       3 Divorce
                                       4 Widow")
table(data_2000$marriage)

# r24: marriage_y
data_2000 <- rename(data_2000, "marriage_y" = "r241")
var_lab(data_2000$marriage_y) <- c("Marriage year")
table(data_2000$marriage_y)

# r253: boys
data_2000 <- rename(data_2000, "boys" = "r253")
var_lab(data_2000$boys) <- c("")

# r254: girls
data_2000 <- rename(data_2000, "girls" = "r254")
var_lab(data_2000$girls) <- c("")

### 8. Age  ----
str(data_2000$age)


# Save data in stata form
write.dta(data_2000, "Working Data/V2.2 Individual level/cens.dta")


