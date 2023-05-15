# This file is for province control, 2000
# prov-code, Wage, distance

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
library(readxl)
mig_dta <- read_dta("Raw Data/AEJ Migration Flow/migration_data.dta")
prov_cde <- read_excel("Raw Data/China Province Code.xlsx")

# Select variables
names(prov_cde) <- c("prov_cde", "prov")
mig_dta <- mig_dta[,-c(5,6,9,10,12,13,14)]
mig_dta <- merge(mig_dta, prov_cde, by.x =c("importer"), by.y=c("prov"), all.x = T)
mig_dta <- rename(mig_dta, "imp_cde" ="prov_cde")
mig_dta <- merge(mig_dta, prov_cde, by.x =c("exporter"), by.y=c("prov"), all.x = T)
mig_dta <- rename(mig_dta, "exp_cde" ="prov_cde")

# Ag and non-ag
mig_dta$imp_sec <- ifelse(str_detect(mig_dta$j, "Ag"), 1,0) # Agriculture =1
mig_dta$exp_sec <- ifelse(str_detect(mig_dta$i, "Ag"), 1,0)

# Rename variable
mig_dta <- rename(mig_dta, "imp_inc" = "Vj")
mig_dta <- rename(mig_dta, "exp_inc" = "Vi")

# Save data in stata form
write.dta(mig_dta, "Working Data/V2.2 Individual level/provc.dta")
