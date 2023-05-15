# This file is for preliminary econometrics analysis of V2.2 Individual level data

# Prep work ----
setwd("/Users/jiaxiangchen/Desktop/ECON32212 Applied Dissertation B/Empirics")
library(haven)
library(tidyverse)
library(readr)
library(readxl)
library(foreign)
library(stargazer)
library(gsubfn)
library(broom)
library(dotwhisker)
library(dplyr)
library(magrittr)
library(hchinamap)
library(reporttools)
library(sandwich)
library(fastDummies)
library(lmtest)
library(formatdown)

# Migration data
cens <- read_dta("Working Data/V2.2 Individual level/cens.dta")
# PD news data
n_s <- read_csv("Working Data/V2.2 Individual level/News/n_s9900.csv")
# Province control
provc <- read_dta("Working Data/V2.2 Individual level/provc.dta")
province_cde <- read_excel("Raw Data/China Province Code.xlsx")


# Population and control selection 1 ----
# Age: 16 ~ 40
# 2000 migrants vs before 1996 & never migrate
# Mig_reason: 1, work an dbusiness
# 15 province select
# Export province(Middlelands), Import province (Import)
# 13(Heibei), 34(Anhui), 36(Jiangxi), 41(Henan), 42(Hubei), 43(Hunan), 45(Guangxi), 51(Sihchuan)
# 11(Beijing), 31(Shanghai), 32(Jiangsu), 33(Zhejiang),  35(Fujian),37(Shandong), 44(Guangdong)
prov_s1 <- c(13,34,36,41,42,43,45,51,
             11,31,32,33,35,37,44)
# Census data selection
cens1 <- cens %>% 
        filter(age >16 & age < 40) %>% 
        filter(!is.na(hukou_typ) & hukou_typ !=0) %>% 
        filter(mig_reason==1|mig=="Before 1996"|mig=="Never") %>% 
        filter(mig=="2000"|mig=="Before 1996"|mig=="Never") %>% 
        filter(province %in% prov_s1 & org_prov %in% c(prov_s1, NA))

# News data selection
n_s1 <- n_s %>% filter(!is.na(index)) %>%
        filter(prov_cde %in% prov_s1) %>% 
        group_by(prov_cde) %>% 
        summarize(n_news= n(),
                  n_economy=sum(economy),
                  n_politics=sum(politics),
                  n_rural=sum(rural),
                  n_technology=sum(technology),
                  n_other=sum(other))
# Province control selection
provc1 <- provc %>% select(imp_cde, exp_cde, imp_sec, imp_inc, exp_sec, exp_inc, distance) %>% 
        filter(imp_cde %in% prov_s1 & exp_cde %in% prov_s1)
provc1[is.na(provc1$distance),]$distance <- 0

# Merge province control
pc1 <- left_join(provc1, n_s1, by = c("imp_cde" = "prov_cde"))
pc1 <- rename(pc1, "imp_news" = "n_news", "imp_economy" = "n_economy",
              "imp_politics" = "n_politics", "imp_rural" = "n_rural",
              "imp_tech" = "n_technology", "imp_other" = "n_other")

pc1 <- left_join(pc1, n_s1, by = c("exp_cde" = "prov_cde"))
pc1 <- rename(pc1, "exp_news" = "n_news", "exp_economy" = "n_economy",
              "exp_politics" = "n_politics", "exp_rural" = "n_rural",
              "exp_tech" = "n_technology", "exp_other" = "n_other")

# Expected income computation ----
# Add income control and counterfactual expected income

# Method1: Probability
# Construct probability of working in agriculture sector in destination by province
cens1$sec <- ifelse(cens1$industry < 600, 1, 0)
cens1$hukou_typ <- ifelse(cens1$hukou_typ==1,1,0)
lmp <- lm(sec ~ age+marriage+literacy+ethnicity+gender+hukou_typ+factor(educ)+factor(province), data=cens1)
summary(lmp)

# Expected Import income 
inc_imp <- cbind(pc1 %>% select(imp_cde, imp_inc, imp_sec) %>% 
                         distinct() %>% as.data.frame() %>% 
                         arrange(imp_cde) %>% filter(imp_sec==1), 
                 pc1 %>% select(imp_cde, imp_inc, imp_sec) %>% 
                         distinct() %>% as.data.frame() %>% 
                         arrange(imp_cde) %>% filter(imp_sec==0))

inc_imp <- inc_imp[,c(1,2,5)]
names(inc_imp) <- c("imp_cde", "imp_inc_ag", "imp_inc_na")
pc1 <- left_join(pc1, inc_imp, by=c("imp_cde"))

# Expected Export income
inc_exp <- cbind(pc1 %>% select(exp_cde, exp_inc, exp_sec) %>% 
                         distinct() %>% as.data.frame() %>% 
                         arrange(exp_cde) %>% filter(exp_sec==1), 
                 pc1 %>% select(exp_cde, exp_inc, exp_sec) %>% 
                         distinct() %>% as.data.frame() %>% 
                         arrange(exp_cde) %>% filter(exp_sec==0))

inc_exp <- inc_exp[,c(1,2,5)]
names(inc_exp) <- c("exp_cde", "exp_inc_ag", "exp_inc_na")
pc1 <- left_join(pc1, inc_exp, by=c("exp_cde"))


## Stargazer option
mark  = '::::'
# star = stargazer(m1, header = F, decimal.mark  = mark, digit.separator = '')

replace_numbers = function(x, low=0.00001, high=1e5, digits = 9, scipen=-999, ...) {
        x = gsub(mark,'.',x)
        x.num = as.numeric(x)
        ifelse(
                (x.num >= low) & (x.num < high), 
                round(x.num, digits = digits), 
                formatC(x.num, format = "e", digits = 2)
        )
}    

reg = paste0("([0-9.\\-]+", mark, "[0-9.\\-]+)")
# cat(gsubfn(reg, ~replace_numbers(x), star), sep='\n')

# Method 2
# Based on IC OLS fitted values



## 1 Model 1: iod construction for 2000 migrants----
cens1$mig_00 <- ifelse(cens1$mig=="2000" & cens1$province!=cens1$org_prov, 1, 0) #13759 observation
cens1$sec <- ifelse(cens1$industry < 600, 1, 0)

### 1.1 Templete control ----
names(pc1)
cens_temp <- pc1 %>% select(imp_cde, exp_cde, distance,
                            imp_inc_ag, imp_inc_na,  exp_inc_ag, exp_inc_na,
                            imp_news, imp_economy, imp_rural, imp_politics, imp_tech, imp_other,
                            exp_news, exp_economy, exp_rural, exp_politics, exp_tech, exp_other) %>%  
        filter(imp_cde!=exp_cde) %>%  distinct()

###  1.2 Create iod data----
cens1$org_prov <- ifelse(cens1$mig_00!=1,cens1$province, cens1$org_prov)
cens1 <- cens1[!is.na(cens1$mig),]
cens1 <- cens1[!is.na(cens1$mig_00),]
cens1 <- cens1[!is.na(cens1$sec),]
cens1$id <- 1:nrow(cens1)
data0 <- full_join(cens_temp,cens1, by=c("exp_cde"="org_prov"))
# View(data1)
data1 <- data0

# Expected income
dp1 <- cbind(data1[,c(51,47,38,29,28,32,39,1)])
dp1 <- rename(dp1, "province"="imp_cde")
data1$p1 <- predict(lmp,dp1)
data1$imp_exinc <- data1$p1*data1$imp_inc_ag + (1-data1$p1)*data1$imp_inc_na

dp2 <- cbind(data1[,c(51,47,38,29,28,32,39,2)])
dp2 <- rename(dp2, "province"="exp_cde")
data1$p2 <- predict(lmp,dp2)
data1$exp_exinc <- data1$p2*data1$exp_inc_ag + (1-data1$p2)*data1$exp_inc_na


# orgin income
data1$exp_inc <-  data1$exp_exinc         
data1$exp_inc <- ifelse(data1$sec==1 & data1$exp_cde == data1$province, data1$exp_inc_ag, data1$exp_inc)         
data1$exp_inc <- ifelse(data1$sec==0 & data1$exp_cde == data1$province, data1$exp_inc_na, data1$exp_inc) 
# dest income
data1$imp_inc <- data1$imp_exinc         
data1$imp_inc <- ifelse(data1$sec==1 & data1$imp_cde == data1$province, data1$imp_inc_ag, data1$imp_inc)         
data1$imp_inc <- ifelse(data1$sec==0 & data1$imp_cde == data1$province, data1$imp_inc_na, data1$imp_inc) 
# mig_00
data1$mig_00 <- ifelse(data1$imp_cde==data1$province & data1$mig=="2000"&data1$exp_cde!=data1$imp_cde, 1, 0)
data1$mig_00.1 <- ifelse(data1$mig=="2000", 1, 0)
# educ: college

data1 <-data1[!is.na(data1$imp_exinc),]
data1$gender <- ifelse(data1$gender==1, 0, 1)

### 1.3 Baseline regression ----
# News
data1$prov44 <- ifelse(data1$imp_cde==44,1, 0)


options(digits=5)


glm1.2 <- glm(mig_00 ~ exp_news+imp_news+distance+prov44+exp_exinc+imp_exinc+
                    age+marriage+literacy+ethnicity+gender+hukou_typ+factor(educ)-1, data=data1, family = "binomial")


stargazer(glm1.2)





glm2.3 <- glm(mig_00 ~ exp_economy+imp_economy+exp_politics+imp_politics+
                    exp_rural+imp_rural+exp_tech+imp_tech+
                    distance+prov44+exp_exinc+imp_exinc+
                    age+marriage+literacy+ethnicity+gender+hukou_typ+factor(educ)-1, data=data1, family = "binomial"


stargazer(glm2.3)

