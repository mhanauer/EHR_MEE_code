---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Variables I need

MEE_PHQ9
Client_ID
SourceClient_ID
PHQ9_Date
PHQ9_Total

MEE_ClientDemo
Gender
Race
ORG_ABBREV
PATID

MEE_DIAG
SourceClient_ID
ICD Code Description
```{r}
library(prettyR)
library(lubridate)
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/MEE_Data")
MEE_PHQ9 = read.csv("MEE_PHQ9.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry")) 
MEE_PHQ9 = MEE_PHQ9[c("Client_ID", "SourceClient_ID", "PHQ9_Date", "PHQ9_Total")]
summary(MEE_PHQ9)
MEE_PHQ9$PHQ9_Date = ymd(MEE_PHQ9$PHQ9_Date)
MEE_PHQ9$SourceClient_ID
head(MEE_PHQ9$SourceClient_ID)
dim(MEE_PHQ9)

MEE_ClientDemo = read.csv("MEE_ClientDemo.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry"))
head(MEE_ClientDemo)
MEE_ClientDemo = MEE_ClientDemo[c("Gender", "Race", "ORG_ABBREV", "SourceClient_ID")]
#apply(MEE_ClientDemo, 2, function(x){describe.factor(x)})
MEE_ClientDemo$gender_minority = ifelse(MEE_ClientDemo$Gender != "MALE", 1, 0)
describe.factor(MEE_ClientDemo$Race)
head(MEE_ClientDemo, 10)
MEE_ClientDemo$racial_minority = ifelse(MEE_ClientDemo$Race != "WHITE/CAUCASIAN",1 , 0)
head(MEE_ClientDemo, 10)
MEE_ClientDemo$Client_ID = MEE_ClientDemo$PATID
  
#MEE_DIAG_dat = read.csv("MEE_DIAG.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry"))
MEE_DIAG = MEE_DIAG_dat[c("SourceClient_ID", "ICD.Code.Description", "Diagnosis.Ranking")]
MEE_DIAG = subset(MEE_DIAG, Diagnosis.Ranking == "PRIMARY")
dim(MEE_DIAG)
MEE_DIAG$MDD = ifelse(MEE_DIAG$ICD.Code.Description == "MAJOR DEPRESSIVE DISORDER", 1, 0)
describe.factor(MEE_DIAG$MDD)

#### Remove all duplicates which I am assuming will be the
library(dplyr)
MEE_DIAG = distinct(MEE_DIAG, SourceClient_ID, .keep_all = TRUE) 
MEE_DIAG

### Try merging on SourceClient_ID with MEE_PHQ9 and MEE_DIAG
phq9_diag = merge(MEE_PHQ9, MEE_DIAG, by = "SourceClient_ID", all.x= TRUE)
dim(phq9_diag)
phq9_diag
phq9_diag
head(MEE_PHQ9$SourceClient_ID)
head(MEE_DIAG$SourceClient_ID)
### Only include primary diagnosis 
MEE_PHQ9


## Now merge on Client_ID with 
phq9_diag_demo = merge(phq9_diag, MEE_ClientDemo, by = "SourceClient_ID", all.x = TRUE)
dim(phq9_diag_demo)
MEE_ClientDemo
### Something not matching with demos


phq9_diag_demo = phq9_diag_demo[c("SourceClient_ID", "PHQ9_Date", "PHQ9_Total", "MDD", "ORG_ABBREV", "gender_minority", "racial_minority")]
apply(phq9_diag_demo[3:7], 2, function(x){describe.factor(x)})
### Create two dummy variables for IL and FL
phq9_diag_demo$IL = ifelse(phq9_diag_demo$ORG_ABBREV == "CIL", 1, 0)
phq9_diag_demo$FL = ifelse(phq9_diag_demo$ORG_ABBREV == "CFL", 1, 0)
phq9_diag_demo

```
Explore missing data
```{r}
### Explore missing data
library(naniar)
library(MissMech)
miss_var_summary(phq9_diag_demo)
miss_case_table(phq9_diag_demo)
pct_miss_case(phq9_diag_demo)
### Go ahead and drop missing so we can include gender
#TestMCARNormality(phq9_diag_demo[c(3,4,6:9)])

phq9_diag_demo_complete = na.omit(phq9_diag_demo)
dim(phq9_diag_demo_complete)
dim(phq9_diag_demo)
```

Create a time variable
Discard everyone in March, because that is a messy month
```{r}
phq9_diag_demo_complete = phq9_diag_demo_complete[order(phq9_diag_demo_complete$SourceClient_ID),]
phq9_diag_demo_complete =subset(phq9_diag_demo_complete, PHQ9_Date <= "2020-2-28" | PHQ9_Date >= "2020-04-01")
phq9_diag_demo_complete = phq9_diag_demo_complete[order(phq9_diag_demo_complete$PHQ9_Date),]
tail(phq9_diag_demo_complete, 7500)
phq9_diag_demo_complete$telehealth = ifelse(phq9_diag_demo_complete$PHQ9_Date >= "2020-04-01",1, 0)
phq9_diag_demo_complete$face_to_face = ifelse(phq9_diag_demo_complete$telehealth == 1, 0,1)
phq9_diag_demo_complete
library(dplyr)
phq9_diag_demo_complete = phq9_diag_demo_complete %>% group_by(SourceClient_ID) %>% mutate(time = row_number()-1)
dim(phq9_diag_demo_complete)

sum(is.na(phq9_diag_demo_complete))
phq9_diag_demo_complete
describe.factor(phq9_diag_demo_complete$time)
### Get the average time between PHQ9 adminstration 
## You need to get matched pairs to be able to say the difference between some time points
### To make it easier just to three 4 for now
## 0 to 1
time_0 = subset(phq9_diag_demo_complete, time == 0)
time_1 = subset(phq9_diag_demo_complete, time == 1)
time_2 = subset(phq9_diag_demo_complete, time == 2)
time_3 = subset(phq9_diag_demo_complete, time == 3)

matched_0_1 = merge(time_0, time_1, by = "SourceClient_ID", all.y = TRUE)
dim(matched_0_1)
dim(time_1)
matched_0_1
mean_diff_0_1 = mean(matched_0_1$PHQ9_Date.x-matched_0_1$PHQ9_Date.y)
mean_diff_0_1

matched_1_2 = merge(time_1, time_2, by = "SourceClient_ID", all.y = TRUE)
dim(matched_1_2)
dim(time_1)
matched_1_2
mean_diff_1_2 = mean(matched_1_2$PHQ9_Date.x-matched_1_2$PHQ9_Date.y)
mean_diff_1_2

matched_2_3 = merge(time_2, time_3, by = "SourceClient_ID", all.y = TRUE)
dim(matched_2_3)
dim(time_1)
matched_2_3
mean_diff_2_3 = mean(matched_2_3$PHQ9_Date.x-matched_2_3$PHQ9_Date.y)
mean_diff_2_3

mean_overall_days = round(mean(as.numeric(mean_diff_0_1), as.numeric(mean_diff_1_2), as.numeric(mean_diff_2_3)),0)
mean_overall_days

### Subset for 0,1,2, and 3, because that is 96% of the data and that is almost 200 days
phq9_diag_demo_complete = subset(phq9_diag_demo_complete, time < 4)
dim(phq9_diag_demo_complete)

```
Participant characterstics
```{r}
fac_des = apply(phq9_diag_demo_complete[c(4,5,6,7, 10)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
fac_des
write.csv(fac_des, "fac_des.csv")

```
Review the outcome variable
```{r}
hist(phq9_diag_demo_complete$PHQ9_Total)
hist(log(phq9_diag_demo_complete$PHQ9_Total))
### Nonlinear bayesian model
library(rstanarm)
data("Orange", package = "datasets")
head(Orange)

## 
startvec <- c(Asym = 2, xmid = 7.25, scal = 3.5)

non_linear_results = stan_nlmer(PHQ9_Total ~ SSlogis(Asym, xmid, scal, telehealth*time)~Asym | SourceClient_ID, cores = 2, seed = 12345, init_r = 0.5)

```

