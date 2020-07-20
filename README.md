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
TestMCARNormality(phq9_diag_demo[c(3,4,6:9)])

phq9_diag_demo_complete = na.omit(phq9_diag_demo)
dim(phq9_diag_demo_complete)
dim(phq9_diag_demo)
```

