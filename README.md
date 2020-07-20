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
Get group means in for time points for telehealth
```{r}
telehealth_time_describe = phq9_diag_demo_complete %>%
  group_by(telehealth, time) %>%
  summarise_at(vars(PHQ9_Total), list(phq_9_median = median, phq_9_mean = mean))
telehealth_time_describe = round(telehealth_time_describe, 2)
telehealth_time_describe$telehealth = as.factor(telehealth_time_describe$telehealth)

```
Graph for means over time
```{r}
plot_means = ggplot(data =telehealth_time_describe, aes(x = time, y = phq_9_mean, group = telehealth))+
  geom_line(aes(color = telehealth))+
  geom_point(aes(color = telehealth))
plot_means
```



Normality diagnostics for best model
http://people.duke.edu/~rnau/testing.htm
Main goal is to have the residual normal
```{r}
library(ggpubr)
ggqqplot(phq9_diag_demo_complete$log_PHQ9_Total)
ggqqplot(phq9_diag_demo_complete$PHQ9_Total)
hist(phq9_diag_demo_complete$log_PHQ9_Total)
hist(phq9_diag_demo_complete$PHQ9_Total)

```

Best results so far
Does not account for non-normal, but does account for nesting effect which is large
Take the log to make somewhat more normal
```{r}
library(lmerTest)
library(HLMdiag)

freq_linear_results = lmer(log_PHQ9_Total~ time*face_to_face + MDD +gender_minority +racial_minority + IL + FL + (1 | SourceClient_ID), data = phq9_diag_demo_complete)
summary(freq_linear_results)
con_inf_freq_linear =  round(confint(freq_linear_results),2)
freq_linear_results
```


Review post best model diagnostics
```{r}
vif(freq_linear_results)
plot(freq_linear_results)
freq_linear_results_sum = summary(freq_linear_results)
hist(freq_linear_results_sum$residuals)

resid_level1 = HLMresid(freq_linear_results,level = 1, type = "LS", standardize = TRUE)
resid_level2 <- HLMresid(object = freq_linear_results, level = "SourceClient_ID")
hist(resid2_fm3$`(Intercept)`)
```



Get differences between first and second adminstration which is about 50 days
This analysis looks at those who had their first session not during telehealth versus those who only had sessions after telehealth

```{r}
matched_0_1_analysis = matched_0_1
matched_0_1_analysis$diff_score = matched_0_1_analysis$PHQ9_Total.y-matched_0_1_analysis$PHQ9_Total.x
hist(matched_0_1_analysis$diff_score)

matched_0_1_analysis$PHQ9_Total.x
matched_0_1_analysis_results = lm(diff_score ~ telehealth.x+ MDD.x + gender_minority.x + racial_minority.x + IL.x + FL.x, data = matched_0_1_analysis)
summary(matched_0_1_analysis_results)
AIC(matched_0_1_analysis_results)
BIC(matched_0_1_analysis_results)
```
Now for 2 versus 3
```{r}
matched_2_3_analysis = matched_2_3
matched_2_3_analysis$diff_score = matched_2_3_analysis$PHQ9_Total.y-matched_2_3_analysis$PHQ9_Total.x
hist(matched_2_3_analysis$diff_score)


matched_2_3_analysis_results = lm(diff_score ~ telehealth.x+ MDD.x + gender_minority.x + racial_minority.x + IL.x + FL.x, data = matched_2_3_analysis)
summary(matched_2_3_analysis_results)

```
What about 0 and 2 and 0 and 3.  Maybe clients need to be in treatment longer

