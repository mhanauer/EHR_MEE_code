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
  
MEE_DIAG_dat = read.csv("MEE_DIAG.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry"))
MEE_DIAG = MEE_DIAG_dat[c("SourceClient_ID", "ICD.Code.Description", "Diagnosis.Ranking", "Diagnosis.Begin.Date")]
#Only including primary diagnosis
MEE_DIAG = subset(MEE_DIAG, Diagnosis.Ranking == "PRIMARY")
dim(MEE_DIAG)
MEE_DIAG$MDD = ifelse(MEE_DIAG$ICD.Code.Description == "MAJOR DEPRESSIVE DISORDER", 1, 0)
describe.factor(MEE_DIAG$MDD)

#### Remove all duplicates which I am assuming will be the
library(dplyr)

MEE_DIAG = MEE_DIAG[order(MEE_DIAG$SourceClient_ID),]

#### Diagnoses are from many years ago.
MEE_DIAG$Diagnosis.Begin.Date = mdy(MEE_DIAG$Diagnosis.Begin.Date)
range(MEE_DIAG$Diagnosis.Begin.Date)
### If you want to only included diagnoses provided 
#MEE_DIAG = subset(MEE_DIAG, Diagnosis.Begin.Date >= "2020-01-01")
dim(MEE_DIAG)
MEE_DIAG

MEE_DIAG = distinct(MEE_DIAG, SourceClient_ID, .keep_all = TRUE)
sum(duplicated(MEE_DIAG$SourceClient_ID))
MEE_DIAG
### Try merging on SourceClient_ID with MEE_PHQ9 and MEE_DIAG
phq9_diag = merge(MEE_PHQ9, MEE_DIAG, by = "SourceClient_ID", all.x= TRUE)
dim(phq9_diag)
dim(MEE_PHQ9)

## Now merge on Client_ID with 
phq9_diag_demo = merge(phq9_diag, MEE_ClientDemo, by = "SourceClient_ID", all.x = TRUE)
dim(phq9_diag_demo)

phq9_diag_demo = phq9_diag_demo[c("SourceClient_ID", "PHQ9_Date", "PHQ9_Total", "MDD", "ORG_ABBREV", "gender_minority", "racial_minority", "Diagnosis.Begin.Date")]
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
1-dim(phq9_diag_demo_complete)[1] / dim(phq9_diag_demo)[1] 

range(phq9_diag_demo_complete$PHQ9_Date)

subset(phq9_diag_demo_complete, PHQ9_Date == "2020-03-01")
```
Discard everyone in March, because that is a messy month
Create a telehealth variable
Remove all IDs with the same PHQ-9 administration date assuming duplicates
Create a time variable that describes each PHQ-9 administration
Remove all time points greater than three
Create new log + 1 because you cannot take the log of zero
```{r}
phq9_diag_demo_complete = phq9_diag_demo_complete[order(phq9_diag_demo_complete$SourceClient_ID),]
#### Comment out to see if the time between sessions changes
phq9_diag_demo_complete =subset(phq9_diag_demo_complete, PHQ9_Date <= "2020-2-28" | PHQ9_Date >= "2020-04-01")
phq9_diag_demo_complete = phq9_diag_demo_complete[order(phq9_diag_demo_complete$PHQ9_Date),]
tail(phq9_diag_demo_complete, 7500)
phq9_diag_demo_complete$telehealth = ifelse(phq9_diag_demo_complete$PHQ9_Date >= "2020-04-01",1, 0)
phq9_diag_demo_complete$face_to_face = ifelse(phq9_diag_demo_complete$telehealth == 1, 0,1)
phq9_diag_demo_complete
library(dplyr)

#### Remove clients who have the same date and ID
phq9_diag_demo_complete = distinct(phq9_diag_demo_complete, SourceClient_ID, PHQ9_Date, .keep_all = TRUE)
subset(phq9_diag_demo_complete, SourceClient_ID == "1||425128")

#### Create a time variable
phq9_diag_demo_complete = phq9_diag_demo_complete %>% dplyr::group_by(SourceClient_ID) %>% dplyr::mutate(time = row_number()-1)
dim(phq9_diag_demo_complete)
phq9_diag_demo_complete[order(phq9_diag_demo_complete$SourceClient_ID),]

sum(is.na(phq9_diag_demo_complete))
phq9_diag_demo_complete[c(1,12)]
describe.factor(phq9_diag_demo_complete$time)

### Subset for 0,1,2, and 3, because that is 96% of the data and that is almost 200 days
phq9_diag_demo_complete = subset(phq9_diag_demo_complete, time < 4)
dim(phq9_diag_demo_complete)

#1||70005671
### Won't put them in the correct order when sorting, but did work
subset(phq9_diag_demo_complete, SourceClient_ID == "1||70005671")

### Check range on date still 1990 diagnosis
range(phq9_diag_demo_complete$Diagnosis.Begin.Date)
phq9_diag_demo_complete$Diagnosis.Begin.Date = NULL


subset(phq9_diag_demo_complete, SourceClient_ID == "1||425128")

### Create new log variable
phq9_diag_demo_complete$log_PHQ9_Total = log(phq9_diag_demo_complete$PHQ9_Total+1)

test = distinct(phq9_diag_demo_complete, SourceClient_ID, time, .keep_all = TRUE)
dim(phq9_diag_demo_complete)
phq9_diag_demo_complete
dim(test)

```
Mean days between administrations
```{r}
### Get the average time between PHQ9 administration 
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
matched_0_1  = matched_0_1[order(matched_0_1$SourceClient_ID),]
matched_0_1[c("PHQ9_Date.x", "PHQ9_Date.y")]

mean_diff_0_1 = median(matched_0_1$PHQ9_Date.x-matched_0_1$PHQ9_Date.y)
mean_diff_0_1

matched_1_2 = merge(time_1, time_2, by = "SourceClient_ID", all.y = TRUE)
dim(matched_1_2)
dim(time_1)
matched_1_2
mean_diff_1_2 = median(matched_1_2$PHQ9_Date.x-matched_1_2$PHQ9_Date.y)
mean_diff_1_2

matched_2_3 = merge(time_2, time_3, by = "SourceClient_ID", all.y = TRUE)
dim(matched_2_3)
dim(time_1)
matched_2_3
mean_diff_2_3 = median(matched_2_3$PHQ9_Date.x-matched_2_3$PHQ9_Date.y)
mean_diff_2_3

mean_overall_days = round(median(as.numeric(mean_diff_0_1), as.numeric(mean_diff_1_2), as.numeric(mean_diff_2_3)),0)
mean_overall_days

```


Participant characteristics
```{r}
dim(phq9_diag_demo_complete)
fac_des = apply(phq9_diag_demo_complete[c(4,5,6,7, 10, 12)], 2, function(x){describe.factor(x)})
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
telehealth_time_describe
```
Graph for means over time
```{r}
library(ggplot2)
plot_means = ggplot(data =telehealth_time_describe, aes(x = time, y = phq_9_mean, group = telehealth))+
  geom_line(aes(color = telehealth))+
  geom_point(aes(color = telehealth))+
  scale_y_continuous(limits = c(0,20))+
  labs(title="Figure 1: Mean PHQ-9 total score by PHQ-9 adminstration", y = "Mean PHQ-9", x = "Adminstration")+
  geom_text(aes(label = phq_9_mean), position=position_dodge(width=.8), vjust=-0.20)
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

Bayesian Linear model
```{r}
library(MASS)
library(rstanarm)


stan_linear_log = stan_glm(log_PHQ9_Total ~ time*telehealth + MDD +gender_minority +racial_minority + IL + FL, data = phq9_diag_demo_complete,  seed = 123)
stan_linear_log_sum = round(stan_linear_log$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum = round(exp(stan_linear_log_sum),3)
### Creates a percentage instead 1 + % 
stan_linear_log_sum= stan_linear_log_sum - 1
stan_linear_log_sum

```
Check model diagnostics
```{r}
car::vif(stan_linear_log)
lmtest::bptest(stan_linear_log)
launch_shinystan(stan_linear_log)
median(bayes_R2(stan_linear_log))
```
Plot interactions
```{r}
library(sjPlot)

stan_linear_total = stan_glm(PHQ9_Total ~ time*telehealth + MDD +gender_minority +racial_minority + IL + FL, data = phq9_diag_demo_complete, seed = 123)
stan_linear_total_sum = round(stan_linear_total$stan_summary[,c(1,3,4,10)],4)
stan_linear_total_sum

plot_stan_linear_total= plot_model(stan_linear_total, type = "int", terms = c("time", "telehealth"), mdrt.values = "meansd")

plot_stan_linear_total +
  scale_y_continuous(limits = c(0,20))+
  labs(title="Figure 2: Predicted values of PHQ-9", y = "PHQ-9 total", x = "Adminstration")
```
Test if excluding zero changes things
```{r}
phq9_diag_demo_complete_no_zero = subset(phq9_diag_demo_complete, PHQ9_Total > 0)
dim(phq9_diag_demo_complete_no_zero)[1] / dim(phq9_diag_demo_complete)[1]
stan_linear_log = stan_glm(log_PHQ9_Total ~ time*telehealth + MDD +gender_minority +racial_minority + IL + FL, data = phq9_diag_demo_complete_no_zero)
stan_linear_log_sum = round(stan_linear_log$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum = round(exp(stan_linear_log_sum),3)
### Creates a percentage instead 1 + % 
stan_linear_log_sum= stan_linear_log_sum - 1
stan_linear_log_sum


```
##################################
Clean telehealth analysis
##################################

Try to get only those who had services in face to face baseline to one and telehealth baseline to one
Need to get rid of people in face to face group with PHQ9_Date.y after 4-1-2020.  Those are people in face to face who have some services via telehealth.
Need to get rid of people in telehealth group who had a PHQ9_Date.x before 4-1-2020.  Those people had some face to face and telehealth.

I don't want people who have a baseline date pre telehealth and a admin 1 date post telehealth.

```{r}
#### SHould be 2634 pairs from baseline to first admin
dat_base = subset(phq9_diag_demo_complete, time == 0)
### If you are in the face to face group and your date is on or after 4-1-2020, then you are dropped 
describe.factor(dat_base$telehealth)
sum(duplicated(dat_base$SourceClient_ID))
tail(dat_base, 3500)

dat_1 = subset(phq9_diag_demo_complete, time == 1)
dat_base_1 = merge(dat_base, dat_1, by = "SourceClient_ID")
dim(dat_base_1)


### Now filter out those with face_to_face with .x dates 2020-04-01 or greater
### Filter otu those who have baseline 
#### Then filter out those with telehealth with .y dates before 2020-04-01
dat_base_1$drop = ifelse(dat_base_1$PHQ9_Date.x < "2020-04-01" & dat_base_1$PHQ9_Date.y > "2020-04-01", 1, 0) 
test = subset(dat_base_1, drop == 1)
test[c("PHQ9_Date.x", "PHQ9_Date.y")]
dat_base_1 = subset(dat_base_1, drop == 0)
dat_base_1[c("PHQ9_Date.x", "PHQ9_Date.y")]
dat_base_1$drop = NULL

clean_compare_dat = dat_base_1
dim(clean_compare_dat)
### Make long

```
Participant characteristics
```{r}

### Average time between administrations
mean(clean_compare_dat$PHQ9_Date.x-clean_compare_dat$PHQ9_Date.y)

dim(clean_compare_dat)
fac_des = apply(clean_compare_dat[c(4:8, 11, 2)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
fac_des
write.csv(fac_des, "fac_des.csv")
2634*2
```

Get group means in for time points for telehealth
```{r}
telehealth_time_describe = clean_compare_dat %>%
  group_by(telehealth, time) %>%
  summarise_at(vars(PHQ9_Total.x), list(phq_9_median = median, phq_9_mean = mean))
telehealth_time_describe = round(telehealth_time_describe, 2)
telehealth_time_describe$telehealth = as.factor(telehealth_time_describe$telehealth)
telehealth_time_describe$time = as.factor(telehealth_time_describe$time)
telehealth_time_describe
```
Graph for means over time
```{r}
library(ggplot2)
plot_means = ggplot(data =telehealth_time_describe, aes(x = time, y = phq_9_mean, group = telehealth))+
  geom_line(aes(color = telehealth))+
  geom_point(aes(color = telehealth))+
  scale_y_continuous(limits = c(0,20))+
  labs(title="Figure 1: Mean PHQ-9 total score by PHQ-9 adminstration \n for complete telehealth and face to face", y = "Mean PHQ-9", x = "Adminstration")+
  labs(fill = "telehealth")+
  geom_text(aes(label = phq_9_mean), position=position_dodge(width=.8), vjust=-0.20)
plot_means
```

Run model comparing clean face to face with clean telehealth
```{r}
library(rstanarm)
stan_linear_log = stan_glm(log_PHQ9_Total.x ~ time*telehealth + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x, data = clean_compare_dat,  seed = 123)
stan_linear_log_sum = round(stan_linear_log$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum = round(exp(stan_linear_log_sum),3)
### Creates a percentage instead 1 + % 
stan_linear_log_sum= stan_linear_log_sum - 1
stan_linear_log_sum


```
Check model diagnostics
```{r}
car::vif(stan_linear_log)
lmtest::bptest(stan_linear_log)
launch_shinystan(stan_linear_log)
median(bayes_R2(stan_linear_log))
```
Plot interactions
```{r}
library(sjPlot)
library(sjmisc)
clean_compare_dat$time = to_factor(clean_compare_dat$time)
stan_linear_total = stan_glm(PHQ9_Total.x ~ time*telehealth + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x, data = clean_compare_dat, seed = 123)
stan_linear_total_sum = round(stan_linear_total$stan_summary[,c(1,3,4,10)],4)
stan_linear_total_sum


plot_stan_linear_total= plot_model(stan_linear_total, type = "int", terms = c("time", "telehealth"))

plot_stan_linear_total +
  scale_y_continuous(limits = c(0,20))+
  labs(title="Figure 2: Predicted values of PHQ-9", y = "PHQ-9 total", x = "Adminstration")
```

Test if excluding zero changes things
```{r}
phq9_diag_demo_complete_no_zero = subset(phq9_diag_demo_complete, PHQ9_Total > 0)
dim(phq9_diag_demo_complete_no_zero)[1] / dim(phq9_diag_demo_complete)[1]
stan_linear_log = stan_glm(log_PHQ9_Total ~ time*telehealth + MDD +gender_minority +racial_minority + IL + FL, data = phq9_diag_demo_complete_no_zero)
stan_linear_log_sum = round(stan_linear_log$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum = round(exp(stan_linear_log_sum),3)
### Creates a percentage instead 1 + % 
stan_linear_log_sum= stan_linear_log_sum - 1
stan_linear_log_sum


```

###########################
Extra
###########################

Review residuals for linear model
```{r}

vif(simple_linear_log)
plot(simple_linear_log)
hist(simple_linear_log$residuals)
cook_d = cooks.distance(simple_linear_log)
cooks_d_data = data.frame(phq9_diag_demo_complete, cook_d)
cooks_d_data[cooks_d_data$cook_d > 4/dim(phq9_diag_demo_complete)[1]]
subset(cooks_d_data, cook_d > 4/dim(phq9_diag_demo_complete)[1])

```
Best results so far
Does not account for non-normal, but does account for nesting effect which is large
Take the log to make somewhat more normal and there is a problem with level two residuals
```{r}
library(lmerTest)
library(HLMdiag)
library(robustlmm)
library(lme4)
library(car)
## Log + 1 because you cannot take the log of zero
phq9_diag_demo_complete$log_PHQ9_Total = log(phq9_diag_demo_complete$PHQ9_Total+1)
freq_linear_results = lmer(log_PHQ9_Total~ time*telehealth + MDD +gender_minority +racial_minority + IL + FL + (1 | SourceClient_ID), data = phq9_diag_demo_complete)
summary(freq_linear_results)
vif(freq_linear_results)
con_inf_freq_linear =  round(confint(freq_linear_results),2)
con_inf_freq_linear
library(r2glmm)
r2beta(freq_linear_results)
range(phq9_diag_demo_complete$PHQ9_Date)
0.5519 /(0.5519+0.2712)
library(descr)

telehealth_dat = subset(phq9_diag_demo_complete, telehealth  == 1)
face_to_face = subset(phq9_diag_demo_complete, face_to_face == 1)
compmeans(telehealth_dat$PHQ9_Total, telehealth_dat$time)
compmeans(face_to_face$PHQ9_Total, face_to_face$time)
```

Review residuals for multilevel model
```{r}
vif(freq_linear_results)
plot(freq_linear_results)
freq_linear_results_sum = summary(freq_linear_results)
hist(freq_linear_results_sum$residuals)

resid_level1 = HLMresid(freq_linear_results,level = 1, type = "LS", standardize = TRUE)
resid_level2 <- HLMresid(object = freq_linear_results, level = "SourceClient_ID")
hist(resid2_fm3$`(Intercept)`)
hist(resid2_fm3$`(Intercept)`)

cooksd_fm4 <- cooks.distance(freq_linear_results, group = "SourceClient_ID")
cooksd_fm4
dotplot_diag(x = cooksd_fm4, cutoff = "internal", name = "cooks.distance", modify = "dotplot") + ylab("Cook's distance") + xlab("school")
```
Test out interactions
```{r}
library(interactions)

phq9_diag_demo_complete$log_PHQ9_Total = log(phq9_diag_demo_complete$PHQ9_Total+1)
freq_linear_results_total = lmer(PHQ9_Total~ time*telehealth + MDD +gender_minority +racial_minority + IL + FL + (1 | SourceClient_ID), data = phq9_diag_demo_complete)
summary(freq_linear_results_total)
#johnson_neyman(freq_linear_results_total, pred = "telehealth", modx = "time", control.fdr = TRUE)
interact_plot(freq_linear_results_total, pred = "time", modx =  "telehealth")

```



Get differences between first and second administration which is about 50 days
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

