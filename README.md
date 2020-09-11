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
### Just include adults
MEE_PHQ9$Form_DESC = ifelse(MEE_PHQ9$Form_DESC == "PHQ A", "PHQ9 A", MEE_PHQ9$Form_DESC)
MEE_PHQ9 = MEE_PHQ9[c("Client_ID", "SourceClient_ID", "PHQ9_Date", "PHQ9_1", "PHQ9_2", "PHQ9_3", "PHQ9_4", "PHQ9_5", "PHQ9_6", "PHQ9_7", "PHQ9_8", "PHQ9_9", "PHQ9_Total", "Form_DESC")]




MEE_PHQ9$PHQ9_Date = ymd(MEE_PHQ9$PHQ9_Date)

MEE_ClientDemo = read.csv("MEE_ClientDemo.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry"))
MEE_ClientDemo = MEE_ClientDemo[c("Gender", "Race", "ORG_ABBREV", "SourceClient_ID")]
MEE_ClientDemo$gender_minority = ifelse(MEE_ClientDemo$Gender != "MALE", 1, 0)


MEE_ClientDemo$racial_minority = ifelse(MEE_ClientDemo$Race != "WHITE/CAUCASIAN",1 , 0)
MEE_ClientDemo$Client_ID = MEE_ClientDemo$PATID
  
MEE_DIAG_dat = read.csv("MEE_DIAG.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry"))
MEE_DIAG = MEE_DIAG_dat[c("SourceClient_ID", "ICD.Code.Description", "Diagnosis.Ranking", "Diagnosis.Begin.Date")]

#Only including primary diagnosis
MEE_DIAG = subset(MEE_DIAG, Diagnosis.Ranking == "PRIMARY")
dim(MEE_DIAG)
MEE_DIAG$MDD = ifelse(MEE_DIAG$ICD.Code.Description == "MAJOR DEPRESSIVE DISORDER", 1, 0)

library(dplyr)
MEE_DIAG = MEE_DIAG[order(MEE_DIAG$SourceClient_ID),]
#### Diagnoses are from many years ago.
MEE_DIAG$Diagnosis.Begin.Date = mdy(MEE_DIAG$Diagnosis.Begin.Date)
range(MEE_DIAG$Diagnosis.Begin.Date)
### If you want to only included diagnoses provided 
#MEE_DIAG = subset(MEE_DIAG, Diagnosis.Begin.Date >= "2020-01-01")
### Keeping the first instance of primary diagnosis
MEE_DIAG = distinct(MEE_DIAG, SourceClient_ID, .keep_all = TRUE)
### Should be no duplicates
sum(duplicated(MEE_DIAG$SourceClient_ID))

### Try merging on SourceClient_ID with MEE_PHQ9 and MEE_DIAG and keeping all PHQ-9 responses
phq9_diag = merge(MEE_PHQ9, MEE_DIAG, by = "SourceClient_ID", all.x= TRUE)
dim(phq9_diag)
dim(MEE_PHQ9)

## Now merge with client demos 
phq9_diag_demo = merge(phq9_diag, MEE_ClientDemo, by = "SourceClient_ID", all.x = TRUE)
dim(phq9_diag_demo)

phq9_diag_demo = phq9_diag_demo[c("SourceClient_ID", "PHQ9_Date", "PHQ9_1", "PHQ9_2", "PHQ9_3", "PHQ9_4", "PHQ9_5", "PHQ9_6", "PHQ9_7", "PHQ9_8", "PHQ9_9", "PHQ9_Total", "MDD", "ORG_ABBREV", "gender_minority", "racial_minority", "Diagnosis.Begin.Date", "Form_DESC")]

### Create two dummy variables for IL and FL
phq9_diag_demo$IL = ifelse(phq9_diag_demo$ORG_ABBREV == "CIL", 1, 0)
phq9_diag_demo$FL = ifelse(phq9_diag_demo$ORG_ABBREV == "CFL", 1, 0)
phq9_diag_demo

```

Discard everyone in March, because that is a messy month
Create a telehealth variable
Remove all IDs with the same PHQ-9 administration date assuming duplicates
Create a time variable that describes each PHQ-9 administration
Remove all time points greater than three
Create new log + 1 because you cannot take the log of zero
```{r}
### Don't want to change the name so just keeping complete data set
phq9_diag_demo_complete = phq9_diag_demo

phq9_diag_demo_complete = phq9_diag_demo_complete[order(phq9_diag_demo_complete$SourceClient_ID),]
#### Comment out to see if the time between sessions changes
### Get rid of those in March
phq9_diag_demo_complete =subset(phq9_diag_demo_complete, PHQ9_Date <= "2020-2-28" | PHQ9_Date >= "2020-04-01")
### Check that it worked
phq9_diag_demo_complete = phq9_diag_demo_complete[order(phq9_diag_demo_complete$PHQ9_Date),]
tail(phq9_diag_demo_complete, 7500)

### Create telehealth variable
phq9_diag_demo_complete$telehealth = ifelse(phq9_diag_demo_complete$PHQ9_Date >= "2020-04-01",1, 0)
phq9_diag_demo_complete$face_to_face = ifelse(phq9_diag_demo_complete$telehealth == 1, 0,1)
phq9_diag_demo_complete
library(dplyr)

#### Remove clients who have the same date and ID
phq9_diag_demo_complete = distinct(phq9_diag_demo_complete, SourceClient_ID, PHQ9_Date, .keep_all = TRUE)
### Check that it worked
subset(phq9_diag_demo_complete, SourceClient_ID == "1||425128")

#### Create a time variable
phq9_diag_demo_complete = phq9_diag_demo_complete %>% dplyr::group_by(SourceClient_ID) %>% dplyr::mutate(time = row_number()-1)
dim(phq9_diag_demo_complete)
### Check that it worked
phq9_diag_demo_complete[order(phq9_diag_demo_complete$SourceClient_ID),]
sum(is.na(phq9_diag_demo_complete))

### Subset for 0,1,2, and 3, because that is 96% of the data and that is almost 200 days
phq9_diag_demo_complete = subset(phq9_diag_demo_complete, time < 4)
dim(phq9_diag_demo_complete)

#1||70005671
### Won't put them in the correct order when sorting, but did work
subset(phq9_diag_demo_complete, SourceClient_ID == "1||70005671")

### Check range on date still 1990 diagnosis and remove diagnosis date
range(phq9_diag_demo_complete$Diagnosis.Begin.Date)
phq9_diag_demo_complete$Diagnosis.Begin.Date = NULL


subset(phq9_diag_demo_complete, SourceClient_ID == "1||425128")

### Create new log variable
phq9_diag_demo_complete$log_PHQ9_Total = log(phq9_diag_demo_complete$PHQ9_Total+1)

### Make sure no repeating IDs other than time
test = distinct(phq9_diag_demo_complete, SourceClient_ID, time, .keep_all = TRUE)
dim(phq9_diag_demo_complete)
dim(test)

```
#############################
Now creating matched pairs between 0,1,2 administrations for only those in face to face group 
#############################
```{r}
#### SHould be 2634 pairs from baseline to first admin
dat_base = subset(phq9_diag_demo_complete, time == 0)
dat_1 = subset(phq9_diag_demo_complete, time == 1)
dat_base_1 = merge(dat_base, dat_1, by = "SourceClient_ID")
dim(dat_base_1)



### Now filter out those with face_to_face with .x dates 2020-04-01 or greater
#### Then filter out those with telehealth with .y dates before 2020-04-01
dat_base_1$drop = ifelse(dat_base_1$PHQ9_Date.x < "2020-04-01" & dat_base_1$PHQ9_Date.y > "2020-04-01", 1, 0) 
### Visually confirmed it worked
test = subset(dat_base_1, drop == 1)
test[c("PHQ9_Date.x", "PHQ9_Date.y")]

### Include those with all admins before or after telehealth
dat_base_1 = subset(dat_base_1, drop == 0)
## Visually confirm it worked
dat_base_1[c("PHQ9_Date.x", "PHQ9_Date.y")]
# Get rid of drop variable
dat_base_1$drop = NULL
#### Now merge with only those who have a second administration then remove those folks with overlapping third as well
dat_base_2 = subset(phq9_diag_demo_complete, time == 2)
dim(dat_base_2)
dat_base_1_2 = merge(dat_base_1, dat_base_2, by = "SourceClient_ID", all.y = TRUE)
dim(dat_base_1_2)
### Now if base and second admin are within range drop
dat_base_1_2$drop = ifelse(dat_base_1_2$PHQ9_Date.x < "2020-04-01" & dat_base_1_2$PHQ9_Date > "2020-04-01", 1, 0)
# Visually confirm it worked
test = subset(dat_base_1_2, drop == 1)
test[c("PHQ9_Date.x", "PHQ9_Date")]
### Include those with all admins before or after telehealth  
dat_base_1_2 = subset(dat_base_1_2, drop ==0)
dim(dat_base_1_2)

clean_compare_dat = dat_base_1_2
dim(clean_compare_dat)
clean_compare_dat
clean_compare_dat$drop = NULL
clean_compare_dat
### Make long
library(sjmisc)

```
Explore missing data
```{r}
### Explore missing data
library(naniar)
library(MissMech)
miss_var_summary(clean_compare_dat)
miss_case_table(clean_compare_dat)
pct_miss_case(clean_compare_dat)
clean_compare_dat
#TestMCARNormality(clean_compare_dat[,c(12,13, 15:18)])
### Go ahead and drop missing so we can include gender
phq9_diag_demo_complete = na.omit(clean_compare_dat)
dim(clean_compare_dat)
dim(phq9_diag_demo_complete)
range(phq9_diag_demo_complete$PHQ9_Date)
subset(phq9_diag_demo_complete, PHQ9_Date == "2020-03-01")
```
Make long format
```{r}
clean_compare_dat_long = reshape(phq9_diag_demo_complete, varying = list(c("PHQ9_Date.x", "PHQ9_Date.y", "PHQ9_Date"), c("PHQ9_Total.x", "PHQ9_Total.y", "PHQ9_Total"), c("PHQ9_1.x", "PHQ9_1.y", "PHQ9_1"), c("PHQ9_2.x", "PHQ9_2.y", "PHQ9_2"),c("PHQ9_3.x", "PHQ9_3.y", "PHQ9_3"), c("PHQ9_4.x", "PHQ9_4.y", "PHQ9_4"),  c("PHQ9_5.x", "PHQ9_5.y", "PHQ9_5"), c("PHQ9_6.x", "PHQ9_6.y", "PHQ9_6"), c("PHQ9_7.x", "PHQ9_7.y", "PHQ9_7"), c("PHQ9_8.x", "PHQ9_8.y", "PHQ9_8"), c("PHQ9_9.x", "PHQ9_9.y", "PHQ9_9"), c("MDD.x", "MDD.y", "MDD"), c("ORG_ABBREV.x", "ORG_ABBREV.y", "ORG_ABBREV"), c("gender_minority.x","gender_minority.y", "gender_minority"), c("racial_minority.x", "racial_minority.y", "racial_minority"), c("IL.x", "IL.y", "IL"), c("FL.x", "FL.y", "FL"), c("telehealth.x", "telehealth.y", "telehealth"), c("face_to_face.x", "face_to_face.y", "face_to_face"), c("log_PHQ9_Total.x", "log_PHQ9_Total.y", "log_PHQ9_Total"), c("time.x", "time.y", "time"), c("Form_DESC.x", "Form_DESC.y", "Form_DESC")), times = c(0,1,2), direction = "long")
dim(clean_compare_dat_long)
colnames(clean_compare_dat_long)[20] = "telehealth"
clean_compare_dat_long
```

Participant characteristics
```{r}
### Create base for descriptive
clean_compare_dat_base = subset(clean_compare_dat_long, time == 0)
# N
dim(clean_compare_dat_base)
### Average time between administrations
mean_diff_admin = mean(c(mean(clean_compare_dat$PHQ9_Date.x-clean_compare_dat$PHQ9_Date.y),mean(clean_compare_dat$PHQ9_Date.y-clean_compare_dat$PHQ9_Date)))
mean_diff_admin
dim(clean_compare_dat_base)

fac_des = apply(clean_compare_dat_base[c(14:17, 20,24)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
fac_des
write.csv(fac_des, "fac_des.csv")

```
Get participant characteristics for telehealth group and face to face group
```{r}
clean_compare_dat_base_face_to_face = subset(clean_compare_dat_base, face_to_face.x == 1)
fac_des = apply(clean_compare_dat_base_face_to_face[c(14:17, 20,24)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
fac_des
write.csv(fac_des, "fac_des.csv")

clean_compare_dat_base_telehealth = subset(clean_compare_dat_base, telehealth == 1)
fac_des = apply(clean_compare_dat_base_telehealth[c(14:17, 20,24)], 2, function(x){describe.factor(x)})
fac_des = data.frame(fac_des)
fac_des = t(fac_des)
fac_des
write.csv(fac_des, "fac_des.csv")

```
Psychometrics
```{r}
library(psych)
## Get omegas as each time point
pysch_dat = clean_compare_dat_long[,c(2,5:13)]
time_0 = subset(pysch_dat, time == 0)
time_0$time = NULL
time_1 = subset(pysch_dat, time == 1)
time_1$time = NULL
time_2 = subset(pysch_dat, time == 2)
time_2$time = NULL

summary(omega(time_0))
summary(omega(time_1))
summary(omega(time_2))

```



Try putting together logistic regression to compare them
```{r}
library(rstanarm)
compare_des_model = stan_glm(telehealth ~ MDD.x + FL.x + IL.x + gender_minority.x + racial_minority.x + Form_DESC.x, family = "binomial", data = clean_compare_dat_base, seed= 123)
summary(compare_des_model)
compare_des_model_results =  round(compare_des_model$stan_summary[,c(1,3,4,10)],3)
compare_des_model_results = round(exp(compare_des_model_results),3)
### Creates a percentage instead 1 + % 
#compare_des_model_results= compare_des_model_results - 1 
compare_des_model_results
car::vif(compare_des_model)
```

Get group means in for time points for telehealth
```{r}
telehealth_time_describe = clean_compare_dat_long %>%
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
  labs(title="Figure 2: Mean PHQ-9 total score by PHQ-9 administration \n for complete telemental health and face to face", y = "Mean PHQ-9", x = "Administration")+
  geom_text(aes(label = phq_9_mean), position=position_dodge(width=.8), vjust=-0.20)
plot_means$labels$colour = c("Telemental \n health")
plot_means

```
Try ipw package
1/.10
That seems odd seems like you want to upweight those who are more likely to be in treatment
Use this framwework makes more sense: https://www.r-bloggers.com/when-you-use-inverse-probability-weighting-for-estimation-what-are-the-weights-actually-doing/

This is good too: https://www.r-bloggers.com/when-theres-a-fork-in-the-road-take-it-or-taking-a-look-at-marginal-structural-models/

Need to identify for trimming and also robust regression, because weights could be outliers (maybe look for sds greater than 3)
```{r}

library(tidyr)
library(rstanarm)
telehealth_treat_prob = stan_glm(telehealth ~  MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x + Form_DESC.x, data = clean_compare_dat_long, family = "binomial", seed =  124)
predict_treat_prob = predict(telehealth_treat_prob, type = "response")
ipw_var = 1/predict_treat_prob
### Don't seem to be any outliers in the weights 
range(scale(ipw_var))
### With 400 samples we expect a few samples (6) to be at or above 3  and only 3.9
describe.factor(scale(ipw_var))

clean_compare_dat_long = cbind(clean_compare_dat_long, ipw_var)
#0.10743, 0.10385
test_model_freq = lm(log_PHQ9_Total.x ~ time*face_to_face.x + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x, data = clean_compare_dat_long,  weights = ipw_var)
summary(test_model_freq)

```

Run model comparing clean face to face with clean telehealth
```{r}
library(rstanarm)

my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
stan_linear_log = stan_glm(log_PHQ9_Total.x ~ time*face_to_face.x + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x + Form_DESC.x, data = clean_compare_dat_long, weights = ipw_var, prior = my_prior, seed =  124)
stan_linear_log_sum = round(stan_linear_log$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum = round(exp(stan_linear_log_sum),3)
### Creates a percentage instead 1 + % 
stan_linear_log_sum= stan_linear_log_sum - 1
stan_linear_log_sum
posterior_face =  as.data.frame(stan_linear_log)
### Reverse for percentage change
stan_linear_log = stan_glm(log_PHQ9_Total.x ~ time*telehealth + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x, data = clean_compare_dat_long,  seed = 124,  weights = ipw_var, prior = my_prior)
stan_linear_log_sum = round(stan_linear_log$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum = round(exp(stan_linear_log_sum),3)
### Creates a percentage instead 1 + % 
stan_linear_log_sum= stan_linear_log_sum - 1
stan_linear_log_sum


```
Plot the posterior for face to face 
```{r}
library(ggplot2)
colnames(posterior_face)[10] = "face_to_face_time"
#posterior_face_dat = exp(posterior_face)-1

ggplot(posterior_face, aes(x=face_to_face_time))+
  geom_histogram()+
  geom_vline(aes(xintercept=-.1),
             linetype="dashed")+
   geom_vline(aes(xintercept=-0.027),
             linetype="solid")+
  geom_vline(aes(xintercept=0.219),
             linetype="solid")+
  labs(y="Count of parameter estimates", x = "Parameter estimate value (i.e., percentage change)", title  = "Figure 3: Distribution of parameter estimates for the effect of face to face versus\n telemental health over time")+
  annotate(geom="text", x=.275, y=410, label=paste0("Upper 95% CI", ":", "22%"),
              color="red")+
  annotate(geom="text", x=.025, y=410, label=paste0("Lower 95% CI", ":", "-3%"),
              color="red")+
  annotate(geom="text", x=-.07, y=410, label=paste0("NIM", ":", "-10%"),
              color="red")
```
Plot for presentation
```{r}
library(ggplot2)
colnames(posterior_face)[10] = "face_to_face_time"
#posterior_face_dat = exp(posterior_face)-1

ggplot(posterior_face, aes(x=face_to_face_time))+
  geom_histogram()+
  geom_vline(aes(xintercept=-.1),
             linetype="dashed")+
   geom_vline(aes(xintercept=-0.027),
             linetype="solid")+
  geom_vline(aes(xintercept=0.219),
             linetype="solid")+
  labs(y="Count of parameter estimates", x = "Parameter estimate value (i.e., percentage change)", title  = "Telehealth is on par (i.e., no worse) relative to face to face services")+
  annotate(geom="text", x=.275, y=410, label=paste0("Upper 95% CI", ":", " ", "22%"),
              color="red")+
  annotate(geom="text", x=.025, y=410, label=paste0("Lower 95% CI", ":", "-3%"),
              color="red")+
  annotate(geom="text", x=-.07, y=410, label=paste0("NIM", ":", "-10%"),
              color="red")
```



Test interactions with subgroups
Somehow this werid loop worked: http://biostat.mc.vanderbilt.edu/wiki/Main/ForLoopRegression
```{r}
library(Hmisc)

varnames <- Cs(MDD.x, gender_minority.x, racial_minority.x, IL.x, FL.x, Form_DESC.x)
modelfits <- vector(length(varnames), mode = "list")
names(modelfits) <- varnames
#interactions_test = Cs(MDD.x, gender_minority.x, racial_minority.x, IL.x, FL.x, Form_DESC.x)
stan_linear_log_sum = list()
model_formula = list()
for(i in varnames){
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
model_formula = paste0("log_PHQ9_Total.x ~ MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x + Form_DESC.x + time*face_to_face.x*",i)
modelfits[[i]] = stan_glm(as.formula(model_formula), data = clean_compare_dat_long, weights = ipw_var, prior = my_prior, seed =  124)
stan_linear_log_sum[[i]] = round(modelfits[[i]]$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum[[i]] = round(exp(stan_linear_log_sum[[i]]),3)
### Creates a percentage instead 1 + % 
#stan_linear_log_sum[[i]]= stan_linear_log_sum[[i]] - 1
}
modelfits
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
clean_compare_dat_long_plot = clean_compare_dat_long

clean_compare_dat_long_plot$time = to_factor(clean_compare_dat_long_plot$time)
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
stan_linear_total = stan_glm(PHQ9_Total.x ~ time*telehealth + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x + Form_DESC.x, prior = my_prior, weights = ipw_var, data = clean_compare_dat_long_plot, seed = 123)
stan_linear_total_sum = round(stan_linear_total$stan_summary[,c(1,3,4,10)],4)
stan_linear_total_sum

plot_stan_linear_total= plot_model(stan_linear_total, type = "int", terms = c("time", "telehealth"), legend.title = "telehealth", dot.size = 3)+
  scale_y_continuous(limits = c(7,13))+
  labs(title="Figure 4: Predicted values of PHQ-9 total scores", y = "PHQ-9 total", x = "Adminstration")

plot_stan_linear_total$labels$colour = c("Telemental \n health = 1")
plot_stan_linear_total
```
Plot for presentation
```{r}
library(sjPlot)
library(sjmisc)
clean_compare_dat_long_plot = clean_compare_dat_long

clean_compare_dat_long_plot$time = to_factor(clean_compare_dat_long_plot$time)
my_prior = normal(location = 0, scale = .2, autoscale = FALSE)
stan_linear_total = stan_glm(PHQ9_Total.x ~ time*telehealth + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x + Form_DESC.x, prior = my_prior, weights = ipw_var, data = clean_compare_dat_long_plot, seed = 123)
stan_linear_total_sum = round(stan_linear_total$stan_summary[,c(1,3,4,10)],4)
stan_linear_total_sum

plot_stan_linear_total= plot_model(stan_linear_total, type = "int", terms = c("time", "telehealth"), legend.title = "telehealth", dot.size = 3)+
  scale_y_continuous(limits = c(7,13))+
  labs(title="Telehealth PHQ-9 total scores trending lower than face to face but \n not statistically significantly", y = "PHQ-9 total", x = "Adminstration")

plot_stan_linear_total$labels$colour = c("Telemental \n health = 1")
plot_stan_linear_total
```



Test if excluding zero changes things
```{r}
phq9_diag_demo_complete_no_zero = subset(clean_compare_dat_long, PHQ9_Total.x > 0)
dim(phq9_diag_demo_complete_no_zero)[1] / dim(phq9_diag_demo_complete)[1]
stan_linear_log = stan_glm(log_PHQ9_Total.x ~ time*telehealth + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x, data = phq9_diag_demo_complete_no_zero, seed = 123)
stan_linear_log_sum = round(stan_linear_log$stan_summary[,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum = round(exp(stan_linear_log_sum),3)
### Creates a percentage instead 1 + % 
stan_linear_log_sum= stan_linear_log_sum - 1
stan_linear_log_sum


```


Impute the data and evaluate if there are differences in results
```{r}
impute_long = reshape(clean_compare_dat, varying = list(c("PHQ9_Date.x", "PHQ9_Date.y", "PHQ9_Date"), c("PHQ9_Total.x", "PHQ9_Total.y", "PHQ9_Total"), c("PHQ9_1.x", "PHQ9_1.y", "PHQ9_1"), c("PHQ9_2.x", "PHQ9_2.y", "PHQ9_2"),c("PHQ9_3.x", "PHQ9_3.y", "PHQ9_3"), c("PHQ9_4.x", "PHQ9_4.y", "PHQ9_4"),  c("PHQ9_5.x", "PHQ9_5.y", "PHQ9_5"), c("PHQ9_6.x", "PHQ9_6.y", "PHQ9_6"), c("PHQ9_7.x", "PHQ9_7.y", "PHQ9_7"), c("PHQ9_8.x", "PHQ9_8.y", "PHQ9_8"), c("PHQ9_9.x", "PHQ9_9.y", "PHQ9_9"), c("MDD.x", "MDD.y", "MDD"), c("ORG_ABBREV.x", "ORG_ABBREV.y", "ORG_ABBREV"), c("gender_minority.x","gender_minority.y", "gender_minority"), c("racial_minority.x", "racial_minority.y", "racial_minority"), c("IL.x", "IL.y", "IL"), c("FL.x", "FL.y", "FL"), c("telehealth.x", "telehealth.y", "telehealth"), c("face_to_face.x", "face_to_face.y", "face_to_face"), c("log_PHQ9_Total.x", "log_PHQ9_Total.y", "log_PHQ9_Total"), c("time.x", "time.y", "time"), c("Form_DESC.x", "Form_DESC.y", "Form_DESC")), times = c(0,1,2), direction = "long")
impute_long$Form_DESC.x = ifelse(impute_long$Form_DESC.x == "PHQ9 A", 1, 0)
dim(impute_long)
colnames(impute_long)[20] = "telehealth"
impute_long = impute_long[,c("time", "MDD.x", "gender_minority.x", "racial_minority.x", "IL.x", "FL.x", "face_to_face.x", "telehealth", "Form_DESC.x", "log_PHQ9_Total.x")]
library(Amelia)

#a_out_ehr = amelia(x = impute_long, m = 20, noms = c("time", "MDD.x", "gender_minority.x", "racial_minority.x", "IL.x", "FL.x", "Form_DESC.x"), idvars = c("face_to_face.x", "telehealth"))
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/MEE_Data")
#saveRDS(a_out_ehr, file = "a_out_ehr.rds")
a_out_ehr = readRDS(file = "a_out_ehr.rds")
compare.density(a_out_ehr, var = "log_PHQ9_Total.x")

impute_dat_loop = a_out_ehr$imputations
impute_dat_loop[[1]]
```
Create the weights for each imputed data set
```{r}
library(tidyr)
telehealth_treat_prob = list()
predict_treat_prob = list()
impute_dat_loop_ipw = list()
ipw_var = list()
scale_out = list()
for(i in 1:length(impute_dat_loop)){
telehealth_treat_prob[[i]] = glm(telehealth ~  MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x + Form_DESC.x, data = impute_dat_loop[[i]], family = "binomial")
predict_treat_prob[[i]] = predict(telehealth_treat_prob[[i]], type = "response")
ipw_var[[i]] = 1/predict_treat_prob[[i]]
impute_dat_loop_ipw[[i]] = cbind(impute_dat_loop[[i]], ipw_var = ipw_var[[i]])
scale_out[[i]]  = scale(ipw_var[[i]])
}
hist(unlist(scale_out))
```


Run main model
Probably just average the CIs not really sure what else to do: https://stats.stackexchange.com/questions/33596/how-can-i-pool-posterior-means-and-credible-intervals-after-multiple-imputation
```{r}
stan_linear_log = list()
stan_linear_log_sum = list()

my_prior = normal(location = 0, scale = .2, autoscale = FALSE)

for(i in 1:length(impute_dat_loop_ipw)){
stan_linear_log[[i]] = stan_glm(log_PHQ9_Total.x ~ time*face_to_face.x + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x + Form_DESC.x, data = impute_dat_loop_ipw[[i]], weights = ipw_var, prior = my_prior, seed =  124)
# Grab just face to face so row 9
stan_linear_log_sum[[i]] = round(stan_linear_log[[i]]$stan_summary[10,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum[[i]] = round(exp(stan_linear_log_sum[[i]]),3)
### Creates a percentage instead 1 + % 
#stan_linear_log_sum= stan_linear_log_sum[[i]] - 1
}

unlist_results = unlist(stan_linear_log_sum)
matrix_results = matrix(unlist_results, ncol = 4, byrow = TRUE)
colnames(matrix_results) = c("mean", "sd", "lower", "upper")
matrix_results = data.frame(matrix_results)
matrix_results = apply(matrix_results, 2, mean)
matrix_results
```



###########################
Extra
###########################

Random effects model
```{r}
bayes_linear_results = stan_lmer(log_PHQ9_Total.x ~ time*telehealth + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x  + (time | SourceClient_ID), data = clean_compare_dat_long, iter = 5000)
summary(bayes_linear_results)
bayes_linear_results_sum = round(bayes_linear_results$stan_summary[1:9,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum = round(exp(stan_linear_log_sum),3)
### Creates a percentage instead 1 + % 
stan_linear_log_sum= stan_linear_log_sum - 1
stan_linear_log_sum
test =  round(bayes_linear_results$stan_summary[1:11,c(1,3,4,10)],4)
test = data.frame(test)
write.csv(test, "test.csv")



bayes_linear_results = stan_lmer(log_PHQ9_Total.x ~ time*telehealth + MDD.x +gender_minority.x +racial_minority.x + IL.x + FL.x  + (1 | SourceClient_ID), data = clean_compare_dat_long, iter = 5000)
summary(bayes_linear_results)
bayes_linear_results_sum = round(bayes_linear_results$stan_summary[1:9,c(1,3,4,10)],4)
## To get percentage change interpretation need to exp the parameter estimates
stan_linear_log_sum = round(exp(stan_linear_log_sum),3)
### Creates a percentage instead 1 + % 
stan_linear_log_sum= stan_linear_log_sum - 1
stan_linear_log_sum

```

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

