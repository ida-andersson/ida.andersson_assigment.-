###########################################################
#Assignment 1 
###########################################################

data1 = read.csv("https://tinyurl.com/ha-dataset1")
library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(tidyverse) # for tidy code	
library(lm.beta) # for st. B

summary(data1)	#explore
describe(data1)	#explore
str(data1) #explore
View(data1) #explore

mod_1 <- lm(pain ~ age + sex, data = data1) #model_1
mod_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data1) # model2

# CHECKING FOR CODING MISTAKES AND OUTLIERS 

data1 %>%
  ggplot() +
  aes(x = pain) +
  geom_histogram(bins = 50) #OK

data1 %>% 
  ggplot() +
  aes(x = age) +
  geom_bar() 
# found coding mistake - ID93 age "444", probably 44 but can't be sure? - delete 
data1 <- data1[-c(93), ] # delete ID93 i.e. participant 93 

data2 %>% 
  ggplot() +
  aes(x = age) +
  geom_bar() 

data1 %>% 
  ggplot() +
  aes(x = sex) +
  geom_bar() # OK

data1 %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram(bins = 50)
#### Code mistake ID 150 - unvalid value 3.9 (scale starting at 20) - changing to 39. 
data1cor <- data1 %>%
  mutate(STAI_trait =replace(STAI_trait,  STAI_trait =="3.9", 39.0))
summary(data1cor$STAI_trait) #OK

mod_1 <- lm(pain ~ age + sex, data = data1cor) #change to data1cor 
mod_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data1cor)

data1 %>%
  ggplot() +
  aes(x = pain_cat) +
  geom_histogram(bins = 50) # OK

data1 %>%
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 50) 
describe(data1$mindfulness) # OK 

data1 %>%
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram(bins = 50) # OK 

data1 %>%
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram(bins = 50) # OK 


# Outliers 

mod_1 %>%
  plot(which = 4) # Cooks distance
data1cor %>%
  slice(c(99, 127, 140)) # looks OK, no values > 1, no found coding mistakes - keeping all participants. 

mod_2 %>%
  plot(which = 4) # Cooks distance 
data2cor %>%
  slice(c(68, 99, 113)) # no values > 1, no found coding mistakes - keeping all participants.

# ASSUMPTIONS FOR REGRESSION (4) 

# NORMALITY

# mod_1
mod_1 %>%
  plot(which = 2) # Q-Qplot OK 

residuals_mod1 = enframe(residuals(mod_1)) #histogram mod 1 - normal
residuals_mod1 %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(mod_1)) 
# no problems with skewness (0.18) or kurtosis (-0.02)

#mod_2
mod_2 %>%
  plot(which = 2) #Q-Qplot OK 

residuals_mod2 = enframe(residuals(mod_2)) #histogram mod_2 - normal 
residuals_mod2 %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(mod_2)) 
# no problems with skewness (-0.13) or kurtosis (-0.4). 

# LINEARITY 

mod_1 %>%
  residualPlots() #OK indicating no violation of linearity 

mod_2 %>%
  residualPlots() # OK indicating no violation of linearity, no significance 


# HOMOSCEDASTICITY

mod_1 %>% # OK  (evenly distibuted is good)  
  plot(which = 3)

mod_1 %>%
  ncvTest() #OK

mod_2 %>% # residual plot
  plot(which = 3) # OK
mod_2 %>%
  ncvTest() #OK

# NO MULTICOLLIENARITY

mod_1 %>% # OK
  vif()

mod_2 %>%
  vif() #PROBLEM 

data1cor %>%
  select(pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_saliva, cortisol_serum) %>% 
  pairs.panels(col = "green", lm = T)
# too high correlation between cortisol measures, as expected (0.89). 
data2cor %>%
  select(cortisol_saliva, cortisol_serum) %>%
  cor() #solution - delete one of them from the model. which one? explain reasoning and theory. 
summary(mod_2)

# decision - deleting cortisol_serum 
mod_2wo.serum <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data1cor)

#rechecking assumptions for mod_2wo.serum


mod_2wo.serum %>%
  plot(which = 2) #Q-Qplot

residuals_mod_2wo.serum = enframe(residuals(mod_2wo.serum)) #histogram mod_2 - normal 
residuals_mod_2wo.serum %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(mod_2wo.serum)) 

mod_2wo.serum %>%
  residualPlots() 

mod_2wo.serum %>% # residual plot
  plot(which = 3) # OK
mod_2wo.serum %>%
  ncvTest()

mod_2wo.serum %>%
  vif() 

# mod_2wo.serum OK. 
###### assumption check OK  

sm1 <- summary(mod_1)
sm1

sm2 <- summary(mod_2wo.serum)
sm2

#comparing models
summary(mod_1)$adj.r.squared # [1] 0.05277876
summary(mod_2wo.serum)$adj.r.squared # [1] 0.4758178

AIC(mod_1) # [1] 588.6782
AIC(mod_2wo.serum) # [1] 498.4688

anova(mod_1, mod_2wo.serum) #  


confint(mod_1) # confidence intervals for table 
lm.beta(mod_1) #st. B 

confint(mod_2wo.serum)
lm.beta(mod_2wo.serum)

###########################################################
#Assignment 2 
###########################################################

data1 = read.csv("https://tinyurl.com/ha-dataset1")

library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) 
library(tidyverse) # 
library(lm.boot) # 

summary(data1)	#explore
describe(data1)	#explore
str(data1) #explore
View(data1) #explore

# earlier found coding mistake - ID93 age "444", (probably 44 but can't be sure)
data2 <- data1[-c(93), ] # delete ID93 i.e. participant 93  

data2cor <- data2 %>% # fixing earlier coding mistake 
  mutate(STAI_trait =replace(STAI_trait,  STAI_trait =="3.9", 39.0))
describe(data2cor$STAI_trait) # min. now 23, OK 

my_mod <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data2cor)
in_mod <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data2cor)

### EXPLORE NEW VARIABLES 

data2cor %>%
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram(bins = 50) # OK 

data2cor %>%
  ggplot() +
  aes(x = IQ) +
  geom_dotplot() # two that could be outliers, look into 
describe(data1$IQ) # OK 

data2cor %>%
  ggplot() +
  aes(x = weight) +
  geom_dotplot() # one outlier, though 105 should be considered normal 
describe(data2cor$weight)

data2cor %>%
  ggplot() +
  aes(x = household_income) +
  geom_dotplot() # - 3732 ID 109. Exclude due to minus income 
data2cor2 <- data2cor[-c(109), ] 

# change to drw from data data2cor2
my_mod <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data2cor2)
in_mod <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data2cor2)

# Outliers 

my_mod %>%
  plot(which = 4) # Cooks distance
data2cor2 %>%
  slice(c(53, 99, 113)) # no values > 1, no found coding mistakes - keeping all participants. 

in_mod %>%
  plot(which = 4) # Cooks distance 
data2cor2 %>%
  slice(c(3, 102, 113)) # no values > 1, no found coding mistakes - keeping all participants.

# ASSUMPTIONS FOR REGRESSION (4) 

# NORMALITY

# my model
my_mod%>%
  plot(which = 2) # Q-Qplot

residuals_my_mod = enframe(residuals(my_mod)) #histogram my_mod - normal
residuals_my_mod %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(my_mod)) 
# no problems with skewness or kurtosis 

#initial model
in_mod %>%
  plot(which = 2) #Q-Qplot

residuals_in_mod = enframe(residuals(in_mod)) #histogram mod_2 - normal 
residuals_in_mod %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(in_mod)) 
# no problems with skewness or kurtosis 

# LINEARITY 

my_mod %>%
  residualPlots()
# #No significance, indicating no violation of linearity 

in_mod %>%
  residualPlots() # OK  
#No significance, indicating no violation of linearity 

# HOMOSCEDASTICITY

my_mod %>% # residual plot expectation - evenly distibuted. 
  plot(which = 3)
#problem if : funnel shape i.e. small on one side, larger on the other - no problem 
in_mod %>%
  ncvTest()

my_mod %>% # residual plot
  plot(which = 3)
in_mod %>%
  ncvTest()

# NO MULTICOLLIENARITY

my_mod %>% # OK, 2.48 highest value 
  vif()

in_mod %>%
  vif() # OK 2.30 highest value 

################ end of assumptions 

sm_my<- summary(my_mod)
sm_my

sm_in <- summary(in_mod)
sm_in

summary(my_mod)$adj.r.squared # 
summary(in_mod)$adj.r.squared # 

AIC(my_mod) # 
AIC(in_mod) # 
AIC(back_mod) # 
anova(my_mod, in_mod) # 

######### 
back_mod =step(in_mod, direction = "backward") #creating the backwards model  

back_mod <- lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data2cor2)

sm_backw <- summary(back_mod)
sm_backw

summary(back_mod)$adj.r.squared # 
lb.beta(back_mod)

confint(back_mod) #confidence interval 
lm.beta(back_mod) #standardized b 


AIC(my_mod) 
AIC(back_mod) 
AIC(in_mod)

anova(my_mod, back_mod) 

######################### testing on new data 

data_ny = read.csv("https://tinyurl.com/ha-dataset2")
View(data_ny)


### Checking the variables  

is.na(data_ny)

data_ny %>%
  ggplot() +
  aes(x = pain) +
  geom_histogram(bins = 50) #normally distributed 

data_ny %>% 
  ggplot() +
  aes(x = age) +
  geom_bar() # OK 

data_ny  %>% 
  ggplot() +
  aes(x = sex) +
  geom_bar() # OK

data_ny  %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram(bins = 50) #OK

data_ny  %>%
  ggplot() +
  aes(x = pain_cat) +
  geom_histogram(bins = 50) # OK
describe(data_ny$pain_cat) 

data_ny  %>%
  ggplot() +
  aes(x = mindfulness) 
geom_histogram(bins = 50) # one outlier, unvalid score > 6. 
data_ny1 <- data_ny[-c(8), ] # delete ID8 i.e. participant 8 
describe(data_ny1$mindfulness) 

data_ny %>%
  ggplot() +
  aes(x = cortisol_serum) +
  geom_dotplot(bins = 20) # OK 

data_ny %>%
  ggplot() +
  aes(x = IQ) +
  geom_dotplot() 
describe(data1$IQ) # OK 

data_ny %>%
  ggplot() +
  aes(x = weight) +
  geom_dotplot() # ok 

data_ny %>%
  ggplot() +
  aes(x = household_income) +
  geom_dotplot() # OK 

####### Data OK 
### Use data_ny1 

# calculate predicted values
pred_test_my <-predict(my_mod, data_ny1)
pred_test_back <-predict(back_mod, data_ny1)

# sum of squared residuals

RSS_test_my =sum((data_ny1[, "pain"] - pred_test_my)^2)
RSS_test_back =sum((data_ny1[, "pain"] - pred_test_back)^2)
RSS_test_my
RSS_test_back

######################################################################
#Assignment 3 
######################################################################

data3 = read.csv("https://tinyurl.com/ha-dataset3")
View(data3)

is.na(data3) # checking for missing values 

library(tidyverse)	#tidy code, ggplot
library(gridExtra)	
library(psych)# for describe
library(cAIC4)# for cAIC
library(r2glmm)# for r2beta
library(lme4)# for lmer
library(lmerTest)# to get singificance test in lmer
library(MuMIn)# for r.squaredGLMM
library(optimx)# for optimx optimizer
library(influence.ME) # to check clustering 
library(VIM) # for missing data 

### Explore the data, look for irregularities 
data3 %>%
  ggplot() +
  aes(x = pain) +
  geom_histogram(bins = 50) #normally distributed 

data3 %>% 
  ggplot() +
  aes(x = age) +
  geom_bar() "ok"

data3 %>% 
  ggplot() +
  aes(x = sex) +
  geom_bar() 



data3new = data3 %>% 	# chaning femlae to female 
  mutate(sex = recode(sex,	
                      "female" = 1,	
                      "male" = 2,	
                      "femlae" = 3	
                      
  ))	
data3f = data3new %>% 
  mutate(sex =replace(sex, sex =="3", 1))

data3f %>% 
  ggplot() +
  aes(x = sex) +
  geom_bar() 

data3 %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram(bins = 50)
describe(data3$STAI_trait) # OK 

data3 %>%
  ggplot() +
  aes(x = pain_cat) +
  geom_histogram(bins = 50) # OK

data3 %>%
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 50) 
describe(data3$mindfulness) # OK 

data3 %>%
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram(bins = 50) # OK 

data3 %>%
  ggplot() +
  aes(x = IQ) +
  geom_dotplot() # OK 
describe(data1$IQ)

View(data3$IQ)

data3 %>%
  ggplot() +
  aes(x = weight) +
  geom_dotplot() # ok  

data3f %>%
  ggplot() +
  aes(x = household_income) +
  geom_dotplot() # - 6994 ID 77. Exclude due to minus income 
data3f <- data3f[-c(77), ] ###  

dev.off()


data3f = data3 %>% 	
  mutate(hospital = recode(hospital,	
                           "hospital_1" = 1,	
                           "hospital_2" = 2,	
                           "hospital_3" = 3,	
                           "hospital_4" = 4,	
                           "hospital_5" = 5,	
                           "hospital_6" = 6,	
                           "hospital_7" = 7,	
                           "hospital_8" = 8,	
                           "hospital_9" = 9,	
                           "hospital_10" = 10	
  ))	
View(data3f) #OK

data3f%>% # clustering 
  ggplot()+
  aes(y = pain, x = hospital)+
  geom_point(aes(color = hospital), size = 6)+
  geom_smooth(method = "lm", se = F)

data3f = data3 %>% 	
  mutate(hospital = factor(hospital))	## Changing hospital varaible to factor 




###################
create mixed linear model (RI_mod)
################ ## nb. use "data3f"
# do random intercept model, RI. 
old_mod <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data3f)
RI_mod = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva +(1|hospital) , data = data3f)  

summary(RI_mod) # 

qqmath(RI_mod, id=0.05) # Normality OK 
qqmath(ranef(RI_mod))

plot(RI_mod, arg = "pearson") 
#Linearity and homoscedasticity OK 

summary(RI_mod)
r.squaredGLMM(RI_mod)

sum(residuals(RI_mod)^2) #prediction error [1] 247.7285
sum(residuals(old_mod)^2) #[1] 319.1795

cAIC(RI_mod)$caic #[1] 644.7155
AIC(old_mod) # AIC #[1] 679.0627

anova(RI_mod, old_mod) #anova, sign. different 7.72e-07 ***

confint(or.squaredGLMM(RI_mod)ld_mod) #confidence intervals 
confint(RI_mod)


stdCoef.merMod <- function(object) {	# to get St. beta 
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

stdCoef.merMod(RI_mod)


# marginal and conditional R squared values

r2beta(RI_mod, method = "nsj", data = data3f) #marginal r2 with conf.int  

r.squaredGLMM(RI_mod)


########### TESTING ON NEW DATA ##############

data4 = read.csv("https://tinyurl.com/ha-dataset4")
View(data4)              

# checking the variables 

data4 %>%
  ggplot() +
  aes(x = pain) +
  geom_histogram(bins = 50) #normally distributed 

data4 %>% 
  ggplot() +
  aes(x = age) +
  geom_bar() 

data4 %>% 
  ggplot() +
  aes(x = sex) +
  geom_bar() 


data4 %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram(bins = 50)
describe(data4$STAI_trait) # OK 


data4 %>%
  ggplot() +
  aes(x = pain_cat) +
  geom_histogram(bins = 50) # OK

data4 %>%
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 50) 
describe(data4$mindfulness) # OK 

data4 %>%
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram(bins = 50) # OK 

data4 %>%
  ggplot() +
  aes(x = IQ) +
  geom_dotplot() # OK 
describe(data4$IQ)


data4 %>%
  ggplot() +
  aes(x = weight) +
  geom_dotplot() # ok  

data4 %>%
  ggplot() +
  aes(x = household_income) +
  geom_dotplot() # - 6994 ID 77. Exclude due to minus income 
data4cor <- data4[-c(77), ] 


data4 %>%
  ggplot() +
  aes(x = hospital, y = pain) +
  geom_dotplot() 

data4 <- data4[-c(5), ]  #negativ income -23482, deleted

pred_data4 <-predict(RI_mod, data4cor, allow.new.levels = TRUE)
pred_data4

RSS_test =sum((data4cor[, "pain"] - pred_data4)^2)
RSS_test # [1] 310.5703 

mod_tss = lm(pain ~ 1, data = data4cor) 
mod_tss
TSS =sum((data4cor$pain- predict(mod_tss))^2)
TSS # [1] 435.2965 

# variance explained by the model 
1-310.57/435.29 # 0.286 - by using the regression model, could explain 29% of the variance. 


######### only one predictor model  on data 3 

mod_cortisol =lmer(pain ~ cortisol_saliva + (cortisol_saliva|hospital), data = data3f) 
#failed to converge 
mod_cortisol2 = lmer(pain ~ cortisol_saliva + (cortisol_saliva|hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = data3f)	

summary(data3f$cortisol_saliva)
summary(mod_cortisol2)

sum(residuals(mod_cortisol2)^2)
r2beta(mod_cortisol2, method = "nsj", data = data3f) #marginal and conditional r-square 
r.squaredGLMM(mod_cortisol2)

confint(or.squaredGLMM(mod_cortisol2)ld_mod) #confidence intervals 
confint(mod_cortisol2)

anova(mod_cortisol2, RI_mod)

#only cortisol as predictor-plot 
cort_plot = data3f %>%
  ggplot()+
  aes(y = pain, x = cortisol_saliva, color = hospital)+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = F, fullrange=TRUE)+
  xlim(0, 8)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)
cort_plot



