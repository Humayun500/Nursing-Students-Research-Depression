#R codes for Nursing students Depression Study

setwd("C:/Users/humay/Dropbox/Personal Drive/Own project/Nursing students/Depression")

Nsg_St <- read.csv("C:/Users/humay/Dropbox/Personal Drive/Own project/Nursing students/Main data/Excel/Managed Data Nursing Students.csv")


save.image("C:/Users/humay/Dropbox/Personal Drive/Own project/Nursing students/Main data/R/Nsg_St.RData")


#this study use its' own data

#### Data exploration##########
library (tidyverse)

glimpse (Nsg_St)

options (scipen=999)

##### Depression Among the Nursing Students########

Nsg_St$Age
attach (Nsg_St)

shapiro.test (Age)


#Demoghraphic information
Nsg_St$ Age 
glimpse (Nsg_St)
Nsg_St<-Nsg_St[!(Nsg_St$Age=="60"),]
Nsg_St<-Nsg_St[!(Nsg_St$Age=="33"),]

summary (Nsg_St$Age)
sd (Nsg_St$Age)
length (Nsg_St$Age)


Nsg_St %>% 
  ggplot (aes(Age))+
  geom_histogram ()

Nsg_St$Sex
class (Nsg_St$Sex.fct)
Nsg_St$Sex.fct = as.factor(Nsg_St$Sex)
table (Nsg_St$Sex.fct)

class 

levels (Nsg_St$Sex.fct)

Nsg_St$Sex.fct_new= recode (Nsg_St$Sex.fct, 
                            "Male"="Male", 
                            " Female"= "Female")
                                         


Nsg_St$Father_education.fct= as.factor(Nsg_St$ Father_education)
Nsg_St %>% 
  select (Father_education.fct) %>% 
  table ()

Nsg_St$Father_education.fct_new= recode (Nsg_St$Father_education.fct,
                                  "Graduated"="Graduated",
                                  "HSC passed"= "Non-graduate",
                                  "SSC passed"="Non-graduate",
                                  "Up to SSC"="Non-graduate")
table (Nsg_St$Father_education.fct_new)
remove (Father_education.fct_new)

Nsg_St$Mother_education.fct = as.factor(Nsg_St$ Mother_education )
summary (Nsg_St$Mother_education.fct)
Nsg_St %>% 
  select (Mother_education.fct) %>% 
  table ()


Nsg_St$Mother_education.fct_new= recode (Mother_education.fct_new,
                                  "Graduated"="Graduated",
                                  "Non-graduate"= "Ngraduate"
                                  )

table (Nsg_St$Mother_education.fct_new)
remove (Mother_education.fct_new)

Nsg_St$Family_income.fct= as.factor (Nsg_St$ Family_income)
summary (Nsg_St$Family_income.fct)
class (Nsg_St$Family_income.fct)
table (Nsg_St$Family_income.fct)
Nsg_St %>% 
  select (Family_income.fct) %>% 
  table ()

Nsg_St$Family_income.fct_new = recode (Nsg_St$Family_income.fct, 
                                "<15000 BDT"="<15000 BDT",
                                "15000 BDT -20000 BDT"="15000 BDT -20000 BDT",
                                "21000 BDT - 25000 BDT"=">20000 BDT",
                                "26000 BDT - 30000 BDT"=">20000 BDT",
                                "31000 BDT - 35000 BDT"=">20000 BDT",
                                "36000 BDT - 40000 BDT"=">20000 BDT",
                                "> 40,000 BDT"=">20000 BDT")
table (Nsg_St$Family_income.fct_new)


Nsg_St$ Division
Nsg_St$ Division.fct = as.factor (Nsg_St$ Division)
class (Nsg_St$ Division.fct)
Nsg_St %>% 
  select (Division.fct) %>% 
  table ()

Nsg_St$Division.fct_new = recode (Nsg_St$Division.fct, 
                                "Dhaka"="Dhaka",
                                "Sylhet"="Sylhet",
                                "Barishal"="others",
                                "Chattagram"="others",
                                "Khulna"="others",
                                "Mymensingh"="others",
                                "Rajshahi"="others",
                           "Rangpur"="others")
table (Nsg_St$Division.fct_new)
remove (Division.fct_new)


Nsg_St$ Current_residence
Nsg_St$ Current_residence.fct = as.factor (Nsg_St$ Current_residence)
class (Nsg_St$ Current_residence.fct)
table (Nsg_St$Current_residence)
Nsg_St %>% 
  select (Current_residence.fct) %>% 
  table ()

Nsg_St$Current_residence.fct_new = recode (Nsg_St$Current_residence.fct, 
                           "District city"="District city",
                           "Division city"="Division city",
                           "Upazila"="Village",
                           "Village"="Village")
table (Nsg_St$Current_residence.fct_new)

#Academic information
Nsg_St$ Course_type 
class (Nsg_St$ Course_type.fct)
Nsg_St$ Course_type.fct = as.factor(Nsg_St$ Course_type)
Nsg_St %>% 
  select (Course_type.fct_new) %>% 
  table ()
Nsg_St$Course_type.fct_new= as.factor (recode (Nsg_St$Course_type.fct, 
                                         "B.Sc in Nursing" ="B.Sc", 
                                         "Diploma in Nursing Science and Midwifery"="Diploma", 
                                         "M.Sc in Nursing"="B.Sc", 
                                         "Post-Basic B.Sc in Nursing"="B.Sc" 
                                       ))

#Nsg_St$Course_type.fct_new for analysis

Nsg_St$ Academic_year
Nsg_St$ Academic_year.fct= as.factor(Nsg_St$ Academic_year)
Nsg_St %>% 
  select (Academic_year.fct) %>% 
  table ()

Nsg_St$Institution_type 
Nsg_St$Institution_type.fct =as.factor (Nsg_St$Institution_type )
Nsg_St %>% 
  select (Institution_type.fct) %>% 
  table ()

Nsg_St$Have_qualified_teacher
Nsg_St$Have_qualified_teacher.fct=as.factor (Nsg_St$Have_qualified_teacher)
Nsg_St %>% 
  select (Have_qualified_teacher.fct) %>% 
  table ()

Nsg_St$Have_subject_specific_teacher
Nsg_St$Have_subject_specific_teacher.fct=as.factor (Nsg_St$Have_subject_specific_teacher)
Nsg_St %>% 
  select (Have_subject_specific_teacher.fct) %>% 
  table ()

Nsg_St$Have_qualified_clinical.teacher
Nsg_St$Have_qualified_clinical.teacher.fct=as.factor (Have_qualified_clinical.teacher)
Nsg_St %>% 
  select (Have_qualified_clinical.teacher.fct) %>% 
  table ()

Nsg_St$Have_qualified_language_teacher
Nsg_St$Have_qualified_language_teacher.fct=as.factor (Nsg_St$Have_qualified_language_teacher)
Nsg_St %>% 
  select (Have_qualified_language_teacher.fct) %>% 
  table ()

Nsg_St$Nursing_choice
Nsg_St$Nursing_choice.fct=as.factor (Nsg_St$Nursing_choice)
Nsg_St %>% 
  select (Nursing_choice.fct) %>% 
  table ()

############## outcome variable ##############

Nsg_St$PHQ_Score= Nsg_St$PHQ_1+Nsg_St$PHQ_2+Nsg_St$PHQ_3+Nsg_St$PHQ_4+Nsg_St$PHQ_5+Nsg_St$PHQ_6+Nsg_St$PHQ_7+Nsg_St$PHQ_8+Nsg_St$PHQ_9
summary (Nsg_St$PHQ_Score)
Nsg_St$PHQ_Score
sum (is.na (Nsg_St$PHQ_Score))

library(psych)
library(GPArotation)

DF_PHQ= data.frame(Nsg_St$PHQ_1, Nsg_St$PHQ_2, Nsg_St$PHQ_3, Nsg_St$PHQ_4, Nsg_St$PHQ_5, Nsg_St$PHQ_6, Nsg_St$PHQ_7, Nsg_St$PHQ_8, Nsg_St$PHQ_9)
omega(DF_PHQ)

Nsg_St$PHQ_cat= as.factor (cut (Nsg_St$PHQ_Score,
                              breaks = c(-1,4,9,14,19,27),
                              lables= c("Normal", 
                                          "Mild",
                                          "Moderate", 
                                          "Severe", 
                                          "Extremely severe" 
                                )))


levels (Nsg_St$PHQ_cat)
sum (is.na (Nsg_St$PHQ_cat))

library(freqtables)

Nsg_St %>% 
  freq_table(Nsg_St$PHQ_cat)

Nsg_St$PHQ_cat.lbs= recode (Nsg_St$PHQ_cat,
                            "(-1,4]" = "Normal",
                            "(4,9]"="Mild",
                            "(9,14]" ="Moderate", 
                            "(14,19]" ="Severe", 
                            "(19,27]" ="Extremely severe" 
                            )

Nsg_St$PHQ_cat.lbs
Nsg_St$PHQ_cat.lbs= as.factor (Nsg_St$PHQ_cat.lbs)
Nsg_St %>% 
  freq_table(Nsg_St$PHQ_cat.lbs)

##Pie
pie(Nsg_St$PHQ_cat.lbs)

PHQ_cat.table <- table(Nsg_St$PHQ_cat.lbs)

pie(PHQ_cat.table,
    col = hcl.colors(length(PHQ_cat.table), "BluYl"))

# Pie with percentage
library(ggplot2)

ggplot(Nsg_St, aes(x = "", y = Nsg_St$PHQ_cat.lbs, fill = Nsg_St$PHQ_cat.lbs)) +
  geom_col() +
  coord_polar(theta = "y")


library(ggplot2)
# install.packages("dplyr")
# install.packages("scales")
library(dplyr)

PHQ_cat.df <- Nsg_St %>% 
  group_by(PHQ_cat.lbs) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

#Final
ggplot(PHQ_cat.df, aes(x = "", y = perc, fill = PHQ_cat.lbs)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE,cex = 3, alpha= 0.01, color = "black"
           ) +
  guides(fill = guide_legend(title = "Depression", face = "bold")) +
  scale_fill_viridis_d() +
  coord_polar(theta = "y") + 
  theme_void()
 


Nsg_St %>% 
  freq_table(Nsg_St$PHQ_cat.lbs)


view ( Nsg_St$PHQ_Score)

sum (is.na (Nsg_St$PHQ_Score))

############ Descriptive analsis ################

summary (Nsg_St$Age)
lenght (Nsg_St$Age)


library(Rmisc)
summary (Nsg_St$PHQ_Score)
sd (Nsg_St$PHQ_Score)
CI(Nsg_St$PHQ_Score, ci=0.95)

summary (Nsg_St$WEMWBS_Score)
sd (Nsg_St$WEMWBS_Score)
CI(Nsg_St$WEMWBS_Score, ci=0.95)

DF_WEMWBS= data.frame(Nsg_St$WEMWBS_1, Nsg_St$WEMWBS_2, Nsg_St$WEMWBS_3, Nsg_St$WEMWBS_4, Nsg_St$WEMWBS_5, Nsg_St$WEMWBS_6, Nsg_St$WEMWBS_7, Nsg_St$WEMWBS_8, Nsg_St$WEMWBS_9, Nsg_St$WEMWBS_10, Nsg_St$WEMWBS_11, Nsg_St$WEMWBS_12, Nsg_St$WEMWBS_13, Nsg_St$WEMWBS_14)
omega(DF_WEMWBS)

typeof (Nsg_St$PHQ_Score)



Nsg_St %>% 
  count(Sex.fct) %>% 
  mutate(prop = (n / sum(n))*100) 

Nsg_St %>% 
  count(Father_education.fct_new) %>% 
  mutate(prop = (n / sum(n))*100) 

Nsg_St %>% 
  count(Mother_education.fct_new) %>% 
  mutate(prop = (n / sum(n))*100) 

Nsg_St %>% 
  count(Family_income.fct_new) %>% 
  mutate(prop = (n / sum(n))*100)  

Nsg_St %>% 
  count(Division.fct_new) %>% 
  mutate(prop = (n / sum(n))*100) 


Nsg_St %>% 
  count(Current_residence.fct_new) %>% 
  mutate(prop = (n / sum(n))*100) 

Nsg_St %>% 
  count(Course_type.fct_new) %>% 
  mutate(prop = (n / sum(n))*100) 

Nsg_St %>% 
  count(Institution_type.fct) %>% 
  mutate(prop = (n / sum(n))*100)

Nsg_St %>% 
  count(Nursing_choice.fct) %>% 
  mutate(prop = (n / sum(n))*100)

Nsg_St %>% 
  count(Academic_year.fct) %>% 
  mutate(prop = (n / sum(n))*100)

Nsg_St %>% 
  count(Have_subject_specific_teacher.fct) %>% 
  mutate(prop = (n / sum(n))*100)

Nsg_St %>% 
  count(Have_qualified_teacher.fct) %>% 
  mutate(prop = (n / sum(n))*100)

Nsg_St %>% 
  count(Have_qualified_language_teacher.fct) %>% 
  mutate(prop = (n / sum(n))*100)

############# un-adjusted analysis by t-test #############

glimpse (Nsg_St)

t.test (Nsg_St$PHQ_Score~Nsg_St$Sex.fct_new) #p-value = 0.05921
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Sex.fct_new), FUN=sd)

t.test (Nsg_St$PHQ_Score~Nsg_St$Father_education.fct_new) #no-sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Father_education.fct_new), FUN=sd)

t.test (Nsg_St$PHQ_Score~Nsg_St$Mother_education.fct_new) #no-sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Mother_education.fct_new), FUN=sd)


t.test (Nsg_St$PHQ_Score~ Nsg_St$Course_type.fct_new)#sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Course_type.fct_new), FUN=sd)


t.test (Nsg_St$PHQ_Score~Nsg_St$Institution_type.fct) #sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Institution_type.fct), FUN=sd)


t.test (Nsg_St$PHQ_Score~Nsg_St$Nursing_choice.fct)#sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Nursing_choice.fct), FUN=sd)


t.test (Nsg_St$PHQ_Score~Nsg_St$Have_subject_specific_teacher.fct)#sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Have_subject_specific_teacher.fct), FUN=sd)


t.test (Nsg_St$PHQ_Score~Nsg_St$Have_qualified_teacher.fct) #sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Have_qualified_teacher.fct), FUN=sd)

t.test (Nsg_St$PHQ_Score~Nsg_St$Have_qualified_language_teacher.fct)#sig  
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Have_qualified_language_teacher.fct), FUN=sd)


############# un-adjusted analysis by anova-test #############
aov_Father_education=aov(PHQ_Score ~ Father_education.fct, data = Nsg_St) 
summary (aov_Father_education)#non-sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Father_education.fct), FUN=mean)
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Father_education.fct), FUN=sd)

aov_Mother_education=aov(PHQ_Score ~ Mother_education.fct_new, data = Nsg_St)#non-sig
summary (aov_Mother_education)
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Mother_education.fct), FUN=mean)
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Mother_education.fct), FUN=sd)

aov_Family_income_new=aov(PHQ_Score ~ Family_income.fct_new, data = Nsg_St)#non-sig
summary(aov_Family_income_new)
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Family_income.fct_new), FUN=mean)
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Family_income.fct_new), FUN=sd)

aov_Division=aov(PHQ_Score ~ Division.fct_new, data = Nsg_St)
summary(aov_Division)#non-sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Division.fct_new), FUN=mean)
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Division.fct_new), FUN=sd)

Current_residence.fct_new=aov(PHQ_Score ~ Current_residence.fct_new, data = Nsg_St)
summary(Current_residence.fct_new)#non-sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Current_residence.fct_new), FUN=mean)
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Current_residence.fct_new), FUN=sd)

aov_Academic_year=aov(PHQ_Score ~ Academic_year.fct, data = Nsg_St)
summary(aov_Academic_year)#sig
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Academic_year.fct), FUN=mean)
aggregate(Nsg_St$PHQ_Score, list(Nsg_St$Academic_year.fct), FUN=sd)

#normality 
library(ggplot2)

histogram.depression=ggplot(Nsg_St, aes(x = PHQ_Score)) + 
  geom_histogram(aes(y =..density..), binwidth = 1,
                 colour = "white", 
                 fill = "#00BA38") +
  labs(x="Depression score",
       y= NULL,
       title= "")+
  stat_function(fun = dnorm, args = list(mean = mean(Nsg_St$PHQ_Score), sd = sd(Nsg_St$PHQ_Score)))

histogram.depression

#### adjusted analysis##############
linear_model.3=lm (Nsg_St$PHQ_Score~
      Nsg_St$WEMWBS_Score+
      Nsg_St$Age+
      Nsg_St$Sex.fct_new+ 
      Nsg_St$Course_type.fct_new+ 
      Nsg_St$Institution_type.fct+
      Nsg_St$Academic_year.fct+
      Nsg_St$Nursing_choice.fct+
      Nsg_St$Have_subject_specific_teacher.fct+ 
      Nsg_St$Have_qualified_teacher.fct+  
      Nsg_St$Have_qualified_language_teacher.fct)

summary (linear_model.3)
confint(linear_model.3)

#VIF
library("olsrr")

ols_vif_tol(linear_model.3)

#cooks distance
cooks.distance(linear_model.3)
cooks.depression= ols_plot_cooksd_bar(linear_model.3) 

cooks.depression.1 <- 
  cooks.depression +
  xlab("Observation") +
  ylab("Cook's D") + 
  ggtitle("")
  
cooks.depression.1


#### adjusted analysis##############
linear_model.2=lm (Nsg_St$PHQ_Score~
                     Nsg_St$Age+
                     Nsg_St$Sex.fct_new+ 
                     Nsg_St$Course_type.fct_new+ 
                     Nsg_St$Institution_type.fct+
                     Nsg_St$Academic_year.fct+
                     Nsg_St$Nursing_choice.fct+
                     Nsg_St$Have_subject_specific_teacher.fct+ 
                     Nsg_St$Have_qualified_teacher.fct+  
                     Nsg_St$Have_qualified_language_teacher.fct)

summary (linear_model.2)
confint(linear_model.2)

linear_model.1=lm (Nsg_St$PHQ_Score~
                     Nsg_St$Age+
                     Nsg_St$Sex.fct_new) 
summary (linear_model.1)
confint(linear_model.1)




# reference change

Nsg_St$Sex.fct_new <- relevel(Nsg_St$Sex.fct_new, ref = "Male") 
Nsg_St$Academic_year.fct <- relevel(Nsg_St$Academic_year.fct, ref = "Fourth year") 

Nsg_St$Nursing_choice.fct <- relevel(Nsg_St$Nursing_choice.fct, ref = "Yes") 

Nsg_St$Have_subject_specific_teacher.fct <- relevel(Nsg_St$Have_subject_specific_teacher.fct, ref = "Yes") 

Nsg_St$Have_qualified_teacher.fct <- relevel(Nsg_St$Have_qualified_teacher.fct, ref = "Yes") 
Nsg_St$Have_qualified_language_teacher.fct <- relevel(Nsg_St$Have_qualified_language_teacher.fct, ref = "Yes") 

########### graphical presentation ############
Figure.df= data.frame (WEMWBS=Nsg_St$WEMWBS_Score)
Figure.df$PHQ=Nsg_St$PHQ_Score
Figure.df$Sex=Nsg_St$Sex.fct
Figure.df$Academic_year=Nsg_St$Academic_year.fct
Figure.df$Nursing_choice=Nsg_St$Nursing_choice.fct
Figure.df$Have_qualified_teacher=Nsg_St$Have_qualified_teacher.fct

#WEMWBS vs PHQ
ggplot (Figure.df, aes(x=WEMWBS, y= PHQ))+
  geom_jitter (color = "violet"
  )  +
  labs (x= "WEMWBS Score", y= "PHQ Score")+
  geom_smooth (method="lm", se=T)+
  theme_bw()

#WEMWBS vs PHQ by Sex
ggplot (Figure.df, aes(x=WEMWBS, y= PHQ, color=Sex))+
  geom_jitter (color = "seagreen2"
  )  +
  labs (x= "WEMWBS Score", y= "PHQ Score")+
  geom_smooth (method="lm", se=T)+
  theme_bw()

#WEMWBS vs PHQ by Academic_year
ggplot (Figure.df, aes(x=WEMWBS, y= PHQ,color=Academic_year))+
  geom_jitter (color = "seagreen2"
  )  +
  labs (x= "WEMWBS Score", y= "PHQ Score")+
  geom_smooth (method="lm", se=F)+
  theme_bw()

#WEMWBS vs PHQ by Nursing_choice 
ggplot (Figure.df, aes(x=WEMWBS, y= PHQ,color=Nursing_choice))+
  geom_jitter (color = "red"
  )  +
  labs (x= "WEMWBS Score", y= "PHQ Score")+
  geom_smooth (method="lm", se=F)+
  theme_bw()

#WEMWBS vs PHQ by Have_qualified_teacher 
ggplot (Figure.df, aes(x=WEMWBS, y= PHQ,color=Have_qualified_teacher))+
  geom_jitter (color = "red"
  )  +
  labs (x= "WEMWBS Score", y= "PHQ Score")+
  geom_smooth (method="lm", se=F)+
  theme_bw()

#Forest plot for model 3
# there are some changes in the variable name and therefore, a new model was used with the rename of the variables.
# Variables to be rename:

Nsg_St$Nursing_choice.fct
Nsg_St$Have_qualified_teacher.fct
Nsg_St$Have_qualified_language_teacher.fct

library (tidyverse)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

set_theme(base = theme_blank ())
linear_model.3

Forest_plot_PHQ_1 = plot_model(linear_model.3,
                               vline.color = "#808000",
                               show.values = F,
                               width = 0.1,
                               value.offset = 0.4,
                               cex = 3,
                               p.shape = TRUE,
                               xmin=error_lower,
                               xmax=error_upper,,
                               face = "bold",
           axis.labels = c("Language teachers have postgraduate degrees= No",
                           "Teachers have postgraduate degrees = No",
                           "Have subject specific teachers = No",
                           "Family force to come into nursing = Yes",
                           "Academic year = Third year",
                           "Academic year = Second year",
                           "Academic year = First year",
                           "Type of institution = Private",
                           "Type of course = Diploma", 
                           "Sex = Female",
                           "Age", 
                           "WEMWBS Score"),
           face = "bold",
           title = "") 

        
Forest_plot_PHQ_1
