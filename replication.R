###Title: Final Project  - DiD on Trust in Police in Mali
###Authors: Sara Kallis, Fredrik Wallin
###Date: 6 June, 2022

##1. Preliminary tasks ####
rm(list=ls())
library(haven)
library(tidyverse)
library(sjmisc)
library(stargazer)
#Set working directory
  #setwd("C:/Users/wetts/Documents/IHEID/FS22/RI-SP062_Stats_II/Final Project/Police Trust EU Mission") #Fredrik
  #setwd("/Users/sara/Documents/IHEID Academic/Semester 2/6. Statistics II/Final Project") #Sara

##2. Short theory ####
# The policing mission was launched in January 2015, after which its funding got extended multiple times
# (including in 2021), meaning that the mission is still ongoing. The goals of the mission are multi-faceted,
# whereby the Malian Police, Gendarmerie and National Guard including the relevant ministries are sought to be 
# supported. Apart from helping with security sector reform, the aims include "the areas of human resources 
# management, counterterrorism, organized crime, border management, logistics management, rule of law, fight
# against impunity and the redeployment of the civilian administration." (see also https://www.eeas.europa.eu/eucap-sahel-mali/about-eucap-sahel-mali_en?s=331#10107).
# Consequently, we hypothesise more trust in the police after the launch of EUCAP Mali.

##3. Prepare Data####

##Import Afrobarometer
Mali_wave4_2008 <- read_sav("mli_r4_data.sav") #December 2008
Mali_wave5_2012 <- read_sav("mli_r5_data_july_2015.sav") #December 2012-Jan 2013
Mali_wave6_2014 <- read_sav("mli_r6_data_2015_eng.sav") #December 2014
Mali_wave7_2017 <- read_sav("mli_r7_data_eng.sav") #February 2017

###Cleaning data sets
## Variables needed:
# (i) outcome (trust in police),
# (ii) treatment (presence of EUCAP Mali, Region == 509),
# (iii) control area (absence of EUCAP Mali, Region == 503),
# (iv) time (waves),
# (v) control variables to account for population characteristics (and that might influence trust in police).
# (vi) variables that account for security in relation to crime for robustness checks.

#### Wave 4 ####
wave4 <- Mali_wave4_2008 %>% 
  select(Q49G, REGION, EA_FAC_C, Q1, Q9B, Q79, Q89, Q90, Q101) %>% 
  rename(c(region = REGION, postat = EA_FAC_C, age = Q1,
           theft = Q9B, ethnicity = Q79, educ = Q89, religion = Q90, gender = Q101,
           policetrust = Q49G))
  wave4$wave = 4
  #Harmonize Religious Identity with other Afrobarometer Waves
  wave4$religion[wave4$religion == 501] <-  510 #recode Hamalite/Hamadiya' to new uniform label
  wave4$religion[wave4$religion == 502] <-  511 #reconde Wahhabiya to new uniform label
  #Harmonize Ethnicity Groups with other Afrobarometer Waves
  wave4$ethnicity[wave4$ethnicity == 508] <- 510 #Khassonké
  wave4$ethnicity[wave4$ethnicity == 509] <- 511 #Malinké
  wave4$ethnicity[wave4$ethnicity == 510] <- 512 #Maure
  wave4$ethnicity[wave4$ethnicity == 512] <- 514 #Peulh/Fulfulde/Fulfudde
  wave4$ethnicity[wave4$ethnicity == 514] <- 517 #Soninké/Sarakollé
  wave4$ethnicity[wave4$ethnicity == 515] <- 518 #Sonrhai
  wave4$ethnicity[wave4$ethnicity == 516] <- 519 #Tamasheq
  wave4$ethnicity[wave4$ethnicity == 517] <- 515 #Samogo
  wave4$ethnicity[wave4$ethnicity == 518] <- 509 #Kakolo
  wave4$ethnicity[wave4$ethnicity == 519] <- 506 #Haoussa
  wave4$ethnicity[wave4$ethnicity == 990] <- 9900 #mali/not in terms of ethnicity
  #Assign NA to "Don't Know" Answers
  wave4$postat[wave4$postat == 9] <-  NA
  wave4$age[wave4$age == 999] <-  NA
  wave4$theft[wave4$theft == 9] <-  NA
  wave4$policetrust[wave4$policetrust == 9] <-  NA
  rm(Mali_wave4_2008)

#### Wave 5 ####
wave5 <- Mali_wave5_2012 %>% 
  select(Q59H, REGION, Q1, Q84, Q97, Q98A, Q101, EA_FAC_C, Q10A) %>% 
  rename(c(region = REGION, postat = EA_FAC_C, theft = Q10A,
           age = Q1, ethnicity = Q84, educ = Q97, religion = Q98A, gender = Q101,
           policetrust = Q59H))
  wave5$wave = 5 #Add wave variable to ensure DiD possibility. Year is more difficult as it splits wave 5, since it took place as the year changed
  #Harmonize Religious Identity with other Afrobarometer Waves
  wave5$religion[wave5$religion == 501] <-  510 #Hamalite/Hamadiya
  wave5$religion[wave5$religion == 500] <-  511 #Wahhabiya
  #Assign NA to "Don't Know" Answers
  wave5$postat[wave5$postat == 9] <-  NA
  wave5$age[wave5$age == 999] <-  NA
  wave5$theft[wave5$theft == 9] <-  NA
  wave5$policetrust[wave5$policetrust == 9] <-  NA
  rm(Mali_wave5_2012)

#### Wave 6 ####
wave6 <- Mali_wave6_2014 %>% 
  select(Q52H, REGION, Q1, Q87, Q97, Q98A, Q101, EA_FAC_C, Q11A) %>% 
  rename(c(region = REGION, postat = EA_FAC_C, age = Q1, theft = Q11A,
           ethnicity = Q87, educ = Q97, religion = Q98A, gender = Q101,
           policetrust = Q52H))
  wave6$wave = 6 
  #Harmonize Religious Identity with other Afrobarometer Waves
  wave6$religion[wave6$religion == 500] <-  510 #Hamalite/Hamadiya
  wave6$religion[wave6$religion == 502] <-  510 #Hamalite/Hamadiya
  wave6$religion[wave6$religion == 501] <-  511 #Wahhabiya
  #Assign NA to "Don't Know" Answers
  wave6$postat[wave6$postat == 9] <-  NA
  wave6$age[wave6$age == 999] <-  NA
  wave6$theft[wave6$theft == 9] <-  NA
  wave6$policetrust[wave6$policetrust == 9] <-  NA
  rm(Mali_wave6_2014)

#### Wave 7 ####
wave7 <- Mali_wave7_2017 %>% 
  select(Q43G, REGION, Q1, Q84, Q97, Q98, Q101, EA_FAC_C, Q11A) %>%
  rename(c(region = REGION, postat = EA_FAC_C, age = Q1, theft = Q11A,
           ethnicity = Q84, educ = Q97, religion = Q98, gender = Q101,
           policetrust = Q43G))
  wave7$wave = 7 
  #Harmonize Ethnicity Groups with other Afrobarometer Waves
  wave7$religion[wave7$religion == 500] <-  503 #recode 'Ansardine' to be like other waves
  wave7$ethnicity[wave7$ethnicity == 505] <- 506 #Dogon
  wave7$ethnicity[wave7$ethnicity == 508] <- 510 #Khassonké
  wave7$ethnicity[wave7$ethnicity == 509] <- 511 #Malinké
  wave7$ethnicity[wave7$ethnicity == 510] <- 512 #Maure
  wave7$ethnicity[wave7$ethnicity == 513] <- 514 #Peulh/Fulfulde/Fulfudde
  wave7$ethnicity[wave7$ethnicity == 516] <- 517 #Soninké/Sarakollé
  wave7$ethnicity[wave7$ethnicity == 517] <- 518 #Sonrhai
  wave7$ethnicity[wave7$ethnicity == 518] <- 519 #Tamasheq
  wave7$ethnicity[wave7$ethnicity == 514] <- 515 #Samogo
  wave7$ethnicity[wave7$ethnicity == 507] <- 509 #Kakolo
  wave7$ethnicity[wave7$ethnicity == 506] <- 506 #Haoussa
  #Assign NA to "Don't Know" Answers
  wave7$ethnicity[wave7$ethnicity == 9998] <- NA #Refused Answer
  wave7$postat[wave7$postat == 9] <-  NA
  wave7$age[wave7$age == 999] <-  NA
  wave7$theft[wave7$theft == 9] <-  NA
  wave7$policetrust[wave7$policetrust == 9] <-  NA
  rm(Mali_wave7_2017)

####Merge####
data_placebo <- full_join(wave4, wave5)
data_placebo <- full_join(data_placebo, wave6) #data for placebo DiD regression for pre-EUCAP, i.e., 2008-2014
data <- full_join(wave6, wave7) #data for DiD analysis 2014-2017, N = 2400, pooled cross-section
data_full <- full_join(data_placebo, wave7) #Full data set with all waves for descriptive statistics

###Add IV: Presence of EUCAP Office####
### Regions, differentiating variable
## in Mali ## REGION (see also https://www.google.com/url?sa=i&url=https%3A%2F%2Fcommons.wikimedia.org%2Fwiki%2FFile%3AMali_Regions.png&psig=AOvVaw14grY4SthSMV77KjN9hRjt&ust=1654178713301000&source=images&cd=vfe&ved=0CAkQjRxqFwoTCKims4m2jPgCFQAAAAAdAAAAABAJ for old map)
# 501= Kayes
# 502= Koulikoro
# 503= Sikasso
# 504= S?gou
# 505= Mopti -->here, they had a Mobile Unit (https://www.eeas.europa.eu/eucap-sahel-mali/about-eucap-sahel-mali_en?s=331#10107)
# 506= Tombouctou
# 507= Gao
# 508= Kidal
# 509= Bamako -->where the HQ was located (https://www.eeas.europa.eu/eucap-sahel-mali/about-eucap-sahel-mali_en?s=331#10107)

####Restrict to Sikasso and Bamako, our control and treated units####
  data_r <- data %>%
    filter(region == 509 | region == 503) #N = 744
  
  ##Add in Main Dataset
  #Condition 1: Wave = 7
  #Condition 2: Region = 509
  data_r$eucap <- 0
  data_r[(data_r$region == 509), "eucap"] <- 1 #Assign eucap binary to Bamako
  
  ##How to add in Placebo?
  data_rp <- data_placebo %>%
    filter(region == 509 | region == 503)
  data_rp$eucap <- 0
  data_rp[(data_rp$region == 509), "eucap"] <- 1  ##Assign eucap binary to Bamako
  
  ##Add to full data set too
  data_full <- data_full %>%
    filter(region == 509 | region == 503) #N = 1512
  data_full$eucap <- 0
  data_full[(data_full$region == 509), "eucap"] <- 1

##4: Descriptive Statistics ####

###4.1 Control Variables####
  ####Table##
  #Stargazer creates table with summary statistics for all variables in dataset
  stargazer(data.frame(data_full), type = "html", title = "Descriptive Statistics", digits = 1, out = "desc1.doc")

####Are Bamako and Sikasso comparable units?####
##Education
data_full  %>%
  mutate(treat_condition = if_else(region==509, 
                                   "Bamako (treated)", 
                                   "Sikasso (control)")) %>% 
  ggplot(aes(wave, educ, color = treat_condition)) +
  stat_summary(fun = "mean", geom = "line") +
  labs(x = "wave", y = "Education, Average") +
  scale_color_discrete(name = "Compositional Comparison") +
  theme_minimal() +
  ylim(0,4)

##Age
data_full  %>%
  mutate(treat_condition = if_else(region==509, 
                                   "Bamako (treated)", 
                                   "Sikasso (control)")) %>% 
  ggplot(aes(wave, age, color = treat_condition)) +
  stat_summary(fun = "mean", geom = "line") +
  labs(x = "wave", y = "Age, Average") +
  scale_color_discrete(name = "Compositional Comparison") +
  theme_minimal() +
  ylim(0,50)

##Gender
data_r  %>%
  mutate(gender = case_when(
    gender == 2 ~ "Female",
    gender == 1 ~ "Male")) %>%
  mutate(region = case_when(
    region == 503 ~ "Sikasso",
    region == 509 ~ "Bamako")) %>%
  ggplot(aes(x = gender, fill = as.factor(wave),
           position = "dodge", stat = "count")) +
  geom_bar() +
  ggtitle("Compositional Comparison: Gender") +
  theme_minimal() +
  labs(x = NULL, fill = "Wave") +
  facet_wrap(~ region)

##Ethnicity
data_r  %>%
  mutate(ethnicity = case_when(
    ethnicity == 505 ~ "Dafing/Daffing",
    ethnicity == 506 ~ "Dogon",
    ethnicity == 507 ~ "Gana",
    ethnicity == 508 ~ "Haoussa",
    ethnicity == 509 ~ "Kakolo",
    ethnicity == 510 ~ "Khassonke",
    ethnicity == 511 ~ "Malinke",
    ethnicity == 512 ~ "Maure",
    ethnicity == 513 ~ "Mianka",
    ethnicity == 514 ~ "Peulh/Fulfulde/Fulfude",
    ethnicity == 515 ~ "Samogo",
    ethnicity == 516 ~ "Senufo",
    ethnicity == 517 ~ "Soninke/Sarakolle",
    ethnicity == 518 ~ "Sonrhai",
    ethnicity == 519 ~ "Tamasheq",
    ethnicity == 9990 ~ "Mali/Do not consider myself in these terms")) %>%
  mutate(region = case_when(
    region == 503 ~ "Sikasso",
    region == 509 ~ "Bamako")) %>%
  ggplot(aes(x = ethnicity, fill = as.factor(wave),
             position = "dodge", stat = "count")) +
  geom_bar() +
  ggtitle("Compositional Comparison: Ethnicity") +
  theme_minimal() +
  labs(x = NULL, fill = "Wave") +
  facet_wrap(~ region)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##Religion
data_r  %>%
  mutate(religion = case_when(
    religion == 0 ~ "None",
    religion == 1 ~ "Christian only",
    religion == 2 ~ "Roman Catholic",
    religion == 3 ~ "Orthodox",
    religion == 4 ~ "Coptic",
    religion == 5 ~ "Anglican",
    religion == 6 ~ "Lutheran",
    religion == 7 ~ "Methodist",
    religion == 8 ~ "Presbyterian",
    religion == 9 ~ "Baptist",
    religion == 10 ~ "Quaker/Friends",
    religion == 11 ~ "Mennonite",
    religion == 12 ~ "Evangelical",
    religion == 13 ~ "Pentecostal",
    religion == 14 ~ "Independent",
    religion == 15 ~ "Jehovah's Witness",
    religion == 16 ~ "Seventh Day Adventist",
    religion == 17 ~ "Mormon",
    religion == 18 ~ "Muslim only",
    religion == 19 ~ "Sunni only",
    religion == 20 ~ "Ismaeli",
    religion == 21 ~ "Mouridiya Brotherhood",
    religion == 22 ~ "Tijaniya Brotherhood",
    religion == 23 ~ "Qadiriya Brotherhood",
    religion == 24 ~ "Shia only",
    religion == 25 ~ "Traditional/ethnic religion",
    religion == 26 ~ "Hindu",
    religion == 27 ~ "Bahai",
    religion == 28 ~ "Agnostic",
    religion == 29 ~ "Atheist",
    religion == 32 ~ "Church of Christ",
    religion == 503 ~ "Ansardine", 
    religion == 510 ~ "Hamalite/Hamadiya",
    religion == 511 ~ "Wahabiya",
    religion == 9995 ~ "Other")) %>%
  mutate(region = case_when(
    region == 503 ~ "Sikasso",
    region == 509 ~ "Bamako")) %>%
  ggplot(aes(x = religion, fill = as.factor(wave),
             position = "dodge", stat = "count")) +
  geom_bar() +
  ggtitle("Compositional Comparison: Religion") +
  theme_minimal() +
  labs(x = NULL, fill = "Wave") +
  facet_wrap(~ region)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

###4.2 Outcome: Trust Levels####
####Do we have parallel trends? ####
data_full  %>%
  mutate(treat_condition = if_else(eucap==1, 
                                   "EUCAP Office in town (treatment)", 
                                   "Not EUCAP Office in town (control)")) %>% 
  ggplot(aes(wave, policetrust, color = treat_condition)) +
  stat_summary(fun = "mean", geom = "line") +
  geom_vline(xintercept = 6.08) + #this is were the treatment started (January 2015)
  labs(x = "wave", y = "Police Trust, Average") +
  scale_color_discrete(name = "Treatment Conditions") +
  theme_minimal() +
  ylim(0,4) 

##5. Analysis: Difference-in-Difference####
  DiD0 <- lm(policetrust ~ eucap*wave, data = data_r)
  summary(DiD0)
  
  DiD1 <- lm(policetrust ~ eucap*wave + age + factor(ethnicity) + educ
             + factor(religion) + factor(gender), data = data_r)
  summary(DiD1)

##6. Robustness Checks ####
###6.1 Placebo test: Create fake treatment in pre-treatment period ###
  DiD_plac1 <- lm(policetrust ~ eucap*wave, data = data_rp)
  summary(DiD_plac1)
  
  DiD_plac2 <- lm(policetrust ~ eucap*wave + age + factor(ethnicity) + educ
                  + factor(religion) + factor(gender), data = data_rp)
  summary(DiD_plac2)

###6.2 Effect of being victim of theft in own home on policetrust###
  DiD_theft <- lm(policetrust ~ theft*wave, data = data_r)
  summary(DiD_theft)
  
  DiD_theft2 <- lm(policetrust ~ theft*wave + age + factor(ethnicity) + educ
                  + factor(religion) + factor(gender), data = data_r)
  summary(DiD_theft2)

###6.3 Effect of police station in vicinity on policetrust###
  DiD_postat <- lm(policetrust ~ postat*wave, data = data_r)
  summary(DiD_postat)
  
  DiD_postat2 <- lm(policetrust ~ postat*wave + age + factor(ethnicity) + educ
                   + factor(religion) + factor(gender), data = data_r)
  summary(DiD_postat2)
  
  #Create Regression Table
  stargazer(DiD0, DiD1, DiD_plac2, DiD_theft2, DiD_postat2,
            type = "html", out = "analysis_robustness.doc")
  
  #End

