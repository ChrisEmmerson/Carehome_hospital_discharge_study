#############################################################################################
# the code for analysis provided in this script depends on the data                         #
# being created in the script 'Discharge_study_data_prep'                                   #
#                                                                                           #
# for full details of the development of this code and conditions of use, please go to:     #
#                                                                                           #
# https://github.com/ChrisEmmerson/Carehome_hospital_discharge_study                        #
#                                                                                           #
#                                                                                           #
#############################################################################################

 


#Final Analysis 
library(lubridate)
library(tidyverse)
library(survival)
library(broom)
# Import Data ------
All_Data <- read_csv("Care Home Data for cox regression_all_dataV2.csv") %>%   
  select(Group_ref,With_split,start_date,end_date,Discharge,Onset,start,end,everything()) 

#Create extract for Mike and co ------
Data_7_21 <- All_Data %>% 
  filter(Group_ref == "Intervals_7_21") 

Data_7_21 %>% 
  write_csv("Care home Data 7 to 21 days.csv")

# Data_7_21 <- All_Data %>% 
#   filter(Group_ref == "Intervals_10_31") 

Intervals_9_30
# Create co-variaties ------


Data_7_21 


#Capacity-----

Capacity_levels <- c("Under 10", "10 - 24","25 - 50","Over 50" )

Data_7_21.1 <- Data_7_21%>% 
  mutate(Capacity = case_when(MaxCapacity < 10 ~ "Under 10",
                              between(MaxCapacity,10,24) ~ "10 - 24",
                              between(MaxCapacity,25,49) ~ "25 - 50",
                              MaxCapacity >= 50 ~ "Over 50"),
         Capacity = factor(Capacity, levels = Capacity_levels))

#Dementia   

table(Data_7_21.1$DementiaService)

Dementia <- c("Specialist","Non Specialist")

levels <- c("No","Unknown","Dementia_Care")

Data_7_21.2 <- Data_7_21.1 %>% 
  mutate(Dementia = case_when(DementiaService %in% Dementia ~ "Dementia_Care",
                              T ~ DementiaService),
         Dementia = factor(Dementia,levels = levels))

#Service sub type ------

table(Data_7_21.2$ServiceSubType, useNA = "always")

Data_7_21.3 <- Data_7_21.2 %>% 
  mutate(ServiceSubType = ServiceSubType == "Adults With Nursing")

#Learning disabilty -----
table(Data_7_21.3$Provisionforlearningdisability)

#Assume missing Provisionforlearningdisability is No if missing
Data_7_21.4 <- Data_7_21.3 %>% 
  mutate(Provisionforlearningdisability = replace_na(Provisionforlearningdisability,"No"),
         Provisionforlearningdisability = Provisionforlearningdisability == "Yes")


#Add health board----- 


glimpse(Data_7_21.4)


Data_7_21.5 <- Data_7_21.4 %>% 
  select(CiW_URN, With_split, start_date, end_date,start, end, Onset, Discharge,Capacity,Dementia,Provisionforlearningdisability,HealthBoard,HealthBoard,ServiceSubType)


HB_Levels <- c("Swansea Bay", "Aneurin Bevan","BCU", "Cardiff & Vale", "CTM",  "Hywel Dda", "Powys")

Data_7_21.Final <- Data_7_21.5 %>% 
  mutate(AB = HealthBoard == "Aneurin Bevan",
         BCU = HealthBoard == "BCU",
         CV = HealthBoard == "Cardiff & Vale",
         CTM = HealthBoard == "CTM",
         HD = HealthBoard == "Hywel Dda",
         P = HealthBoard == "Powys",
         SB = HealthBoard == "Swansea Bay",
         HealthBoard = factor(HealthBoard, levels = HB_Levels))


#Add varible for post 1st May-----
Data_7_21.No_split <- Data_7_21.Final %>% 
  filter(With_split == F)

Data_7_21.With_split <- Data_7_21.Final %>% 
  filter(With_split == T) %>% 
  mutate(Before_may = end_date < ymd("2020,05,01"))

Data_7_21.With_split %>% 
  count(Before_may)

#Analysis
#Discharge 
Discharge_Mod<- coxph(Surv(time = start, time2 = end, Onset) ~ Discharge , data = Data_7_21.No_split)
summary(Discharge_Mod)

Discharge_May_Mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Discharge  + Discharge*Before_may + Before_may, data = Data_7_21.With_split)
summary(Discharge_May_Mod)

#HBs -----

HB_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ HealthBoard, data = Data_7_21.With_split)
summary(HB_mod)



AB_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ AB, data = Data_7_21.With_split)
summary(AB_mod)

AB_Discharge_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ AB +Discharge + AB*Discharge, data = Data_7_21.With_split)
summary(AB_Discharge_mod)


#BCU

BCU_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ BCU, data = Data_7_21.With_split)
summary(BCU_mod)

BCU_Discharge_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ BCU +Discharge + BCU*Discharge, data = Data_7_21.With_split)
summary(BCU_Discharge_mod)

# CTM

CTM_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ CTM, data = Data_7_21.With_split)
summary(CTM_mod)

CTM_Discharge_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ CTM +Discharge + CTM*Discharge, data = Data_7_21.With_split)
summary(CTM_Discharge_mod)

#CV

CV_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ CV, data = Data_7_21.With_split)
summary(CV_mod)

CV_Discharge_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Capacity + CV +Discharge + CV*Discharge, data = Data_7_21.With_split)
summary(CV_Discharge_mod)

#HD
HD_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ HD, data = Data_7_21.With_split)
summary(HD_mod)

HD_Discharge_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ HD +Discharge + HD*Discharge, data = Data_7_21.With_split)
summary(HD_Discharge_mod)

#P
P_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ P, data = Data_7_21.With_split)
summary(P_mod)

P_Discharge_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ P +Discharge + P*Discharge, data = Data_7_21.With_split)
summary(P_Discharge_mod)

#sb ----

SB_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ SB, data = Data_7_21.With_split)
summary(SB_mod)

SB_Discharge_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ SB +Discharge + SB*Discharge, data = Data_7_21.With_split)
summary(SB_Discharge_mod)


# Dementia

Dementia_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Dementia, data = Data_7_21.With_split)
summary(Dementia_mod)

#Capacity 

Capacity_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Capacity, data = Data_7_21.With_split)
summary(Capacity_mod)


Capacity_with_Discharge_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Capacity + Capacity*Discharge +Discharge, data = Data_7_21.With_split)
summary(Capacity_with_Discharge_mod)


#survice subtype -----

ServiceSubType_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ ServiceSubType, data = Data_7_21.With_split)
summary(ServiceSubType_mod)

#Learning Disabiltiy 
learningdisability_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Provisionforlearningdisability, data = Data_7_21.With_split)
summary(learningdisability_mod)


# Final Modle 
glimpse(Data_7_21.With_split)


Final_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Capacity  + Dementia + ServiceSubType + HealthBoard +   Provisionforlearningdisability + Discharge, data = Data_7_21.With_split)
summary(Final_mod)

Final_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Capacity  +  Discharge + HD, data = Data_7_21.With_split)
summary(Final_mod)

test.fit <- cox.zph(Final_mod)

test.fit

plot(test.fit)


# Single Homes -------


Homes_only <- Data_7_21.With_split %>% 
  group_by(CiW_URN,Capacity,Dementia,Provisionforlearningdisability,ServiceSubType,HealthBoard) %>% 
summarise(onset = max(Onset),
          Discharge = max(Discharge)) %>% 
  ungroup()

Homes_only %>% 
  count(Capacity)

Homes_only %>% 
  count(Capacity,onset) %>% 
  spread(onset,n)

#ServiceSubType
Homes_only %>% 
  count(ServiceSubType)

Homes_only %>% 
  count(ServiceSubType,onset) %>% 
  spread(onset,n)


Homes_only %>% 
  count(Dementia)

Homes_only %>% 
  count(Dementia,onset) %>% 
  spread(onset,n)


#Provisionforlearningdisability

Homes_only %>% 
  count(Provisionforlearningdisability)

Homes_only %>% 
  count(Provisionforlearningdisability,onset) %>% 
  spread(onset,n)


#HB

Homes_only %>% 
  count(HealthBoard)

Homes_only %>% 
  count(HealthBoard,onset) %>% 
  spread(onset,n)



Homes_only %>% 
  count(Discharge)

Homes_only %>% 
  count(Discharge,onset) %>% 
  spread(onset,n)

# Number of days 

Data_7_21.With_split %>% 
  glimpse()

Data_7_21.With_split %>% 
  mutate(days = end - start) %>% 
  summarise(days = sum(days),
            outbreaks = sum(Onset))


Data_7_21.With_split %>% 
  mutate(days = end - start) %>% 
  group_by(Discharge) %>% 
  summarise(days = sum(days),
            outbreaks = sum(Onset))


Data_7_21.With_split %>% 
  mutate(days = end - start) %>% 
  group_by(Discharge,Capacity) %>% 
  summarise(days = sum(days),
            outbreaks = sum(Onset)) %>% 
  arrange(Capacity)



Data_7_21.With_split %>% 
  mutate(days = end - start) %>% 
  group_by(Discharge,HealthBoard) %>% 
  summarise(days = sum(days),
            outbreaks = sum(Onset))
