#############################################################################################  
#                                                                                           #
# This script provides code to get conduct a sensitivity analysis on data analysed          #
#   using the script 'Discharge_study_final_analysis'                                       #
#                                                                                           #
# for full details of the development of this code and conditions of use, please go to:     #
#                                                                                           #
# https://github.com/ChrisEmmerson/Carehome_hospital_discharge_study                        #
#                                                                                           #
#############################################################################################                  


library(broom)


# Import Data ------
All_Data <- read_csv("Care Home Data for cox regression_all_dataV2.csv") %>%   
  select(Group_ref,With_split,start_date,end_date,Discharge,Onset,start,end,everything()) 




# Create co-variaties ------


All_Data <- All_Data


#Capacity-----

Capacity_levels <- c("Under 10", "10 - 24","25 - 50","Over 50" )

All_Data.1 <- All_Data%>% 
  mutate(Capacity = case_when(MaxCapacity < 10 ~ "Under 10",
                              between(MaxCapacity,10,24) ~ "10 - 24",
                              between(MaxCapacity,25,49) ~ "25 - 50",
                              MaxCapacity >= 50 ~ "Over 50"),
         Capacity = factor(Capacity, levels = Capacity_levels))

#Dementia   

table(All_Data.1$DementiaService)

Dementia <- c("Specialist","Non Specialist")

levels <- c("No","Unknown","Dementia_Care")

All_Data.2 <- All_Data.1 %>% 
  mutate(Dementia = case_when(DementiaService %in% Dementia ~ "Dementia_Care",
                              T ~ DementiaService),
         Dementia = factor(Dementia,levels = levels))

#Service sub type ------

table(All_Data.2$ServiceSubType, useNA = "always")

All_Data.3 <- All_Data.2 %>%
  mutate(ServiceSubType = ServiceSubType == "Adults Without Nursing")

#Learning disabilty -----
table(All_Data.3$Provisionforlearningdisability)

#Assume missing Provisionforlearningdisability is No if missing
All_Data.4 <- All_Data.3 %>% 
  mutate(Provisionforlearningdisability = replace_na(Provisionforlearningdisability,"No"),
         Provisionforlearningdisability = Provisionforlearningdisability == "Yes")


#Add health board----- 


glimpse(All_Data.4)



All_Data.5 <- All_Data.4 %>% 
  select(CiW_URN, With_split, start_date, end_date,start, end, Onset, Discharge,Capacity,Dementia,Provisionforlearningdisability,HealthBoard,HealthBoard,ServiceSubType,Group_ref)


HB_Levels <- c("Swansea Bay", "Aneurin Bevan","BCU", "Cardiff & Vale", "CTM",  "Hywel Dda", "Powys")

All_Data.Final <- All_Data.5 %>% 
  mutate(AB = HealthBoard == "Aneurin Bevan",
         BCU = HealthBoard == "BCU",
         CV = HealthBoard == "Cardiff & Vale",
         CTM = HealthBoard == "CTM",
         HD = HealthBoard == "Hywel Dda",
         P = HealthBoard == "Powys",
         SB = HealthBoard == "Swansea Bay",
         HealthBoard = factor(HealthBoard, levels = HB_Levels))




All_Data.Final_Split <- All_Data.Final %>%  filter(With_split == T)

temp <- All_Data.Final_Split %>%  filter(Group_ref == "Intervals_0_14")



Final_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Capacity  + Dementia + ServiceSubType + Heal + Provisionforlearningdisability + Discharge + HealthBoard, data = temp)
summary(Final_mod)


carete_mod <- function(df) {
  
  Final_mod <- coxph(Surv(time = start, time2 = end, Onset) ~ Capacity  + Dementia + ServiceSubType  + Provisionforlearningdisability + Discharge + HealthBoard, data = df)
}


Final_mods <- All_Data.Final_Split %>% 
  group_by(Group_ref) %>% 
  nest() %>% 
  mutate(Cox = map(data,carete_mod),
         tidy = map(Cox, tidy)) %>% 
  unnest(tidy) %>% 
  ungroup()

Final_mods %>% 
  select(Group_ref,term,estimate) %>% 
  mutate(estimate = exp(estimate)) %>% 
spread(term,estimate) %>% 
  write_csv("Sensitivity Analysis HR estimates.csv")


Final_mods %>% 
  select(Group_ref,term,p.value) %>% 
  spread(term,p.value) %>% 
  write_csv("Sensitivity Analysis PValues.csv")



Final_mods %>% 
  select(Group_ref,term, estimate)  %>% 
  #filter(term == "Discharge") %>% 
  separate(Group_ref, into = c("temp", "start", "end")) %>% 
  mutate(end = as.numeric(end),
         start = as.numeric(start),
         count = end - start,
         estimate = exp(estimate))  %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_boxplot() + 
  facet_wrap(~count ) + 
  coord_flip() + 
  scale_y_continuous(limits =  c(0,5))


