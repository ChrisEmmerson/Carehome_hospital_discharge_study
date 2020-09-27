#############################################################################################  
#                                                                                           #
# This script provides code to get data into a form where it can be                         #
#   analysed using the script 'Discharge_study_final_analysis'                              #
#                                                                                           #
# for full details of the development of this code and conditions of use, please go to:     #
#                                                                                           #
# https://github.com/ChrisEmmerson/Carehome_hospital_discharge_study                        #
#                                                                                           #
#############################################################################################                  


library(readxl)
library(tidyverse)
library(skimr)
library(lubridate)

# Import data - Change nmax if there are more homes added

#This code has been set out assuming that the data are in a single Escel workbook with three sheets:

#   workbook name: "Discharge_study"
#      Sheets:  "Results_data"
#               "Discharge_data"
#               "Care_home_data"


# Import data
# Results data should contain 
#         (1) Unique care home identifier ("CiW_URN")
#         (2) Date (here date of collection of specimen "SpecDate" is used)

Results_data <-
  read_excel(
    "Discharge_study.xlsx",
    sheet = "Results_data"
  )


glimpse(Results_data)



# create a list of care homes whith the date of first and last case. 
First_Case_in_carehome <-  Results_data %>%
  group_by(CiW_URN) %>%
  summarise(
    Cases = n(),
    First_case = min(SpecDate),
    Last_case = max(SpecDate)
  ) %>% 
  ungroup()


First_Case_in_carehome <- First_Case_in_carehome %>% 
  mutate_if(is.POSIXct,as_date)

glimpse(First_Case_in_carehome)

# Import care home data -----
#This dataset requires (1) care home identifier for linkage ("CiW_URN")
#                   (2) health board ("Health Board")
#                   (3) whether  nursing care was provided ("Service sub type")
#                   (4) whether provision for care of those with learning disabilities ("Provision for learning disability")
#                   (5) maximum capacity ("Max capacity")
#                   (6) whether provision for dementia ("Dementia service")

Care_home_info <-
  read_excel("Discharge_study.xlsx",
  sheet = "Care_home_data"
)


Homes_Master   <- Care_home_info %>% 
  select(CiW_URN) 

First_Case_in_carehome %>% 
  anti_join(Homes_Master)


#Import_discharge_Data ------ 


#THis dataset requires: (1) Care home identifier for linkage "CiW_URN"
#                       (2) Date of discharge(s)

Discharge_raw_data <- read_excel("Discharge_study.xlsx", 
                             sheet = "Discharge_data")



glimpse(Discharge_raw_data)

Discharge_raw_data_cleaned <- Discharge_raw_data %>% 
  mutate(Discharge_Date = as.character(Discharge_Date),
         Discharge_Date = ymd(Discharge_Date))


glimpse(Discharge_raw_data_cleaned)

#Putting all data together ------


Discharge_data <- Homes_Master %>% 
  left_join(Discharge_raw_data_cleaned) %>% 
  rename(Date = Discharge_Date) %>% 
  mutate(Date_type = "Discharge") %>% 
    ungroup()

###############
# THIS COMPLETES THE IMPORT AND TIDYING OF THE DATA
########################


# define start and end period


Find_periods <-
  function(df,
           Start_period,
           End_period,
           Start_expoure = 0,
           End_exposure = 14) {
    
    #  df 
    # # 
    #  Start_period <- ymd("2020/02/22")
    # # 
    #  End_period <- ymd("2020/07/01")
    #  Start_expoure = 7
    #  End_exposure = 21
    
    df.1 <- df %>%
      filter(!is.na(Date)) %>%
      mutate(Period_end = Date + End_exposure,
             Date = Date + Start_expoure) %>% 
      filter(Start_expoure <= End_period) %>% 
      mutate(Period_end = ifelse(Period_end > End_period,End_period,Period_end),
             Period_end = as_date(Period_end))
    
    # group overlapping periods
    df.1 <- df.1 %>%
      arrange(Date) %>% 
      mutate(indx = c(0, cumsum(
        as.numeric(lead(Date)) >
          cummax(as.numeric(Period_end))
      )[-n()])) %>%
      group_by(indx) %>%
      summarise(Start = min(Date),
                End = max(Period_end))
    
    
    
    Start_end <- c(Start_period, End_period)
    start_end_event <- c("Start", "End")
    
    Start_end_df <- as.data.frame(list(date = Start_end))
    
    
    # create the periods, one line per period. starting from the
    df.2 <- df.1 %>%
      select(-indx) %>%
      gather(Date_type, date) %>%
      select(date) %>%
      full_join(Start_end_df, by = "date") %>%
      arrange(date)
    
    df.3 <- df.2 %>%
      mutate(End_Date = lead(date, 1) - 1,
             Start_date = as_date(date)) %>%
      select(-date) %>%
      filter(!is.na(End_Date))
    
    # create day numbers
    
    df.3 %>%
      mutate(
        start = as.numeric(Start_date - Start_period),
        end = as.numeric(End_Date - Start_period)
      ) %>%
      select(start, end, Start_date, End_Date) %>%
      rename(end_date = End_Date,
             start_date = Start_date)
  }

#Tests for the above. Use the Ciw_URN to check that the above code works. it should spit the periods into periods of expouse and non expousre up to the end point. However, it will not label as such.  it will not be cut off at the start of an oubreak and only looks at discharges. Use a few random care homes to tests 

Start_period <- ymd("2020/02/22")

End_period <- ymd("2020/06/18")

#add a care home here 
df <- Discharge_data %>%
  filter(CiW_URN == "")

df

Find_periods(df,
             Start_period,
             End_period,
             Start_expoure = 0,
             End_exposure = 21)


# Caculated the periods needed for investigation. this function creates a whole dataset for each sensititvity window and uses the function above 

Create_All_intervals <- function(df,start_period = ymd("2020/02/22"),end_period,start,end) { 

Final <- df %>%
  split(.$CiW_URN) %>%
  map( ~ Find_periods(
    .,
    Start_period = start_period,
    End_period = end_period,
    Start_expoure = start,
    End_exposure = end
  )) %>%
  map( ~ as.data.frame.list(.)) %>%
  bind_rows(.id = "CiW_URN") %>%
  mutate(interval = interval(start_date, end_date))

Final
}
# This section can take a long time to run. if needed only use code like the comment below to only run one selected window. The rest of the code should work..... 

# If anouther analysis widow is required then copy the code below and adjust the start and end values. if the start date is not the 1st March, then the keyword start_period can be used to review. 

# All_Periods <- Intervals_7_21

Intervals_0_14 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 0, end = 14)
Intervals_1_15 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 1, end = 15) 
Intervals_2_16 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 2, end = 16)
Intervals_3_17 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 3, end = 17)
Intervals_4_18 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 4, end = 18)
Intervals_5_19 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 5,end = 19)
Intervals_6_20 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 6,end = 20)
Intervals_7_21 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 7,end = 21)
Intervals_8_22 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 8,end = 22)
Intervals_9_23 <- Create_All_intervals(Discharge_data,end_period = ymd("2020/07/01"), start = 9,end = 23)
Intervals_10_24 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 10, end = 24)


All_periods_14days <- bind_rows(Intervals_0_14, Intervals_1_15,Intervals_2_16, Intervals_3_17, Intervals_4_18, Intervals_5_19, Intervals_6_20, Intervals_7_21, Intervals_8_22, Intervals_9_23, Intervals_10_24, .id = "Group")

All_periods_14days <- All_periods_14days %>% 
  mutate(Group_ref = case_when( Group == "1" ~ "Intervals_0_14",
                                Group == "2" ~ "Intervals_1_15",
                                Group == "3" ~ "Intervals_2_16",
                                Group == "4" ~ "Intervals_3_17",
                                Group == "5" ~ "Intervals_4_18",
                                Group == "6" ~ "Intervals_5_19",
                                Group == "7" ~ "Intervals_6_20",
                                Group == "8" ~ "Intervals_7_21",
                                Group == "9" ~ "Intervals_8_22",
                                Group == "10" ~ "Intervals_9_23",
                                Group == "11" ~ "Intervals_10_24"))

Intervals_0_21 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 0, end = 21)
Intervals_1_22 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 1, end = 22)
Intervals_2_23 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 2, end = 23)
Intervals_3_24 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 3, end = 24)
Intervals_4_25 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 4, end = 25)
Intervals_5_26 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 5, end = 26)
Intervals_6_27 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 6, end = 27)
Intervals_7_28 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 7, end = 28)
Intervals_8_29 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 8, end = 29)
Intervals_9_30 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 9, end = 30)
Intervals_10_31 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 10, end = 31)


All_periods_21days <- bind_rows(Intervals_0_21, Intervals_1_22,Intervals_2_23, Intervals_3_24, Intervals_4_25, Intervals_5_26, Intervals_6_27, Intervals_7_28, Intervals_8_29, Intervals_9_30, Intervals_10_31, .id = "Group")

All_periods_21days <- All_periods_21days %>% 
  mutate(Group_ref = case_when( Group == "1" ~ "Intervals_0_21",
                                Group == "2" ~ "Intervals_1_22",
                                Group == "3" ~ "Intervals_2_23",
                                Group == "4" ~ "Intervals_3_24",
                                Group == "5" ~ "Intervals_4_25",
                                Group == "6" ~ "Intervals_5_26",
                                Group == "7" ~ "Intervals_6_27",
                                Group == "8" ~ "Intervals_7_28",
                                Group == "9" ~ "Intervals_8_29",
                                Group == "10" ~ "Intervals_9_30",
                                Group == "11" ~ "Intervals_10_31"))


Intervals_0_7  <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 0, end = 7)
Intervals_1_8  <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 1, end = 8)
Intervals_2_9  <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 2, end = 9)
Intervals_3_10 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 3, end = 10)
Intervals_4_11 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 4, end = 11)
Intervals_5_12 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 5, end = 12)
Intervals_6_13  <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 6, end = 13)
Intervals_7_14  <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 7, end = 14)
Intervals_8_15  <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 8, end = 15)
Intervals_9_16  <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 9, end = 16)
Intervals_10_17 <- Create_All_intervals(Discharge_data, end_period = ymd("2020/07/01"), start = 10, end = 17)


All_periods_7days <- bind_rows(Intervals_0_7, Intervals_1_8,Intervals_2_9, Intervals_3_10, Intervals_4_11, Intervals_5_12, Intervals_6_13, Intervals_7_14, Intervals_8_15, Intervals_9_16, Intervals_10_17, .id = "Group")

All_periods_7days <- All_periods_7days %>% 
  mutate(Group_ref = case_when( Group == "1" ~ "Intervals_0_7",
                                Group == "2" ~ "Intervals_1_8",
                                Group == "3" ~ "Intervals_2_9",
                                Group == "4" ~ "Intervals_3_10",
                                Group == "5" ~ "Intervals_4_11",
                                Group == "6" ~ "Intervals_5_12",
                                Group == "7" ~ "Intervals_6_13",
                                Group == "8" ~ "Intervals_7_14",
                                Group == "9" ~ "Intervals_8_15",
                                Group == "10" ~ "Intervals_9_16",
                                Group == "11" ~ "Intervals_10_17"))


All_Periods <- bind_rows(All_periods_7days,All_periods_14days,All_periods_21days)


All_periods.V1 <- All_Periods %>% 
  mutate(Group = as.numeric(Group),
         Period_Start = Group -1) %>% 
  select(-Group)

# The rest of the codes finds the periods of Discharge and onset. it also splits the data on the 1st of May which was when changes occured in Welsh goverment policy. 

# Cut off ends to fix end date ------

#Change if needed to a new end date 
Period_end_date <- ymd("2020/06/27")

All_periods.V1 <- All_periods.V1 %>% 
  filter(start_date <= Period_end_date) %>% 
  mutate(end_date = ifelse(end_date > Period_end_date, Period_end_date, end_date),
         end_date = as_date(end_date),
         interval = interval(start_date, end_date))




#Create a second period group which only - thishas not been done yet, need to do. guess to split into 2 sets, one where 1st may not in period and the 2nd in period. 
Periods_without_split <- All_periods.V1 %>% 
  select(- start,-end) 


Over_1st_may <- All_periods.V1 %>%
  filter(ymd("2020,05,01") %within% interval,
         start_date != ymd("2020,05,01"))


Over_1st_may.1 <- Over_1st_may %>% 
  as_tibble() %>% 
  mutate(start_date__2 = ymd("2020/05/01"),
         end_date__2 = end_date,
         start_date__1 = start_date,
         end_date__1 = ymd("2020/04/30")) %>% 
  select(-start_date, -end_date, - start,-end, - interval)
  
glimpse(Over_1st_may.1)

Over_1st_may.1 <- Over_1st_may.1 %>% 
  gather(Period,Date,- CiW_URN, - Group_ref, - Period_Start) %>% 
  separate(Period, into = c("Period","Cat"), sep = "__") %>% 
  spread(Period,Date) %>% 
  select(-Cat)


# Merge with rest of dataset 
Periods_with_split <- All_periods.V1 %>%
  filter(! ymd("2020,05,01") %within% interval | start_date == ymd("2020,05,01") ) %>% 
  select(- start,-end, - interval) %>% 
  bind_rows(Over_1st_may.1) %>% 
  mutate(interval = interval(start_date, end_date))
  
glimpse(Periods_with_split)
glimpse(Periods_without_split)

# Clean onset data

#Create a file which can be appended on to the Discharge data


  
  
  Onset  <- Homes_Master %>% 
    left_join(First_Case_in_carehome) %>% 
  select(CiW_URN,First_case) %>% 
  rename(Date = First_case) %>% 
  mutate(Date_type = "Outbreak") 

  All_dates <- Onset %>% 
  bind_rows(Discharge_data) %>% 
  filter(!is.na(Date)) %>% 
  mutate(temp = "temp")


# merge with periods list. each event should link with each period regardless of in the period or not.

  Periods_with_split_Dates <- Periods_with_split %>% 
    mutate(temp = "temp") %>% 
    full_join(All_dates, by = c("CiW_URN", "temp"))
  
  Periods_without_split_Dates <- Periods_without_split %>% 
    mutate(temp = "temp") %>% 
    full_join(All_dates, by = c("CiW_URN", "temp"))
  


# Check to see if an event happened within a given period and 
  #Create one line per preiod, with 1 if event or discharge took place
  
  Periods_with_split_Dates.1 <- Periods_with_split_Dates %>%
  mutate(
    Date2 = ifelse(Date_type == "Discharge",Date + Period_Start , Date),
    Date2 = as_date(Date2),
    Discharge = (Date2 %within% interval & Date_type == "Discharge"),
    Onset = (Date2 %within% interval & Date_type == "Outbreak")
  )  %>% 
  mutate_at(c("Discharge", "Onset"), replace_na, 0) %>% 
    group_by(CiW_URN,start_date, end_date,Period_Start,Group_ref) %>%
    summarise(Discharge = max(Discharge),
              Onset = max(Onset)) %>%
    ungroup()  %>% 
    mutate(With_split = T)
  
  
  Periods_without_split_Dates.1 <- Periods_without_split_Dates %>%
    mutate(
      Date2 = ifelse(Date_type == "Discharge",Date + Period_Start , Date),
      Date2 = as_date(Date2),
      Discharge = (Date2 %within% interval & Date_type == "Discharge"),
      Onset = (Date2 %within% interval & Date_type == "Outbreak")
    ) %>% 
    mutate_at(c("Discharge", "Onset"), replace_na, 0) %>% 
    group_by(CiW_URN,start_date, end_date,Period_Start,Group_ref) %>%
    summarise(Discharge = max(Discharge),
              Onset = max(Onset)) %>%
    ungroup() %>% 
    mutate(With_split = F)
  

  End_data <- bind_rows(Periods_without_split_Dates.1,Periods_with_split_Dates.1)

# Remove all data after an oubreak.
Final_Periods <- End_data %>%
  arrange(With_split,Group_ref,CiW_URN, start_date) %>%
  group_by(With_split,Group_ref,CiW_URN) %>%
  mutate(
    row = row_number(),
    outbreak = ifelse((Onset == 1), row, 0),
    keep = (row <= max(outbreak) | max(outbreak) == 0)
  )  %>%
  filter(keep == TRUE) %>%
  ungroup() %>%
  select(-row, -outbreak, -keep)


#Change the final date to be the date of 1st case if there is one 
Final_Periods <- Final_Periods %>%
  left_join(Onset) %>%
  mutate(
    end_date = ifelse(Onset == 1, Date, end_date),
    end_date = as_date(end_date)
  ) %>%
  select(-Date_type, -Date)

#Cut off sttarting period and re-adjust start and end numbers  

Start_date <- ymd("2020/03/01")

Final_Periods <- Final_Periods %>% 
  filter(end_date >= Start_date) %>% 
  mutate(start_date = ifelse(start_date <Start_date, Start_date, start_date),
         start_date = as_date(start_date))

#Calcuate start and end numbers 

Final_Periods <- Final_Periods %>% 
  mutate(start =  start_date  - Start_date,
         start = as.numeric(start),
         end = end_date - Start_date,
         end = as.numeric(end)) 


#Change start period to be plus one for continiutiy in regression model

Final_Periods <- Final_Periods %>%
  mutate(end = end + 1)
# Add home info ------


Care_home_info
glimpse(Care_home_info)

names <- names(Care_home_info)
names <- str_replace_all(names, " ", "")
names(Care_home_info) <- names

glimpse(Care_home_info)

Kept_varibles <- c(
  "CiW_URN",
  "ServiceSubType",
  "HealthBoard",
  "Provisionforlearningdisability",
  "MaxCapacity",
  "DementiaService"
  )




Home_data.a <- Care_home_info %>%
  select(Kept_varibles)

Home_data.a %>%
  semi_join(Final_Periods) %>%
  count(CiW_URN)  %>%
  filter(n > 1)
# Check that all names in epioidic data is in master list


Final_Data <- Final_Periods %>%
  left_join(Home_data.a, by = "CiW_URN")



# write final data


write_csv(Final_Data, "Care Home Data for cox regression_all_dataV2.csv")


#Tests  -----


# This code looks over the final dataset. 

# All care homes should only have the number of days to either an outbreak or the end of the study period accross all periods. this code checks this. i have found that some care homes have one less day then expected however when review i found this it to be an artifiact of R counting days...



Days <- Onset %>% 
  mutate(Date = ifelse(Date >  ymd("2020/06/27"), NA,Date),
         Date = as_date(Date),
         Date = replace_na(Date, ymd("2020/06/27")),
         days = Date - ymd("2020,03,01"),
         days = as.numeric(days))


Final_Data %>% 
  mutate(days2 = end_date - start_date + 1,
         days2 = as.numeric(days2)) %>% 
  group_by(CiW_URN,Group_ref,With_split) %>% 
  summarise(days2 = sum(days2)) %>% 
  ungroup() %>% 
  left_join(Days) %>% 
  mutate(diff = days - days2) %>% 
  filter(days != days2,
         diff != -1) %>% 
  count(CiW_URN)

Days %>% 
  filter(CiW_URN == "")

Final_Data %>% 
  select(With_split,Group_ref,CiW_URN,start_date,end_date,start,end,Discharge,Onset) %>% 
  mutate(temp = as.numeric(end_date - start_date) + 1) %>% 
  filter(CiW_URN == "",
         Group_ref == "Intervals_0_14") %>% 
  arrange(With_split,Start_date)
         
Final_Data %>% 
  filter(CiW_URN == "",
         Group_ref == "Intervals_0_14",
         With_split == F)