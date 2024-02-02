
# pull member monthly table from SF ---------------------------------------
# Imports---------------
library(DBI)
library(RODBC)
library(odbc)
library(curl)
library(httr)
library(RCurl)
library(tidyverse)
library(dbplyr)
library(lubridate)
library(purrr)

# Server Connection-------------
con <- dbConnect(odbc(), 
                 Driver = "SnowflakeDSIIDriver", 
                 Server = "usable-abi.privatelink.snowflakecomputing.com", 
                 authenticator = "externalbrowser",
                 Database = "SNOWFLAKE-PRD1-DS",
                 UID = "hwang@arkbluecross.com", 
                 PWD = "FillIn")
# specify role and warehouse---------------
DBI::dbGetQuery(con, 'use role CDP_PRD_DATASCIENTIST_FR')
DBI::dbGetQuery(con, 'use warehouse PRD_ML_STD_WH')
DBI::dbGetQuery(con, 'USE DATABASE PRD_DATASCIENCE_DB')
DBI::dbGetQuery(con,'USE SCHEMA HEALTHECON')


# Query -----------------
# Identify the table
MEMBER_WEEKLY_COMPLETE <- tbl(con, in_catalog('PRD_DATASCIENCE_DB','HEALTHECON','MEMBER_WEEKLY_COMPLETE'))


# Read in auxiliary table---------------
library(pins)
board <- board_connect()
single_pregnancies_2022 <- pin_read(board, "tvscanlon/Maternity_members_2022")

# round to week level---------------
# define the start of the week (set as Monday here)
single_pregnancies_2022 <- single_pregnancies_2022 %>% 
  filter(preg_count==1) %>% 
  mutate(cal_due_dt_RoundWeek= floor_date(cal_due_dt, "week", week_start=1),
         cal_concieved_dtRoundWeek= floor_date(cal_concieved_dt,"week", week_start=1)) %>% 
  select(MEMBERID,COMPANYKEY,cal_concieved_dt,cal_concieved_dtRoundWeek,cal_due_dt,cal_due_dt_RoundWeek)



  


# Apply criteria--------------
Maternity_weekly_data <- MEMBER_WEEKLY_COMPLETE %>% 
  right_join(single_pregnancies_2022, by='MEMBERID',copy = TRUE) %>% 
  group_by(MEMBERID) %>% 
  filter(WEEK_ROUND>= cal_concieved_dtRoundWeek & WEEK_ROUND<= cal_due_dt_RoundWeek)

# collect---------------
Maternity_weekly_data <- Maternity_weekly_data %>% collect()

missingMbrs <- setdiff(single_pregnancies_2022$MEMBERID,Maternity_weekly_data$MEMBERID) %>% as.data.frame()
write.csv(missingMbrs,file = 'rOutput/missingMbrs.csv')
# after merging with 'MEMBER_WEEKLY_COMPLETE', we found out there were 2277 missing members compare to 'single_pregnancies_2022'table. 
# further analysis found out eligibility table cause the issue!  checked with Joe. 


# quite using 'MEMBER_WEEKLY_COMPLETE' table, instead we are gonna to make another table only by 'MEMBER_WEEKLY_DIAG' & 'MEMBER_WEEKLY_VTD' tables
# by pass the eligibility table

MEMBER_WEEKLY_VTD <- tbl(con, in_catalog('PRD_DATASCIENCE_DB','HEALTHECON','MEMBER_WEEKLY_VTD'))
MEMBER_WEEKLY_DIAG <- tbl(con, in_catalog('PRD_DATASCIENCE_DB','HEALTHECON','MEMBER_WEEKLY_DIAG'))
MEMBER_WEEKLY_ENROLLMENT <- tbl(con, in_catalog('PRD_DATASCIENCE_DB','HEALTHECON','MEMBER_WEEKLY_ENROLLMENT'))



# full join
result_temp <- MEMBER_WEEKLY_VTD %>% 
  full_join(MEMBER_WEEKLY_DIAG, by=c('MEMBERID','WEEK_ROUND')) %>% 
  right_join(single_pregnancies_2022, by='MEMBERID', copy = TRUE) %>% 
  filter(WEEK_ROUND>= cal_concieved_dtRoundWeek & WEEK_ROUND<= cal_due_dt_RoundWeek)
result_temp <- result_temp %>% collect()



# impute missing weeks
complete_dates <- single_pregnancies_2022 %>% 
  filter(!is.na(cal_due_dt)) %>% 
  group_by(MEMBERID) %>% 
  reframe(start_week=cal_concieved_dtRoundWeek,end_week=cal_due_dt_RoundWeek) %>% 
  mutate(WEEK_ROUND= purrr::map2(start_week, end_week, seq, by='week')) %>% 
  unnest(cols = c(WEEK_ROUND)) %>% 
  ungroup() %>% 
  select(MEMBERID,WEEK_ROUND)

imputed_data <- complete_dates %>% 
  left_join(result_temp, by=c('MEMBERID','WEEK_ROUND'),copy = TRUE) %>% 
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), 0, .x))) %>% 
  group_by(MEMBERID) %>% 
  fill(cal_concieved_dtRoundWeek,
       cal_due_dt_RoundWeek) %>% 
  ungroup()
# This is the finally table we will pass 
imputed_data









 