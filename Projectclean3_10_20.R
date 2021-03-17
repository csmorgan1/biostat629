require(tidyverse)
require(fs)

file_paths <-
  dir_ls(
    "/nfs/turbo/umms-HDS629/MIPACT/HealthKit_Live/HealthKit_HeartRateVariability"
  )
files <- file_paths %>%
  map(function(path) {
    read_csv(
      path,
      col_types = cols(
        ParticipantResearchID = col_double(),
        Device = col_character(),
        HealthKitType = col_character(),
        StartDate = col_datetime(format = ""),
        Date = col_datetime(format = ""),
        Value = col_double(),
        Units = col_character(),
        SourceIdentifier = col_character(),
        SourceVersion = col_character(),
        DeviceModel = col_character(),
        DeviceManufacturer = col_character(),
        DeviceHardwareVersion = col_character(),
        DeviceSoftwareVersion = col_character()
      )
    )
  })

files_tbl <- files %>%
  set_names(
    dir_ls(
      "/nfs/turbo/umms-HDS629/MIPACT/HealthKit_Live/HealthKit_HeartRateVariability"
    )
  ) %>%
  bind_rows(.id = "file_path")



surveys <-
  dir_ls("/nfs/turbo/umms-HDS629/MIPACT/HealthKit_Live/Surveys")
surveys <- surveys %>%
  map(function(path) {
    read_csv(
      path,
      col_types = cols(
        ParticipantResearchID = col_double(),
        SurveyKey = col_character(),
        SurveyName = col_character(),
        SurveyVersion = col_double(),
        ResultIdentifier = col_character(),
        SurveyStepType = col_character(),
        SurveyTaskStatus = col_character(),
        SurveyStartDate = col_datetime(format = ""),
        SurveyEndDate = col_datetime(format = ""),
        SurveyDueDate = col_datetime(format = ""),
        SurveyQuestionStartDate = col_datetime(format = ""),
        SurveyQuestionEndDate = col_datetime(format = ""),
        SurveyQuestion = col_character(),
        SurveyAnswer = col_character()
        
      )
    )
  })

surveys_tbl <- surveys %>%
  set_names(dir_ls("/nfs/turbo/umms-HDS629/MIPACT/HealthKit_Live/Surveys")) %>%
  bind_rows(.id = "file_path")

anxiety_surveys <- surveys_tbl %>%
  filter(
    SurveyName == "Generalized Anxiety Disorder - Intake" |
      SurveyName == "Generalized Anxiety Disorder - Quarterly"
  )

anxiety_surveys <-
  anxiety_surveys[order(anxiety_surveys$ParticipantResearchID), ]

quart <-
  anxiety_surveys %>% 
  select(SurveyName, SurveyQuestion) %>% 
  filter(SurveyName == "Generalized Anxiety Disorder - Quarterly")

intake <- anxiety_surveys %>% 
  select(SurveyName,SurveyQuestion) %>% 
  filter(SurveyName=="Generalized Anxiety Disorder - Intake")

demo <- read_csv("/nfs/turbo/umms-HDS629/MIPACT/HealthKit_Live/EHR/EHR_Demographic_202010.csv")

full_tbl <-
  semi_join(files_tbl, anxiety_surveys, by = "ParticipantResearchID")
full_tbl <- full_tbl %>%
  mutate(DateJoin = as_date(StartDate))
full_tbl <- full_tbl[order(full_tbl$ParticipantResearchID), ]

full_tbl1 <- full_tbl %>%
  select(ParticipantResearchID, Date, DateJoin, Value)

anxiety_tbl <- anxiety_surveys %>%
  mutate(
    DateJoin = as_date(SurveyQuestionEndDate),
    SurveyAnswer = factor(SurveyAnswer)
  ) %>%
  dplyr::select(
    ParticipantResearchID,
    SurveyQuestionEndDate,
    SurveyQuestion,
    SurveyAnswer,
    DateJoin
  )

HRV_measures <-
  full_tbl1 %>% 
  group_by(ParticipantResearchID) %>% 
  summarise(mean_HRV = mean(Value), 
            median_HRV = median(Value))

intake_scores <- intake %>% 
  group_by(ParticipantResearchID) %>% 
  summarise(Score = sum(as.numeric(SurveyAnswer), na.rm = T),
            Questions = length(unique(SurveyQuestion)))

intake_scores <- intake_scores %>%
  mutate(Anxiety=ifelse(Score>=2*Questions,1,0))

quarterly_scores <- quart %>%
  group_by(ParticipantResearchID) %>%
  summarise(Score2 = sum(as.numeric(SurveyAnswer), na.rm = T),
            Questions = length(unique(SurveyQuestion)))

quarterly_scores <- quarterly_scores %>%
  mutate(Anxiety=ifelse(Score2>=2*Questions,1,0))

both_scores <- left_join(intake_scores,quarterly_scores,by="ParticipantResearchID")

both_scoress.1 <- both_scores.1 %>% mutate(Anxiety = ifelse(Anxiety.x==1&is.na(Anxiety.y),1,ifelse(Anxiety.x==1&Anxiety.y==1,1,0)))

temp_demo <- semi_join(demo,both_scores.1,by="ParticipantResearchID")
temp1 <- left_join(HRV_measure,temp_demo,by="ParticipantResearchID")
temp2 <- left_join(temp1,both_scores.1,by="ParticipantResearchID")

dat <- temp2 %>%
  select(ParticipantResearchID,HRV_mean,AgeAtEnrollment,GenderName,RaceName,Anxiety) %>%
  mutate(Anxiety=factor(Anxiety,labels = c("No","Yes")))

table(dat$Anxiety)

write.csv(dat,"dat.csv")