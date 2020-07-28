#' Get Data for Individual Rounds
#'
#' @param df The rawData dataframe.
#' @param roundNum The round number you wish to get data for.
#'
#'
#' @return A dataframe that has the data for that round
#'
isolateRound <- function(df, roundNum) {

  roundStr <- paste0("ROUND ", roundNum, "$")

  roundData <- df %>% filter(str_detect(.$data, roundStr))

  return(roundData)
}


rawDataRating <- readr::read_delim(file = 'IAPS_B.iaps_stream_rating.01.nosessionid.2020-07-16T10_27_19', delim = "\t",
                          col_names = c("time", "data"))

dataRating <- rawDataRating %>% select(data)

rawDataChoice <- readr::read_delim(file = 'scratch/IAPS_B.iaps_choice.01.nosessionid.2020-07-16T11_18_33', delim = "\t",
                                   col_names = c("time", "data"))

dataChoice <- rawDataChoice %>% select(data)


numRounds <- 242
vec <- seq(1,242)

sortedRounds <- map_df(vec, ~isolateRound(df = data, roundNum = .x), .id = "round")


startingTime <- data %>% filter(str_detect(.$Data, "starting game"))
endTime <- data %>% filter(str_detect(.$Data, "FINAL RATING SELECTION"))

roundIndicators <- data %>% filter(str_detect(.$data, "ROUND 1")) %>% separate(col = data, into = c("message", "round"), sep = ",")



r1 <- dataRating %>% isolateRound(1)
r1 %>% filter(str_detect(.$data, "Question")) %>% mutate(question = case_when(str_detect(.$data, "NEGATIVE") ~ "negative",
                                                                                 TRUE ~ "positive"))






question <- dataRating %>%
  filter(str_detect(.$data, "Question")) %>%
  transmute(question = if_else(condition = str_detect(.$data, "NEGATIVE"),
                            true = "negative",
                            false = "positive"))



picID <- dataRating %>%
  filter(str_detect(.$data, "Image")) %>%
  filter(!str_detect(.$data, "ROUND 0")) %>%
  separate(col = data,
           into = c("show", "image", "hyphen", "id", "round", "roundNumber"),
           extra = "drop",
           sep = " ",
           remove = FALSE) %>%
  select("id") %>%
  transmute(id = str_remove(.$id, ".jpg,"))


submitRating <- dataRating %>%
filter(str_detect(.$data, "SUBMIT")) %>%
filter(!str_detect(.$data, "ROUND 0")) %>%
  separate(col = data,
           into = c("submit", "rating", "wordValue", "value"),
           extra = "drop",
           sep = " ",
           remove = FALSE) %>%
  select(value) %>%
  mutate(value = str_remove(.$value, ","))


ratings <- bind_cols(picID, question, submitRating) %>%
  mutate(round = row_number())






getPhases(rawDataChoice)




# choice parsing ----------------------------------------------------------

dataChoice <- dataChoice %>%
  mutate(row = row_number())

#get phase start/end information
phase1Start <- dataChoice %>%
  filter(str_detect(.$data, "Phase 1 Group")) %>% select(row)
phase2Start <- dataChoice %>%
  filter(str_detect(.$data, "Phase 2 Group")) %>% select(row)
phase3Start <- dataChoice %>%
  filter(str_detect(.$data, "Phase 3 Group")) %>% select(row)
phase3End <- dataChoice %>%
  filter(str_detect(.$data, "CLEAR TRANSITION SCREEN")) %>%
  slice_tail() %>%
  select(row)

phase1 <- dataChoice %>%
  mutate(row = row_number()) %>%
  filter(between(row, phase1Start, phase2Start - 1)) #phase2start - 1 corresponds to the end of phase 1, round 25

phase2 <- dataChoice %>%
  mutate(row = row_number()) %>%
  filter(between(row, phase2Start, phase3Start - 1)) #phase3start - 1 corresponds to the end of phase 2,round 75

phase3 <- dataChoice %>%
  mutate(row = row_number()) %>%
  filter(between(row, phase3Start, endPhase3 - 1)) #endPhase3 - 1 corresponds to the final submit of phase 3, round 150.


#get icon
phase3 %>% filter(str_detect(.$data, "SHOW: OPTION SELECTION")) %>%
  separate(col = data,
           into = c("show", "image", "hyphen", "id", "round", "roundNumber"),
           extra = "drop",
           sep = " ",
           remove = FALSE) %>% select(roundNumber) %>%
  mutate(roundNumber = str_remove_all(.$roundNumber, "\"")) %>%
  mutate(roundNumber = str_remove_all(.$roundNumber, "\\{")) %>%
  mutate(roundNumber = str_remove_all(.$roundNumber, "\\}")) %>%
  separate(col = roundNumber, into = c("order1", "icon1", "icon2"),
           extra = "warn",
           remove = FALSE,
           sep = ":") %>%
  separate(col = icon1, into = c("icon1", "order2"),
           extra = "warn",
           remove = FALSE,
           sep = ",") %>%
  mutate(icon2 = str_remove_all(.$icon2, ",")) %>%
  select(icon1, icon2) %>%
  pivot_longer(cols = c(icon1, icon2),
              names_to = "order",
              values_to = "icon") %>%
  mutate(order = str_remove(.$order, "icon"),
         order = as.numeric(order))


#get choice
phase1 %>% filter(str_detect(.$data, "HIGHLIGHT CHOICE")) %>%
  mutate(data = str_remove_all(.$data, "HIGHLIGHT CHOICE: ")) %>%
  separate(col = data,
           into = c("wordOption", "option", "wordChoice", "choice", "wordGroup", "group", "roundWord", "round"),
           extra = "warn",
           remove = FALSE,
           sep = " ") %>%
  select(option, choice, group, round) %>%
  mutate(option = forcats::as_factor(str_remove_all(option, ",")),
         choice = forcats::as_factor(str_remove_all(choice, ",")),
         group = as_factor(str_remove_all(group, ",")),
         round = as.numeric(round))


getGroupRewardProbs <- function(phase) {

  #subset the phase into just the relevant data column to test for which phase it is.
  testPhase <- phase %>%
    slice(1) %>%
    select(-row)

  if (str_detect(testPhase, "Phase 1 Group")) {
    output <- phase %>%
      slice(1) %>%
      mutate(data = str_remove_all(.$data, "Phase 1 Group \\{Reward, Probability\\}: "))

  }else if (str_detect(testPhase, "Phase 2 Group")) {
    output <- phase %>%
      slice(1) %>%
      mutate(data = str_remove_all(.$data, "Phase 2 Group \\{Reward, Probability\\}: "))

  }else if (str_detect(testPhase, "Phase 3 Group")) {
    output <- phase %>%
      slice(1) %>%
      mutate(data = str_remove_all(.$data, "Phase 3 Group \\{Reward, Probability\\}: "))
  }

  output <- output %>%
    separate(col = data,
             into = c("1", "2", "3", "4", "5", "6"),
             extra = "warn",
             remove = TRUE,
             sep = "\\},") %>%
    select(-row) %>%
    mutate(`1` = str_remove(`1`, "Group 1 \\{\\$"),
           `2` = str_remove(`2`, "Group 2 \\{\\$"),
           `3` = str_remove(`3`, "Group 3 \\{\\$"),
           `4` = str_remove(`4`, "Group 4 \\{\\$"),
           `5` = str_remove(`5`, "Group 5 \\{\\$"),
           `6` = str_remove(`6`, "Group 6 \\{\\$")) %>%
    pivot_longer(cols = everything(),
                 names_to = "group",
                 values_to = "info") %>%
    mutate(info = str_trim(info),
           info = str_remove_all(info, ","),
           info = str_remove_all(info, "\\}"),
           info = str_remove_all(info, "%")) %>%
    separate(col = info,
             into = c("reward", "probability"),
             extra = "warn",
             remove = TRUE,
             sep = " ") %>%
    mutate(group = forcats::as_factor(group),
           probability = as.numeric(probability),
           probability = probability/100) %>%
    drop_na()

  return(output)
}



getPhases(rawDataChoice)

data <- rawDataChoice %>%
  dplyr::select(data) %>%
  dplyr::mutate(row = dplyr::row_number())

start1 <- data %>%
  dplyr::filter(stringr::str_detect(.$data, "Phase 1 Group")) %>%
  dplyr::select(row)

start2 <- data %>%
  dplyr::filter(stringr::str_detect(.$data, "Phase 2 Group")) %>%
  dplyr::select(row)

start3 <- data %>%
  dplyr::filter(stringr::str_detect(.$data, "Phase 3 Group")) %>%
  dplyr::select(row)

data %>%
  dplyr::filter(stringr::str_detect(.$data, "CLEAR TRANSITION SCREEN")) %>%
  utils::tail(1) %>%
  dplyr::select(row)


end3 <- data %>%
  dplyr::filter(stringr::str_detect(.$data, "CLEAR TRANSITION SCREEN")) %>%
  dplyr::slice_tail() %>%
  dplyr::select(row)

phase$one <- data %>%
  dplyr::filter(dplyr::between(row, start1, start2 - 1)) %>%
  dplyr::mutate(phase = rep(1))

phase$two <- data %>%
  dplyr::filter(dplyr::between(row, start2, start3 - 1)) %>%
  dplyr::mutate(phase = rep(2))

phase$three <- data %>%
  dplyr::filter(dplyr::between(row, start3, end3 - 1)) %>%
  dplyr::mutate(phase = rep(3))





#get choice
phase3 %>% filter(str_detect(.$data, "SHOW: image")) %>%
  transmute(data = str_remove_all(.$data, "SHOW: image - ")) %>%
  separate(col = data,
           into = c("image", "round"),
           extra = "warn",
           remove = FALSE,
           sep = " , ") %>%
  select(image) %>%
  mutate(image = stringr::str_remove(image, ".jpg"))




rawDataChoice %>% processChoiceData()


phase$one







# process Rating data stuff -----------------------------------------------


yada <- iapsr::processRatingsData(iapsr::readRatings('scratch/IAPS_B.iaps_stream_rating.01.nosessionid.2020-07-16T10_27_19'))







