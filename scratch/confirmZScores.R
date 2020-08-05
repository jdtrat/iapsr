# Try to recreate pos ratings ---------------------------------------------



lm(valmn ~ positive + negative, data = finalData) %>% predict(finalData) %>% bind_cols(finalData) %>%
  rename(predict = `...1`) %>%
  arrange(valmn)


read_csv('~/Downloads/TotalDataOutput.csv') %>%
  rename(mean = Positive_DataMeans.Means) %>%
  arrange(mean) %>%
  mutate(IAPS = as.character(IAPS)) %>%
  select(IAPS, mean) %>%
  mutate(row = row_number()) %>% left_join(finalData %>% group_by(IAPS) %>% mutate(mean = mean(positive)) %>% count(mean) %>% arrange(mean) %>% ungroup() %>% select(IAPS, mean) %>% mutate(row = row_number()), by = "IAPS") %>%
  arrange(desc(mean.x), desc(mean.y)) %>% mutate(equal = ifelse(row.x == row.y, TRUE, FALSE)) %>% View()

read_csv('~/Desktop/OneDrive - Wake Forest University/Research/Kishida Lab/SubjectiveFeelingAssessment_MMink/SFAdata_unscanned/Positive_Data_withMeans.csv') %>%
  select(-c(X43, Means, `Standard Deviation`)) %>%
  mutate(mean = rowMeans(., X1:X48)) %>%
  arrange(mean) %>%
  pull(mean)

read_csv('~/Desktop/OneDrive - Wake Forest University/Research/Kishida Lab/SubjectiveFeelingAssessment_MMink/SFAdata_unscanned/Positive_Data_withMeans.csv') %>%
  arrange(Means) %>%
  #mutate(Means = round(Means, 4)) %>%
  pull(Means)



all_equal(read_csv('~/Downloads/TotalDataOutput.csv') %>%
            rename(mean = Positive_DataMeans.Means) %>%
            arrange(mean) %>%
            mutate(IAPS = as.character(IAPS)) %>%
            select(IAPS),
          finalData %>% group_by(IAPS) %>% mutate(mean = mean(positive)) %>% count(mean) %>% arrange(mean) %>% select(IAPS, mean) %>% mutate(row = row_number())

finalData %>% group_by(IAPS) %>% mutate(mean = mean(positive)) %>% count(mean) %>% arrange(mean) %>% select(IAPS, mean) %>%
  left_join(read_csv('~/Downloads/TotalDataOutput.csv') %>%
              rename(posDataMean = Positive_DataMeans.Means) %>%
              mutate(IAPS = as.character(IAPS)) %>%
              arrange(posDataMean) %>%
              select(IAPS, posDataMean)) %>%
  mutate(diff = (mean - posDataMean)) %>%
  ggplot(aes(mean, posDataMean)) +
  geom_line() +
  geom_smooth(method = "lm") +
  gglmannotate::geom_lmannotate()




read_csv('~/Desktop/OneDrive - Wake Forest University/Research/Kishida Lab/SubjectiveFeelingAssessment_MMink/SFAdata_unscanned/Positive_Data.csv') %>%
  select(-X1)


finalData %>%
  group_by(IAPS) %>%
  summarize(mean = mean(positive)) %>%
  arrange(mean)



# Z Score Stuff -----------------------------------------------------------

#load totalDataOutput and add a z score column, arrange in from most positive to negatie of Z Scores
totalDataOutput <- read_csv('~/Downloads/TotalDataOutput.csv') %>%
  mutate(posZScore = ((Positive_DataMeans.Means - mean(Positive_DataMeans.Means)) / sd(Positive_DataMeans.Means))) %>%
  arrange(desc(posZScore)) %>%
  select(-X1) %>%
  mutate(row = row_number())


# Create closest function -------------------------------------------------

#' Get Row Closest to a Value
#'
#' This function returns the row number of the variable closest to the number you supply.
#'
#' @param data The dataframe
#' @param variable The column variable name whose value you are looking to pull the closest row of.
#' @param number The number to match the closest value
#'
#' @return
#' @export
#'
getClosest <- function(data, variable, number) {

  if(!"row" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(row = dplyr::row_number())
  }

  row <- data %>%
    dplyr::filter(base::abs({{ variable }} - (number) ) == base::min(base::abs( {{ variable}} - (number) ))) %>%
    dplyr::pull(row)

  if(length(row) > 1) {
    row <- row[2]
  }

  return(row)
}



# Calculate Z Scores ------------------------------------------------------

#Get a function to return the IAPS classes. If meanValence = FALSE it will do so as Angela did based on the mean positive ratings.
# if meanValence = TRUE, it will do so based on the valmn.
getClasses <- function(data, meanValence = FALSE) {

  if (meanValence) {
    data <- data %>%
      arrange(desc(valmn))
  }

  #Calculate Z scores based on original PRPT payment structure
  zScores <- tribble(~monetary, -1.25, -1.00, -0.75, -0.25, 0.50, 1.00, 1.50, 2.50) %>%
    mutate(zScore = (monetary - mean(monetary)) / sd(monetary),
           class = as.character(monetary)) %>%
    select(zScore, class)

  zScorez <- as.list(zScores$zScore) #make list to use map function
  names(zScorez) <- zScores$class

  #do some unmappable ones by hand.
  pos2.5 <- slice_head(data, n = 15) %>% mutate(class = "2.50")
  neg1.25 <- slice_tail(data, n = 15) %>% mutate(class = "-1.25")
  pos1 <- data %>%
    filter(between(row, getClosest(data, variable = posZScore, number = zScorez$`1`) - 20,
                   getClosest(data, variable = posZScore, number = zScorez$`1`) + 19)) %>%
    mutate(class = "1.00")
  neg1 <- data %>%
    filter(between(row, getClosest(data, variable = posZScore, number = zScorez$`-1`) - 20,
                   getClosest(data, variable = posZScore, number = zScorez$`-1`) + 19)) %>%
    mutate(class = "-1.00")

  #get most of them via the map function
  most <- map(zScorez[c(3:5,7)], ~getClosest(data, posZScore, .x), .id = "class") %>%
    map_df(~ data %>% filter(between(row, .x - 10, .x + 9)), .id = "class")

  # create IAPS classes object
  IAPSClasses <- bind_rows(pos2.5,
                           neg1.25,
                           pos1,
                           neg1,
                           most) %>%
    mutate(IAPS = as.character(IAPS),
           class = case_when(class == "1.5" ~ "1.50",
                             class == "0.5" ~ "0.50",
                             TRUE ~ class))

  return(IAPSClasses)

}

#Function to get IAPS classes used by Angela
getAngelaClasses <- function() {
  two50 <- data.frame(IAPS = c("5210.jpg", "1440.jpg", "1722.jpg", "2154.jpg", "1460.jpg", "2150.jpg", "1441.jpg",
                               "2655.jpg", "5830.jpg", "1710.jpg", "5833.jpg", "5825.jpg", "2340.jpg", "1750.jpg", "5760.jpg")) %>%
    mutate(class = "2.50")

  one50 <- data.frame(IAPS = c("4533.jpg", "7461.jpg", "8178.jpg", "7476.jpg", "8490.jpg", "2070.jpg", "8190.jpg",
                               "4220.jpg", "7352.jpg", "1510.jpg", "2080.jpg", "7402.jpg", "4598.jpg", "5991.jpg",
                               "2050.jpg", "2260.jpg", "2250.jpg", "8492.jpg", "1650.jpg", "5626.jpg")) %>%
    mutate(class = "1.50")


  one <- data.frame(IAPS = c("4180.jpg",  "4533.jpg",  "4232.jpg",  "4141.jpg",  "7461.jpg",  "8179.jpg",  "8178.jpg",
                             "7476.jpg", "8490.jpg",  "2070.jpg",  "8190.jpg",  "4220.jpg",  "7352.jpg",  "1510.jpg",
                             "2150.jpg",  "2080.jpg",  "4008.jpg", "4001.jpg",  "2393.jpg",  "4000.jpg",  "7402.jpg",
                             "1640.jpg",  "4598.jpg",  "5991.jpg",  "2050.jpg",  "5830.jpg", "2040.jpg",  "4561.jpg",
                             "4085.jpg",  "4235.jpg",  "4510.jpg",  "4664.1.jpg",  "4002.jpg",
                             "4142.jpg",  "2260.jpg", "2250.jpg", "8492.jpg", "1650.jpg", "3310.jpg", "5626.jpg")) %>%
    mutate(class = "1.00")


  fifty <- data.frame(IAPS = c("4180.jpg", "4232.jpg", "4141.jpg", "8179.jpg", "2512.jpg", "3302.jpg", "4770.jpg",
                               "4008.jpg", "4001.jpg", "2393.jpg", "4000.jpg", "4561.jpg", "4085.jpg", "4235.jpg",
                               "4510.jpg", "1560.jpg", "4002.jpg", "4142.jpg", "2525.jpg", "3310.jpg")) %>%
    mutate(class = "0.50")


  negTwenty5 <- data.frame(IAPS = c("6910.jpg", "7211.jpg","2493.jpg", "1050.jpg", "4230.jpg", "1931.jpg", "8466.jpg",
                                    "7006.jpg", "7038.jpg", "4302.jpg", "2026.jpg", "7491.jpg", "7175.jpg", "8475.jpg",
                                    "5534.jpg", "7217.jpg", "6570.2.jpg", "5535.jpg", "1030.jpg", "2190.jpg")) %>%
    mutate(class = "-0.25")


  negSeventy5 <- data.frame(IAPS = c("9480.jpg", "9582.jpg", "1070.jpg", "1040.jpg", "9410.jpg", "7184.jpg", "2493.jpg",
                                     "1022.jpg", "3360.jpg", "1050.jpg", "7006.jpg", "7038.jpg", "1080.jpg", "2730.jpg",
                                     "9005.jpg", "7491.jpg", "5534.jpg", "1030.jpg", "6010.jpg", "5972.jpg")) %>%
    mutate(class = "-0.75")


  negOne <- data.frame(IAPS = c("9480.jpg", "9582.jpg", "1070.jpg", "1040.jpg", "3120.jpg", "3266.jpg", "3000.jpg",
                                "3015.jpg", "9410.jpg", "6910.jpg", "7184.jpg", "2493.jpg", "1022.jpg", "3360.jpg",
                                "1050.jpg", "3100.jpg", "3053.jpg", "7006.jpg", "7038.jpg", "1080.jpg", "2730.jpg",
                                "9321.jpg", "9005.jpg", "4302.jpg", "2026.jpg", "7491.jpg", "7175.jpg", "3168.jpg",
                                "9040.jpg", "3170.jpg", "3064.jpg", "9075.jpg", "3130.jpg", "5534.jpg", "6570.2.jpg",
                                "1030.jpg", "6010.jpg", "6550.jpg", "5972.jpg", "3005.1.jpg")) %>%
    mutate(class = "-1.00")


  negOne25 <- data.frame(IAPS = c("3266.jpg", "3015.jpg", "3100.jpg", "3080.jpg", "3063.jpg", "3053.jpg", "3131.jpg",
                                  "9940.jpg", "3005.1.jpg", "3001.jpg", "3168.jpg", "3102.jpg", "3064.jpg", "3130.jpg", "9075.jpg")) %>%
    mutate(class = "-1.25")

  angelaIAPSClasses <- bind_rows(two50, one50, one, fifty, negTwenty5, negSeventy5, negOne, negOne25) %>%
    mutate(IAPS = str_remove(IAPS, ".jpg")) %>% tibble()

  return(angelaIAPSClasses)
}



# Compare IAPS classes ----------------------------------------------------


#Test to see which images are in Angela's classes but not ours
getAngelaClasses() %>% anti_join(getClasses(totalDataOutput))

#Test to see which images are in our classes but not Angela's
getClasses(totalDataOutput) %>% anti_join(getAngelaClasses())

#Test to see which images are in Angela's classes but not ours
#when organized by mean valence instead of mean positive ratings
getAngelaClasses() %>% anti_join(getClasses(totalDataOutput, meanValence = TRUE))

#Test to see which images are in our classes but not Angela's
#when organized by mean valence instead of mean positive ratings
getClasses(totalDataOutput, meanValence = TRUE) %>% anti_join(getAngelaClasses())



#Get the mean valence for each class using the valmn outcome to arrange the classes.
valRatingMeans <- getClasses(totalDataOutput, meanValence = TRUE) %>% group_by(class) %>% summarize(meanValmn = mean(valmn))

#Get the mean valence for each class using the posRating outcome to arrange the classes.
posRatingMeans <- getClasses(totalDataOutput, meanValence = FALSE) %>% group_by(class) %>% summarize(meanPosmn = mean(valmn))

#Combine the different class mean valences and look at the differences.
#meanValmn is the mean valence for each class using the valmn outcome
#meanPosmn is the mean valence for each class using the posRating outcome.
diffRatingSchemes <- left_join(valRatingMeans, posRatingMeans) %>%
  mutate(diff = meanValmn - meanPosmn)
