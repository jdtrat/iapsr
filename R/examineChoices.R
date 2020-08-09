#' Determine if the neutral image is preferred to positive reinforcers
#'
#' This function takes in the data frame that contains the subject choice and
#' rating data as well as a TRUE/FALSE value signifying whether the subject
#' prefers the neutral gray image over the positive reinforcers as measured by
#' the average rating of the positive reinforcers showed and the rating for the
#' neutral image. It then mutates the data frame with an optimal column based on
#' their preference. Specifically, if an individual does prefer gray, then the
#' optimal choice is to choose icons that have the lowest probability of getting
#' a reinforcer. If they don't prefer gray, then the optimal choice is to choose
#' icons that have the highest probability of getting a reinforcer.
#'
#' @param df The data frame that contains the subject choice and rating data
#'   (see examples).
#' @param value Logical: TRUE or FALSE signifying whether or not an individual
#'   prefers the neutral gray image to positive reinforcers.
#' @param ID Character: Subject ID for whom we are determining preference.
#'
#' @return A data frame consisting of the phase 1 information for the specified
#'   subject. It has, in addition to the choice and rating information for a
#'   subject, an optimal column based on the subject's preference for the
#'   neutral image relative to the reinforcers.
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#'
#' choiced <- processChoiceData(sampleChoiceData) %>%
#'   dplyr::mutate(subject = "RJT", .before = phase)
#'
#' rated <- processRatingsData(sampleRatingsData) %>%
#'   regSetup() %>%
#'   dplyr::mutate(subject = "RJT", .before = IAPS)
#'
#' testDF <- dplyr::left_join(choiced,
#' rated %>% dplyr::rename(reinforcer = IAPS),
#' by = c("subject", "reinforcer")) %>%
#' dplyr::filter(reinforcer != "7006") %>%
#' dplyr::mutate(reinforcer =
#' base::ifelse(reinforcer == "neutral", "neutral", "reinforcer"))
#'
#' testValue <- testDF %>%
#'   dplyr::filter(phase == 1) %>%
#'   dplyr::group_by(subject, reinforcer) %>%
#'   dplyr::summarize(meanPos = mean(positive), .groups = "drop") %>%
#'   tidyr::pivot_wider(names_from = reinforcer, values_from = meanPos) %>%
#'   dplyr::mutate(prefersGrayPositive = base::ifelse(neutral > reinforcer, TRUE, FALSE)) %>%
#'   dplyr::pull(prefersGrayPositive, subject) %>%
#'   base::as.list()
#'
#' testID <- base::names(testValue)
#'
#' iapsr:::grayPos(testDF, testValue[[1]], testID)
#'
#'

grayPos <- function(df, value, ID) {

  #if they do prefer gray, then the optimal choice is to choose the icon that
  #has the lowest probability of getting a reinforcer.
  if (value) {

    phase1 <- df %>%
      dplyr::filter(.data$phase == 1 & .data$subject == {{ ID }}) %>%
      dplyr::mutate(optimal = dplyr::case_when(.data$chosenIcon == .data$icon1 & .data$icon1Prob > .data$icon2Prob ~ 0,
                                               .data$chosenIcon == .data$icon1 & .data$icon1Prob < .data$icon2Prob ~ 1,
                                               .data$chosenIcon == .data$icon2 & .data$icon1Prob > .data$icon2Prob ~ 1,
                                               .data$chosenIcon == .data$icon2 & .data$icon1Prob < .data$icon2Prob ~ 0))

    #If thy don't prefer gray, then the optimal choice is to choose the icon
    #that has the highest probability of getting a reinforcer.
  } else if (!value) {

    phase1 <- df %>%
      dplyr::filter(.data$phase == 1 & .data$subject == {{ ID }}) %>%
      dplyr::mutate(optimal = dplyr::case_when(.data$chosenIcon == .data$icon1 & .data$icon1Prob > .data$icon2Prob ~ 1,
                                               .data$chosenIcon == .data$icon1 & .data$icon1Prob < .data$icon2Prob ~ 0,
                                               .data$chosenIcon == .data$icon2 & .data$icon1Prob > .data$icon2Prob ~ 0,
                                               .data$chosenIcon == .data$icon2 & .data$icon1Prob < .data$icon2Prob ~ 1))

  }

  return(phase1)

}

#' Determine if the neutral image is preferred to negative reinforcers
#'
#'
#' This function takes in the data frame that contains the subject choice and
#' rating data as well as a TRUE/FALSE value signifying whether the subject
#' prefers the neutral gray image over the negative reinforcers as measured by
#' the average rating of the negative reinforcers showed and the rating for the
#' neutral image. It then mutates the data frame with an optimal column based on
#' their preference. Specifically, if an individual does prefer gray, then the
#' optimal choice is to choose icons that have the lowest probability of getting
#' a reinforcer. If they don't prefer gray, then the optimal choice is to choose
#' icons that have the highest probability of getting a reinforcer.
#'
#' @param df The data frame that contains the subject choice and rating data
#'   (see examples).
#' @param value Logical: TRUE or FALSE signifying whether or not an individual
#'   prefers the neutral gray image to negative reinforcers.
#' @param ID Character: Subject ID for whom we are determining preference.
#'
#' @return A data frame consisting of the phase 2 information for the specified
#'   subject. It has, in addition to the choice and rating information for a
#'   subject, an optimal column based on the subject's preference for the
#'   neutral image relative to the reinforcers.
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' choiced <- processChoiceData(sampleChoiceData) %>%
#'   dplyr::mutate(subject = "RJT", .before = phase)
#'
#' rated <- processRatingsData(sampleRatingsData) %>%
#'   regSetup() %>%
#'   dplyr::mutate(subject = "RJT", .before = IAPS)
#'
#' testDF <- dplyr::left_join(choiced, rated %>%
#' dplyr::rename(reinforcer = IAPS),
#' by = c("subject", "reinforcer")) %>%
#'  dplyr::filter(reinforcer != "7006") %>%
#'  dplyr::mutate(reinforcer =
#'  base::ifelse(reinforcer == "neutral", "neutral", "reinforcer"))
#'
#' testValue <- testDF %>%
#'   dplyr::filter(phase == 1) %>%
#'   dplyr::group_by(subject, reinforcer) %>%
#'   dplyr::summarize(meanNeg = -mean(negative), .groups = "drop") %>%
#'   tidyr::pivot_wider(names_from = reinforcer, values_from = meanNeg) %>%
#'   dplyr::mutate(prefersGrayNegative = base::ifelse(neutral > reinforcer, TRUE, FALSE)) %>%
#'   dplyr::pull(prefersGrayNegative, subject) %>%
#'   base::as.list()
#'
#' testID <- base::names(testValue)
#'
#' iapsr:::grayNeg(testDF, testValue[[1]], testID)
#'
grayNeg <- function(df, value, ID) {

  # If they they prefer gray to negatively reinforcing images, then it's optimal
  # to choose the icons that lead to neutral with the highest probability (leads
  # to reinforcers with the lowest probability).
  if (value) {

    phase2 <- df %>%
      dplyr::filter(phase == 2 & .data$subject == {{ ID }}) %>%
      dplyr::mutate(optimal = dplyr::case_when(.data$chosenIcon == .data$icon1 & .data$icon1Prob > .data$icon2Prob ~ 0,
                                               .data$chosenIcon == .data$icon1 & .data$icon1Prob < .data$icon2Prob ~ 1,
                                               .data$chosenIcon == .data$icon2 & .data$icon1Prob > .data$icon2Prob ~ 1,
                                               .data$chosenIcon == .data$icon2 & .data$icon1Prob < .data$icon2Prob ~ 0))

    # If they they prefer negatively reinforcing images over gray, then it's optimal to
    # choose the icons that lead to them with the highest probability.
  } else if (!value) {

    phase2 <- df %>%
      dplyr:: filter(phase == 2 & .data$subject == {{ ID }}) %>%
      dplyr::mutate(optimal = dplyr::case_when(.data$chosenIcon == .data$icon1 & .data$icon1Prob > .data$icon2Prob ~ 1,
                                               .data$chosenIcon == .data$icon1 & .data$icon1Prob < .data$icon2Prob ~ 0,
                                               .data$chosenIcon == .data$icon2 & .data$icon1Prob > .data$icon2Prob ~ 0,
                                               .data$chosenIcon == .data$icon2 & .data$icon1Prob < .data$icon2Prob ~ 1))

  }

  return(phase2)

}

#' Examine subjects' choice preferences
#'
#'
#' This function takes individuals' IAPS ratings and choice task performance and
#' returns an optimal column describing whether their choice for a given round
#' was optimal given their preference for the neutral image relative to the
#' reinforcers used. It manipulates the data to get the mean rating of the
#' positive images (phase 1) and negative images (phase 2) that each subject saw
#' and the neutral image rating and then tests to see if the person prefers gray
#' over their average rating of the reinforcing images they saw during phase 1
#' and 2. It then calculates an optimal column based on each subjects'
#' preference for the neutral image relative to the reinforcers in phase 1 and
#' 2.
#'
#' Note that both the choice columns and data columns must have a subject
#' identifier, so ensure that there is a subject column with a unique ID.
#'
#' @param choiceData The output of \code{\link{processChoiceData}} on one or
#'   more subjects.
#' @param ratingsData The output of \code{\link{processRatingsData}} on one or
#'   more subjects.
#'
#' @return A data frame consisting of the phase 1, 2, and 3 information for each
#'   subject. In addition to the choice and rating information for each subject,
#'   it contains an optimal column based on the subject's preference for the
#'   neutral image relative to the reinforcers for phase 1 and 2. Phase 3's
#'   optimal column is filled with NAs.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' choice <- processChoiceData(sampleChoiceData) %>%
#' dplyr::mutate(subject = "RJT", .before = phase)
#'
#' rate <- processRatingsData(sampleRatingsData) %>%
#' regSetup() %>%
#' dplyr::mutate(subject = "RJT", .before = IAPS)
#'
#' examineChoices(choice, rate)
#'
examineChoices <- function(choiceData, ratingsData) {

  data <- dplyr::left_join(choiceData, ratingsData %>%
                             dplyr::rename(reinforcer = .data$IAPS),
                           by = c("subject", "reinforcer")) %>%
    #this function is mutating the positive and negative rows whenever the
    #reinforcing image was 7006. The values used are the average positive
    #and negative ratings from the original cohort's rating of image 7006.
    #new cohort subjects were not asked to rate this. The code for this is:
    #ratingsOldCohort %>% filter(IAPS == "7006") %>%
    #summarize(Pos = mean(positive), Neg = mean(negative))
    mutate_rows(.data$reinforcer == "7006", positive = 2.630435, negative = 1.73913) %>%
    dplyr::mutate(reinforcer = base::ifelse(.data$reinforcer == "neutral", "neutral", "reinforcer"))


  #For phase 1, get the mean rating of the positive images seen and the neutral image
  #then test to see if the person prefers gray over their average (positive) rating of the
  #reinforcing images they saw.
  grayPosValues <- data %>%
    dplyr::filter(.data$phase == 1) %>%
    dplyr::group_by(.data$subject, .data$reinforcer) %>%
    dplyr::summarize(meanPos = mean(.data$positive), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$reinforcer, values_from =.data$ meanPos) %>%
    dplyr::mutate(prefersGrayPositive = base::ifelse(.data$neutral > .data$reinforcer, TRUE, FALSE)) %>%
    dplyr::pull(.data$prefersGrayPositive, .data$subject) %>%
    base::as.list()

  grayPosNames <- base::names(grayPosValues)

  #For phase 2, get the mean rating of the negative images seen and the neutral image.
  #then test to see if the person prefers gray over their average rating of the (negatively)
  #reinforcing images they saw.

  grayNegValues <- data %>%
    dplyr::filter(.data$phase == 2) %>%
    dplyr::group_by(.data$subject, .data$reinforcer) %>%
    dplyr::summarize(meanNeg = -mean(.data$negative), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$reinforcer, values_from = .data$meanNeg) %>%
    dplyr::mutate(prefersGrayNegative = base::ifelse(.data$neutral > .data$reinforcer, TRUE, FALSE)) %>%
    dplyr::pull(.data$prefersGrayNegative, .data$subject) %>%
    base::as.list()

  grayNegNames <- base::names(grayNegValues)

  #For each subject, bind the grayPosValues phase 1 as passed through the
  #grayPos function with the grayNegValues phase 2 as passed through the grayNeg
  #function. Then bind the phase 3 data and sort it by subject and phase.
  output <- purrr::map2_df(.x = grayPosValues, .y = grayPosNames, ~grayPos(df = data, value = .x, ID = .y)) %>%
    dplyr::bind_rows(purrr::map2_df(.x = grayNegValues, .y = grayNegNames, ~grayNeg(df = data, value = .x, ID = .y))) %>%
    dplyr::bind_rows(data %>% dplyr::filter(.data$phase == 3)) %>%
    dplyr::arrange(.data$subject, .data$phase)

  return(output)

}
