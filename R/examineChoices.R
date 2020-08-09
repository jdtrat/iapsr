
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
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' \dontrun{
#'
#'
#'
#'
#' choiced <- processChoiceData(sampleChoiceData) %>%
#'   mutate(subject = "RJT", .before = phase)
#'
#' rated <- processRatingsData(sampleRatingsData) %>%
#'   regSetup() %>%
#'   mutate(subject = "RJT", .before = IAPS)
#'
#' testDF <- left_join(choiced,
#' rated %>% dplyr::rename(reinforcer = IAPS),
#' by = c("subject", "reinforcer")) %>%
#' dplyr::filter(reinforcer != "7006") %>%
#'dplyr::mutate(reinforcer =
#'base::ifelse(reinforcer == "neutral", "neutral", "reinforcer"))
#'
#' testValue <- testDF %>%
#'   filter(phase == 1) %>%
#'   group_by(subject, reinforcer) %>%
#'   summarize(meanPos = mean(positive), .groups = "drop") %>%
#'   pivot_wider(names_from = reinforcer, values_from = meanPos) %>%
#'   mutate(prefersGrayPositive = ifelse(neutral > reinforcer, TRUE, FALSE)) %>%
#'   pull(prefersGrayPositive, subject) %>%
#'   as.list()
#'
#' testID <- names(testValue)
#'
#' grayPos(testDF, testValue[[1]], testID)
#'
#' }

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
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#'
#'
#' choiced <- processChoiceData(sampleChoiceData) %>%
#'   mutate(subject = "RJT", .before = phase)
#'
#' rated <- processRatingsData(sampleRatingsData) %>%
#'   regSetup() %>%
#'   mutate(subject = "RJT", .before = IAPS)
#'
#' testDF <- left_join(choiced, rated %>%
#' dplyr::rename(reinforcer = IAPS),
#' by = c("subject", "reinforcer")) %>%
#'  dplyr::filter(reinforcer != "7006") %>%
#'  dplyr::mutate(reinforcer =
#'  base::ifelse(reinforcer == "neutral", "neutral", "reinforcer"))
#'
#' testValue <- testDF %>%
#'   filter(phase == 1) %>%
#'   group_by(subject, reinforcer) %>%
#'   summarize(meanNeg = -mean(negative), .groups = "drop") %>%
#'   pivot_wider(names_from = reinforcer, values_from = meanNeg) %>%
#'   mutate(prefersGrayNegative = ifelse(neutral > reinforcer, TRUE, FALSE)) %>%
#'   pull(prefersGrayNegative, subject) %>%
#'   as.list()
#'
#' testID <- names(testValue)
#'
#' grayNeg(testDF, testValue[[1]], testID)
#'
#' }
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
#' \dontrun{
#'
#' choice <- processChoiceData(sampleChoiceData)
#' rate <- processRatingsData(sampleRatingsData)
#'
#' examineChoices(choice, rate) }
#'
examineChoices <- function(choiceData, ratingsData) {

  data <- dplyr::left_join(choiceData, ratingsData %>%
                             dplyr::rename(reinforcer = .data$IAPS),
                           by = c("subject", "reinforcer")) %>%
    dplyr::filter(.data$reinforcer != "7006") %>%
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
