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
#' iapsr:::getPhase1Optimal(testDF, testValue[[1]], testID)
#'
#'

getPhase1Optimal <- function(df, value, ID) {

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
#' @param ID Character: Subject ID for whom we are determining preference.
#' @param posValue Logical: TRUE or FALSE signifying whether or not an individual
#'   prefers the neutral gray image to positive reinforcers.
#' @param negValue Logical: TRUE or FALSE signifying whether or not an individual
#'   prefers the neutral gray image to negative reinforcers.
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
#' #iapsr:::getPhase2Optimal(testDF, testValue[[1]], testID)
#'
getPhase2Optimal <- function(df, posValue, negValue, ID) {

  df <- df %>%
    dplyr::filter(phase == 2 & .data$subject == {{ ID }}) %>%
    dplyr::group_by(round) %>%
    tidyr::nest()

  optimalFunk <- function(df, posValue, negValue) {

    # If they they prefer gray to negatively reinforcing images, then it's optimal
    # to choose the icons that lead to neutral with the highest probability (leads
    # to reinforcers with the lowest probability).
    if (negValue & df$icon1Sign == "neg") {

      df <- df %>%
        dplyr::mutate(optimal = dplyr::case_when(.data$chosenIcon == .data$icon1 & .data$icon1Prob > .data$icon2Prob ~ 0,
                                                 .data$chosenIcon == .data$icon1 & .data$icon1Prob < .data$icon2Prob ~ 1,
                                                 .data$chosenIcon == .data$icon2 & .data$icon1Prob > .data$icon2Prob ~ 1,
                                                 .data$chosenIcon == .data$icon2 & .data$icon1Prob < .data$icon2Prob ~ 0))

      # If they they prefer negatively reinforcing images over gray, then it's optimal to
      # choose the icons that lead to them with the highest probability.
    } else if (!negValue & df$icon1Sign == "neg") {

      df <- df %>%
        dplyr::mutate(optimal = dplyr::case_when(.data$chosenIcon == .data$icon1 & .data$icon1Prob > .data$icon2Prob ~ 1,
                                                 .data$chosenIcon == .data$icon1 & .data$icon1Prob < .data$icon2Prob ~ 0,
                                                 .data$chosenIcon == .data$icon2 & .data$icon1Prob > .data$icon2Prob ~ 0,
                                                 .data$chosenIcon == .data$icon2 & .data$icon1Prob < .data$icon2Prob ~ 1))

    }

    if (posValue & df$icon1Sign == "pos") {

      df <- df %>%
        dplyr::mutate(optimal = dplyr::case_when(.data$chosenIcon == .data$icon1 & .data$icon1Prob > .data$icon2Prob ~ 0,
                                                 .data$chosenIcon == .data$icon1 & .data$icon1Prob < .data$icon2Prob ~ 1,
                                                 .data$chosenIcon == .data$icon2 & .data$icon1Prob > .data$icon2Prob ~ 1,
                                                 .data$chosenIcon == .data$icon2 & .data$icon1Prob < .data$icon2Prob ~ 0))

      #If thy don't prefer gray, then the optimal choice is to choose the icon
      #that has the highest probability of getting a reinforcer.
    } else if (!posValue & df$icon1Sign == "pos") {

      df <- df %>%
        dplyr::mutate(optimal = dplyr::case_when(.data$chosenIcon == .data$icon1 & .data$icon1Prob > .data$icon2Prob ~ 1,
                                                 .data$chosenIcon == .data$icon1 & .data$icon1Prob < .data$icon2Prob ~ 0,
                                                 .data$chosenIcon == .data$icon2 & .data$icon1Prob > .data$icon2Prob ~ 0,
                                                 .data$chosenIcon == .data$icon2 & .data$icon1Prob < .data$icon2Prob ~ 1))

    }

    return(df)

  }

  output <- purrr::map_df(df$data, ~optimalFunk(.x, posValue, negValue), .id = "round") %>%
    dplyr::mutate(round = base::as.numeric(round), .after = phase)

  return(output)

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
    dplyr::mutate(reinforcer_group = base::ifelse(.data$reinforcer == "neutral", "neutral", "reinforce_avg"))


  #For phase 1, get the mean rating of the positive images seen and the neutral image
  #then test to see if the person prefers gray over their average (positive) rating of the
  #reinforcing images they saw.
  phase1 <- data %>%
    dplyr::filter(.data$phase == 1) %>%
    dplyr::group_by(.data$subject, .data$reinforcer_group) %>%
    dplyr::summarize(meanPos = mean(.data$positive), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$reinforcer_group, values_from =.data$meanPos) %>%
    dplyr::mutate(preferstestgrayPositive = base::ifelse(.data$neutral > .data$reinforce_avg, TRUE, FALSE)) %>%
    dplyr::pull(.data$preferstestgrayPositive, .data$subject) %>%
    base::as.list()

  phase1Names <- base::names(phase1)

  #For phase 2, get the mean rating of the negative images seen and the neutral image.
  #then test to see if the person prefers gray over their average rating of the (negatively)
  #reinforcing images they saw.
  phase2 <- data %>%
    dplyr::filter(.data$phase == 2 & .data$icon1Sign == "pos") %>%
    dplyr::group_by(.data$subject, .data$reinforcer_group) %>%
    dplyr::summarize(meanPos = mean(.data$positive), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$reinforcer_group, values_from = .data$meanPos) %>%
    dplyr::mutate(prefersGrayPos = base::ifelse(.data$neutral > .data$reinforce_avg, TRUE, FALSE)) %>%
    dplyr::left_join(data %>%
                       dplyr::filter(.data$phase == 2 & .data$icon1Sign == "neg") %>%
                       dplyr::group_by(.data$subject, .data$reinforcer_group) %>%
                       dplyr::summarize(meanNeg = -mean(.data$negative), .groups = "drop") %>%
                       tidyr::pivot_wider(names_from = .data$reinforcer_group, values_from = .data$meanNeg) %>%
                       dplyr::mutate(prefersGrayNeg = base::ifelse(.data$neutral > .data$reinforce_avg, TRUE, FALSE)), by = "subject") %>%
    dplyr::rename(neutralPos = .data$neutral.x,
                  posReinforce = .data$reinforce_avg.x,
                  neutralNeg = .data$neutral.y,
                  negReinforce = .data$reinforce_avg.y)

  phase2Pos <- phase2 %>% dplyr::pull(.data$prefersGrayPos, .data$subject) %>% base::as.list()
  phase2Neg <- phase2 %>% dplyr::pull(.data$prefersGrayNeg, .data$subject) %>% base::as.list()

  #Could also use phase2Neg. Doesn't really matter.
  phase2Names <- base::names(phase2Pos)

  #For each subject, bind the phase 1 as passed through the
  #testgrayPos function with the testgrayNegValues phase 2 as passed through the testgrayNeg
  #function. Then bind the phase 3 data and sort it by subject and phase.
  output <- purrr::map2_df(.x = phase1, .y = phase1Names, ~getPhase1Optimal(df = data, value = .x, ID = .y)) %>%
    dplyr::bind_rows(purrr::pmap_df(list(phase2Pos, phase2Neg, phase2Names), ~getPhase2Optimal(df = data, posValue = ..1, negValue = ..2, ID = ..3))) %>%
    dplyr::bind_rows(data %>% dplyr::filter(.data$phase == 3)) %>%
    dplyr::arrange(.data$subject, .data$phase) %>%
    dplyr::select(-.data$reinforcer_group)

  return(output)

}



#' Plot the Percent of Optimal Choices for Each Subject
#'
#' This function is an adapted form of \code{\link{plotPercentOptimal}}. It
#' takes the output of \code{\link{examineChoices}}. It checks to see if the
#' data has been modified to include runningRound and percentOptimal and, if
#' not, does so. See examples for more detail.
#'
#' @param processedData The output of \code{\link{processChoiceData}}.
#' @param facet Logical: TRUE and plots will be faceted by subject. FALSE and it
#'   won't. Default is FALSE.
#' @param subjectName Optional argument to include a subject name if plotting
#'   for only one subject. Name must be in quotations.
#'
#' @importFrom rlang .data
#'
#' @return A plot showing the percent of optimal choices over time (round
#'   number), with each phase specified.
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
#' # The plotExamined can be used with the output of examineChoices that is
#' # modified:
#' examineChoices(choice, rate) %>%
#'  dplyr::group_by(subject, phase) %>%
#'  dplyr::mutate(percentOptimal = base::cumsum(optimal) / dplyr::row_number()) %>%
#'  dplyr::ungroup() %>%
#'  dplyr::group_by(subject) %>%
#'  dplyr::mutate(runningRound = dplyr::row_number()) %>%
#'  dplyr::ungroup() %>% plotExamined()
#'
#' # This function can be used with the output of examineChoices that is not
#' # modified and will modify it internally:
#'
#' examineChoices(choice, rate) %>% plotExamined()
#'
#'

plotExamined <- function(processedData, facet = FALSE, subjectName = NULL) {

  myGGTheme <- ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5, face = "bold.italic", size=16), #plot title aesthetics
                              plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "bold", size = 12), #plot subtitle aesthetics
                              axis.title.x = ggplot2::element_text(size = 12, color= "black", face = "bold"), #x axis title aesthetics
                              axis.title.y = ggplot2::element_text(size = 12, color= "black", face = "bold"), #y axis title aesthetics
                              axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 12),
                              #legend aesthetics
                              legend.title = ggplot2::element_text(size= 14,
                                                                   color = "black",
                                                                   face = "bold"),
                              legend.title.align = 0.5,
                              legend.text = ggplot2::element_text(size = 10,
                                                                  color = "black",
                                                                  face = "bold"),
                              legend.text.align = 0)

  #Check to see if the processedData was manipulated to have the running Round
  #and the percentOptimal column. If not, manipulate it. Also remove phase 3.

  if ("runningRound" %in% base::names(processedData) & "percentOptimal" %in% base::names(processedData)) {
    #remove phase 3 for this plot.
    plotData <- processedData %>% dplyr::filter(.data$phase != 3)
  } else if (!"runningRound" %in% base::names(processedData) & !"percentOptimal" %in% base::names(processedData)) {

    plotData <- processedData %>%
      dplyr::group_by(.data$subject, .data$phase) %>%
      dplyr:: mutate(percentOptimal = base::cumsum(.data$optimal) / dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$subject) %>%
      dplyr::mutate(runningRound = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$phase != 3)
  }





  phaseLabels <- tibble::tribble(~label,      ~x,             ~y,
                                 "Phase 1", 12.5, base::max(plotData$percentOptimal) + 0.05,
                                 "Phase 2", 50,   base::max(plotData$percentOptimal) + 0.05)


  #If only plotting for one subject, then subject column won't be in the
  #processedData. If the subjectName argument is defined, the subject column
  #will take that value. Else, it will just say "Sample".
  if (!"subject" %in% base::names(plotData)) {
    if (base::is.null(subjectName)) {
      plotData <- plotData %>% dplyr::mutate(subject = "Sample")
    } else if (!base::is.null(subjectName)) {
      plotData <- plotData %>% dplyr::mutate(subject = {{ subjectName }})
    }
  }

  #this is a tribble that contains the x and y coordinates for the end of each
  #line segment to be plotted on the discontinuous optimal choice plot.
  segmentDots <- plotData %>%
    dplyr::group_by(.data$subject) %>%
    dplyr::slice(c(1, 25, 26,75)) %>%
    dplyr::mutate(x = c(1, 25, 26, 75)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$subject, .data$x, y = .data$percentOptimal)

  plot <- plotData %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$runningRound, y = .data$percentOptimal)) +
    ggplot2::geom_line(ggplot2::aes(color = .data$subject, group = interaction(as.factor(.data$subject), as.factor(.data$phase))), size = 1) +
    ggplot2::geom_point(data = segmentDots, ggplot2::aes(x = .data$x, y = .data$y, color = .data$subject), fill = "white", size = 2, stroke = 1, shape = 21) +
    ggplot2::scale_y_continuous(labels = scales::percent, breaks = c(seq(0,1, by = 0.2))) +
    ggplot2::scale_x_continuous(breaks = c(0, 10, seq(25, 75, by = 10))) +
    ggplot2::labs(x = "Round Number",
                  y = "Percent of Optimal choices",
                  title = "Optimal Choices Over Time",
                  color = "Subject") +
    #geom_vline(xintercept = c(25, 75)) +
    myGGTheme +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"),
                   strip.background = ggplot2::element_rect(fill = "aliceblue")) +
    ggplot2::scale_size(guide = "none")#remove the size legend

  if(facet) {
    plot <- plot +
      #annotate the background for phase 1
      ggplot2::annotate(geom = "rect", xmin = 0, xmax = 25, ymin = -0.05, ymax = max(plotData$percentOptimal) + 0.1, fill = "green", alpha = 0.125) +
      #annotate the background for phase 2
      ggplot2::annotate(geom = "rect", xmin = 25, xmax = 77, ymin = -0.05, ymax = max(plotData$percentOptimal) + 0.1, fill = "blue", alpha = 0.125) +
      ggplot2::geom_text(data = phaseLabels,
                         mapping = ggplot2::aes(x = .data$x,
                                                y = .data$y,
                                                label = .data$label,
                                                fontface = "bold"),
                         size = 2,
                         show.legend = FALSE) +
      ggplot2::facet_wrap(~.data$subject) +
      ggplot2::theme(legend.position = "none")
  } else if (!facet) {
    plot <- plot +
      #annotate the background for phase 1
      ggplot2::annotate(geom = "rect", xmin = 0, xmax = 25, ymin = -0.05, ymax = max(plotData$percentOptimal) + 0.1, fill = "green", alpha = 0.125) +
      #annotate the background for phase 2
      ggplot2::annotate(geom = "rect", xmin = 25, xmax = 75.75, ymin = -0.05, ymax = max(plotData$percentOptimal) + 0.1, fill = "blue", alpha = 0.125) +
      ggplot2::geom_text(data = phaseLabels,
                         mapping = ggplot2::aes(x = .data$x,
                                                y = .data$y,
                                                label = .data$label,
                                                fontface = "bold"),
                         size = 4.5,
                         show.legend = FALSE)

  }

  return(plot)

}

