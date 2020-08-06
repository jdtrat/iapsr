
#Create a phase environment
phase <- base::new.env(parent = base::emptyenv())

#' Read Choice Data
#'
#' @param filePath The path to the ratings data file. Should be in single quotes.
#'
#' @return The raw data downloaded from the task. It is a data frame with two columns, time and data, and 1875 observations.
#' @export
#'
readChoices <- function(filePath) {

  ratingData <- readr::read_delim(file = filePath,
                                  delim = "\t",
                                  col_names = c("time", "data"))
  return(ratingData)
}

#' Get Task Phases
#'
#' This function takes the data column from the output of
#' \code{\link{readChoices}} and separates the three phases of the task, saving
#' them to the environment \code{phrase}, which is initialized behind the
#' scenes. It also separates the initial icon rating phase and the final icon
#' rating phase. This function \strong{must} be run in order to use the other
#' functions that parse the choice data. If it is not, the phases will need to
#' be isolated by hand and assigned to the phase environment (or local/global
#' variables). See the \emph{Details} section for more how to access the three
#' phases.
#'
#' The task and icon rating phases may be accessed by calling the variables:
#' \itemize{ \item \code{phase$one} \item \code{phase$two} \item
#' \code{phase$three} \item \code{phase$initIcon} \item \code{phase$finalIcon}}
#'
#' @importFrom rlang .data
#' @param data The data output from \code{\link{readChoices}}
#'
#' @export
#'

getPhases <- function(data) {

  data <- data %>%
    dplyr::select(.data$data) %>%
    dplyr::mutate(row = dplyr::row_number())

  startInitIcon <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "starting game")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  endInitIcon <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "SHOW: ICON END TRANSITION SCREEN")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  start1 <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "Phase 1 Group")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  start2 <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "Phase 2 Group")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  start3 <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "Phase 3 Group")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  end3 <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "CLEAR TRANSITION SCREEN")) %>%
    utils::tail(1) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  startFinalIcon <- end3

  endFinalIcon <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "Experiment Complete:")) %>%
    dplyr::select(.data$row) %>%
    dplyr::pull(.data$row)

  phase$initIcon <- data %>%
    dplyr::filter(dplyr::between(.data$row, startInitIcon, endInitIcon - 1))

  phase$one <- data %>%
    dplyr::filter(dplyr::between(.data$row, start1, start2 - 1)) %>%
    dplyr::mutate(phase = rep(1))

  phase$two <- data %>%
    dplyr::filter(dplyr::between(.data$row, start2, start3 - 1)) %>%
    dplyr::mutate(phase = rep(2))

  phase$three <- data %>%
    dplyr::filter(dplyr::between(.data$row, start3, end3 - 1)) %>%
    dplyr::mutate(phase = rep(3))

  phase$finalIcon <- data %>%
    dplyr::filter(dplyr::between(.data$row, startFinalIcon, endFinalIcon - 1))

}

#' Get Icons for Each Round
#'
#' This gets the icons for each round in each phase.
#'
#' @param phaseData One of the three phase objects defined by calling \code{\link{getPhases}}.
#'
#' @param pivot Logical: pivoting the data makes it tidier, but makes it harder
#'   to join with the other information. For this reason, it is being set to
#'   FALSE by default.
#'
#' @return Returns a dataframe with four columns:
#' \itemize{
#' \item \emph{icon1:} The icon presented in the first order position.
#' \item \emph{icon2:} The icon presented in the second order position.
#' \item \emph{phase:} The phase these icons were displayed in (constant for each \code{phase} object).
#' \item \emph{round:} The round number these icons were displayed in, specific to the phase.
#' }
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{getIcons(phase$one)}

getIcons <- function(phaseData, pivot = FALSE) {

  icons <- phaseData %>%
    dplyr::filter(stringr::str_detect(.data$data, "SHOW: OPTION SELECTION")) %>%
    tidyr::separate(col = .data$data,
             into = c("show", "option", "selection", "icon", "order", "icon_group"),
             extra = "drop",
             sep = " ",
             remove = TRUE) %>%
    dplyr::select(.data$icon_group, phase) %>%
    dplyr::mutate(icon_group = stringr::str_remove_all(.data$icon_group, "\"")) %>%
    dplyr::mutate(icon_group = stringr::str_remove_all(.data$icon_group, "\\{")) %>%
    dplyr::mutate(icon_group = stringr::str_remove_all(.data$icon_group, "\\}")) %>%
    tidyr::separate(col = .data$icon_group, into = c("order1", "icon1", "icon2"),
             extra = "warn",
             remove = FALSE,
             sep = ":") %>%
    tidyr::separate(col = .data$icon1, into = c("icon1", "order2"),
             extra = "warn",
             remove = FALSE,
             sep = ",") %>%
    dplyr::mutate(icon2 = stringr::str_remove_all(.data$icon2, ",")) %>%
    dplyr::select(.data$icon1, .data$icon2, phase) %>%
    dplyr::mutate(round = dplyr::row_number())

  if(pivot) {
    icons <- icons %>%
      tidyr::pivot_longer(cols = c(.data$icon1, .data$icon2),
                   names_to = "image_order",
                   values_to = "icon") %>%
      dplyr::mutate(image_order = stringr::str_remove(.data$image_order, "icon"),
             image_order = base::as.numeric(.data$image_order))

  }

  return(icons)

}

#' Get Choices for Each Round
#'
#' This function gets the choices for each round for each phase.
#'
#' @param phaseData One of the three phase objects defined by calling \code{\link{getPhases}}.
#'
#' @return Returns a dataframe with five columns:
#' \itemize{
#' \item \emph{option:} The option the subject chose.
#' This should correspond to the icon order as presented in the task.
#' \item \emph{choice:} The icon the subject chose.
#' \item \emph{group:} The (payment weighting) group the icon corresponds to.
#' \item \emph{phase:} The phase the choices were made in (constant for each \code{phase} object).
#' \item \emph{round:} The round number these choices were made in, specific to the phase.
#' }
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{getChoices(phase$two)}

getChoices <- function(phaseData) {

  choices <- phaseData %>%
    dplyr::filter(stringr::str_detect(.data$data, "HIGHLIGHT CHOICE")) %>%
    dplyr::mutate(data = stringr::str_remove_all(.data$data, "HIGHLIGHT CHOICE: ")) %>%
    tidyr::separate(col = .data$data,
             into = c("wordOption", "option", "wordChoice", "choice", "wordGroup", "group", "roundWord", "round"),
             extra = "warn",
             remove = FALSE,
             sep = " ") %>%
    dplyr::select(.data$option, .data$choice, .data$group, .data$round, .data$phase) %>%
    dplyr::mutate(option = forcats::as_factor(stringr::str_remove_all(.data$option, ",")),
           choice = forcats::as_factor(stringr::str_remove_all(.data$choice, ",")),
           group = forcats::as_factor(stringr::str_remove_all(.data$group, ",")),
           round = dplyr::row_number())

  return(choices)
}

#' Get Images Shown for Each Round
#'
#' This function gets the images shown for each round in each phase.
#'
#' @param phaseData One of the three phase objects defined by calling \code{\link{getPhases}}.
#'
#' #' @return Returns a dataframe with three columns:
#' \itemize{
#' \item \emph{image:} The image the subject was shown after making a choice.
#' \item \emph{phase:} The phase the choices were made in (constant for each \code{phase} object).
#' \item \emph{round:} The round number these choices were made in, specific to the phase.
#' }
#' @export
#' @importFrom rlang .data
#'
#' @examples
#'
#' \dontrun{getImageShown(phase$three)}

getImageShown <- function(phaseData){

  output <- phaseData %>%
    dplyr::filter(stringr::str_detect(.data$data, "SHOW: image")) %>%
    dplyr::mutate(data = stringr::str_remove_all(.data$data, "SHOW: image - ")) %>%
    tidyr::separate(col = .data$data,
             into = c("image", "round"),
             extra = "warn",
             remove = FALSE,
             sep = " , ") %>%
    dplyr::select(.data$image, phase) %>%
    dplyr::mutate(image = stringr::str_remove(.data$image, ".jpg"),
           round = dplyr::row_number())

  return(output)

}

#' Process Choice Data
#'
#' This function takes the data column from the output of
#' \code{\link{readChoices}} and runs five functions, binding the results in a
#' dataframe for easy manipulation: \itemize{ \item \code{\link{getPhases}}
#' which separates the three phases of the task, saving them to the environment
#' \code{phrase}, which is initialized behind the scenes. \item
#' \code{\link{getGroupInfo}} which returns a dataframe of the icons and the
#' payment weighting group they correspond to in each phase. It has the expected
#' utility and relative ranks of choosing each option. \item \code{\link{getIcons}}
#' which gets the icon options shown to the subject. \item
#' \code{\link{getChoices}} which gets the choice the subject makes. \item
#' \code{\link{getImageShown}} which gets the images the subject is shown after
#' choosing an icon.}
#'
#'
#' @param data The data output from \code{\link{readChoices}}
#'
#' @return A dataframe 150 rows and 13 columns: \itemize{ \item \emph{phase:}
#'   The phase the choices were made in. \item \emph{round:} The round number
#'   these choices were made in, specific to the phase. \item
#'   \emph{runningRound:} The round number these choices were made in (1-150),
#'   independent of phase. This is useful for plotting the percent of optimal
#'   choices over time. \item \emph{icon1:} The icon presented in the first
#'   order position. \item \emph{rank1:} The rank of icon1. The higher this is,
#'   the higher the expected utility of choosing icon1. \item \emph{icon2:} The
#'   icon presented in the second order position. \item \emph{rank2:} The rank
#'   of icon2. The higher this is, the higher the expected utility of choosing
#'   icon2.\item \emph{option:} The option the subject chose. This should
#'   correspond to the icon order as presented in the task. \item \emph{choice:}
#'   The icon the subject chose. \item \emph{group:} The (payment weighting)
#'   group the chosen icon corresponds to. \item \emph{image:} The image the
#'   subject was shown after making a choice. \item \emph{optimal:} A binary
#'   variable tracking whether or not the choice was optimal. This is 1 if the
#'   rank of the icon chosen is higher than the rank of the one not chosen. It
#'   is 0 otherwise. The ranks are based on the expected utility for each icon.
#'   See \code{\link{getGroupInfo}} for that information. \item
#'   \emph{percentOptimal:} The cumulative sum of optimal choices divided by the
#'   total round (1-150).}
#'
#' @importFrom rlang .data
#'
#' @export

processChoiceData <- function(data){

  getPhases(data)
  groupKey <- getGroupInfo(data)
  phases <- list(phase$one, phase$two, phase$three)

  icons <- purrr::map_df(phases, ~getIcons(.x))
  choices <- purrr::map_df(phases, ~getChoices(.x))
  images <- purrr::map_df(phases, ~getImageShown(.x))


  combined <- dplyr::full_join(icons, choices, by = c("phase", "round")) %>%
    dplyr::full_join(images, by = c("phase", "round")) %>%
    dplyr::select(phase, round, dplyr::everything())

  #join the ranks with icon1
  icon1 <- combined %>%
    dplyr::rename(icon = .data$icon1) %>%
    dplyr::left_join(groupKey, by = c("phase", "icon")) %>%
    dplyr::rename(icon1 = .data$icon,
           rank1 =.data$rank) %>%
    dplyr::select(-c(.data$group.y, .data$reward, .data$probability, .data$EU)) %>%
    dplyr::relocate(.data$rank1, .after = .data$icon1)

  #join the ranks with icon2
  icon2 <- combined %>%
    dplyr::rename(icon = icon2) %>%
    dplyr::left_join(groupKey, by = c("phase", "icon")) %>%
    dplyr::rename(icon2 = .data$icon,
           rank2 = .data$rank) %>%
    dplyr::select(-c(.data$group.y, .data$reward, .data$probability, .data$EU)) %>%
    dplyr::relocate(.data$rank2, .after = .data$icon2)

  #rejoin the icon1 and icon2 rankings together, add optimal and percentOptimal
  #columns.
  together <- dplyr::full_join(icon1, icon2,
                               by = c("phase", "round", "icon1",
                                      "icon2", "option", "choice",
                                      "group.x", "image")) %>%
    dplyr::rename(group = .data$group.x) %>%
    dplyr::relocate(.data$rank2, .after = .data$icon2) %>%
    dplyr::mutate(optimal = dplyr::case_when(option == 1 & .data$rank1 > .data$rank2 ~ 1,
                                             option == 1 & .data$rank1 < .data$rank2 ~ 0,
                                             option == 2 & .data$rank1 > .data$rank2 ~ 0,
                                             option == 2 & .data$rank1 < .data$rank2 ~ 1),
                  #percent optimal is the cumulative sum of optimal divided by the number of total rounds
                  percentOptimal = base::cumsum(.data$optimal) / dplyr::row_number(),
                  #create a total round column for plotting purposes
                  runningRound = dplyr::row_number()) %>%
    dplyr::relocate(.data$runningRound, .after = round)

  return(together)
}


#' Get One Phase's Group Reward and Probability Info
#'
#' @param phaseData One of the three phase objects defined by calling
#'   \code{\link{getPhases}}.
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with four columns. It has the reward and probability of
#'   receiving it as a function of each image's group per phase. This can be
#'   compared to the output of \code{\link{getChoices}} or
#'   \code{\link{processChoiceData}}. This function is called by
#'   \code{\link{getGroupInfo}} which combines the information for all three of
#'   the phases.

getGroupRewardProbs <- function(phaseData) {

  #subset the phaseData into just the relevant data column to test for which phase it is.
  testPhase <- phaseData %>%
    dplyr::slice(1) %>%
    dplyr::select(-c(row, phase))

  if (stringr::str_detect(testPhase, "Phase 1 Group")) {
    output <- phaseData %>%
      dplyr::slice(1) %>%
      dplyr::mutate(data = stringr::str_remove_all(.data$data, "Phase 1 Group \\{Reward, Probability\\}: "))

  }else if (stringr::str_detect(testPhase, "Phase 2 Group")) {
    output <- phaseData %>%
      dplyr::slice(1) %>%
      dplyr::mutate(data = stringr::str_remove_all(.data$data, "Phase 2 Group \\{Reward, Probability\\}: "))

  }else if (stringr::str_detect(testPhase, "Phase 3 Group")) {
    output <- phaseData %>%
      dplyr::slice(1) %>%
      dplyr::mutate(data = stringr::str_remove_all(.data$data, "Phase 3 Group \\{Reward, Probability\\}: "))
  }

  output <- output %>%
    tidyr::separate(col = .data$data,
                    into = c("1", "2", "3", "4", "5", "6"),
                    extra = "warn",
                    remove = TRUE,
                    sep = "\\},") %>%
    dplyr::select(-row) %>%
    dplyr::mutate(`1` = stringr::str_remove(.data$`1`, "Group 1 \\{\\$"),
                  `2` = stringr::str_remove(.data$`2`, "Group 2 \\{\\$"),
                  `3` = stringr::str_remove(.data$`3`, "Group 3 \\{\\$"),
                  `4` = stringr::str_remove(.data$`4`, "Group 4 \\{\\$"),
                  `5` = stringr::str_remove(.data$`5`, "Group 5 \\{\\$"),
                  `6` = stringr::str_remove(.data$`6`, "Group 6 \\{\\$")) %>%
    tidyr::pivot_longer(cols = -.data$phase,
                        names_to = "group",
                        values_to = "info") %>%
    dplyr::mutate(info = stringr::str_trim(.data$info),
                  info = stringr::str_remove_all(.data$info, ","),
                  info = stringr::str_remove_all(.data$info, "\\}"),
                  info = stringr::str_remove_all(.data$info, "%")) %>%
    tidyr::separate(col = .data$info,
                    into = c("reward", "probability"),
                    extra = "warn",
                    remove = TRUE,
                    sep = " ") %>%
    dplyr::mutate(group = forcats::as_factor(.data$group),
                  probability = base::as.numeric(.data$probability),
                  probability = .data$probability/100) %>%
    tidyr::drop_na()

  return(output)
}

#' Get Group Reward and Probability Info for All Phases
#'
#' @param data The data output from \code{\link{readChoices}}
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with 15 rows and seven columns. It has the reward amount
#'   and probability of receiving a reward as a function of each image's group
#'   per phase. This also has the icon to group mapping for the subject. Most
#'   importantly, it has the EU and rank columns. These are used to determine
#'   optimal choice when measuring behavioral performance. This can be used in
#'   conjunction with the output of \code{\link{getChoices}} or
#'   \code{\link{processChoiceData}}.
#' @export

getGroupInfo <- function(data) {

  getPhases(data)
  phases <- list(phase$one, phase$two, phase$three)

  iconGroupMaps <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "ICON TO GROUP MAPPING: ")) %>%
    dplyr::select(-.data$time) %>%
    dplyr::mutate(data = stringr::str_remove(.data$data, "ICON TO GROUP MAPPING:"),
                  data = stringr::str_remove(.data$data, "\\{"),
                  data = stringr::str_remove(.data$data, "\\}")) %>%
    tidyr::separate(col = .data$data,
                    into = c("icon1", "icon2", "icon3", "icon4", "icon5", "icon6"),
                    sep = "\\,") %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "iconNum") %>%
    dplyr::mutate(value = stringr::str_remove_all(.data$value, "\\\"")) %>%
    tidyr::separate(.data$value,
                    into = c("icon", "group"),
                    sep = "\\:") %>%
    dplyr::arrange(.data$group) %>%
    dplyr::transmute(icon = stringr::str_trim(.data$icon),
                     group = base::as.factor(.data$group))

  #warnings suppressed due to there only being three groups in phase 1. It throws a warning but it doesn't matter.
  output <- base::suppressWarnings(dplyr::full_join(iconGroupMaps, purrr::map_df(phases, ~getGroupRewardProbs(.x)), by = "group")) %>%
    dplyr::arrange(phase) %>%
    dplyr::select(phase, .data$icon, .data$group, .data$reward, .data$probability) %>%
    dplyr::mutate(EU = base::as.numeric(.data$reward) * .data$probability) %>%
    dplyr::group_by(phase) %>%
    dplyr::mutate(rank = dplyr::min_rank(.data$EU))

  return(output)

}


#' Plot the Percent of Optimal Choices for Each Subject
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

plotPercentOptimal <- function(processedData, facet = FALSE, subjectName = NULL) {

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

  phaseLabels <- tibble::tribble(~label,      ~x,             ~y,
                                 "Phase 1", 12.5, base::max(processedData$percentOptimal) + 0.05,
                                 "Phase 2", 50,   base::max(processedData$percentOptimal) + 0.05,
                                 "Phase 3", 115,  base::max(processedData$percentOptimal) + 0.05)


  #If only plotting for one subject, then subject column won't be in the
  #processedData. If the subjectName argument is defined, the subject column
  #will take that value. Else, it will just say "Sample".
  if (!"subject" %in% base::names(processedData)) {
    if (base::is.null(subjectName)) {
      processedData <- processedData %>% dplyr::mutate(subject = "Sample")
    } else if (!base::is.null(subjectName)) {
      processedData <- processedData %>% dplyr::mutate(subject = {{ subjectName }})
    }
  }

  plot <- ggplot2::ggplot(processedData, ggplot2::aes(x = .data$runningRound, y = .data$percentOptimal)) +
    ggplot2::geom_line(ggplot2::aes(color = .data$subject), size = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent, breaks = c(seq(0,1, by = 0.2))) +
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
        ggplot2::annotate(geom = "rect", xmin = 0, xmax = 25, ymin = -0.05, ymax = max(processedData$percentOptimal) + 0.1, fill = "green", alpha = 0.125) +
        #annotate the background for phase 2
        ggplot2::annotate(geom = "rect", xmin = 25, xmax = 75, ymin = -0.05, ymax = max(processedData$percentOptimal) + 0.1, fill = "blue", alpha = 0.125) +
        #annotate the background for phase 3
        ggplot2::annotate(geom = "rect", xmin = 75, xmax = 150, ymin = -0.05, ymax = max(processedData$percentOptimal) + 0.1, fill = "pink", alpha = 0.125) +
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
        ggplot2::annotate(geom = "rect", xmin = 0, xmax = 25, ymin = -0.05, ymax = max(processedData$percentOptimal) + 0.1, fill = "green", alpha = 0.125) +
        #annotate the background for phase 2
        ggplot2::annotate(geom = "rect", xmin = 25, xmax = 75, ymin = -0.05, ymax = max(processedData$percentOptimal) + 0.1, fill = "blue", alpha = 0.125) +
        #annotate the background for phase 3
        ggplot2::annotate(geom = "rect", xmin = 75, xmax = 150, ymin = -0.05, ymax = max(processedData$percentOptimal) + 0.1, fill = "pink", alpha = 0.125) +
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



