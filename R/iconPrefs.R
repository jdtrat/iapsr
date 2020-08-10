#' Get Icon Preferences Before and After Choice Task
#'
#' This function calls \code{\link{getPhases}} and uses the objects
#' phase$initIcon and phase$finalIcon to determine proportion of icons an
#' individual chose.
#'
#' @param choiceData The output of \code{\link{readChoices}} on one or
#'   more subjects.
#'
#' @importFrom rlang .data
#'
#' @return A data frame consisting of 72 rows and 10 columns showing the round
#'   number, icon options and choice. Additionally, there is a survey column
#'   which specifies whether or not the icon preference was before or after the
#'   choice task.
#'
getIconPrefs <- function(choiceData) {

  getPhases(choiceData)

  #This function returns the icon choices for both the initial and final icon preference surveys.
  getIconChoices <- function(df) {

    df %>%
    dplyr::filter(stringr::str_detect(.data$data, "KEYPRESS") & stringr::str_detect(.data$data, "OPTION: ")) %>%
      tidyr::separate(col = .data$data,
                      into = c("wordKEYPRESS", "NUMBER", "wordOption", "option", "wordChoice", "choice", "wordGroup", "group", "wordIcon", "wordRound", "roundNumber"),
                      extra = "drop",
                      sep = " ",
                      remove = FALSE) %>%
      dplyr::select(.data$option, .data$choice, .data$group, .data$roundNumber) %>%
      dplyr::mutate(option = forcats::as_factor(stringr::str_remove_all(.data$option, ",")),
                    choice = forcats::as_factor(stringr::str_remove_all(.data$choice, ",")),
                    group = forcats::as_factor(stringr::str_remove_all(.data$group, ",")),
                    roundNumber = base::as.numeric(.data$roundNumber))


  }

  #this function returns the icon options for both the initial and final icon preference surveys.
  getIconIcons <- function(df) {

    df %>%
    dplyr::filter(stringr::str_detect(.data$data, "SHOW: ICON SELECTION")) %>%
      tidyr::separate(col = .data$data,
                      into = c("a", "b", "c", "d", "e", "iconStuff", "g", "h", "roundNumber"),
                      extra = "drop",
                      remove = FALSE,
                      sep = " ") %>% dplyr::select(.data$iconStuff, .data$roundNumber) %>%
      dplyr::mutate(iconStuff = stringr::str_remove_all(.data$iconStuff, "\"")) %>%
      dplyr::mutate(iconStuff = stringr::str_remove_all(.data$iconStuff, "\\{")) %>%
      dplyr::mutate(iconStuff = stringr::str_remove_all(.data$iconStuff, "\\}")) %>%
      tidyr::separate(col = .data$iconStuff, into = c("order1", "icon1", "icon2"),
                      extra = "drop",
                      remove = FALSE,
                      sep = ":") %>%
      tidyr::separate(col = .data$icon1, into = c("icon1", "order2"),
                      extra = "drop",
                      remove = FALSE,
                      sep = ",") %>%
      dplyr::select(-c(.data$iconStuff)) %>%
      dplyr::relocate(1,3,4,2) %>%
      dplyr::mutate(icon2 = forcats::as_factor(stringr::str_remove_all(.data$icon2, ",")),
                    order1 = base::as.numeric(.data$order1),
                    order2 = base::as.numeric(.data$order2),
                    roundNumber = base::as.numeric(.data$roundNumber))
  }

  #get choice for initial icon
  choiceInit <- phase$initIcon %>% getIconChoices()

  #get icon for initial icon
  iconInit <- phase$initIcon %>% getIconIcons()


  #combine for initIcon
  finalInit <- dplyr::full_join(choiceInit, iconInit, by = "roundNumber") %>%
    dplyr::relocate(4, 5, 6, 7, 8, 1, 2, 3)

  #get choice for finalIcon
  choiceFinal <- phase$finalIcon %>% getIconChoices()

  #get icon for finalIcon
  iconFinal <- phase$finalIcon %>% getIconIcons()

  #conbine finalIcon
  finalFinal <- dplyr::full_join(choiceFinal, iconFinal, by = "roundNumber") %>%
    dplyr::relocate(4, 5, 6, 7, 8, 1, 2, 3)

  iconChoices <-
    finalInit %>% dplyr::mutate(survey = "initial") %>%
    dplyr::bind_rows(finalFinal %>% dplyr::mutate(survey = "final"))

  if (!"subject" %in% base::names(iconChoices)) {
    iconChoices <- iconChoices %>%
      dplyr::mutate(subject = "RJT", .before = .data$roundNumber)
  }

  return(iconChoices)

}

#' Which icons were presented?
#'
#' This function returns the unique values of an icon for a given subject.
#'
#' @param df The iconChoices data frame as output by the \code{\link{getIconPrefs}} function.
#'
#' @return A list with the different icons an individual had to choose from.
#'
#'

getIconOptions <- function(df) {

  options <- df %>%
    dplyr::distinct(.data$icon1) %>%
    dplyr::pull(.data$icon1) %>%
    base::as.list()

  base::names(options) <- df %>%
    dplyr::distinct(.data$icon1) %>%
    dplyr::pull(.data$icon1)

  return(options)

}

#' Summarize Icon Choices
#'
#' This function gets the initial or final proportion of times an icon is chosen
#' by an individual.
#'
#' @param df The iconChoices data frame as output from
#'   \code{\link{getIconPrefs}}.
#' @param icon One of the icons in the list as output by the function
#'   \code{\link{getIconOptions}}.
#' @param type A character string "initial" or "final" signifying which icon
#'   choice summary to get.
#' @importFrom rlang :=
#'
#' @return A data frame with two rows and three columns showing the subject, the
#'   side the icon was displayed on, and the proportion the image was chosen
#'   during either the initial survey or final survey (depending on the type).
#'
summarizeIconChoices <- function(df, icon, type) {

  if (type != "initial" & type != "final") {
    stop("Invalid type provided. It must be a character string of either \"initial\" or \"final.\"")
  }

  name <- names(icon)

  df %>%
    dplyr::filter(.data$survey == {{ type }}) %>%
    dplyr::filter(.data$icon1 == {{ icon }} | .data$icon2 == {{ icon }}) %>%
    dplyr::arrange(.data$icon1) %>%
    #accounting for laterality. The first two rows of this say that if both icon
    #options are equal, and the left is chosen (option 1), then make side left. If
    #right is chosen (option 2), then make side right.
    dplyr::mutate(side = dplyr::case_when(.data$icon1 == .data$icon2 & .data$option == 1 ~ "left",
                                          .data$icon1 == .data$icon2  & .data$option == 2 ~ "right",
                                          .data$icon1 == {{ icon }} ~ "left",
                                          .data$icon2 == {{ icon }} ~ "right",
                                          TRUE ~ "error")) %>%
    dplyr::group_by(.data$subject, .data$side) %>%
    dplyr::mutate(iconChosen = .data$choice == {{ icon }}) %>%
    dplyr::summarize("{{ type }}_prop_{{ name }}" := (base::sum(.data$choice == {{ icon }}) / dplyr::n()), .groups = "drop") %>%
    janitor::clean_names()

}

#' Get the difference in icon selection
#'
#' This function gets the difference in the proportions of icon selection during
#' the initial and final rounds for each subject with a unique combination of
#' icons.
#'
#' @param choiceData The output of \code{\link{readChoices}}.
#' @param side Logical: TRUE and the difference will be categorized by the side
#'   the icon appeared on the screen. FALSE and it won't be. Default is FALSE.
#'
#' @return A data frame with three columns containing subject, the icons they
#'   saw, and the difference between their initial preference for a given icon
#'   (as a proportion of the times they chose it when available) and their final
#'   icon preference.
#' @export
#'

getIconDiffs <- function(choiceData, side = FALSE) {

  choices <- getIconPrefs(choiceData)

  options <- getIconOptions(choices)

  iconDiffs <- purrr::map_df(.x = options, ~summarizeIconChoices(choices, .x, "initial"), .id = "icon") %>%
    dplyr::left_join(purrr::map_df(options, ~summarizeIconChoices(choices, .x, "final"), .id = "icon"), by = c("icon", "side", "subject")) %>%
    dplyr::rename(initial = .data$initial_prop_null,
           final = .data$final_prop_null) %>%
    dplyr::relocate(c(.data$subject, .data$side), .before = .data$icon)

  if (!side) {
    iconDiffs <- iconDiffs %>%
      dplyr::group_by(.data$subject, .data$icon) %>%
      dplyr::summarize(diff = base::mean(.data$final - .data$initial), .groups = "drop")
  }

  return(iconDiffs)

}


#' Organize Icon Group Mappings for Every Subject
#'
#' This function maps the group-icon information for each subject with the
#' difference in their icon preference from before the choice task and after. It
#' calls the functions \code{\link{getGroupInfo}} and
#' \code{\link{getIconDiffs}}.
#'
#' @param rawChoiceDF The output of \code{\link{readChoices}}.
#' @param processedRatingDF The output of \code{\link{processRatingsData}}.
#'
#' @return A dataframe with the subject-specific mean rating for each icon class
#'   (corresponding to the PRPT task) as well as the EU and difference in
#'   preference over time.
#' @export
#'
#' @examples
#'
#' #RJTRJTRJT is the subject name for this example. It is set up this in the example
#' #since the function is built for bactch processing and parses the subject ID out
#' #of the file path which is nine characters.
#' choice <- base::list("Path File/choice/RJTRJTRJTchoice.txt.txt" = sampleChoiceData)
#' rate <- sampleRatingsData %>%
#'  processRatingsData() %>%
#'  #add the picture 7006 to prevent error since new cohort didn't rate it.
#'  dplyr::add_row(round = 00, picture = "7006",
#'                 question = "negative", rating = 1.73913) %>%
#'  dplyr::add_row(round = 01, picture = "7006",
#'                 question = "positive", rating = 2.630435) %>%
#'  regSetup() %>%
#'  dplyr::mutate(subject = "RJTRJTRJT", .before = IAPS)
#' organizeIconGroupings(choice, rate)
#'
organizeIconGroupings <- function(rawChoiceDF, processedRatingDF) {

  groupInfo <- purrr::map_df(rawChoiceDF, ~getGroupInfo(.x), .id = "subject") %>%
    dplyr::mutate(subject = stringr::str_extract(.data$subject, "[:graph:]{9}(?=choice.txt)"))

  iconDiffs <- purrr::map_df(rawChoiceDF, ~getIconDiffs(.x), .id = "subject") %>%
    dplyr::mutate(subject = stringr::str_extract(.data$subject, "[:graph:]{9}(?=choice.txt)"))

  iapsr::imageGroupings %>%
    dplyr::left_join(processedRatingDF, by = "IAPS") %>%
    dplyr::group_by(.data$subject, .data$group) %>%
    dplyr::summarize(meanClassRating = mean(.data$positive), .groups = "drop") %>%
    dplyr::rename(reward = .data$group) %>%
    dplyr::left_join(groupInfo, by = c("subject", "reward")) %>%
    dplyr::mutate(EU = .data$meanClassRating * .data$probability) %>%
    dplyr::arrange(.data$subject, .data$phase) %>%
    dplyr::left_join(iconDiffs, by = c("subject", "icon"))

}


