
#' Get Initial and Final Rates of Icon Choices
#'
#' @param choiceData The output of \code{\link{readChoices}} on one or more
#'   subjects.
#' @param choice_time Either "initial" or "final", representing which segment of
#'   the IAPS choice data we should access.
#'
#' @return A data frame consisting of 36 rows, and six columns corresponding to
#'   the round number, the presented icons, the numeric choice (option), the
#'   name of the chosen icon (choice), and the valence group of the chosen icon.
#'
#' @export
#'
#' @examples
#'
#' get_icon_choice(sampleChoiceData, "initial")
#'
#' get_icon_choice(sampleChoiceData, "final")
#'
get_icon_choice <- function(choiceData, choice_time) {

  getPhases(choiceData)

  if (choice_time == "initial") {
    choice_df <- phase$initIcon
  } else if (choice_time == "final") {
    choice_df <- phase$finalIcon
  }

  shown_icons <- choice_df %>%
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
    dplyr::select(-c(.data$iconStuff, .data$order1, .data$order2)) %>%
    dplyr::relocate(.data$icon1, .before = .data$icon2) %>%
    dplyr::mutate(icon2 = forcats::as_factor(stringr::str_remove_all(.data$icon2, ",")),
                  icon1 = forcats::as_factor(.data$icon1),
                  roundNumber = base::as.numeric(.data$roundNumber))


  icon_choices <- choice_df %>%
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

  dplyr::left_join(shown_icons, icon_choices, by = "roundNumber") %>%
    dplyr::relocate(.data$roundNumber, .before = 1)

}
