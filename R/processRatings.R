#' Read Ratings Data
#'
#' @param filePath The path to the ratings data file. Should be in single quotes.
#'
#' @return The raw data downloaded from the task. It is a data frame with two columns, time and data, and 3631 observations.
#' @export
#'

readRatings <- function(filePath) {

  ratingData <- readr::read_delim(file = filePath,
                                     delim = "\t",
                                     col_names = c("time", "data"))
  return(ratingData)
}


#' Get Question Type Asked
#'
#' This function takes the data column from the output of
#' \code{\link{readRatings}} and filters it for rows that contain the phrase
#' "Question". It then replaces any rows that contain "NEGATIVE," meaning the
#' subject was asked "How negative does this image make you feel?" and replaces
#' it with the string "negative." It replaces rows that contain the string
#' "POSITIVE" with the string "positive."
#'
#' @param data The data output from \code{\link{readRatings}}.
#'
#'
#' @return A dataframe/tibble column titled "question" with variables positive
#'   and negative signifying the question asked on each round.
#' @export
#'

getQuestion <- function(data) {

  question <- data %>%
    dplyr::filter(stringr::str_detect(.$data, "Question")) %>%
    dplyr::transmute(question = dplyr::if_else(condition = stringr::str_detect(.$data, "NEGATIVE"),
                                 true = "negative",
                                 false = "positive"))
  return(question)
}


#' Get Picture IDs
#'
#' This function takes the data column from the output of
#' \code{\link{readRatings}} and filters it for rows that contain the phrase
#' "Image", and removes the "ROUND 0" trial. It then parses the string into the
#' different components, isolating the picture ID variable.
#'
#' @param data The data output from \code{\link{readRatings}}.
#'
#' @return A dataframe/tibble column titled "picture" with the different images
#'   displayed on each round.
#' @export
#'

getPicID <- function(data) {

  picID <- data %>%
    dplyr::filter(stringr::str_detect(.$data, "Image")) %>%
    dplyr::filter(!stringr::str_detect(.$data, "ROUND 0")) %>%
    tidyr::separate(col = data,
             into = c("show", "image", "hyphen", "id", "round", "roundNumber"),
             extra = "drop",
             sep = " ",
             remove = FALSE) %>%
    dplyr::select("id") %>%
    dplyr::transmute(picture = stringr::str_remove(.$id, ".jpg,"))

  return(picID)

}


#' Get Image Ratings
#'
#' This function takes the data column from the output of
#' \code{\link{readRatings}} and filters it for rows that contain the phrase
#' "SUBMIT", and removes the "ROUND 0" trial. It then parses the strings in each row into the
#' different components, isolating the submitted rating value variable.
#'
#' @param data The data output from \code{\link{readRatings}}.
#'
#' @return A dataframe/tibble column titled "rating" with the different images
#'   displayed on each round.
#'
#' @export
#'

getRating <- function(data) {

  submitRating <- data %>%
    dplyr::filter(stringr::str_detect(.$data, "SUBMIT")) %>%
    dplyr::filter(!stringr::str_detect(.$data, "ROUND 0")) %>%
    tidyr::separate(col = data,
             into = c("submit", "rating", "wordValue", "value"),
             extra = "drop",
             sep = " ",
             remove = FALSE) %>%
    dplyr::select(value) %>%
    dplyr::transmute(rating = stringr::str_remove(.$value, ","))

  return(submitRating)

}

#'Process Ratings Data
#'
#'This function takes the data column from the output of
#'\code{\link{readRatings}} and calls the \code{\link{getPicID}},
#'\code{\link{getQuestion}}, and \code{\link{getRating}} functions. It combines
#'the outputs of them, and returns a dataframe/tibble with 242 rows and four
#'columns: \itemize{ \item \emph{round:} The round number (integer) \item \emph{picture:}
#'The picture ID from IAPS dataset (factor) \item \emph{question:} "positive" or
#'"negative" values based on whether the question was "How positive does this
#'make you feel?" or "How negative does this make you feel?" (factor) \item
#'\emph{rating:} The image rating for that round/question combination (double)}
#'
#'@param data The data output from \code{\link{readRatings}}.
#'
#'@return Dataframe/tibble with 242 rows and four columns containing information
#'  about the round, picture ID, question asked, and rating value. See description
#'  for more information.
#'@export

processRatingsData <- function(data) {

  picID <- getPicID(data)
  question <- getQuestion(data)
  ratings <- getRating(data)

  combined <- dplyr::bind_cols(picID, question, ratings) %>%
    dplyr::mutate(round = dplyr::row_number(),
           picture = forcats::as_factor(picture),
           question = forcats::as_factor(question),
           rating = base::as.numeric(rating)) %>%
    dplyr::select(round, dplyr::everything())

  return(combined)
}

#' Setup Data for Testing Regression Models
#'
#' This function is to be used in order to set up the new data collected from
#' the IAPS task to test the regression models generated from previous data.
#'
#' @param processedData The data output from \code{\link{processRatingsData}}.
#'
#' @return A tibble that's 120 rows by 3 columns. It has the IAPS column, which
#'   is a factor that contains the IDs of each image in the IAPS data set, as
#'   well as a negative and positive column that correspond to the ratings for
#'   each image when the subjects were asked "How positive does this image make
#'   you feel?" and "How negative does this image make you feel?"
#'
#' @export
#'
regSetup <- function(processedData) {

  pos <- processedData %>%
    dplyr::filter(question == "positive") %>%
    tidyr::pivot_wider(names_from = question,
                values_from = rating)

  neg <- processedData %>%
    dplyr::filter(question == "negative") %>%
    tidyr::pivot_wider(names_from = question,
                values_from = rating)

  out <- dplyr::full_join(pos, neg, by = "picture") %>%
    dplyr::select(-c(round.x, round.y)) %>%
    dplyr::filter(picture != "neutral") %>%
    dplyr::rename(IAPS = picture)

  return(out)

}



