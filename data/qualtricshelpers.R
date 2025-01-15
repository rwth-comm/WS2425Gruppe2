require(lubridate)
require(tidyverse)

#' Loads Qualtrics generated csv files that contain 3 line headers and removes the first and the second line of meta-information.
#'
#' @param filename Character of the file location.
#'
#' @return raw data with a single header
#' @export
#'
#' @examples
load_qualtrics_csv <- function(filename) {
  suppressWarnings(
    header <- read_csv(filename, n_max = 2))
  suppressWarnings(
    raw <- read_csv(filename, skip = 2)
  )
  names(raw) <- names(header)
  
  remove_nbsp <- function(text){
    gsub("\u00A0", " ", text, fixed = TRUE)
  }
  raw <- raw %>% mutate_if(is.character,remove_nbsp)
  
  raw
}

#' 
#' @param df shortened Dataframe.
#' @param filename Path to unshortened Data with matching names to df. 
#' @param path Path where codebook should be generated.
#'
#' @return raw data with a single header
#' @export
#'
#' @examples
generate_codebook <- function(df, filename, path) {
  suppressWarnings(
    header <- read_csv(filename, n_max = 2))
  raw <- select(header, one_of(names(df))) 
  variable <- names(raw)
  variable_old <- names(raw)
  text <- as.character(as.vector(raw[1,]))
  tmp_text <- strsplit(as.character(raw[1,]), " - ")
  item <- NULL
  for (elem in tmp_text) {
    item <- append(item, elem[2])
  }
  info <- as.character(as.vector(raw[2,]))
  codebook <- tibble(variable, variable_old, item, text, info)
  suppressMessages(
    write_delim(codebook, path, delim = ";"))
  
}


read_codebook <- function(filename){
  read_delim(filename, delim = ";")
}

vars4psych <- function(x, pat){
  names <- names(x)
  all <- str_subset(names, pattern = paste0("^", pat))
  input_text <- str_subset(names, pattern = "text")
  items <- all[!all %in% input_text]
  neg <- str_subset(items, pattern = paste0("n$"))
  if(!is_empty(neg)){
    res <- c(items[!items %in% neg], paste0("-",neg))
  }else{
    res <- items[!items %in% neg]
  }
  if(is_empty(res)){
    NA
  }else{
    res
  }
}

#' Detect careless responses
#'
#' A function to detect careless survey responses using the careless package.
#' The big improvement is that subject with out of bounds individual criterions (speeders, longstring, average longstring and inter response variance)
#' are automatically excluded from the calculation of inter-individual criterions (psychometric synonyms, psychometric antonyms and Mahananobis Distance).
#'
#'
#'
#'
#' @param dat A data.frame that contains the survey data.
#' @param likert_vector A vector containing the numeric addresses of the numeric likert items in dat.
#' Leaving this parameter NULL (default) still allows for speeder analysis.
#' @param id_column A string that contains the name of the id column. As a default
#' we use "ResponseId", which is used by Qualtrics.
#'
#' @param duration_column A string that contains the name of the column that
#' has the duration of survey responses. As a default we use
#' Duration (in seconds), which is used by Qualtrics.
#' @param speeder_analysis A criterion to test against speeders. The
#' speeder_analysis parameter can be one of the following: median,
#' median/2 (default), median/3, or a numeric value that is directly used
#' as a threshold. Using FALSE will ignore speeder analysis.
#' @param critval_longstring A numeric as a threshold for longstring analysis. Longstrings will be flagged and not considered in further analysis.
#' @param critval_avgstring A numeric as a threshold for average longstring analysis. Average longstrings will be flagged and not considered in further analysis.
#' @param critval_irv_low A numeric as a lower threshold for inter response variance analysis. IRV will be flagged and not considered in further analysis.
#' @param critval_irv_high A numeric as an upper threshold for inter response variance analysis. IRV will be flagged and not considered in further analysis.
#' @param antonyms Should psychometric antonyms be calculated?
#' @param critval_ant Threshold for antonym analysis (Default: -0.6)
#' @param synonyms Should psychometric synonyms be calculated?
#' @param critval_syn Threshold for antonym analysis (Default: 0.6)
#' @param mahaD Whether or not to test for mahalanobis distance with confidence of 0.99. (Default: TRUE)
#' @return Returns an updated dataframe containing indices
#' @export
#'
# @examples
careless_indices <- function(dat,
                             likert_vector = NULL,
                             id_column = "ResponseId",
                             duration_column = "Duration (in seconds)",
                             speeder_analysis = "median/2",
                             critval_longstring = FALSE,
                             critval_avgstring = FALSE,
                             critval_irv_low = FALSE,
                             critval_irv_high = FALSE,
                             antonyms = TRUE,
                             critval_ant = -0.6,
                             synonyms = TRUE,
                             critval_syn = 0.6,
                             mahaD = TRUE)  {
  
  
  stopifnot("speeder_analysis must be either \"median\", \"median/2\",\"median/3\", FALSE, or a numeric value." =
              {
                !(!isFALSE(speeder_analysis) &&
                    !is.character(speeder_analysis) &&
                    (substr(speeder_analysis, 1,7) == "median/" &&
                       numbers_only(substr(speeder_analysis, 8))) &&
                    !is.numeric(speeder_analysis))
              })
  
  
  id <- id_column
  
  # data.quality is a constantly row-shrinking data frame, containting all subjects that are counted as optimizers, *yet*.
  # We start with the full data set:
  data.quality <- dat
  
  # quality.indices is a constantly column-increasing data frame, containing all result-indices.
  quality.indices <-
    data.frame(ResponseId = data.quality[[id]])
  # This is to apply a custom id name to quality_indices.
  names(quality.indices)[names(quality.indices) == "ResponseId"] <- id
  
  
  
  # First to speeder analysis
  if (!isFALSE(speeder_analysis)) {
    if (is.numeric(speeder_analysis)) {
      speedlimit <- speeder_analysis
    } else{
      speedercondition <- strsplit(speeder_analysis, "/")
      speedlimit <-
        stats::median(dat[[duration_column]]) / as.numeric(speedercondition[[1]][2])
    }
    quality.indices$careless_speed <- dat[[duration_column]]
    quality.indices$speeder_flag <-
      ifelse(dat[[duration_column]] > speedlimit, FALSE, TRUE)
    
    temp_ids <- quality.indices[which(quality.indices$speeder_flag == FALSE), ][[id]]
    data.quality |>
      dplyr::filter( !!as.symbol(id) %in% temp_ids ) -> data.quality
  }
  # data.quality contains now a filtered list, containting only non-speeders.
  
  
  
  
  if (!is.null(likert_vector)) {
    # Attention: Since longstrings, avgstring and irv are inter-subject, they are calculated on speeders as well.
    # TODO: provide sensible error message if not all vectors can be numerics
    items_to_be_tested <- dplyr::mutate_all(dat[, likert_vector], as.numeric)
    
    # add longstrings to data frame
    temp <- careless::longstring(items_to_be_tested, avg = T)
    quality.indices$careless_longstr <- temp$longstr
    
    if(is.numeric(critval_longstring)){
      quality.indices$longstring_flag <- ifelse(quality.indices$careless_longstr > critval_longstring, TRUE, FALSE)
      
      temp_ids <- quality.indices[which(quality.indices$longstring_flag == FALSE), ][[id]]
      data.quality |>
        dplyr::filter( !!as.symbol(id) %in% temp_ids ) -> data.quality
    }
    
    quality.indices$careless_avgstr <- temp$avgstr
    
    if(is.numeric(critval_avgstring)){
      quality.indices$avgstring_flag <- ifelse(quality.indices$careless_avgstr > critval_avgstring, TRUE, FALSE)
      
      temp_ids <- quality.indices[which(quality.indices$avgstring_flag == FALSE), ][[id]]
      data.quality |>
        dplyr::filter( !!as.symbol(id) %in% temp_ids ) -> data.quality
      
    }
    
    # add inter response variability
    quality.indices$careless_irv <- careless::irv(items_to_be_tested)
    
    if(is.numeric(critval_irv_low)){
      quality.indices$irv_flag_low <- ifelse(quality.indices$careless_irv < critval_irv_low, TRUE, FALSE)
      
      temp_ids <- quality.indices[which(quality.indices$irv_flag_low == FALSE), ][[id]]
      data.quality |>
        dplyr::filter( !!as.symbol(id) %in% temp_ids ) -> data.quality
    }
    
    if(is.numeric(critval_irv_high)){
      quality.indices$irv_flag_high <- ifelse(quality.indices$careless_irv > critval_irv_high, TRUE, FALSE)
      
      temp_ids <- quality.indices[which(quality.indices$irv_flag_high == FALSE), ][[id]]
      data.quality |>
        dplyr::filter( !!as.symbol(id) %in% temp_ids ) -> data.quality
    }
    
    # data.quality now contains only datasets that are not yet identified as satisficers.
    
    items_to_be_tested <- dplyr::mutate_all(data.quality[, likert_vector], as.numeric)
    
    # quality.indices2 is a subset of quality.indices to calculate further scores with a subset of quality.indices.
    quality.indices2 <- data.frame(tempname = data.quality[[id]])
    names(quality.indices2) <- id
    
    # Analysis of psychsyn, psychant and mahalanobis Distance.
    if (synonyms) {
      tryCatch(temp <-
                 careless::psychsyn(items_to_be_tested, critval = critval_syn, diag = TRUE),
               error = function(e) {
                 warning(e)
               })
      quality.indices2$careless_psychsyn_pairs <- temp$numPairs
      quality.indices2$careless_psychsyn <- temp$cor
      
    }
    if (antonyms) {
      tryCatch(temp <-
                 careless::psychsyn(items_to_be_tested, critval = -0.6, anto = T, diag = TRUE),
               error = function(e) { warning(e) })
      quality.indices2$careless_psychant_pairs <- temp$numPairs
      quality.indices2$careless_psychant <- temp$cor
    }
    
    if (mahaD) {
      tryCatch(temp <-
                 careless::mahad(items_to_be_tested, flag = TRUE, confidence = 0.99, na.rm = TRUE, plot = FALSE),
               error = function(e) { warning(e) })
      quality.indices2$careless_mahadD <- temp$d_sq
      quality.indices2$careless_mahadflag <- temp$flagged
    }
  }
  
  quality.indices <- merge(x = quality.indices,
                           y = quality.indices2,
                           by = id,
                           all.x = TRUE)
  
  
  # join by merging, TODO needs testing
  res <- merge(x = dat,
               y = quality.indices,
               by = id,
               all.x = TRUE)
  res
}


#' Tests if a vector of strings can be a numeric
#'
#' @param x a vector of strings
#'
#' @return Returns a vector of booleans
# @export
numbers_only <- function(x) {
  !grepl("^[A-Za-z]+$", as.character(x), perl = T)
}

