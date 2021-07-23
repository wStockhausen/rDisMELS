#'
#' @title Convert strings to camel case
#'
#' @description Function to convert strings to camel case
#'
#' @param x - character vector to convert
#'
#' @return character vector
#'
#' @import stringr
#'
toCamelCase<-function(x){
  y = x %>% stringr::str_to_title() %>%
            stringr::str_trim() %>%
            stringr::str_remove_all(stringr::fixed(" "))
  return(y);
}

#'
#' @title Add guards to column names
#'
#' @description Function to add guards to column names
#'
#' @param x - character vector to convert
#'
#' @return character vector with guards added (as necessary)
#'
#' @import stringr
#'
addGuards<-function(x){
  y = x;
  w = stringr::str_which(x,stringr::fixed(" "));
  if (length(w)>0) y[w] = paste0("`",x[w],"`");
  return(y);
}
