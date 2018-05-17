#' Get descriptive label
#'
#' Create a more descriptive label, including units where appropriate, for a
#' variable from information in the metadata table.
#' @param meta_df a metadatar metadata table
#' @param var character string. the variable for which to
#'
#' @return a character string descriptive label of the variable
#' @export
#'
#' @examples
mt_label <- function(meta_df, var){
    if(!var %in% meta_df$attributeName){
        stop("var not a valid attributeName")
    }
    descr <- meta_df[meta_df$attributeName == var, "attributeDefinition"]
    unit <- meta_df[meta_df$attributeName == var, "unit"]
    if(grepl("[[:alnum:]]", unit)){
        unit <- paste(" (", unit, ")", sep = "")
    }
    paste(descr, unit, sep = "")
}
