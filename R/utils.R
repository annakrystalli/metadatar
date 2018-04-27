
#' Get attribute shell
#'
#' Create shell metadata table of variable attributes for a dataset. The function attempts to guess
#' guess the values where possible. The rest will need completing manually.
#' @param df a dataframe of data for which metadata attribute table is to be created
#'
#' @return a dataframe consiting of one row per column of the input df. Columns
#'  represent minimum metadata requirements.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gapminder)
#' get_meta_shell(gapminder)
#' }
get_meta_shell <- function(df){
    rows <- ncol(df)
    meta <- data.frame(attributeName = rep(NA, times = rows),
                       attributeDefinition = rep(NA, times = rows),
                       columnClasses = rep(NA, times = rows),
                       numberType = rep(NA, times = rows),
                       unit = rep(NA, times = rows),
                       minimum = rep(NA, times = rows),
                       maximum = rep(NA, times = rows),
                       formatString = rep(NA, times = rows),
                       definition = rep(NA, times = rows),
                       code = rep(NA, times = rows),
                       levels = rep(NA, times = rows),
                       stringsAsFactors = FALSE)

    meta$attributeName <- names(df)
    meta$columnClasses <- sapply(df, class)
    meta$columnClasses[meta$columnClasses == "integer"] <- "numeric"

    return(meta)
}

#' Extract attribute table
#'
#' Extract attribute table to pass to EML attributeList
#' @param meta_tbl a completed metadata table
#'
#' @return a dataframe containing attribute metadata in a format appropriate for passing to
#'  an EML attributeList
#' @export
#'
extract_attr_tbl <- function(meta_tbl) {
    attr_hd <- c("attributeName", "attributeDefinition", "columnClasses", "numberType",
                 "unit", "minimum", "maximum", "formatString", "definition")
    out <- meta_tbl[,attr_hd]
    out$columnClasses <- NA
    out
}

#' Extract factors table
#'
#' Extract factors table to pass to EML attributeList
#' @param meta_tbl a completed metadata table with codes and definitions separated
#'  by a character. Defaults to ;
#' @param sep separator used to separate codes and definitions for each level in
#'  the metadata table
#' @return a dataframe containing factor metadata in a format appropriate for passing to
#'  an EML attributeList
#' @export
#'
get_meta_factors <- function(meta_tbl, sep =";") {
    meta_tbl[] <- lapply(meta_tbl, as.character)
    factors <- NULL
    vars <- meta_tbl$attributeName[meta_tbl$columnClasses %in% c("ordered", "factor")]
    for(var in vars){
        x <- meta_tbl[meta_tbl$attributeName == var, , drop = F]
        f_tbl <- data.frame(
            attributeName = x$attributeName,
            code = unlist(strsplit(x$code, sep)),
            definition = unlist(strsplit(x$levels, sep))
        )
        factors <- rbind(factors, f_tbl)
    }
    return(factors)
}
