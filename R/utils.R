
#' Get attribute shell
#'
#' Get shell of attribute from a dataframe or column
#' @param df a dataframe of data for which metadata attribute table is to be created
#'
#' @return a dataframe consiting of one row per column of the input df
#' @export
#'
#' @examples
get_attr_shell <- function(df){
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

extract_attr_tbl <- function(attr_tbl) {
    attr_hd <- c("attributeName", "attributeDefinition", "columnClasses", "numberType",
                 "unit", "minimum", "maximum", "formatString", "definition")
    out <- attr_tbl[,attr_hd]
    out$columnClasses <- NA
    out
}

get_attr_factors <- function(attr_tbl) {
    attr_tbl[] <- lapply(attr_tbl, as.character)
    factors <- NULL
    vars <- attr_tbl$attributeName[attr_tbl$columnClasses %in% c("ordered", "factor")]
    for(var in vars){
        x <- attr_tbl[attr_tbl$attributeName == var, , drop = F]
        f_tbl <- data.frame(
            attributeName = x$attributeName,
            code = unlist(strsplit(x$code, ";")),
            definition = unlist(strsplit(x$levels, ";"))
        )
        factors <- rbind(factors, f_tbl)
    }
    return(factors)
}
