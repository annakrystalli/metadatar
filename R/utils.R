
#' Create metadata shell
#'
#' Create metadata table shell of variable attributes from a dataset. The function attempts to
#' guess values where possible. The rest require completing manually
#' @param df a dataframe of data for which metadata attribute table is to be created
#' @param factor_cols character string or vector of names of columns containing factors
#' Code and level information is extracted auromatically from factor columns.
#' @param sep separator used to separate codes and definitions for each level in
#'  the metadata table
#'
#' @return a dataframe consiting of one row per column of the input df. Columns
#'  represent minimum metadata requirements.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gapminder)
#' create_meta_shell(gapminder)
#' }
create_meta_shell <- function(df, factor_cols = NULL, sep = ";"){
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

    if(any(sapply(df, FUN = is.factor))){
        factor_cols <- unique(c(factor_cols,
                                names(df)[sapply(df, FUN = is.factor)]))
    }

    if(!is.null(factor_cols)){
        if(!all(factor_cols %in% names(df))){
            stop(factor_cols[which(!factor_cols %in% names(df))],
                 " not a valid colname")
        }
        for(col in factor_cols){
            meta[meta$attributeName == col,c("code", "levels")] <-
                collapse_factor_levels(df[,col, drop = T], sep)
        }
    }

    tibble::as.tibble(meta)
}

#' Update metadata table
#'
#' Update the metadata table of a dataset. The function will identify variable columns in df
#' for which rows are missing in meta_tbl and append new rows for each. It will also
#' remove any deprecated rows which do not match columns in the dataset.
#' @param df a dataframe or tibble to which the metadata table relates to
#' @param meta_tbl a metadata table associated with df
#' @param factor_cols character string or vector of names of columns containing
#' factors (will only apply to new columns)
#' @param sep separator used to separate codes and definitions for each level in
#'  the metadata table (must be the same used throughout meta_tbl)
#'
#' @return an updated meta_tbl with rows added for new df columns and any
#' deprecated rows trimmed.
#'
#' @export
update_meta_tbl <- function(df, meta_tbl, factor_cols = NULL, sep = ";") {
    if(all(names(df) %in% meta_tbl$attributeName) &
       all(meta_tbl$attributeName %in% names(df))){return(meta_tbl)}

    if(!all(names(df) %in% meta_tbl$attributeName)){
        add_cols <- names(df)[!names(df) %in% meta_tbl$attributeName]
        meta_tbl <- rbind(meta_tbl, create_meta_shell(df[, add_cols, drop = F]))
    }
    if(!all(meta_tbl$attributeName %in% names(df))){
        meta_tbl <- meta_tbl[meta_tbl$attributeName %in% names(df),]
    }
    meta_tbl <- meta_tbl[match(names(df), meta_tbl$attributeName),]
    row.names(meta_tbl) <- NULL

    tibble::as.tibble(meta_tbl)
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
    tibble::as.tibble(out)
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
extract_factors_tbl <- function(meta_tbl, sep =";") {
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
    tibble::as.tibble(factors)
}

# extract factor data from a vector and return a collapsed string
collapse_factor_levels <- function(x, sep = ";"){
    if(is.factor(x)){paste(levels(x), collapse = sep)}else{
        paste(as.character(sort(unique(x))), collapse = sep)
    }
}



