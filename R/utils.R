
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
mt_create_meta_shell <- function(df, factor_cols = NULL, sep = ";"){
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

    if(!is.null(factor_cols)){
        meta_tbl <- mt_update_factors(df, meta_tbl = meta,
                                      factor_cols = factor_cols)
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
#' @family \code(meta_tbl) utilities
#' @export
mt_update_meta_tbl <- function(df, meta_tbl, factor_cols = NULL, sep = ";") {
    if(all(names(df) %in% meta_tbl$attributeName) &
       all(meta_tbl$attributeName %in% names(df))){return(meta_tbl)}

    if(!all(names(df) %in% meta_tbl$attributeName)){
        add_cols <- names(df)[!names(df) %in% meta_tbl$attributeName]
        meta_tbl <- rbind(meta_tbl, mt_create_meta_shell(df[, add_cols, drop = F]))
    }
    if(!all(meta_tbl$attributeName %in% names(df))){
        meta_tbl <- meta_tbl[meta_tbl$attributeName %in% names(df),]
    }
    meta_tbl <- meta_tbl[match(names(df), meta_tbl$attributeName),]
    row.names(meta_tbl) <- NULL

    tibble::as.tibble(meta_tbl)
}




#' @inherit mt_update_meta_tbl
#' @export
#' @examples
#' \dontrun{
#' library(gapminder)
#' df <- gapminder::gapminder
#' meta_tbl <- readr::read_csv(file.path(here::here("inst", "extdata",
#'                                                  "gapminder_meta.csv")))
#' # update metadata for factor columns from levels
#' mt_update_factors(df, meta_tbl, overwrite = F)
#' # update metadata for columns indicated as factors in the metadata table
#' mt_update_factors(df, meta_tbl, factor_cols = get_factors_meta(meta_tbl), overwrite = F)
#' }
mt_update_factors <- function(df, meta_tbl, factor_cols = NULL, sep = ";",
                              overwrite = F){

    if(is.null(factor_cols)){
        factor_cols <- get_factors_df(df)}

    if(!all(factor_cols %in% names(df))){
        stop(factor_cols[which(!factor_cols %in% names(df))],
             " not a valid colname")
    }
    for(col in factor_cols){
        empty <- check_empty(col, meta_tbl, c("code", "levels"))
        if(empty | overwrite){
            meta_tbl[meta_tbl$attributeName == col, c("code", "levels")] <-
                collapse_factor_levels(df[,col, drop = T], sep)
            meta_tbl[meta_tbl$attributeName == col, "columnClasses"] <-
                "factor"
        }else{
            stop("Action would overwrite current factor metadata,
                      use overwrite = T to override this")}
    }

    done(field("meta_tbl"), " fields ", value("code"), " & ", value("levels"),
         " successfully updated for vars: ",
         value(paste0(factor_cols, collapse = ", "))," \n")

    meta_tbl
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
mt_extract_attr_tbl <- function(meta_tbl) {
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
mt_extract_factors_tbl <- function(meta_tbl, sep =";") {
    meta_tbl[] <- lapply(meta_tbl, as.character)
    factors <- NULL
    vars <- mt_extract_factor_cols(meta_tbl)
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

# extract a vector of column names containing `factor` or `ordered` data.
mt_extract_factor_cols <- function(meta_tbl){
    vars <- meta_tbl$attributeName[meta_tbl$columnClasses %in%
                                       c("ordered", "factor")]
}

# check_empty
check_empty <- function(data_col, meta_tbl,
                        meta_fields){

    match.arg(arg = data_col, choices = get_vars_meta(meta_tbl))
    match.arg(arg = meta_fields, choices = input_meta_fields(meta_tbl),
              several.ok = T)

    non_empty_meta_fields_test <- is.na(meta_tbl[meta_tbl$attributeName == data_col,
                                                 meta_fields] %>% unlist)
    if(all(non_empty_meta_fields_test)){
        return(TRUE)
    }else{
        non_empty_meta_fields <-  meta_fields[!non_empty_meta_fields_test]
        todo(field("data_col "), value(data_col), ": ", field("meta_tbl "),
             "already contain metadata for ", field("meta_field "),
             value(non_empty_meta_fields), " \n")
        return(FALSE)
    }
}

input_meta_fields <- function(meta_tbl){
    names(meta_tbl) %>% magrittr::extract(! . %in% c("attributeName",
                                                     "columnClasses"))
}

#' Extract useful metadata from data and metadata tables
#'
#' @param df a dataframe of data for which metadata attribute table is to be created
#' @param meta_tbl a metadata table associated with df
#'
#' @return character vectors of variables names
#' @details functions appended `df` extract information from a `df` of the data,
#'  while those appended `meta` from the `meta_tbl` containing the attribute metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' get_vars_df(df)
#' get_vars_meta(meta_tbl)
#' get_factors_df(df)
#' get_factors_meta(meta_tbl)
#' }
get_vars_df <- function(df){
    names(df)
}

#' @inherit get_vars_df
#' @export
get_vars_meta <- function(meta_tbl){
    meta_tbl$attributeName
}

#' @inherit get_vars_df
#' @export
get_factors_meta <- function(meta_tbl){
    meta_tbl$attributeName[meta_tbl$columnClasses == "factor"]
}

#' @inherit get_vars_df
#' @export
get_factors_df <- function(df){
    names(df)[sapply(df, FUN = is.factor)]
}
