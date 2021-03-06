% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{mt_extract_factors_tbl}
\alias{mt_extract_factors_tbl}
\title{Extract factors table}
\usage{
mt_extract_factors_tbl(meta_tbl, sep = ";")
}
\arguments{
\item{meta_tbl}{a completed metadata table with codes and definitions separated
by a character. Defaults to ;}

\item{sep}{separator used to separate codes and definitions for each level in
the metadata table}
}
\value{
a dataframe containing factor metadata in a format appropriate for passing to
 an EML attributeList
}
\description{
Extract factors table to pass to EML attributeList
}
