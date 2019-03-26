context("test-update-utils")

library(dplyr)

df <- gapminder::gapminder
meta_shell <- mt_create_meta_shell(gapminder::gapminder)
meta_tbl <- readr::read_csv(file.path(here::here("inst", "extdata",
                                                 "gapminder_meta.csv")))

test_that("check_empty indentifies meta_tbl vars with factor data", {
    expect_equal(input_meta_fields(meta_tbl),
                 c("attributeDefinition", "numberType", "unit", "minimum", "maximum",
                   "formatString", "definition", "code", "levels"))
})

test_that("data vars extracted correctly", {
    data_vars <- c("country", "continent", "year", "lifeExp", "pop", "gdpPercap")
    expect_equal(get_vars_meta(meta_tbl), data_vars)
    expect_equal(get_vars_df(df), data_vars)
})


test_that("test check overwrite on comlpeted data",{
    expect_equal(check_empty("continent", meta_tbl, "code"), FALSE)
    expect_equal(check_empty("continent", meta_tbl, c("code", "levels")),
                 FALSE)
})


test_that("test check overwrite on incompleted data",{
    expect_equal(check_empty("pop", meta_tbl, "code"), TRUE)
    expect_equal(check_empty("pop", meta_tbl, c("code", "levels")),
                 TRUE)
})


test_that("factors updated correctly", {
    expect_equal(mt_update_factors(df, meta_shell, factor_cols = NULL, overwrite = F),
                 readRDS(here::here("inst","test-dat","factor_check.rds")))

    expect_error(mt_update_factors(df, meta_tbl, factor_cols = NULL, overwrite = F),
                 "Action would overwrite current factor metadata,
                      use overwrite = T to override this")

    expect_equal(mt_update_factors(df, meta_shell, factor_cols = NULL, overwrite = F),
                 readRDS(here::here("inst","test-dat","factor_check.rds")))

    # test data with no factors
    df_clear <- df
    df_clear[,c("country", "continent")] <-
        lapply(df[,c("country", "continent")], as.character)
    meta_clear <- mt_create_meta_shell(df_clear)

    expect_equal(mt_update_factors(df_clear, meta_clear,
                                   factor_cols = c("country", "continent"),
                                   overwrite = F),
                 readRDS(here::here("inst","test-dat","factor_check.rds")))

    # test setting factors through the metadata table
    meta_semi <- meta_clear %>%
        mutate(columnClasses = case_when(
            attributeName %in% c("country", "continent") ~ "factor",
            TRUE ~ columnClasses))

    expect_equal(mt_update_factors(df_clear, meta_semi,
                                   factor_cols = get_factors_meta(meta_semi),
                                   overwrite = F),
                 readRDS(here::here("inst","test-dat","factor_check.rds")))

})
