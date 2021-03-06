# from usethis commit no: https://github.com/r-lib/usethis/commit/3cc50ae7c4877d5f10747d0e8f25469897206cfe
bullet <- function(line, bullet) {
    line <- paste0(bullet, " ", line)
    cat_line(line)
}

todo_bullet <- function() crayon::red(clisymbols::symbol$bullet)

todo <- function(...) {
    bullet(paste0(...), bullet = todo_bullet())
}
done <- function(...) {
    bullet(paste0(...), bullet = crayon::green(clisymbols::symbol$tick))
}

code_block <- function(..., copy = interactive()) {
    block <- paste0("  ", c(...), collapse = "\n")
    if (copy && clipr::clipr_available()) {
        clipr::write_clip(paste0(c(...), collapse = "\n"))
        message("Copying code to clipboard:")
    }
    cat_line(crayon::make_style("darkgrey")(block))
}

cat_line <- function(...) {
    cat(..., "\n", sep = "")
}

field <- function(...) {
    x <- paste0(...)
    crayon::green(x)
}
value <- function(...) {
    x <- paste0(...)
    crayon::blue(encodeString(x, quote = "'"))
}

code <- function(...) {
    x <- paste0(...)
    crayon::make_style("darkgrey")(encodeString(x, quote = "`"))
}

collapse <- function(x, sep = ", ") {
    paste0(x, collapse = sep)
}
