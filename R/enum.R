prepend_class <- function(x, cls = NULL) {
    structure(x, class = unique(c(cls, class(x))))
}

enum_regex <- "^[.A-Z][A-Z0-9_.]*$"

#' @export
enum <- function(..., .classname = NULL, .coerce = risio.core::identity) {
    vals <- list(...) |> as.character()
    stopifnot(all(grepl(enum_regex, vals)))
    stopifnot(!any(duplicated(vals)))
    vals |>
        xfun::as_strict_list() |>
        rlang::set_names(vals) |>
        prepend_class(c(.classname, "enum"))
}
    

CardSuit <- enum("HEARTS", "DIAMONDS", "SPADES", "CLUBS", .classname = "CardSuit")

