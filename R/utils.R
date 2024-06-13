`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

is_scalar <- function(x) length(x) == 1L

map_lgl <- function(.x, .f, ...) {
    .rlang_purrr_map_mold(.x, .f, logical(1L), ...)
}

map_int <- function(.x, .f, ...) {
    .rlang_purrr_map_mold(.x, .f, integer(1L), ...)
}

map_dbl <- function(.x, .f, ...) {
    .rlang_purrr_map_mold(.x, .f, double(1L), ...)
}

map_chr <- function(.x, .f, ...) {
    .rlang_purrr_map_mold(.x, .f, character(1L), ...)
}

.rlang_purrr_map_mold <- function(.x, .f, .mold, ...) {
    out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
    names(out) <- names(.x)
    out
}

map2 <- function(.x, .y, .f, ...) {
    out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
    if (length(out) == length(.x)) {
        rlang::set_names(out, names(.x))
    } else {
        rlang::set_names(out, NULL)
    }
}

map2_lgl <- function(.x, .y, .f, ...) {
    as.vector(map2(.x, .y, .f, ...), "logical")
}

map2_int <- function(.x, .y, .f, ...) {
    as.vector(map2(.x, .y, .f, ...), "integer")
}

map2_dbl <- function(.x, .y, .f, ...) {
    as.vector(map2(.x, .y, .f, ...), "double")
}

map2_chr <- function(.x, .y, .f, ...) {
    as.vector(map2(.x, .y, .f, ...), "character")
}

imap <- function(.x, .f, ...) {
    map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}

# cli container for named list
cli_nl <- function(label, items, sep = ": ", trunc = Inf, add_num = FALSE) {
    items <- cli::cli_vec(items, list("vec-trunc" = trunc))
    message <- "{.field {label}}{sep}{.val {items}}"
    if (add_num) {
        message <- paste(message, "({length(items)} item{?s})", sep = " ")
    }
    cli::cli_li(message)
}

cli_nlist <- function(list, ...) {
    imap(list, function(x, i, ...) {
        cli_nl(label = i, items = x, ...)
    }, ...)
}

cli_olist <- function(list, ...) {
    lapply(list, function(x, ...) {
        cli::cli_ol(items = x, ...)
    }, ...)
}
