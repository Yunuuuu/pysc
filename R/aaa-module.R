module <- function(name, as = NULL, convert = FALSE, delay_load = TRUE) {
    structure(name,
        as = as,
        convert = convert,
        delay_load = delay_load,
        class = "py_module"
    )
}

`$.py_module` <- function(x, i) {
    x[[i]]
}

`[[.py_module` <- function(x, i) {
    py_module <- import(
        module = x,
        as = attr(x, "as"),
        convert = attr(x, "convert"),
        delay_load = attr(x, "delay_load")
    )
    py_module[[i]]
}

#' @export
print.py_module <- function(x, ...) {
    attrs <- list(
        convert = attr(x, "convert"),
        delay_load = attr(x, "delay_load")
    )
    as <- attr(x, "as")
    main <- "Import Python module {.field {x}}"
    if (!is.null(as)) {
        main <- paste(main, "as {.field {as}}", sep = " ")
    }
    cli::cli_text(main)
    cli::cli_ul()
    cli_nlist(attrs)
    cli::cli_end()
    invisible(x)
}

import <- function(module, ...) {
    assert_module(module)
    reticulate::import(module = module, ...)
}

assert_module <- function(name, call = rlang::caller_env()) {
    if (!module_available(name)) {
        cli::cli_abort("Cannot find {.field {name}} Python module", call = call)
    }
}

module_available <- local({
    cache <- new.env()
    function(name) {
        out <- cache[[name]]
        if (is.null(out)) {
            out <- reticulate::py_module_available(name)
            cache[[name]] <<- out
        }
        out
    }
})
