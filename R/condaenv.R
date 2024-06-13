#' Global conda environment
#'
#' @description
#' All python related function will accept `conda` argument, if it's `NULL`,
#' then the global conda environment will be used, if it's `NA`, no conda
#' environment will be used, otherwise it must be a string indicates the conda
#' environment name.
#'
#' @param conda Conda environment to be use. If `NULL`, will return current
#' global conda environment, if `NA`, will remove the global conda environment
#' settting. If a string, will set the global conda environment.
#' @export
condaenv <- function(conda = NULL) {
    if (is.null(conda)) {
        get_envir()
    } else if (is_scalar(conda)) {
        if (!is.na(conda) && conda == "") {
            cli::cli_abort("{.arg conda} cannot be empty")
        }
        set_envir(as.character(conda))
    } else {
        cli::cli_abort("{.arg conda} must be a character string")
    }
}

GLOBALS <- new.env(parent = emptyenv())
GLOBALS$envir <- NA_character_

with_envir <- function(envir, code) {
    old <- get_envir()
    set_envir(envir)
    on.exit(set_envir(old))
    force(code)
}

get_envir <- function() GLOBALS$envir

set_envir <- function(envir) GLOBALS$envir <- envir

import2 <- function(module, ..., envir = NULL, delay_load = NULL,
                    convert = FALSE, call = rlang::caller_env()) {
    envir <- envir %||% .subset2(GLOBALS, "envir")
    if (!is.na(envir)) {
        delay_load <- c(delay_load, list(environment = envir))
    }
    tryCatch(
        reticulate::import(
            module = module, ...,
            delay_load = delay_load,
            convert = convert
        ),
        error = function(error) {
            cli::cli_abort(
                "Cannot find {.field {name}} Python module",
                call = call
            )
        }
    )
}
