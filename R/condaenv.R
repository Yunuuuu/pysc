#' Global conda environment
#'
#' @description
#' All python related function will accept `conda` argument, if it's `NULL`,
#' then the global conda environment will be used, if it's `NA`, no conda
#' environment will be used, otherwise it must be a character non-empty string
#' indicates the conda environment name. `condaenv` just provide the way to
#' query or set the global conda environment.
#'
#' @param conda Conda environment to be use. If `NULL`, will return current
#' global conda environment, if `NA`, will remove the global conda environment
#' settting. If a character vector, will set the global conda environment.
#' @export
condaenv <- function(conda = NULL) {
    old <- get_condaenv()
    if (is.null(conda)) return(old) # styler: off
    condaenv <- check_condaenv(conda)
    set_condaenv(condaenv)
    invisible(old)
}

GLOBALS <- new.env(parent = emptyenv())
GLOBALS$condaenv <- NA_character_

with_condaenv <- function(condaenv, code) {
    old <- get_condaenv()
    set_condaenv(condaenv)
    on.exit(set_condaenv(old))
    force(code)
}

check_condaenv <- function(conda = NULL, arg = rlang::caller_arg(conda),
                           call = rlang::caller_env()) {
    if (is.null(conda)) {
        get_condaenv()
    } else {
        conda <- as.character(conda)
        if (all(conda != "", na.rm = TRUE)) {
            if (anyNA(conda) && any(!is.na(conda))) {
                cli::cli_abort(
                    paste(
                        "You must provide a single `NA` or multiple string",
                        "in {.arg {arg}}"
                    ),
                    call = call
                )
            }
            conda
        } else {
            cli::cli_abort("{.arg {arg}} cannot be empty string", call = call)
        }
    }
}

get_condaenv <- function() GLOBALS$condaenv

set_condaenv <- function(condaenv) GLOBALS$condaenv <- condaenv

import2 <- function(module, ..., envir = NULL, delay_load = NULL,
                    convert = FALSE, call = rlang::caller_env()) {
    condaenv <- check_condaenv(condaenv)
    if (!is.na(condaenv)) {
        delay_load <- c(delay_load, list(environment = condaenv))
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
