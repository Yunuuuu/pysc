#' Turn an object into a data.table
#'
#' @param x A python object.
#' @param ... Additional arguments to tidying method.
#' @export
pysc_tidy <- function(x, ...) {
    UseMethod("pysc_tidy")
}

#' @export
pysc_tidy.sccoda.util.result_classes.CAResult <- function(x, ...) {
    intercept <- reticulate::py_to_r(x$intercept_df)
    data.table::setDT(intercept, keep.rownames = "terms")
    effect <- reticulate::py_to_r(x$effect_df)
    data.table::setDT(effect)
    data.table::setnames(intercept, function(x) paste0("Intercept ", x))
    data.table::setnames(effect, function(x) paste0("Effect ", x))
    cbind(intercept, effect)
}

#' @export
pysc_tidy.default <- function(x, ...) {
    cli::cli_abort("No method for {obj_type_friendly(x)}")
}
