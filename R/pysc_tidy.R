#' Turn an object into a data.table
#'
#' @param x A python object.
#' @param ... Additional arguments to tidying method.
#' @export
pysc_tidy <- function(x, ...) {
    UseMethod("pysc_tidy")
}

#' @param fdr Desired FDR value.
#' @export
#' @rdname pysc_tidy
pysc_tidy.sccoda.util.result_classes.CAResult <- function(x, fdr = 0.05, ...) {
    lst <- reticulate::py_to_r(x$summary_prepare(est_fdr = fdr, ...))
    intercept <- lst[[1L]]
    data.table::setDT(intercept, keep.rownames = "terms")
    effect <- lst[[2L]]
    data.table::setDT(effect)
    data.table::setnames(intercept, function(x) paste0("Intercept ", x))
    data.table::setnames(effect, function(x) paste0("Effect ", x))
    cbind(intercept, effect)
}

#' @export
pysc_tidy.default <- function(x, ...) {
    cli::cli_abort("No method for {obj_type_friendly(x)}")
}
