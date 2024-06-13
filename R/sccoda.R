#' Prepare a Anndata for scCODA
#'
#' @param data A data.frame. Alternatively, a
#' [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment] or
#' [SingleCellExperiment][SingleCellExperiment::SingleCellExperiment], in this
#' way, the `colData` will be used to build `AnnData`.
#' @param sample A string. If specified, `data` is expected to be in long
#' format, with `celltype` defining the column information of cell types. If
#' `counts` is `NULL`, the internal function will count each cell type.
#' Otherwise, the column in `counts` will be used directly and transformed into
#' a wide data.frame. If `NULL`, `data` is expected to be in wide format, with
#' columns specified in `celltype` containing the count data for each cell type.
#' @param celltype A string define the cell types for each cell (in rows) if
#' `sample` is not `NULL`, or an atomic character define the columns of counts
#' for each cell if `sample` is `NULL`.
#' @param covariates Other columns to be added into final object.
#' @param counts A string define the column of sample information, only used
#' when `sample` is not `NULL`.
#' @param ... Additional arguments passed to specific methods.
#' @inheritParams sccoda
#' @return A Python `AnnData` object
#' @export
sccoda_data <- function(data, ...) {
    UseMethod("sccoda_data")
}

#' @rdname sccoda_data
#' @export
sccoda_data.data.frame <- function(data, covariates, celltype, sample = NULL, counts = NULL, ..., conda = NULL) {
    assert_string(sample, null_ok = TRUE)
    if (is.null(sample)) {
        sccoda_data_wide_data_frame(
            data = data,
            covariates = covariates, celltype = celltype,
            conda = conda
        )
    } else {
        sccoda_data_long_data_frame(
            data = data, sample = sample, covariates = covariates,
            celltype = celltype, counts = counts,
            conda = conda
        )
    }
}

#' @rdname sccoda_data
#' @export
sccoda_data.SummarizedExperiment <- function(data, covariates, celltype = "label", sample = "Sample", counts = NULL, ..., conda = NULL) {
    assert_string(sample)
    data <- data.table::as.data.table(SummarizedExperiment::colData(data))
    sccoda_data_long_data_frame(
        data = data, sample = sample, covariates = covariates,
        celltype = celltype, counts = counts, conda = conda
    )
}

sccoda_data_wide_data_frame <- function(data, covariates, celltype, conda = NULL) {
    data <- data.table::as.data.table(data)
    data <- data[, .SD, .SDcols = c(celltype, covariates)] # nolint
    sccoda_data_prepare_pandas(data, covariates, conda = conda)
}

sccoda_data_long_data_frame <- function(data, sample, covariates, celltype, counts = NULL, conda = NULL) {
    assert_string(celltype)
    assert_string(counts, null_ok = TRUE)
    for (covariate in covariates) {
        assert_data_frame_hierarchy(data, covariate, sample)
    }
    data <- data.table::as.data.table(data)
    data <- data[, .SD, .SDcols = c(sample, celltype, covariates, counts)] # nolint
    if (is.null(counts)) {
        data <- data[, list(
            .__counts__. = .N # nolint
        ), by = c(sample, celltype, covariates)]
    } else {
        if (!is.numeric(data[[counts]])) {
            cli::cli_abort("{.code data[[counts]]} must be an atomic integer")
        }
        if (anyDuplicated(data, by = c(sample, celltype))) {
            cli::cli_abort(c(
                "{.arg sample}, {.arg celltype}, {.arg covariates} must identify unique observations"
            ))
        }
        data$.__counts__. <- as.integer(data[[counts]])
    }
    lhs <- Reduce(function(x, y) {
        rlang::expr(!!x + !!y)
    }, rlang::syms(c(sample, covariates)))
    rhs <- rlang::sym(celltype)
    data <- data.table::dcast(data,
        rlang::new_formula(lhs, rhs),
        value.var = ".__counts__.",
        fill = 0L
    )
    sccoda_data_prepare_pandas(data, c(sample, covariates), conda = conda)
}

sccoda_data_prepare_pandas <- function(data, covariates, conda = NULL) {
    if (length(covariates) == 1L) {
        covariates <- list(covariates)
    }
    pysccoda(conda = conda)$util$cell_composition_data$from_pandas(
        df = data,
        covariate_columns = covariates
    )
}

#' Bayesian model for compositional single-cell data analysis
#'
#' scCODA is a toolbox for statistical models to analyze changes in
#' compositional data, especially from single-cell RNA-seq experiments. Its main
#' purpose is to provide a platform and implementation for the scCODA model,
#' which is described by [Büttner, Ostner et
#' al.](https://www.nature.com/articles/s41467-021-27150-6).
#'
#' @inheritParams sccoda_analysis
#' @inheritDotParams sccoda_sampling -model
#' @param conda A character vector of preferred python environment names to
#' search for and use. Details see [condaenv].
#' @export
sccoda <- function(data, formula, reference = "automatic", ..., conda = NULL) {
    model <- sccoda_analysis(data, formula, reference, conda = conda)
    sccoda_sampling(model = model, ...)
}

#' Performing compositional analysis with scCODA.
#'
#' @param data A Python `AnnData` object returned by [sccoda_data].
#' @param formula Patsy-style formula for building the covariate matrix.
#' Categorical covariates are handled automatically, with the covariate value of
#' the first sample being used as the reference category. To set a different
#' level as the base category for a categorical covariate, use
#' `C(<CovariateName>, Treatment('<ReferenceLevelName>'))`.
#' @param reference The `scCODA` model requires a cell type to be set as the
#' reference category. However, choosing this cell type is often difficult. A
#' good first choice is a referenece cell type that closely preserves the
#' changes in relative abundance during the compositional analysis.
#' @inheritParams sccoda_data
#' @inheritParams sccoda
#' @export
sccoda_analysis <- function(data, formula, reference = "automatic",
                            conda = NULL) {
    assert_s3_class(data, "anndata._core.anndata.AnnData")
    if (rlang::is_formula(formula)) {
        formula <- deparse(rlang::f_rhs(formula))
    } else if (!rlang::is_string(formula)) {
        cli::cli_abort("{.arg formula} must be a string or a {.cls formula}")
    }
    pysccoda(conda)$util$comp_ana$CompositionalAnalysis(
        data = data,
        formula = formula,
        reference_cell_type = reference
    )
}

#' Performing MCMC sampling
#'
#' @param model Result returned by [sccoda_analysis]
#' @param sampling There are three different MCMC sampling methods available for
#' `scCODA`. Default: "hmc".
#'  - `hmc`: Hamiltonian Monte Carlo (HMC) sampling,
#'  - `da`: HMC sampling with Dual-averaging step size adaptation.
#'  - `nuts`: No-U-Turn sampling.
#' @param num_results MCMC chain length. Default: 20000 for "hmc" and "da"
#' sampling; 10000 for "nuts" sampling.
#' @param num_burnin Number of burnin iterations. Default: 5000.
#' @param num_adapt_steps Length of step size adaptation procedure.
#' @param num_leapfrog_steps HMC leapfrog steps (default 10).
#' @param max_tree_depth Maximum tree depth (default 10).
#' @param step_size Initial step size (default 0.01).
#' @param verbose If true, a progress bar is printed during MCMC sampling.
#' @export
sccoda_sampling <- function(
    model, num_results = NULL, num_burnin = 5000L, num_adapt_steps = NULL,
    num_leapfrog_steps = 10L, max_tree_depth = 10L, step_size = 0.01,
    verbose = TRUE, sampling = NULL) {
    sampling <- match.arg(sampling, c("hmc", "da", "nuts"))
    num_results <- num_results %||%
        switch(sampling,
            hmc = ,
            da = 20000L,
            nuts = 10000L
        )
    params <- list(
        num_results = num_results, num_burnin = num_burnin,
        num_adapt_steps = num_adapt_steps, step_size = step_size,
        verbose = verbose
    )
    params <- switch(sampling,
        hmc = ,
        da = c(params, list(num_leapfrog_steps = num_leapfrog_steps)),
        nuts = c(params, list(max_tree_depth = max_tree_depth))
    )
    if (sampling == "da") sampling <- "hmc_da"
    sampling <- sprintf("sample_%s", sampling)
    rlang::inject(model[[sampling]](!!!params))
}

#' Synthetic data generation in scCODA
#'
#' @param type A string from following items.
#'  - `case_control`: Generates compositional data with binary covariates.
#'  - `bw`: Calculates intercepts and slopes from a starting count and an
#'    absolute change for the first cell type.
#'  - `effect_matrix`: Generates a sparse effect matrix
#'  - `counts`: Calculates a count vector from a given first entry, length and
#'    sum.
#' @param ... Additional arguments to generate data.
#' @inheritParams sccoda
#' @seealso
#' <https://sccoda.readthedocs.io/en/latest/api.html>
#' @export
sccoda_generate_data <- function(type = "case_control", ..., conda = NULL) {
    type <- match.arg(
        type,
        c("case_control", "effect_matrix", "bw", "counts")
    )
    nm <- switch(type,
        case_control = "generate_case_control",
        bw = "b_w_from_abs_change",
        counts = "counts_from_first",
        effect_matrix = "sparse_effect_matrix"
    )
    pysccoda(conda)$util$data_generation[[nm]](...)
}

#' Import datasets from scCODA
#'
#' @param name The name of dataset to extract.
#' @inheritParams sccoda
#' @export
sccoda_datasets <- function(name, conda = NULL) {
    pysccoda(conda)$datasets[[name]]()
}

pysccoda <- function(conda) {
    check_condaenv(conda)
    import2("sccoda", envir = conda)
}
