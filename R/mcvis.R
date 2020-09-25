#' @author Chen Lin, Kevin Wang, Samuel Mueller
#' @title Multi-collinearity Visualization
#' @param X A matrix of regressors (without intercept terms).
#' @param sampling_method The resampling method for the data.
#' Currently supports 'bootstrap' or 'cv' (cross-validation).
#' @param standardise_method The standardisation method for the data.
#' Currently supports 'euclidean' (default, centered by mean and divide by Euclidiean length)
#' and 'studentise' (centred by mean and divide by standard deviation)
#' and 'none' (no standardisation)
#' @param times Number of resampling runs we perform. Default is set to 1000.
#' @param k Number of partitions in averaging the MC-index. Default is set to 10.
#' @return A list of outputs:
#' \itemize{
#' \item{t_square:}{The t^2 statistics for the regression between the VIFs and the tau's.}
#' \item{MC:}{The MC-indices}
#' \item{col_names:}{Column names (export for plotting purposes)}
#' }
#' @importFrom magrittr %>%
#' @importFrom purrr map map2
#' @importFrom stats coef lm
#' @importFrom graphics par plot text
#' @importFrom assertthat assert_that
#' @rdname mcvis
#' @export
#' @examples
#' set.seed(1)
#' p = 10
#' n = 100
#' X = matrix(rnorm(n*p), ncol = p)
#' X[,1] = X[,2] + rnorm(n, 0, 0.1)
#' mcvis_result = mcvis(X = X)
#' mcvis_result
mcvis <- function(X, sampling_method = "bootstrap", standardise_method = "studentise", times = 1000L, k = 10L) {
    assertthat::assert_that(all(sapply(X, is.numeric)), msg = "All columns of X must be numeric")
    assertthat::assert_that(sum(is.na(X)) == 0, msg = "Missing values detected. Please remove.")
    X = as.matrix(X)

    dup_columns = duplicated(X, MARGIN = 2)

    if (any(dup_columns)) {
        warning("Duplicated columns found, mcvis is stopped. \n Returning indices of duplicated columns.")
        return(dup_columns)
    }


    n = nrow(X)
    p = ncol(X)  ## We now enforce no intercept terms

    if (is.null(colnames(X))) {
        n_digits = floor(log10(p)) + 1L
        col_names = sprintf(paste0("X%0", n_digits, "d"), seq_len(p))
    } else {
        col_names = colnames(X)
    }


    ## One can choose the max variables and eigenvectors in the plot

    ## Initialise the matrice

    X = as.matrix(X)

    if (sampling_method == "bootstrap") {
        index = replicate(times, sample(n, replace = TRUE), simplify = FALSE)
    } else if (sampling_method == "cv") {
        index = replicate(times, sample(n, replace = FALSE)[1:(floor(sqrt(p * n)))], simplify = FALSE)
    } else {
        stop("Only bootstrap and cross-validation are currently supported")
    }

    list_mcvis_result = switch(standardise_method,
                               euclidean = purrr::map(.x = index, .f = ~ one_mcvis_euclidean(X = X, index = .x)),
                               studentise = purrr::map(.x = index, .f = ~ one_mcvis_studentise(X = X, index = .x)),
                               none = purrr::map(.x = index, .f = ~ one_mcvis_none(X = X, index = .x)))

    list_tau = purrr::map(list_mcvis_result, "tau") %>% do.call(cbind, .)
    list_vif = purrr::map(list_mcvis_result, "vif") %>% do.call(cbind, .)
    mean_vif = rowMeans(list_vif)
    names(mean_vif) = col_names

    avg_eigenv = rowMeans(1/list_tau)
    conditional_number = sqrt(avg_eigenv[1]/avg_eigenv[p])
    ##############################
    list_index_block = unname(base::split(1:times, sort((1:times)%%k)))
    t_square = matrix(0, p, p)

    for (j in 1:p) {

        list_tstat = lapply(list_index_block, function(this_index) {
            lm_obj = lm(list_tau[j, this_index] ~ t(list_vif[, this_index]))
            tstat = coef(summary(lm_obj))[, "t value"]
        })

        tstat_mat = unname(do.call(cbind, list_tstat))
        t_square[j, ] = rowMeans(tstat_mat^2)[-1]
    }

    MC = t_square/rowSums(t_square)
    ## MC[j,i]: jth smallest eigenvalue with ith variable rownames(MC) = sprintf('tau_%02d', rev(seq_len(p)))
    rownames(MC) = paste0("tau", 1:p)
    colnames(MC) = col_names
    ####################################################################
    result = list(mean_vif = mean_vif, t_square = t_square, MC = MC, col_names = col_names,
                  conditional_number = conditional_number)

    class(result) = "mcvis"
    return(result)
}

#' @export
print.mcvis = function(x, ...) {
    p = nrow(x$MC)
    print(round(x$MC[p, ,drop = FALSE], 2))
}

one_mcvis_euclidean = function(X, index) {
    X1 = X[index, ]  ## Resampling on the rows
    X2 = sweep(x = X1, MARGIN = 2, STATS = colMeans(X1), FUN = "-")
    s = as.matrix(sqrt(colSums(X2^2)))
    Z = sweep(x = X2, MARGIN = 2, STATS = as.vector(s), FUN = "/")  ## Standardizing
    x_norm = as.matrix(sqrt(colSums(X1^2)))
    v = as.vector(s/x_norm)
    D = diag(v)
    Z1 = Z %*% D
    crossprodZ1 = crossprod(Z1, Z1)
    svd_obj = svd(crossprodZ1)
    tau = 1/svd_obj$d

    X1_student = scale(X1)
    n = nrow(X1_student)
    crossprodX1 = crossprod(X1_student, X1_student)
    vif = (n - 1) * diag(solve(crossprodX1))

    result = list(tau = tau, vif = vif)
    return(result)
}


one_mcvis_studentise = function(X, index) {
    X1_student = scale(X[index, ])  ## Resampling on the rows
    crossprodX1 = crossprod(X1_student, X1_student)
    n = nrow(X1_student)
    svd_obj = svd(crossprodX1)
    tau = 1/svd_obj$d
    vif = (n - 1) * diag(solve(crossprodX1))
    result = list(tau = tau, vif = vif)
    return(result)
}

one_mcvis_none = function(X, index) {
    X1 = X[index, ]  ## Resampling on the rows
    p = ncol(X1)

    r2 = vector("numeric", p)

    for (j in 1:p) {
        y = X1[, j, drop = FALSE]
        r2[j] = summary(lm(y ~ X1[, -j]))$r.squared
    }
    vif = 1/(1 - r2)
    names(vif) = colnames(X1)

    crossprodX1 = crossprod(X1, X1)
    svd_obj = svd(crossprodX1)
    tau = 1/svd_obj$d

    result = list(tau = tau, vif = vif)
    return(result)
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
