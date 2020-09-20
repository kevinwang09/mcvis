#' @author Chen Lin, Kevin Wang, Samuel Mueller
#' @title Multi-collinearity Visualization plots
#' @param x Output of the mcvis function
#' @param type Plotting mcvis result using "igraph" or "ggplot". Default to "ggplot".
#' @param eig_max The maximum number of eigenvalues to be displayed on the plot.
#' @param var_max The maximum number of variables to be displayed on the plot.
#' @param angle Angle for the variable name, default to 0 (horizontal)
#' @param ... additional arguments (currently unused)
#' @rdname plot.mcvis
#' @return A mcvis visualization plot
#' @export
#' @examples
#' set.seed(1)
#' p = 10
#' n = 100
#' X = matrix(rnorm(n*p), ncol = p)
#' X[,1] = X[,2] + rnorm(n, 0, 0.1)
#' mcvis_result = mcvis(X)
#' plot(mcvis_result)
#' plot(mcvis_result, type = "igraph")
#' plot(mcvis_result, type = "alt")
plot.mcvis <- function(x, type = c("ggplot", "igraph", "alt"), eig_max = 1L, var_max = ncol(x$MC),
                       angle = 0, ...){
    type = match.arg(type)
    switch(type,
           ggplot = ggplot_mcvis(mcvis_result = x, eig_max = eig_max, var_max = var_max, angle = angle),
           igraph = igraph_mcvis(mcvis_result = x, eig_max = eig_max, var_max = var_max),
           alt = alt_mcvis(mcvis_result = x, eig_max = eig_max, var_max = var_max))
}
