#' @param mcvis_result Output of the mcvis function
#' @param label_dodge If variable names are too long, it might be helpful to dodge the labelling. Default to FALSE.
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#' @rdname plot.mcvis
#' @export
ggplot_mcvis = function(mcvis_result,
                        eig_max = 1L,
                        var_max = ncol(mcvis_result$MC),
                        label_dodge = FALSE)
  ##if eig_max==1 or var_max==1, the function fails to give an output.
{
  MC_ordered = make_MC_ordered(
    mcvis_result = mcvis_result,
    eig_max = eig_max,
    var_max = var_max)

  taup = rownames(MC_ordered)[1]

  #################  ggplot #######################
  plotdf = make_plotdf(MC_ordered)
  ggplot_size_manual = c(0, 0.5, 1, 2)
  ggplot_alpha_manual = c(0, 0.5, 1, 1)

  if(label_dodge){
    axis_1 = data.frame(x=rangeTransform(as.integer(unique(plotdf$cols))),
                        y=0, label=as.character(unique(plotdf$cols)),
                        y_jitter = rep(c(0, 0.1), length.out = length(plotdf$cols)))
  } else {
    axis_1 = data.frame(x=rangeTransform(as.integer(unique(plotdf$cols))),
                        y=0, label=as.character(unique(plotdf$cols)),
                        y_jitter = rep(0, length.out = length(plotdf$cols)))
  }

  axis_2 = data.frame(x=rangeTransform(as.integer(unique(plotdf$taus))),
                      y=1, label=as.character(unique(plotdf$taus)))

  linetype_manual = c("dotted","solid")
  names(linetype_manual) = c("others", taup)
  # browser()
  gg = ggplot2::ggplot(data=plotdf) +
    geom_segment(aes(
      x=.data$cols_norm, xend=.data$taus_norm,
      y=.data$y1, yend=.data$y2,
      colour = .data$ggplot_size_cat,
      size = .data$ggplot_size_cat,
      alpha = .data$ggplot_size_cat,
      linetype = .data$linetype)) +
    geom_text(data=axis_1, aes(label=.data$label, x=.data$x, y=.data$y - 0.075 - .data$y_jitter)) +
    geom_text(data=axis_2, aes(label=.data$label, x=.data$x, y=.data$y + 0.075)) +
    geom_segment(data=axis_1, aes(x=.data$x, xend=.data$x, y=.data$y, yend=.data$y-0.025), size=0.7) +
    geom_segment(data=axis_2, aes(x=.data$x, xend=.data$x, y=.data$y, yend=.data$y+0.025), size=0.7) +
    geom_segment(x=0, xend=1, y=0, yend=0, size=0.7) +
    geom_segment(x=0, xend=1, y=1, yend=1, size=0.7) +
    scale_colour_brewer(palette = "Set1", drop = FALSE, direction = -1) +
    scale_size_manual(values = ggplot_size_manual, drop = FALSE) +
    scale_alpha_manual(values = ggplot_alpha_manual, drop = FALSE) +
    scale_linetype_manual(values = linetype_manual, drop = FALSE) +
    labs(title = "Multi-collinearity plot") +
    guides(
      colour = guide_legend(title = "Strength of MC"),
      size = FALSE,
      linetype = FALSE,
      alpha = FALSE) +
    theme_bw() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          legend.position = "bottom")

  if(label_dodge){
    result = gg + scale_y_continuous(limits=c(-0.35, 1.2), expand = c(0, 0)) +
      scale_x_continuous(limits=c(-0.2, 1.2), expand = c(0, 0))
  } else {
    result = gg + scale_y_continuous(limits=c(-0.2, 1.2), expand = c(0, 0))
  }

  return(result)
}

rangeTransform = function(x){
  if(min(x) == max(x)){
    return(0)
  } else {
    return((x - min(x)) / (max(x) - min(x)))
  }
}


make_plotdf = function(MC_ordered){
  melt_MC = reshape2::melt(
    MC_ordered,
    varnames = c("taus", "cols"),
    value.name = "weights")

  thickness = 1 - melt_MC$weights


  ## Size category is the scale of the g-matrix.
  # size_cat_5 = 0
  # size_cat_5 = max(thickness)
  # size_cat_5 = thickness[1]
  # size_cat_1 = 0.1*size_cat_5
  # size_cat_2 = 0.5*size_cat_5
  # size_cat_3 = 0.7*size_cat_5
  # size_cat_4 = 0.9*size_cat_5

  # size_cat_1 = 0.1
  # size_cat_2 = 0.15
  # size_cat_3 = 0.2
  # size_cat_4 = 0.4
  # size_cat_5 = 0.6

  ggplot_size_cat = dplyr::case_when(
    thickness <= 0.2 ~ "Small",
    thickness <= 0.3 ~ "Medium",
    TRUE ~ "Strong")


  ggplot_size_cat = factor(
    ggplot_size_cat,
    levels = c("Small", "Medium", "Strong"))

  plotdf = dplyr::mutate(melt_MC, thickness, ggplot_size_cat)

  plotdf$cols_norm = rangeTransform(as.integer(plotdf$cols))
  plotdf$taus_norm = rangeTransform(as.integer(plotdf$taus))

  plotdf$y1 = 0
  plotdf$y2 = 1

  plotdf$linetype = ifelse(plotdf$taus == rownames(MC_ordered)[1], rownames(MC_ordered)[1], "others")
  return(plotdf)
}


make_MC_ordered = function(mcvis_result, eig_max, var_max){
  MC = 1 - mcvis_result$MC
  col_names = mcvis_result$col_names
  p = length(col_names)
  eig_max = min(p, eig_max)
  var_max = min(p, var_max)
  or = order(MC[p,,drop = FALSE]) ## Order the columns of g by the smallest eigen value
  or = or[1:var_max]
  MC_ordered = MC[,or,drop = FALSE]

  if (var_max > 1) {
    MC_ordered = MC_ordered[p:(p-eig_max+1),,drop=FALSE]
  } else {
    MC_ordered = as.matrix(MC_ordered[p:(p-eig_max+1)])
  }
  # if (eig_max == 1) {MC_ordered = t(MC_ordered)}

  return(MC_ordered)
}
