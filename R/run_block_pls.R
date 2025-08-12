#' Individuals and variables graphic of a multiblock PLS regression
#'
#' @param data The data you want to work on
#' @param var_to_select a data.frame with two columns, the first columns contains the names of the variables
#' used for the regression and must be called "var". The second one contains the names of the blocks assigned to ech variables on the var column.
#' @param blocks_X a character vector containing the name of each blocks
#' @param block_Y a character vector containing the name of each variables belonging to Y block
#' @param ncomp the number of components you want to keep for multiblock PLS regression
#' @param design the design matrix of PLS regression (use "null" or "full")
#' @param group if you want to shape the points on the individuals graph by a group variable
#' precise the name of the group variable
#' @param var_color a continuous variable to fill the points with a gradient.
#'
#' @return Both individual and variables graphics
#' @author Gaspard Martet
#' @export
#'
#' @examples res <- run_block_pls(
#' data = df,
#' var_to_select = var_to_select,
#' blocks_X = c("block_1","block_2","block_3","block_4"),
#' block_Y = c("Y_1","Y_2"),
#' ncomp = 2,
#' group = "sex",
#' var_color = "Y_1"
#' )

run_block_pls <- function(data, var_to_select, blocks_X, block_Y,
                          ncomp = 2, design = "null", group, var_color) {

  # 1. Construire Y
  Y <- scale(as.matrix(data[, colnames(data) %in% block_Y]))

  # 2. Construire la liste X automatiquement
  X <- lapply(blocks_X, function(b) {
    scale(as.matrix(data[, colnames(data) %in%
                           var_to_select$var[var_to_select$block %in% b]]))
  })
  names(X) <- blocks_X

  # 3. Lancer le block.spls
  out.pls <- mixOmics::block.spls(X, Y, ncomp = ncomp, design = design)

  # 4. Mettre les résultats en long pour ggplot
  df.pls <- bind_rows(lapply(names(out.pls$variates)[-length(names(out.pls$variates))], function(b) {
    as.data.frame(out.pls$variates[[b]]) %>%
      mutate(block = b,
             type = data[, colnames(data) %in% group]) # adapter si variable de groupe différente
  }))

  # 5. Plot Individus par bloc
  var_expl <- out.pls$prop_expl_var
  plots <- lapply(names(out.pls$variates)[-length(names(out.pls$variates))], function(b) {
    df_block <- df.pls %>% filter(block == b)
    df_block <- cbind.data.frame(df_block, var_color = data[,which(colnames(data) %in% var_color)])
    var1 <- round(var_expl[[b]][1] * 100, 1)
    var2 <- round(var_expl[[b]][2] * 100, 1)

    #print(head(df_block))

    ggplot(df_block, aes(x = comp1, y = comp2, shape = type, color = var_color)) +
      geom_point(alpha = .7, size = 1) +
      geom_hline(yintercept = 0, color = "grey60", linewidth = .1)+
      geom_vline(xintercept = 0, color = "grey60", linewidth = .1)+
      scale_color_gradient2(midpoint=median(df_block$var_color, na.rm = TRUE),
                            low="blue", mid="white",
                            high="red", space ="Lab" )+
      labs(title = b,
           x = paste0("comp1 (", var1, "%)"),
           y = paste0("comp2 (", var2, "%)"),
           color = var_color) +
      theme_minimal(base_size = 5)+
      theme(
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.margin = margin(t = -8),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.width = unit(0.3, "lines"),
        legend.text = element_text(margin = margin(l = 1)),
        panel.grid = element_line(linewidth = .1)
      )
  })

  g1 <- wrap_plots(plots, ncol = 3, nrow = 2, guides = "collect") +
    plot_annotation(title = "Individuals",
                    theme = theme(plot.title = element_text(size = 9, hjust = 0.5),
                                  legend.position = "bottom"))

  # 6. Cercle des corrélations
  circos <- mixOmics::plotVar(out.pls, legend = TRUE)
  dev.off() # pour fermer le device si nécessaire

  circleFun <- function(center = c(0,0), diameter = 2, npoints = 100) {
    r = diameter/2
    tt <- seq(0, 2*pi,length.out = npoints)
    data.frame(x = center[1] + r*cos(tt), y = center[2] + r*sin(tt))
  }
  circle <- rbind(circleFun(diameter = 2), circleFun(diameter = 1))

  g2 <- ggplot(circos, aes(x = x, y = y, col = Block)) +
    geom_path(aes(x, y), data = circle, col = "gray60", size = .2) +
    geom_vline(xintercept = 0, col = "gray60", size = .3) +
    geom_hline(yintercept = 0, col = "gray60", size = .3) +
    geom_point(size = 1, shape = 21, color = "white", stroke = .1) +
    geom_text_repel(aes(label = names), size = 1.3, segment.size  = 0.1,
                    segment.color = "grey50", max.overlaps = 35,
                    show.legend = TRUE) +
    labs(title = "Variables") +
    theme_minimal(base_size = 5) +
    theme(
      plot.title = element_text(hjust = 0.5),
      element_text(hjust = 0.5),
      axis.text = element_text(color = "black"),
      legend.position = "bottom",
      legend.margin = margin(t = -8),
      legend.spacing.x = unit(0.2, "cm"),
      legend.key.width = unit(0.3, "lines"),
      legend.text = element_text(margin = margin(l = 1)),
      panel.grid = element_line(linewidth = .1)
    ) +
    ggsci::scale_color_jama()

  return(list(
    model = out.pls,
    plot_indiv = g1,
    plot_var = g2,
    plot_all = ggpubr::ggarrange(g1, g2)
  ))
}

