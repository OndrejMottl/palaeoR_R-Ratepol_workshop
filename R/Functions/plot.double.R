.plot.double <-function(data_source_a_plot,
                        data_source_b_plot,
                        relative_widths = 1){
  cowplot::plot_grid(
    data_source_a_plot,
    data_source_b_plot + 
      ggpubr::rremove("y.title") + 
      ggpubr::rremove("y.text") + 
      ggpubr::rremove("y.ticks"),
    ncol = 2,
    align = "h",
    axis = "bt",
    rel_widths = relative_widths) %>% 
    return()
}

