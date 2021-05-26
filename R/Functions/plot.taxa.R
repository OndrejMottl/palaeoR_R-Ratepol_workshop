.plot.taxa <- function(data_source_taxa,
                       data_source_levels = NA,
                       n_taxa = 10){
  
  use_levels <- !all(is.na(data_source_levels))
  
  taxa_list_full <- 
    data_source_taxa %>% 
    dplyr::select(-sample_id) %>% 
    colSums() %>% 
    sort(., decreasing = TRUE)
  
  taxa_list_common <- 
    names(taxa_list_full)[1:n_taxa]
  
  data_proportion <-
    data_source_taxa %>% 
    dplyr::mutate(
      row_sum = data_source_taxa %>% 
        dplyr::select(-sample_id) %>% 
        rowSums()) %>% 
    mutate_if(
      is.numeric,
      ~ .x / row_sum)
  
  data_taxa_common <-
    data_proportion %>% 
    dplyr::select(sample_id, dplyr::all_of(taxa_list_common))
  
  if(use_levels == FALSE){
  data_to_plot <-
    data_taxa_common %>% 
    dplyr::mutate(
      levels = row_number()) %>% 
    dplyr::select(-sample_id) %>% 
    tidyr::pivot_longer(
      cols = -levels,
      names_to = "taxon",
      values_to = "proportions")
  } else {
  
    data_to_plot <-
      inner_join(
        data_taxa_common,
        data_source_levels,
        by = "sample_id") %>% 
      rename(levels = age) %>% 
      dplyr::select(-c(sample_id, depth)) %>% 
      tidyr::pivot_longer(
        cols = -levels,
        names_to = "taxon",
        values_to = "proportions")
  }

  raw_plot <-
  data_to_plot %>% 
    ggplot(
      aes(
        x = levels,
        y = proportions,
        col = taxon,
        fill = taxon)) + 
    geom_ribbon(
      aes(
        ymin = rep(0, length(proportions)),
        ymax = proportions, 
        fill = taxon), 
      color = "gray30",
      alpha = 1,
      size = 0.1)+
    facet_wrap( ~ taxon, 
                ncol = n_taxa)+
    scale_x_continuous(trans = "reverse")+
    scale_y_continuous(breaks = c(0,1)) +
    coord_flip(ylim = c(0, 1))+
    theme_custom()+
    ylab("Proportion of total pollen")
  
  
  if(use_levels == FALSE){
    raw_plot +
      xlab("Level order") %>% 
      return()
  } else {
    raw_plot +
      xlab("Age (cal yr BP)") %>% 
      return()
  }
  
  
  
}
