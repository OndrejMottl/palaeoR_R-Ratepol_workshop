.plot.uncertainties <- function(data_source, n_uncertainties = 50){
  data_source %>% 
    t() %>%
    .[, c(sample(1:nrow(data_source), n_uncertainties))] %>% 
    as.data.frame() %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(depth = example_01_level$depth) %>% 
    tidyr::pivot_longer(cols = -c(depth)) %>% 
    ggplot(
      aes(
        x = depth,
        y = value,
        group = name))+
    geom_line(alpha = 1/3, size = 1)+
    coord_flip()+
    scale_x_continuous(trans = "reverse")+
    scale_y_continuous(trans = "reverse")+
    theme_custom()+
    labs(
      y = "Age (cal yr BP)",
      x = "Depth (cm)") %>% 
    return()
}