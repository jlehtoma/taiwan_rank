library(tidyverse)
library(hrbrthemes)

plot_ranks <- function(dat) {
  p <- ggplot(dat, aes(x = year, y = rank, group = university)) +
    geom_point() + geom_line() +
    scale_x_continuous(breaks = 2007:2017) + 
    scale_y_reverse(breaks = c(100, 75, 50, 25, 1), limits = c(100, 1)) +
    geom_vline(xintercept = 2010, linetype = "dashed", color = "darkgreen") + 
    geom_text(aes(x = 2010, label = "\n yliopistouudistus", y = 20), color = "darkgreen", angle = 90) + 
    geom_vline(xintercept = 2012, linetype = "dashed", color = "darkred") + 
    geom_text(aes(x = 2012, label = "\n indeksijäädytys", y = 20), color = "darkred", angle = 90) + 
    geom_vline(xintercept = 2016, linetype = "dashed", color = "darkblue") + 
    geom_text(aes(x = 2016, label = "\n koulutusleikkaukset", y = 20), color = "darkblue", angle = 90) + 
    theme_ipsum_rc()
  return(p)
}

plot_ranks_mass <- function(dat, highlight = NULL) {
  dat$color <- "others"
  if (!is.null(highlight)) {
    dat[which(dat$university == highlight), ]$color <- "highlight"
  }
  p <- ggplot(dat, aes(x = year, y = rank, group = university, color = color)) +
    geom_line() + scale_color_manual(values = c("others" = "lightgrey", "highlight" = "red")) +
    scale_x_continuous(breaks = 2007:2017) + 
    scale_y_reverse(breaks = c(100, 75, 50, 25, 1), limits = c(100, 1)) +
    geom_vline(xintercept = 2010, linetype = "dashed", color = "darkgreen") + 
    geom_text(aes(x = 2010, label = "\n yliopistouudistus", y = 20), color = "darkgreen", angle = 90) + 
    geom_vline(xintercept = 2012, linetype = "dashed", color = "darkred") + 
    geom_text(aes(x = 2012, label = "\n indeksijäädytys", y = 20), color = "darkred", angle = 90) + 
    geom_vline(xintercept = 2016, linetype = "dashed", color = "darkblue") + 
    geom_text(aes(x = 2016, label = "\n koulutusleikkaukset", y = 20), color = "darkblue", angle = 90) + 
    theme_ipsum_rc() + theme(legend.position = "none")
  return(p)
}

# Read data
rank_data <- readr::read_csv("data/taiwan_ranks.csv", na = "-")
 
hel_rank_data <- rank_data %>% 
  dplyr::filter(university == "University of Helsinki")
lun_rank_data <- rank_data %>% 
  dplyr::filter(university == "Lund University")
kar_rank_data <- rank_data %>% 
  dplyr::filter(university == "University of Edinburgh")

mid_range_unis_2007 <- rank_data %>% 
  dplyr::filter(rank >= 42 & rank < 62 & year == 2007) %>% 
  dplyr::select(university)

mid_range_data <- rank_data %>% 
  dplyr::filter(university %in% mid_range_unis_2007$university)

# Plot data

p_hel <- plot_ranks(hel_rank_data)
p_lund <- plot_ranks(lun_rank_data)
p_kar <- plot_ranks(kar_rank_data)
p_mid <- plot_ranks_mass(mid_range_data, highlight = "University of Helsinki")

ggsave("taiwan_rank_uni_hel.png", p_mid)
