# Function for plot of factor variables
# Counts and summarises levels of a factor
# Plots with geom_lollipop from ggalt

factor_plot <- function(df, group){

  group <- enquo(group)

  x <-
    df %>%
    group_by(!!group) %>%
    summarise(N = n()) %>%
    mutate(pct = round((N/sum(N)) * 100, 1))

  ggplot(x, aes_(substitute(group), substitute(N), colour = substitute(group))) +
    geom_lollipop(point.size = 3) +
    geom_text(aes(label=paste0(pct,"%")), size=3, hjust = -0.3, vjust = -0.2) +
    scale_fill_manual(values = ubdc_palette) +
    theme(legend.position = "none",
          title = element_text(hjust = 0),
          axis.title.x = element_text(hjust = 0.5),
          plot.caption = element_text(colour = "#AAAAAA", size = 8)) +
    labs(y = "",
         title = "Number and proportion of individuals receiving social care")

}
