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
    theme(legend.position = "none") +
    labs(y = "Count",
         title = "Number of individuals receiving social care")

}
