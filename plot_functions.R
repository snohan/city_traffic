create_xdt_barplot <- function(trp_xdt_long_format) {

  x_axis_labels <-
    min(trp_xdt_long_format$year):max(trp_xdt_long_format$year)

  trp_xdt_long_format %>%
    ggplot2::ggplot(aes(x = year, y = xdt)) +
    ggplot2::geom_col(
      width = 0.5,
      colour = "#ed9300",
      fill = "#ed9300"
    ) +
    ggplot2::geom_text(
      aes(
        y = 0,
        vjust = -1,
        label = xdt
      )
    ) +
    ggplot2::facet_grid(
      rows = vars(road_category_and_number_and_point_name),
      labeller = label_wrap_gen(width = 12),
      scales = "free_y"
    ) +
    theme_light(base_size = 10) +
    theme(
      #axis.text.x = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text.y = element_text(angle = 90),
      strip.background = element_rect(fill = "#444f55"),
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.position = "bottom",
      legend.background = element_rect(fill = svv_background_color),
      legend.key = element_blank()
    ) +
    scale_x_continuous(
      labels = x_axis_labels,
      breaks = x_axis_labels
    ) +
    labs(
      x = NULL,
      y = "Gjennomsnittlig antall passeringer per dag",
      caption = "Data: Statens vegvesen og fylkeskommunene"
    ) +
    ggtitle(
      "Gjennomsnittlig antall passeringer per dag",
      subtitle = ""
    )
}

create_mdt_barplot <- function(trp_mdt_long_format) {

  min_month_object <- min(trp_mdt_long_format$month_object)
  max_month_object <- max(trp_mdt_long_format$month_object)

  trp_mdt_long_format %>%
    dplyr::mutate(year = as.character(year)) %>%
    ggplot2::ggplot(aes(x = month_object, y = mdt, fill = year)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_grid(
      rows = vars(road_category_and_number_and_point_name),
      labeller = label_wrap_gen(width = 12),
      scales = "free_y"
    ) +
    theme_light(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text.y = element_text(angle = 90),
      strip.background = element_rect(fill = "#444f55"),
      plot.background = element_rect(fill = svv_background_color),
      panel.background = element_rect(fill = svv_background_color),
      legend.position = "bottom",
      legend.background = element_rect(fill = svv_background_color),
      legend.key = element_blank()
    ) +
    scale_x_date(
      breaks = scales::breaks_width("months"),
      labels = scales::label_date("%b"),
      #limits = c(min_month_object, max_month_object)
    ) +
    scale_fill_viridis_d(
      name = "\u00c5r",
      option = "viridis"
    ) +
    labs(x = NULL, y = "M\u00e5nedsd\u00f8gntrafikk",
         caption = "Data: Statens vegvesen og fylkeskommunene") +
    ggtitle("Gjennomsnittlig antall passeringer per dag",
            subtitle = "")
}
