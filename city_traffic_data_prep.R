# Fetch data, wrangle and save
# To avoid the Rmd file to do this for every rendering

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

# Trps ----
trps <-
  get_points() %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(
    name = stringr::str_to_title(name, locale = "no")
  )

trp_data_time_span <-
  get_trp_data_time_span()

points_metadata <- trps %>%
  split_road_system_reference() %>%
  dplyr::left_join(
    trp_data_time_span,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_category_and_number,
    county_geono,
    county_name,
    municipality_name,
    lat, lon,
    road_network_position,
    road_network_link,
    road_link_position,
    first_data_with_quality_metrics
  )



