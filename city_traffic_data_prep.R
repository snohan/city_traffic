# Fetch data, wrangle and save
# To avoid the Rmd file to do this for every rendering

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")
source("H:/Programmering/R/byindeks/index_report_functions.R")


# TRP ----
trps <-
  get_points() %>%
  dplyr::distinct(trp_id, .keep_all = T) %>%
  dplyr::mutate(
    name = stringr::str_to_title(name, locale = "no")
  )

trp_data_time_span <-
  get_trp_data_time_span()

trp_metadata <- trps %>%
  split_road_system_reference() %>%
  dplyr::left_join(
    trp_data_time_span,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_category,
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


# Kristiansand ----

# TODO: car data just by length?
# City index trps
# Pointindex used in city index
# AADT
# Bike trps
# Bike point index
# Bike MDT
# Bike AADT

pointindex_2020_krs <-
  get_published_pointindex_for_months(957, 2020, 12)
pointindex_2021_krs <-
  get_published_pointindex_for_months(957, 2021, 12)
pointindex_2022_krs <-
  get_published_pointindex_for_months(957, 2022, 4)

city_trps <- pointindex_2020_krs[[1]]

pointindices_krs <-
  dplyr::bind_rows(
    pointindex_2020_krs[[2]],
    pointindex_2021_krs[[2]],
    pointindex_2022_krs[[2]]
  ) %>%
  dplyr::filter(
    day_type == "ALL",
    is_excluded == FALSE,
    is_manually_excluded == FALSE,
    length_excluded == FALSE,
    period == "year_to_date",
    #month == 12,
    length_coverage > 0
  ) %>%
  dplyr::group_by(
    trp_id,
    year
  ) %>%
  dplyr::slice_max(
    month
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    trp_id,
    year,
    month,
    index_short,
    index_long
  )

# Find which TRPs have index all years
n_years <-
  pointindices_krs$year %>%
  unique() %>%
  length()

trps_with_index_all_years <-
  pointindices_krs %>%
  dplyr::group_by(
    trp_id
  ) %>%
  dplyr::summarise(
    n_trp_years = n()
  ) %>%
  dplyr::filter(
    n_trp_years == n_years
  )

# Calculate accumulated index



## AADT ----
adt <-
  get_aadt_by_length_for_trp_list(city_trps)

adt_filtered <-
  adt %>%
  dplyr::filter(
    length_range %in% c("[..,5.6)", "[5.6,..)")
  ) %>%
  dplyr::mutate(
    length_quality = round(aadt_valid_length / aadt_total * 100)
  ) %>%
  #dplyr::filter(
  #  length_quality > 90
  #) %>%
  dplyr::filter(
    coverage > 50
  ) %>%
  dplyr::mutate(
    length_range =
      dplyr::case_when(
        length_range == "[..,5.6)" ~ "lette",
        length_range == "[5.6,..)" ~ "tunge",
        TRUE ~ length_range
      )
  ) %>%
  dplyr::select(
    trp_id,
    year,
    length_range,
    aadt = aadt_length_range,
    coverage,
    aadt_total,
    length_quality
  )


trp_adt <-
  trp_metadata %>%
  dplyr::filter(
    trp_id %in% city_trps
  ) %>%
  dplyr::mutate(
    road_category_and_number_and_point_name =
      paste0(road_category_and_number, " ", name)
  ) %>%
  dplyr::select(
    trp_id,
    name,
    road_category,
    road_category_and_number,
    road_category_and_number_and_point_name
  ) %>%
  dplyr::right_join(
    adt_filtered,
    by = "trp_id"
  )

trp_adt %>%
  saveRDS(file = "data/trp_adt_krs.rds")
