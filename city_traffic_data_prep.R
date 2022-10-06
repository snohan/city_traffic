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

trp_metadata <-
  trps %>%
  split_road_system_reference() %>%
  dplyr::left_join(
    trp_data_time_span,
    by = "trp_id"
  ) %>%
  dplyr::select(
    trp_id,
    name,
    traffic_type,
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

## Pointindex and trp ----
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

## MDT ----
mdt_2019 <- get_mdt_for_trp_list(city_trps, "2019")
mdt_2020 <- get_mdt_for_trp_list(city_trps, "2020")
mdt_2021 <- get_mdt_for_trp_list(city_trps, "2021")
mdt_2022 <- get_mdt_for_trp_list(city_trps, "2022")

mdts <- dplyr::bind_rows(
  mdt_2019,
  mdt_2020,
  mdt_2021,
  mdt_2022
) %>%
  dplyr::filter(coverage > 50) %>%
  dplyr::select(trp_id, year, month, mdt) %>%
  tidyr::complete(trp_id = city_trps, year, month) %>%
  dplyr::filter(
    month %in% c(6, 7, 8)
  )


trp_mdt_long <-
  trp_metadata %>%
  dplyr::filter(
    trp_id %in% city_trps,
    road_category %in% c("E", "R")
  ) %>%
  dplyr::mutate(
    road_category_and_number_and_point_name =
      paste0(road_category_and_number, " ", name)
  ) %>%
  dplyr::left_join(mdts, by = "trp_id") %>%
  dplyr::mutate(
    month_object = lubridate::make_date(year = 2000, month = month),
    month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)) %>%
  dplyr::select(
    trp_id,
    road_category_and_number_and_point_name,
    year,
    month_object,
    mdt
  ) %>%
  dplyr::filter(
    trp_id %in% c(
      "40820V121304",
      "57166V121303",
      "00000V1702725",
      "26071V121746",
      "02466V121760",
      "06702V121764",
      "57502V121402"
    )
  )

trp_mdt_long %>%
  saveRDS(file = "data/trp_mdt_long_krs.rds")


## Bike AADT ----
bike_trp_krs <-
  trp_metadata %>%
  dplyr::filter(
    municipality_name == "Kristiansand",
    traffic_type == "BICYCLE"
  ) %>%
  dplyr::mutate(
    road_category_and_number_and_point_name =
      paste0(road_category_and_number, " ", name)
  )

bike_adt <-
  get_aadt_for_trp_list(
    bike_trp_krs$trp_id
  ) %>%
  dplyr::filter(
    year > 2015
  ) %>%
  dplyr::rename(
    aadt = adt
  )

bike_trp_adt <-
  bike_trp_krs %>%
  dplyr::select(
    trp_id,
    name,
    road_category,
    road_category_and_number,
    road_category_and_number_and_point_name
  ) %>%
  dplyr::right_join(
    bike_adt,
    by = "trp_id"
  )

bike_trp_adt %>%
  saveRDS(file = "data/bike_trp_adt_krs.rds")


## Bike MDT ----
bike_mdt_2019 <- get_mdt_for_trp_list(bike_trp_krs$trp_id, "2019")
bike_mdt_2020 <- get_mdt_for_trp_list(bike_trp_krs$trp_id, "2020")
bike_mdt_2021 <- get_mdt_for_trp_list(bike_trp_krs$trp_id, "2021")
bike_mdt_2022 <- get_mdt_for_trp_list(bike_trp_krs$trp_id, "2022")

bike_mdts <-
  dplyr::bind_rows(
    bike_mdt_2019,
    bike_mdt_2020,
    bike_mdt_2021,
    bike_mdt_2022
  ) %>%
  dplyr::filter(
    coverage > 50,
    month %in% c(6, 7, 8)
  ) %>%
  dplyr::select(trp_id, year, month, mdt) %>%
  tidyr::complete(trp_id = bike_trp_adt$trp_id, year, month)


bike_mdt_long <-
  bike_trp_krs %>%
  dplyr::left_join(
    bike_mdts,
    by = "trp_id"
  ) %>%
  dplyr::mutate(
    month_object = lubridate::make_date(year = 2000, month = month),
    month_name = lubridate::month(month_object, label = TRUE, abbr = FALSE)
  ) %>%
  dplyr::select(
    trp_id,
    road_category_and_number_and_point_name,
    year,
    month_object,
    mdt
  )

# TODO: sort by roadref
bike_mdt_long %>%
  saveRDS(file = "data/bike_trp_mdt_long_krs.rds")


# Tromsø ----
## Bike AADT ----
bike_trp_tro <-
  trp_metadata %>%
  dplyr::filter(
    municipality_name == "Troms\u00f8",
    traffic_type == "BICYCLE"
  )

bike_adt_tro <-
  get_aadt_for_trp_list(
    bike_trp_tro$trp_id
  ) %>%
  dplyr::filter(
    year > 2015
  ) %>%
  dplyr::rename(
    aadt = adt
  )

bike_trp_adt_tro <-
  bike_trp_tro %>%
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
    bike_adt_tro,
    by = "trp_id"
  )

bike_trp_adt_tro %>%
  saveRDS(file = "data/bike_trp_adt_tro.rds")


# Big cities ----
bike_trps <-
  trp_metadata %>%
  dplyr::filter(
    municipality_name %in%  c(
      "Oslo",
      "Trondheim",
      "Bergen",
      "Stavanger",
      "Kristiansand",
      "Troms\u00f8"
    ),
    traffic_type == "BICYCLE"
  ) %>%
  dplyr::mutate(
    road_category_and_number_and_point_name =
      paste0(road_category_and_number, " ", name)
  )

## Bike MDT ----
bike_mdt_2019 <- get_mdt_for_trp_list(bike_trps$trp_id, "2019")
bike_mdt_2020 <- get_mdt_for_trp_list(bike_trps$trp_id, "2020")
bike_mdt_2021 <- get_mdt_for_trp_list(bike_trps$trp_id, "2021")
bike_mdt_2022 <- get_mdt_for_trp_list(bike_trps$trp_id, "2022")

bike_mdts <-
  dplyr::bind_rows(
    bike_mdt_2019,
    bike_mdt_2020,
    bike_mdt_2021,
    bike_mdt_2022
  ) %>%
  dplyr::filter(
    coverage > 25,
    month %in% c(5, 6, 7, 8, 9)
  ) %>%
  dplyr::select(trp_id, year, month, mdt) %>%
  tidyr::complete(trp_id = bike_trps$trp_id, year, month)

# Need at least three months in a year
bike_trps_enough_data <-
  bike_mdts |>
  dplyr::filter(
    !is.na(mdt)
  ) |>
  group_by(
    trp_id,
    year
  ) |>
  dplyr::summarise(
    n_months = n(),
    .groups = "drop_last"
  ) |>
  dplyr::filter(
    n_months >= 3
  )

bike_trps_in_2022 <-
  bike_trps_enough_data |>
  dplyr::filter(
    year == 2022
  )

bike_trps_one_more_year_than_2022 <-
  bike_trps_enough_data |>
  dplyr::filter(
    trp_id %in% bike_trps_in_2022$trp_id
  ) |>
  dplyr::group_by(
    trp_id
  ) |>
  dplyr::summarise(
    n_years = n()
  ) |>
  dplyr::filter(
    n_years > 1
  )

bike_mdt_mean <-
  bike_mdts %>%
  dplyr::filter(
    trp_id %in% bike_trps_one_more_year_than_2022$trp_id
  ) |>
  dplyr::group_by(
    trp_id,
    year
  ) |>
  dplyr::summarise(
    mean_mdt = base::mean(mdt, na.rm = TRUE) |>
      base::round()
  ) |>
  dplyr::filter(
    !is.nan(mean_mdt)
  )


bike_trp_mdt <-
  bike_mdt_mean |>
  dplyr::left_join(
    bike_trps,
    by = "trp_id"
  ) |>
  dplyr::select(
    trp_id,
    road_category_and_number_and_point_name,
    municipality_name,
    year,
    mean_mdt
  ) |>
  dplyr::arrange(
    municipality_name,
    road_category_and_number_and_point_name
  )

bike_trp_mdt %>%
  saveRDS(file = "data/big_cities_bike_mdt.rds")
