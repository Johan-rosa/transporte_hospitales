
adjusted_centroids <- readxl::read_excel("data/raw_data/adjusted_cetroid.xlsx") |>
  select(-c(lng, lat)) |>
  separate(new_location, into = c("lat", "lng"), sep = ", ") |>
  mutate(
    across(ends_with("code"), \(code) stringr::str_pad(code, 2, side = "left", pad = "0")),
    across(c(lat, lng), as.numeric),
    id = stringr::str_pad(id, 6, "left", "0")
  )

saveRDS(adjusted_centroids, "data/municipio_adjusted_cengroid.rds")

hospitales_nuevos <- readxl::read_excel("data/raw_data/hospitales.xlsx", "hospitales_nuevos")
