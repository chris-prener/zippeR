


sample <- zi_get_geometry(year = 2020, state = "MA", method = "centroid")

x <- filter(sample, substr(GEOID20, 1, 2) %in% c("02") == TRUE)

sample2 <- zi_get_geometry(year = 2020, state = "MO", county = "29510")



