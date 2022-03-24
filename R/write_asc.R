#' write_asc() Function
#'
#' This function is working for creating ASCII Raster format files of species from GBIF datasets.
#' @param csv_file : The name of csv format file. It should be loacated in the working directory.
#' @keywords csv_file
#' @export
#' @examples
#' write_asc()


# write_asc

write_asc <- function(csv_file){

  require(dplyr)
  require(RangeShiftR)
  require(raster)
  require(dismo)
  require(rstatix)

  data_df <- read.csv(paste0("data/", csv_file), header = T) # csv files of demo and disp

  species_xy <- c()
  rasters <- c()

  for(i in 1:nrow(data_df)){
    species_xy <- c(species_xy, gbif(genus = data_df$genus[i], # genus
                                     species = data_df$species[i], # species
                                     geo = T,
                                     removeZeros = T,
                                     download = T) %>%
                      dplyr::select('lat', 'lon', 'basisOfRecord', 'year') %>%
                      rename('latitude' = 'lat',
                             'longitude' = 'lon',
                             'source' = 'basisOfRecord') %>%
                      mutate(source = factor(source)) %>%
                      filter(!source %in% c("FOSSIL_SPECIMEN")) %>% # Remove Fossil data
                      filter(!is.na(latitude)) %>% # Remove Null Values
                      filter(!is.na(longitude)) %>% # Remove Null Values
                      filter(!is_extreme(latitude)) %>% # Remove Outliers
                      filter(!is_extreme(longitude)) %>% # Remove Outliers
                      filter(year > 1945) %>% # Remove records from before second world war
                      list() # Covert to list()
    )
  }

  'We might also want to exclude very old records, as they are more likely to be unreliable.
  For instance, records from before the second world war are often very imprecise,
  especially if they were geo-referenced based on political entities.
  Additionally old records might be likely from areas where species went extinct
  (for example due to land-use change). '

  # Set coordinates, x-y

  for(i in 1:nrow(data_df)){

    # Create an extent-range.

    geographic.extent <- extent(x = c(floor(min(species_xy[[i]][c(2, 1)]$longitude)),
                                      ceiling(max(species_xy[[i]][c(2, 1)]$longitude)),
                                      floor(min(species_xy[[i]][c(2, 1)]$latitude)),
                                      ceiling(max(species_xy[[i]][c(2, 1)]$latitude))))

    # Build a model

    bioclim.data <- raster::getData(name = "worldclim", # They are the average for the years 1970-2000.
                                    var = "bio",
                                    res = 10,
                                    path = "data/") %>%
      raster::crop(y = geographic.extent) # Crop bioclim data to geographic extent of saguaro

    # Predict presence from species distribution model

    predict.presence <- bioclim.data %>%
      dismo::bioclim(p = species_xy[[i]][c(2, 1)]) %>%
      dismo::predict(x = bioclim.data,
                     ext = geographic.extent)

    # Tuning resolutions and values as integer
    # Resolution: c(0.1666667, 0.1666667) -> c(1000, 1000)
    # If the distance of specific species are smaller than 1000, resolution will be the distance.

    distance_sp <- ifelse(data_df$Distances[i] >= 1000,
                          yes = 1000,
                          no = data_df$Distances[i])

    extent(predict.presence) <- extent(predict.presence) * distance_sp / res(predict.presence)[1]

    # Values: Change to percentage

    values(predict.presence) <- as.integer(values(predict.presence) * 100)

    # Values: NA -> 0

    values(predict.presence)[is.na(values(predict.presence))] <- 0

    # Save a raster file as integer

    raster_name = paste(data_df$genus[i], data_df$species[i])

    writeRaster(predict.presence,
                filename = paste0("data/Inputs/", i, "_", raster_name),
                format = "ascii",
                overwrite = T,
                datatype = "INT4S")

    print(paste0("Create ", raster_name, " asc raster file."))

  }

}
