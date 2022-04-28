#' write_asc() Function
#'
#' This function is working for creating ASCII Raster format files of species from GBIF datasets.
#' @param csv_file : The name of csv format file. It should be located in the working directory.
#' @keywords csv_file
#' @import RangeShiftR
#' @import dismo
#' @import RStoolbox
#' @import magrittr
#' @import dplyr
#' @importFrom rgbif occ_data
#' @importFrom rstatix is_extreme
#' @importFrom dplyr union select rename
#' @importFrom raster getData extent values
#' @importFrom fpc dbscan
#' @export
#' @examples
#' write_asc()


# write_asc

write_asc <- function(csv_file, folder = "data/"){

  dir.create(paste0(folder, "Inputs"), showWarnings = FALSE)
  dir.create(paste0(folder, "Outputs"), showWarnings = FALSE)
  dir.create(paste0(folder, "Output_Maps"), showWarnings = FALSE)

  data_df <- read.csv(paste0(folder, csv_file), header = T) # csv files of demo and disp

  species_xy <- c()
  rasters <- c()

  ### Cristian: I'd recommend to write a single loop instead of three. Each iteration is an species.
  ### Wonkyun: Gotcha. Not two loop, right? (Three loop?)

  for(i in 1:nrow(data_df)){
    species_xy <- c(species_xy, as.data.frame(rgbif::occ_data(scientificName = paste(data_df$Genus[i],
                                                                                     data_df$Species[i]),
                                                              hasCoordinate = T,
                                                              limit = 100000)$data) %>%
                      dplyr::select(c("decimalLatitude", "decimalLongitude", "basisOfRecord", "year")) %>%
                      rename('latitude' = 'decimalLatitude',
                             'longitude' = 'decimalLongitude',
                             'source' = 'basisOfRecord') %>%
                      dplyr::filter(!source %in% c("FOSSIL_SPECIMEN", "PRESERVED_SPECIMEN", "MATERIAL_SAMPLE")) %>%
                      dplyr::filter(latitude != 0) %>%
                      dplyr::filter(longitude != 0) %>%
                      dplyr::filter(!rstatix::is_extreme(latitude)) %>% # Remove extreme outliers
                      dplyr::filter(!rstatix::is_extreme(longitude)) %>% # Remove extreme outliers
                      dplyr::filter(year > 1945) %>%
                      list()
    )

    # Remove outliers through DBSCAN

    obs.data <- species_xy[[i]][c(2, 1)][fpc::dbscan(data = species_xy[[i]][c(2, 1)], eps = 10)$isseed, ]

    # Create an extent-range.

    geographic.extent <- raster::extent(x = c(floor(min(obs.data$longitude)),
                                              ceiling(max(obs.data$longitude)),
                                              floor(min(obs.data$latitude)),
                                              ceiling(max(obs.data$latitude))))

    # Current: build a model

    bioclim.data <- raster::getData(name = "worldclim",
                                    var = "bio",
                                    res = 10,
                                    path = "data/") %>%
      raster::crop(y = geographic.extent)

    # pca

    pcamap <- RStoolbox::rasterPCA(bioclim.data, spca = T)
    bioclim.pca <- raster::subset(x = pcamap$map, c(1, 2))

    # Build species distribution model

    bc.model <- dismo::bioclim(x = bioclim.pca, p = obs.data)

    # Predict presence from species distribution model

    pred <- bioclim.pca %>%
      dismo::bioclim(p = obs.data) %>%
      dismo::predict(x = bioclim.pca,
                     ext = geographic.extent)

    # Tuning resolutions and values as integer
    # Resolution: c(0.1666667, 0.1666667) -> c(1000, 1000)
    # If the distance of specific species are smaller than 1000, resolution will be the distance.

    distance_sp <- ifelse(data_df$DispDist[i] >= 1000, yes = 1000, no = data_df$DispDist[i])

    pred_newres <- pred # Duplicate a new file
    extent(pred_newres) <- raster::extent(pred) * distance_sp / raster::res(pred)[1]

    # Values: Change to percentage

    values(pred_newres) <- as.integer(values(pred_newres) * 100)

    # Values: NA -> 0

    values(pred_newres)[is.na(values(pred_newres))] <- 0

    # Save a raster file as integer

    raster_name <- paste(data_df$Genus[i], data_df$Species[i])

    writeRaster(pred_newres,
                filename = paste0(folder, "Inputs/", i, "_", raster_name),
                format = "ascii",
                overwrite = T,
                datatype = "INT4S")

    print(paste0("Create current ", raster_name, " asc raster file."))

    # Projected: build a model

    forecast.data <- raster::getData(name = "CMIP5", # forecast data
                                     var = "bio",    # bioclim
                                     res = 10,       # 10 minute resolution
                                     path = "data/", # destination directory
                                     model = "GD",   # GFDL-ESM2G
                                     rcp = "45",     # CO2 increase 4.5
                                     year = 70)      # 2070

    names(forecast.data) <- names(bioclim.data)

    # pca

    pcamap_fore <- RStoolbox::rasterPCA(forecast.data, spca = T)
    fore.pca <- raster::subset(x = pcamap_fore$map, c(1, 2))

    # Predict forecast from species distribution model

    fore <- dismo::predict(object = bc.model,
                           x = fore.pca,
                           ext = geographic.extent)

    fore_newres <- fore # Duplicate a new file
    extent(fore_newres) <- extent(fore) * distance_sp / res(fore)[1]

    # Values: Change to percentage

    values(fore_newres) <- as.integer(values(fore_newres) * 100)

    # Values: NA -> 0

    values(fore_newres)[is.na(values(fore_newres))] <- 0

    # Save a raster file as integer

    raster_name <- paste(data_df$Genus[i], data_df$Species[i])

    writeRaster(fore_newres,
                filename = paste0(folder, "Inputs/", i, "_", raster_name, "_70"),
                format = "ascii",
                overwrite = T,
                datatype = "INT4S")

    print(paste0("Create projected ", raster_name, " asc raster file."))

  }

}
