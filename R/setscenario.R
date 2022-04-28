#' setscenario() Function
#'
#' This function is setting up a parameters that can be used in RangeShiftR::RunRS() to run a simulation.
#' @param csv_file : The name of csv format file. It should be loacated in the working directory.
#' @param folder: [complete].
#' @keywords setscenario
#' @import RangeShiftR
#' @export
#' @examples
#' setscenario()


# Parameter Master

setscenario <- function(csv_file, folder = "data/"){

  require(RangeShiftR)
  require(raster)

  inputs_file <- list.files(paste0(folder, "Inputs")) # asc raster files
  data_df <- read.csv(paste0(folder, csv_file), header = T) # csv files of sp_demography and sp_dispersal
  data_df <- data.frame(mapply(rbind, data_df, data_df))
  data_df$ID <- as.integer(data_df$ID)
  data_df$Stages <- as.integer(data_df$Stage)
  data_df$MaxAge <- as.numeric(data_df$MaxAge)
  data_df$pReprod <- as.numeric(data_df$pReprod)
  data_df$nOffspring <- as.numeric(data_df$nOffspring)
  data_df$pSurv <- as.numeric(data_df$pSurv)
  data_df$pDisp_0 <- as.numeric(data_df$pDisp_0)
  data_df$pDisp_1 <- as.numeric(data_df$pDisp_1)
  data_df$DispDist <- as.numeric(data_df$DispDist)
  data_df$DensDep <- as.numeric(data_df$DensDep)


  if(length(inputs_file) == nrow(data_df)){

    lands <- c()
    demos <- c()
    disps <- c()
    inits <- c()

    for(i in 1:length(inputs_file)){

      obs <- data_df[i, ]
      asc_raster <- inputs_file[i]

      # We should save asc raster files in the data folder
      lands <- c(lands, ImportedLandscape(LandscapeFile = paste0(asc_raster),
                                          # If the distance of specific species are smaller than 1000, resolution will be the distance.
                                          Resolution = as.numeric(ifelse(data_df$DispDist[i] >= 1000, yes = 1000,
                                                                         no = data_df$DispDist[i])),
                                          HabPercent = T,
                                          K_or_DensDep = as.numeric(data_df$DensDep[i]))) # Density Dependence 1/b

      demos <- c(demos, sp_demography(obs$Stages,
                                      obs$MaxAge,
                                      obs$pReprod,
                                      obs$nOffspring,
                                      obs$pSurv))

      disps <- c(disps, sp_dispersal(Distances = as.numeric(obs$DispDist),
                                     Stages = obs$Stages,
                                     prob_dispersal_0 = obs$pDisp_0,
                                     prob_dispersal_1 = obs$pDisp_1))

      inits <- c(inits, Initialise(InitType = 0,
                                   FreeType = 1,
                                   InitDens = 2, # Set the number of individuals per cell
                                   IndsHaCell = 1, # set initial density to 2 individuals per cell
                                   PropStages = c(0, 1))) # We initialize only adults.

    }

    params_mat <- matrix(c(lands, demos, disps, inits), ncol = 4, byrow = F)

  }

  else{

    print("Your map files and datasets are not matched.")

    break

  }

  sim <- Simulation(Simulation = 0,
                    Replicates = 10,
                    Years = 70,
                    OutIntPop = 10)

  s_list <- c()

  for(i in 1:nrow(data_df)){

    s <- RSsim(batchnum = i, # Batch ID's prevent overwriting
               land = params_mat[[i, 1]],
               demog = params_mat[[i, 2]],
               dispersal = params_mat[[i, 3]],
               init = params_mat[[i, 4]],
               simul = sim,
               seed = 324135)

    s_list <- c(s_list, s)

  }

  return(s_list)

}
