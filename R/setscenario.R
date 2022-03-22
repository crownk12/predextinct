#' setscenario() Function
#'
#' This function is setting up a parameters that can be used in RangeShiftR::RunRS() to run a simulation.
#' @param csv_file : The name of csv format file. It should be loacated in the working directory.
#' @keywords setscenario
#' @export
#' @examples
#' setscenario()


# Parameter Master

setscenario <- function(csv_file){

  require(RangeShiftR)
  require(raster)

  inputs_file <- list.files(paste0("data/", "Inputs")) # asc raster files
  data_df <- read.csv(paste0("data/", csv_file), header = T) # csv files of demo and disp

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
                                          Resolution = ifelse(data_df$Distances[i] >= 1000,
                                                              yes = 1000,
                                                              no = data_df$Distances[i]),
                                          HabPercent = T,
                                          K_or_DensDep = 0.5)) # Density Dependence 1/b

      demos <- c(demos, demo(obs$Stages,
                             obs$MaxAge,
                             obs$prob_reproduction,
                             obs$num_offsprings,
                             obs$prob_surv))

      disps <- c(disps, disp(Distances = as.numeric(obs$Distances),
                             Stages = obs$Stages,
                             prob_dispersal_0 = obs$prob_dispersal_0,
                             prob_dispersal_1 = obs$prob_dispersal_1))

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
                    Years = 50,
                    OutIntPop = 50)

  s_list <- c()

  for(i in 1:length(inputs_file)){

    s <- RSsim(batchnum = i, # Batch ID's prevent overwriting
               land = params_mat[[i, 1]],
               demog = params_mat[[i, 2]],
               dispersal = params_mat[[i, 3]],
               init = params_mat[[i, 4]],
               simul = sim)

    s_list <- c(s_list, s)

  }

  return(s_list)

}
