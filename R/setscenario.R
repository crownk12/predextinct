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

  inputs_file <- list.files(paste0("data/", "Inputs")) # asc raster files
  data_df <- read.csv(paste0("data/", csv_file), header = T) # csv files of demo and disp

  if(length(inputs_file) == nrow(data_df)){

    lands <- c()
    demos <- c()
    disps <- c()

    for(i in 1:length(inputs_file)){

      obs = data_df[i, ]

      # We should save asc raster files in the data folder
      lands <- c(lands, ImportedLandscape(LandscapeFile = paste0(inputs_file[i]),
                                          Resolution = 1000,
                                          HabPercent = T,
                                          K_or_DensDep = 0.5)) # Density Dependence 1/b


      demos <- c(demos, demo(obs$Stages,
                             obs$MaxAge,
                             obs$ReproductionType,
                             obs$prob_reproduction,
                             obs$num_offsprings,
                             obs$prob_surv))

      disps <- c(disps, disp(Stages = obs$Stages,
                             SexDep = as.logical(obs$SexDep),
                             prob_dispersal_0 = obs$prob_dispersal_0,
                             Distances = as.numeric(obs$Distances)))

    }

    params_mat <- matrix(c(lands, demos, disps), ncol = 3, byrow = F)

  }

  else{

    print("Your map files and datasets are not matched.")

    break

  }

  init <- Initialise(InitType = 0,
                     FreeType = 0,
                     NrCells = 10000, # We randomly distribute 10,000 individuals in suitable habitat with random
                     InitDens = 2, # Set the number of individuals per cell
                     IndsHaCell = 1, # set initial density to 2 individuals per cell
                     PropStages = c(0, 1)) # We initialize only adults.

  sim <- Simulation(Simulation = 0,
                    Replicates = 10,
                    Years = 50,
                    OutIntPop = 50)

  s_list <- c()

  for(i in 1:length(inputs_file)){

    s <- RSsim(batchnum = i, # Batch ID's prevent overwritting
               land = params_mat[[i, 1]],
               demog = params_mat[[i, 2]],
               dispersal = params_mat[[i, 3]],
               init = init,
               simul = sim)

    s_list <- c(s_list, s)

  }

  return(s_list)

}
