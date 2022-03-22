#' disp() Function
#'
#' This function is a sub-function of setscenario().
#' @param Stages : Number of life-Stages (integer). Requires a minimum of 2, for "juvenile" (stage 0) and "adult". Maximum is 3.
#' @param prob_dispersal_0 : Dispersal probability (emigration probability) of stage 0 (juvenile)
#' @param prob_dispersal_1 : Dispersal probability (emigration probability) of stage 1 (sub-juvenile)
#' @param Distnaces : Distances of species dispersal
#' @keywords disp
#' @export
#' @examples
#' disp()


# Dispersal

disp <- function(Distances, Stages, prob_dispersal_0, prob_dispersal_1){

  require(RangeShiftR) # RangeShiftR should be installed.

  # Sub-function
  emig_prob <- function(Stages, prob_dispersal_0, prob_dispersal_1){ # Ignore 'SexDep' for a simple model
    if(Stages == 2){
      matrix(c(0, 1, prob_dispersal_0, 0), nrow = Stages, byrow = F)
    }
    else if(Stages == 3){
      matrix(c(0, 1, 2, prob_dispersal_0, prob_dispersal_1, 0), nrow = Stages, byrow = F)
    }
    else{
      print("You should input 2 or 3 Stages.")
    }
  }

  Dispersal(Emigration = Emigration(EmigProb = emig_prob(Stages, prob_dispersal_0, prob_dispersal_1),
                                    SexDep = F, StageDep = T),
            Transfer = DispersalKernel(Distances = Distances),
            Settlement = Settlement(Settle = 2)) # Randomly choose a suitable neighboring cell or die

}
