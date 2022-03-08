#' disp() Function
#'
#' This function is a sub-function of setscenario().
#' @param Stages : Number of life-Stages (integer). Requires a minimum of 2, for "juvenile" (stage 0) and "adult". Maximum is 3.
#' @param SexDep : Sex-dependent emigration probability? (default: FALSE)
#' @param prob_dispersal_0 : Dispersal probability (emigration probability) of stage 0 (juvenile)
#' @param prob_dispersal_1 : Dispersal probability (emigration probability) of stage 1 (sub-juvenile)
#' @param Distnaces : Distances of species dispersal
#' @keywords disp
#' @export
#' @examples
#' disp()


# Dispersal

disp <- function(Stages, SexDep, prob_dispersal_0, prob_dispersal_1, Distances){

  require(RangeShiftR) # RangeShiftR should be installed.

  # Sub-function
  emig_prob <- function(Stages, SexDep, prob_dispersal_0, prob_dispersal_1){
    if(Stages == 2 & SexDep == F){
      matrix(c(0, 1, prob_dispersal_0, 0), nrow = Stages, byrow = F)
    }
    else if(Stages == 3 & SexDep == F){
      matrix(c(0, 1, 2, prob_dispersal_0, prob_dispersal_1, 0), nrow = Stages, byrow = F)
    }
    else if(SexDep == T){
      print("Still evolving")
    }
    else{
      print("You should input 2 or 3 Stages.")
    }
  }

  Dispersal(Emigration = Emigration(EmigProb = emig_prob(Stages, SexDep = F, prob_dispersal_0, prob_dispersal_1),
                                    SexDep = SexDep, StageDep = T),
            Transfer = DispersalKernel(Distances = Distances),
            Settlement = Settlement(Settle = 2))

}
