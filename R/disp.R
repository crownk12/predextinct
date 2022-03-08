#' disp() Function
#'
#' This function 
#' @param a ,b : two numbers to be operated
#' @keywords Add
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
