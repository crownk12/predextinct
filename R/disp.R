#' disp() Function
#'
#' This function is a sub-function of setscenario().
#' @param Stages : Number of life-Stages (integer). Requires a minimum of 2, for "juvenile" (stage 0) and "adult". Maximum is 3.
#' @param prob_dispersal_0 : Dispersal probability (emigration probability) of stage 0 (juvenile)
#' @param prob_dispersal_1 : Dispersal probability (emigration probability) of stage 1 (sub-juvenile)
#' @param Distances : Distances of species dispersal
#' @keywords disp
#' @import RangeShiftR
#' @export
#' @examples
#' dontrun{
#' disp()
#' }


disp <- function(Distances, Stages, prob_dispersal_0, prob_dispersal_1){

  Dispersal(Emigration = Emigration(EmigProb = emig_prob(Stages,
                                                         prob_dispersal_0,
                                                         prob_dispersal_1),
                                    SexDep = FALSE, StageDep = TRUE),
            Transfer = DispersalKernel(Distances = Distances),
            Settlement = Settlement(Settle = 2)) # Randomly choose a suitable neighboring cell or die

}
