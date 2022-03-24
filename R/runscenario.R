#' runscenario() Function
#'
#' This function is run the simulation from RangeShiftR::RunRS.
#' @param s_list : The output of setscenario(). It is better to set a variable from setscenario.
#' @keywords runscenario
#' @import RangeShiftR
#' @export
#' @examples
#' runscenario()


runscenario <- function(s_list, folder="data/"){
  for(i in 1:length(s_list)){
    RunRS(s_list[[i]], folder)
  }
}

