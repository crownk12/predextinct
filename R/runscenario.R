#' runscenario() Function
#'
#' This function is run the simulation from RangeShiftR::RunRS.
#' @param s_list : The output of setscenario(). It is better to set a variable from setscenario.
#' @keywords runscenario
#' @export
#' @examples
#' runscenario()


runscenario <- function(s_list){

  require(RangeShiftR)

  for(i in 1:length(s_list)){

    RunRS(s_list[[i]], "data/")

  }

}

