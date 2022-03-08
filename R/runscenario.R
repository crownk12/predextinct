#' runscenario() Function
#'
#' This function 
#' @param a ,b : two numbers to be operated
#' @keywords Add
#' @export
#' @examples
#' runscenario()


runscenario <- function(s_list){
  
  require(RangeShiftR)
  
  for(i in 1:length(s_list)){
    
    RunRS(s_list[[i]], "data/")
    
  }
  
}
