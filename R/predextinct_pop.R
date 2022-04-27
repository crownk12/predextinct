#' predextinct_time() Function
#'
#' This function predict the population per 10 years for the species to experience extinction.
#' Read the RangeshiftR output file from RangeShiftR::readPop.
#' @param s : The output of setscenario()
#' @param folder: [complete].
#' @import RangeShiftR
#' @import magrittr
#' @importFrom dplyr union select intersect
#' @keywords predextinct_pop
#' @export
#' @examples
#' predextinct_pop()


# Extinction Time after 50 years

predextinct_pop <- function(s, folder = "data/"){

  pop_ls <- lapply(s, function(x){
    readPop(x, folder) %>%
      group_by(Rep, Year) %>%
      summarise(sumPop = sum(NInd), .groups = "keep") %>%
      group_by(Year) %>%
      summarise(MeanPoP = mean(sumPop))
  })

  return(pop_ls)

}
