#' predextinct_prob() Function
#'
#' This function predict the probability of species extinction. Read the RangeshiftR output file from RangeShiftR::readPop.
#' This function will estimate the extinction probability after 50 years.
#' @param s : The output of setscenario()
#' @param folder: [complete].
#' @keywords predextinct_prob
#' @import RangeShiftR
#' @import magrittr
#' @importFrom dplyr union select intersect
#' @export
#' @examples
#' predextinct_prob()


predextinct_prob <- function(s, folder = "data/"){

  prob_ls <- lapply(s, function(x){

    readPop(x, folder) %>%
      group_by(Rep,Year) %>%
      # Sum individuals over all cells per year and replicate
      summarise(sumPop = sum(NInd), .groups = 'keep') %>%
      group_by(Year) %>%
      # Average extinction probability (1 minus the proportion of replicates with surviving populations)
      summarise(extProb = 1 - sum(sumPop > 0, na.rm = TRUE) / 10)

  })

  return(prob_ls)

}
