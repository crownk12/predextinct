#' predextinct.prob() Function
#'
#' This function predict the probability of species extinction. Read the RangeshiftR output file from RangeShiftR::readPop.
#' @param s : The output of setscenario()
#' @keywords predextinct.prob
#' @export
#' @examples
#' predextinct.prob()


# Extinction Probability after 50 years

predextinct.prob <-function(s){

  require(RangeShiftR)

  require(dplyr)

  prob_ls <- list()

  for(i in 1:length(s)){

    prob_ls[[i]] <- readPop(s[[i]], "data/") %>%
      group_by(Rep,Year) %>%
      # Sum individuals over all cells per year and replicate
      summarise(sumPop = sum(NInd), .groups='keep') %>%
      group_by(Year) %>%
      # Average extinction probability (1 minus the proportion of replicates with surviving populations)
      summarise(extProb = 1 - sum(sumPop > 0, na.rm = T) / 10)

  }

  return(prob_ls)

}
