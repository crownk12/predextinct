#' predextinct.time() Function
#'
#' This function predict the time of species extinction. Read the RangeshiftR output file from RangeShiftR::readPop.
#' @param s : The output of setscenario()
#' @keywords predextinct.time
#' @export
#' @examples
#' predextinct.time()


# Extinction Time after 50 years

predextinct.time <-function(s){

  require(RangeShiftR)

  require(dplyr)

  time_ls <- list()

  for(i in 1:length(s)){

    time_ls[[i]] <- readPop(s[[i]], "data/") %>%
      group_by(Rep, Year) %>%
      summarise(sumPop = sum(NInd), .groups = "keep") %>%
      filter(sumPop == 0) %>%
      pull(Year) %>% mean

  }

  return(data.frame(ID = c(1:length(s)),
                    ExtTime.50y = ifelse(is.na(time_ls),
                                         yes = "No extinction",
                                         no = time_ls)))

}
