#' predextinct_time() Function
#'
#' This function predict the time (in years) for the species to experience extinction.
#' Read the RangeshiftR output file from RangeShiftR::readPop.
#' @param s : The output of setscenario()
#' @param folder: [complete].
#' @import RangeShiftR
#' @importFrom dplyr union select intersect
#' @keywords predextinct_time
#' @export
#' @examples
#' predextinct_time()


# Extinction Time after 50 years

predextinct_time <-function(s, folder = "data/"){

  time_ls <- list()

  time_ls <- lapply(s, function(x){
    readPop(x, folder) %>%
      group_by(Rep, Year) %>%
      summarise(sumPop = sum(NInd), .groups = "keep") %>%
      filter(sumPop == 0) %>%
      pull(Year) %>%
      mean
  })

  sumDataset <- data.frame(ID = c(1:length(s)),
                           ExtTime.50y = ifelse(is.na(time_ls),
                           yes = "No extinction",
                           no = time_ls))

  return(sumDataset)

}
