#' emig_prob is a function that is required by sp_dispersal.
#'
#'
#' Note that this function ignores the SexDep argument.
#'
#' @export
#' #' @examples
#' dontrun{
#' emig_prob()
#' }

emig_prob <- function(Stages, prob_dispersal_0, prob_dispersal_1){

  if(Stages == 2){
    matrix(c(0, 1, prob_dispersal_0, 0), nrow = Stages, byrow = F)
  }

  else if(Stages == 3){
    matrix(c(0, 1, 2, prob_dispersal_0, prob_dispersal_1, 0), nrow = Stages, byrow = F)
  }

  else{
    print("You should input 2 or 3 Stages.")
  }

}
