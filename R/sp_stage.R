#' sp_stage is a function that is required by demo.
#'
#'
#'
#'
#' @export
#' #' @examples
#' dontrun{
#' sp_stage()
#' }

sp_stage <- function(Stages, MaxAge, prob_reproduction, num_offsprings, prob_surv){

  trans_mat <- matrix(c(0, prob_reproduction, num_offsprings, prob_surv), nrow = Stages, ncol = Stages, byrow = F)

  StageStructure(Stages = Stages,
                 TransMatrix = trans_mat,
                 MaxAge = MaxAge,
                 SurvDensDep = T)

}
