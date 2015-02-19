##' L2 glmnet function's Gbenchmark on different dataset  
##' 
##' @title Gbenchmark
##' @description Gbenchmark is a function whose arguments is a simulation data generation function and its argumemts.     
##'              Via this benchmark function, we can calculate different function's growth order with function's 
##'              argument size changed.
##' @param FUN generation function of certain to be benchmarked
##' @param ... other arguments passed to FUN. This is determined by FUN
##' @export
##' @importFrom glmnet glmnet
##' @return time A data.frame records each options time consuming
##' @author Eric xin Zhou \url{xxz220@@miami.edu}
Gbenchmark <- function(FUN, ...){
  options(warn = -1)
  # FUN's argument check
  if(!(any(names(list(...)) %in% names(formals(FUN)))))
    stop("arguments error! Do not match FUN's argument list.")
  data <- FUN(...)
  record <- system.time(glmnet(x = data$x, y = data$y, family = "binomial", alpha = 0))
  
  record[1:3]
}
