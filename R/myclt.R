#' myclt
#'
#' @param n size
#' @param iter iterations
#'
#' @return histogram of sample means
#' @export
#'
#' @examples
#' \dontrun{myclt(n=10,iter=10000)}
myclt=function(n,iter){
  y=runif(n*iter,0,5) # A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  hist(sm)
  sm
}
