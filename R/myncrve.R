#' myncrve
#'
#' @param mu Mean
#' @param sigma Variance
#' @param x X value
#'
#' @return A curve shaded below x
#' @export
#'
#' @examples
#' \dontrun{myncrve(1,1,1)}
myncrve = function(mu, sigma, x){
  curve(dnorm(x,mean=mu,sd=sigma),xlim=c(mu-3*sigma,mu+3*sigma))
  xcurve=seq(mu-3*sigma, x, length=1000)
  ycurve=dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu-3*sigma,xcurve,x),c(0,ycurve,0),col="Gray")
  area = pnorm(x, mu, sigma)
}
