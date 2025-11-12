#' @title My N Curve
#'
#' @param mu The Mean
#' @param sigma The standard deviation
#' @param a The upper limit for the probability
#'
#' @returns a curve graph with the given probability shaded in
#' @export
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @examples
#' myncurve(5, 10, 7)
myncurve = function(mu, sigma, a) {
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)
  prob=pnorm(-2, mean = mu, sd = sigma,) - pnorm(-3, mean = mu, sd = sigma)
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
}
