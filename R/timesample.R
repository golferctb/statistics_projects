#' @title Timed Sample Charts
#'
#' @param n A vector with a sample
#' @param iter number of bar charts to make
#' @param time Time (in seconds) between each bar to be displayed.
#'
#' @returns Generates a new bar chart to console every specified second.
#' @export
#' @importFrom grDevices rainbow
#'
#' @examples
#' timesample(100, 10, 1)
timesample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
