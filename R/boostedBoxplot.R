# --------------------------------------------------------------------------------
# title: boostedBoxplot
# description: Boosted Boxplot : add useful features to R boxplot function
# author: Alassane Samba (alassane.samba@orange.com)
# Copyright (c) 2017 Orange
# ---------------------------------------------------------------------------------
## Boosted Boxplot : add useful features to R boxplot function
#' @title Boosted Boxplot
#'
#' @description  Add useful features to R classical boxplot
#'
#' @param y a numeric vector.
#' @param x a factor vector.
#' @param main the title of the graphic.
#' @param labx,laby \code{character}, labels of the axis.
#' @param plot.mean a boolean to indicate whether the mean value of \code{y} must be plotted for each \code{x} category.
#' @param text.freq a boolean to indicate whether the \code{x} categories frequencies must be drawn on boxes.
#' @param las 1 or 2 to indicate whether the x-axis labels must be respectively horizontal or vertical.
#' @param ylim a vector of 2 numeric values to indicate the range of y-axis. Example : \code{c(0,1)}.
#' @param limitVisibleModalities an integer to indicate the number of categories to retain for the \code{x} variable.
#' If the number of categories is higher than \code{limitVisibleModalities}, only categories presenting the highest
#' and the lowest \code{y} median values are plotted.
#' @param decreasing a boolean indicating whether categories order on x-axis according to their corresponding median \code{y} value.
#' @param dynamic a boolean. If \code{TRUE}, an AmChart is return and the parameters \code{plot.mean} and \code{test.freq} are ignored.
#' @return If \code{dynamicity} is \code{TRUE} it returns an object of class AmChart, else it returns a classical boxplot result list.
#' @import stats
#' @import graphics
#' @import rAmCharts
#' @export
boostedBoxplot<-function(y,x, main="", labx=NULL,laby=NULL, plot.mean=T, text.freq=T, las=1, ylim=c(0,0), limitVisibleModalities=30, decreasing=NULL, dynamic=F){

  xlab=""
  if(is.null(labx))labx=deparse(substitute(x))
  if(is.null(laby))laby=deparse(substitute(y))
  if(main==""){
    main=labx
  }else{
    xlab=labx
  }
  x=droplevels(as.factor(x))
  p=length(levels(as.factor(x)))
  if(!is.null(decreasing)){
    x=factor(x,levels = names(sort(tapply(y,x,median), decreasing = decreasing)), ordered = F)
  }else{
    decreasing=T
  }
  #limitVisibleModalities
  if(limitVisibleModalities<p-1){
    x=factor(x,levels = names(sort(tapply(y,x,median), decreasing = decreasing)), ordered = F)
    lx=levels(as.factor(x))
    leftl=lx[1:floor(limitVisibleModalities/2)]
    rightl=lx[(p-floor(limitVisibleModalities/2)+1):p]
    n_other=length(lx[!lx%in%c(leftl,rightl)])
    x=as.character(x)
    x[!x%in%c(leftl,rightl)]<-paste(c("other(",n_other,")"),collapse="")
    x=as.factor(x)
    x=factor(x,levels = names(sort(tapply(y,x,median), decreasing = decreasing)), ordered = F)
  }
  #
  #dynamicity
  if(dynamic){
    dataf=data.frame(Y=y,X=x)
    rAmCharts::amBoxplot(Y~X,data=dataf,labelRotation = (las==2)*90, ylab = laby, main = main)
  }else{
    if(sum(ylim)==0){
      rb<-boxplot(y~x, main=main, xlab=xlab, ylab=laby, las=las)
      grid()
      #rb<-boxplot(y~x, main=main, xlab=xlab, ylab=laby, las=las, add=T)
    }else{
      rb<-boxplot(y~x, main=main, xlab=xlab, ylab=laby, las=las, ylim=ylim)
      grid()
      #rb<-boxplot(y~x, main=main, xlab=xlab, ylab=laby, las=las, add=T)
    }
    if(plot.mean){
      mn.t <- tapply(y, x, mean, na.rm=T)
      sd.t <- tapply(y, x, sd, na.rm=T)
      xi <- 0.3 + seq(rb$n)
      points(xi, mn.t, col = "red", pch = 18, cex=1)
      arrows(xi, mn.t - sd.t, xi, mn.t + sd.t,code = 3, col = "red", angle = 75, length = .1, lwd = 1)
    }
    if(text.freq)text(x=1:length(rb$names), y=(rb$stats[3,]+rb$stats[4,])/2,label=rb$n)
  }
}
############
