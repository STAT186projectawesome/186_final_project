# Load libs #
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)
library(plyr)
# Helpers functions #
years.to.educ <- function(df.raw, years){
  df <- df.raw
  df$highest_degree <- 1*(years < 12) + 2*(years >= 12 & years <= 15) +
    3*(years >= 16 & years <= 17) + 4*(years > 17)
  df$highest_degree <- as.factor(df$highest_degree)
  levels(df$highest_degree) <- c("no_high_school", "high_school", "bachelor", 
                                 "post_graduate")
  return(df)
}
enough.answers <- function(df, category, threshold = 3){
  t <- sapply(1:nrow(df), function (x) sum(!is.na(df1[x, category])))
  out <- which(t < threshold)
  return(out)
}
extreme_attitude <- function(y) {
  if(class(y) == "factor") vals <- as.integer(levels(y))
  else vals <- unique(y)
  x <- as.integer(y)
  max <- max(vals, na.rm = TRUE)
  mid <- quantile(vals[!is.na(vals)], p = 0.5)
  out <- (x - mid)^2/(max - mid)^2
  return(out)
}
get.pval <- function(df, clusterID, ps){
  idx.clusterID <- df$clusterID == clusterID
  # There are only ncol(ps) - 1 linearly independent columns
  y <- as.matrix(ps[idx.clusterID, 1:(ncol(ps) - 1)])
  x <- as.matrix(df[idx.clusterID, ]$highest_degree)
  fit <- manova(y ~ x)
  temp <- try(p.value <- summary(fit)$stats[1, 6], silent = TRUE)
  if(class(temp) == "try-error") p.value <- 1
  return(p.value)
}
# Run kmeans and update subclasses identification
run_kmeans <- function(df, clusterID, ncl){
  idx <- df$clusterID == clusterID
  idx_next <- df$clusterID > clusterID
  if (sum(idx_next) > 0) 
    df[idx_next, ]$clusterID <- df[idx_next, ]$clusterID + ncl - 1
  a <- (clusterID - 1)
  df[idx, ]$clusterID <- a+kmeans(df[idx, tail(colnames(df), 4)], ncl)$cluster
  return(df)
}
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}