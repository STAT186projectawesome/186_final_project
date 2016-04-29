# setwd("C:/Users/Matteo/186_final_project/Project")
df1 = read.csv("Google Drive/raw_data_readable.csv.xlt")
df1[df1=="Not applicable"] = NA
df1[df1==""] = NA

# which columns have NAs?
for (i in 1:ncol(df1)) {
  prop.na = sum(is.na(df1[ ,i])) / nrow(df1)
  if (prop.na != 0) {
    print(colnames(df1)[i])
    print(prop.na)
  }
}

# what are the levels of the questions
colnames(df1)
for (i in 8:ncol(df1)) {
  print(colnames(df1)[i])
  print(levels(df1[ ,i]))
}

environment.questions = colnames(df1)[c(23:34)]
social.questions = colnames(df1)[c(10:13,14:16,19,21:23,35)]
economic.questions = colnames(df1)[c(8,12,14:15,17:18,20,23:24,26,28:30)]

# answered.enough.questions = function(unit, question.set, threshold=3) {
#   return(as.numeric(sum(is.na(unit[question.set])) < 3))
# }
# df2 = df1
# for (i in c("environment","social","economic")) {
#   df2$newcol = apply(df1, 1, answered.enough.questions, question.set=get(paste(i,"questions",sep=".")))
#   colnames(df2)[length(colnames(df2))] = paste("answered.enough",i,sep=".")
# }

# Recode Education
df1$Highest.year.of.school.completed[df1$Highest.year.of.school.completed == "Don\'t know" | 
                                       df1$Highest.year.of.school.completed == "No answer"] <- NA
# Recode after setting some levels to NA
df1$Highest.year.of.school.completed <- factor(df1$Highest.year.of.school.completed)
years.to.educ <- function(df.raw, years){
  df <- df.raw
  df$highest_degree <- 1*(years < 12) + 2*(years >= 12 & years <= 15) +
    3*(years >= 16 & years <= 17) + 4*(years > 17)
  df$highest_degree <- as.factor(df$highest_degree)
  levels(df$highest_degree) <- c("no_high_school", "high_school", "bachelor", 
                                 "post_graduate")
  return(df)
}
df3 <- years.to.educ(df1, as.integer(as.character(df1$Highest.year.of.school.completed)))

# delete all units whose highest.year.of.school.completed is na
df3 = df3[!is.na(df3$Highest.year.of.school.completed), ]

# remove the 2 units whose age, race, everything is NA
df3 = df3[!is.na(df3$Age.of.respondent), ]
# clean up some data to get it ready for propensity score regression

levels(df3$Hispanic.specified) = c(levels(df3$Hispanic.specified), "Not Hispanic")
df3$Hispanic.specified[is.na(df3$Hispanic.specified)] = "Not Hispanic"
df3$Age.of.respondent = as.numeric(df3$Age.of.respondent)
df3$Respondent.id.number = as.numeric(df3$Respondent.id.number)

covars = c("Race.of.respondent"
          ,"Rs.religious.preference"
          ,"Hispanic.specified"
           )
# build the multinomial regression for propensity scores
df3$highest_degree = ordered(df3$highest_degree, c("no_high_school","high_school","bachelor","post_graduate"))

library(VGAM)
ordered.multinomial = vglm(highest_degree ~ .,
                           data=df3[ ,c("highest_degree",covars)], family=propodds)
c(deviance(ordered.multinomial), df.residual(ordered.multinomial))
predict(ordered.multinomial, newdata=df3, type="response")

library(nnet)
unordered.multinomial = multinom(highest_degree ~ .,
                                 data=df3[ ,c("highest_degree",covars)], maxit=10000)
with(unordered.multinomial, c(deviance, edf))

# the nominal model actually outperforms the ordinal model in terms of deviance!
# probably because there are so many observations, so the order becomes irrelevant

# test for significance of interaction terms
step(unordered.multinomial, direction="forward", scope = ~.^2)
# this takes too much computational power, so we'll have to stick to the linear predictors
# the deviance indicates that much of the variance is explained, so it's not too much of a worry to not include interaction

# unordered.multinomial has a lower deviance, so we'll use it 
prop.scores = predict(unordered.multinomial, newdata=df3, type="probs")
df4 = cbind(df3, prop.scores)

# delete units whose score in any category is outside the range of the prop.scores in other treatment levels
scores.range = list()
for (i in levels(df4$highest_degree)) {
  scores.range[[i]] = apply(
                          df4[df4$highest_degree==i, tail(colnames(df4),4)],
                          2, range)
}
scores.range = as.data.frame(scores.range)

df5 = df4
for (i in levels(df5$highest_degree)) {
  max.min.score = max(scores.range[1,which(levels(df5$highest_degree)==i) + seq(0, by=4, length.out=4)])
  min.max.score = min(scores.range[2,which(levels(df5$highest_degree)==i) + seq(0, by=4, length.out=4)])
  df5 = df5[df5[ ,i] > max.min.score, ]
  df5 = df5[df5[ ,i] < min.max.score, ]
}
c(nrow(df5), nrow(df4))

# only deleted about 1k units, still have over 50k

# now, we have to recalculate the propensity scores, after the irrelevant units have been deleted
df6 = df5[ ,-(ncol(df5) - c(3:0))]
unordered.multinomial1 = multinom(highest_degree ~ .,
                                  data=df6[ ,c("highest_degree",covars)], maxit=10000)
prop.scores1 = predict(unordered.multinomial1, newdata=df6, type="probs")
df7 = cbind(df6, prop.scores1)

# KIRAN CODE TO FIND OPTIMAL NUMBER OF CLUSTERS
# set the minimum number of clusters we want
cluster.manova.pval = function(df, k) {
  # cluster the units based on the propensity score
  set.seed(2016)
  # use the logit of the probability, since these are normally distributed for large samples
  ps.linearized = log(df[ ,tail(colnames(df),4)] / (1 - df[ ,tail(colnames(df),4)]))
  x = kmeans(ps.linearized, k)$cluster
  fit = rep(NA, k)
  for (i in 1:k) {
    fit[i] = summary(
                     manova(as.matrix(df[x==i,tail(colnames(df7),3)]) ~ as.matrix(df[x==i,"highest_degree"]))
                     )[[4]][1,"approx F"]
  }
  return(fit)
}

for (k in 1:10) {
  print(k)
  print(c(mean(cluster.manova.pval(df7, k)), var(cluster.manova.pval(df7, k))))
}

# clear option is to choose 7 clusters, since we have a low mean and variance of F-values

# save(df7, file = "./computed_ps.RData")
# load("./computed_ps.RData")
# # matrix of propensity scores
# ps <- as.matrix(cbind(df7[, tail(colnames(df7), 4)]))
# lps <- log(ps/(1 - ps))
# df7[, tail(colnames(df7), 4)] <- lps
# get.pval <- function(df, clusterID, ps){
#   idx.clusterID <- df$clusterID == clusterID
#   # There are only ncol(ps) - 1 linearly independent columns
#   y <- as.matrix(ps[idx.clusterID, 1:(ncol(ps) - 1)])
#   x <- as.factor(df[idx.clusterID, ]$highest_degree)
#   fit <- manova(y ~ x)
#   p.value <- summary(fit)$stats[1, 6]
#   return(p.value)
# }
# # Run kmeans and update subclasses identification
# run_kmeans <- function(df, clusterID, ncl){
#   idx <- df$clusterID == clusterID
#   idx_next <- df$clusterID > clusterID
#   if (sum(idx_next) > 0) 
#     df[idx_next, ]$clusterID <- df[idx_next, ]$clusterID + ncl - 1
#   a <- (clusterID - 1)
#   df[idx, ]$clusterID <- a+kmeans(df[idx, tail(colnames(df), 4)], ncl)$cluster
#   return(df)
# }
# df8 <- df7
# df8$clusterID <- kmeans(ps, 2)$cluster
# n.cluster <- max(df8$clusterID)
# pv <- sapply(1:n.cluster, function(x) get.pval(x, df = df8, ps = ps))
# are.units.ok <- sapply(1:n.cluster , function(x) 
#   table(df8[df8$clusterID == x, ]$highest_degree))
# are.units.ok <- sapply(1:n.cluster, function(x) any(are.units.ok[, x] > 10))
# sig.idx <- which(pv < 0.01 & are.units.ok)
# count <- 0
# df9 <- df8
# ncl <- 2 #number of clusters to fit at each iteration
# min.units <- 20 # min # of units in each category
# while(length(sig.idx) > 0){
#   offset <- 0
# for(i in sig.idx) {
#   df_temp <- df9
#   df9 <- run_kmeans(df9, i + offset, ncl)
#   u <- sapply((i + offset):(i + offset + ncl - 1), function(x) 
#     table(df9[df9$clusterID == x, ]$highest_degree))
#   offset <- (ncl-1)
#   if(any(u < min.units)) {
#     df9 <- df_temp
#     offset <- 0
#   }
# }
# # Do we have enough units for each highest degree?
# are.units.ok <- sapply(1:max(df9$clusterID), function(x) 
#   table(df9[df9$clusterID == x, ]$highest_degree))
# are.units.ok <- sapply(1:max(df9$clusterID), 
#                        function(x) all(are.units.ok[, x] > min.units))
# pv <- sapply(1:max(df9$clusterID), function(x) get.pval(x, df = df9, ps = ps))
# sig.idx <- which(pv < 0.01 & are.units.ok)
# count <- count + 1
# print(paste("Finished with iteration", count))
# }

df8 = df7
# get histograms for propensity scores based on class
library(ggplot2)
library(gridExtra)
# Example with ncl=7 clusters
ncl <- 7 # clusters
bw <- 0.1 # binwidth
alpha <- 0.5 
df8$clusterID <- kmeans(df7[ ,tail(colnames(df7),4)], ncl)$cluster
df9 <- df8[, c("no_high_school", "high_school", "bachelor", 
               "post_graduate", "clusterID")]
library(reshape2)
df9 <- melt(df9, id = c("clusterID"))
for(i in 1:(ncl - 1)){
  assign(paste0("p", i), ggplot(aes(x = value, fill = variable), 
                                data = df9[df9$clusterID==i, ]) + 
           geom_histogram(aes(y = ..density..), alpha = alpha, 
                          binwidth = bw)) 
}
p.last <- ggplot(aes(x = value, fill = variable), 
                 data = df9[df9$clusterID==ncl, ]) + 
  geom_histogram(aes(y = ..density..), alpha = alpha,
                 binwidth = bw) +  
  theme(legend.direction = "horizontal")
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
grid.arrange(arrangeGrob(p1 + theme(legend.position = "none"), 
                         p2 + theme(legend.position = "none"),
                         p3 + theme(legend.position = "none"), 
                         p.last + theme(legend.position = "none"), nrow = 2),
             g_legend(p.last), nrow = 2, heights = c(10, 1))
# Compute attitude extremity
extreme_attitude <- function(x) {
  max <- max(x)
  mid <- quantile(x, p = 0.5)
  out <- sum(x - max)^2/(max - mid)^2
  return(out)
}

