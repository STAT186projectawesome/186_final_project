setwd("C:/Users/Matteo/186_final_project/Project")
df1 = read.csv("./raw_data_t.csv")
# Some cleaning #
df1[df1=="Not applicable" | df1 == ""] <- NA
df1$Age.of.respondent <- as.integer(df1$Age.of.respondent)
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
df1 <- years.to.educ(df1, as.integer(as.character(df1$Highest.year.of.school.completed)))
latin.american <- c("Latin", "Latino/a", "Latin american")
miss.hispanic <- c("Don\'t know", "No answer", "Not applicable", "Other, not specified")
df1$Hispanic.specified[df1$Hispanic.specified %in% latin.american] <- "Latin american"
df1$Hispanic.specified[df1$Hispanic.specified %in% miss.hispanic] <- "Not hispanic"
df1$Hispanic.specified <- factor(df1$Hispanic.specified)
df1$Hispanic.specified[is.na(df1$Hispanic.specified)] <- "Not hispanic"
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
economic.questions <- c("Should.govt.reduce.income.differences",
                        "Should.govt.improve.standard.of.living.",
                        "Number.of.immigrants.to.america.nowadays.should.be",
                        "Private.enterprise.will.solve.u.s..problems",
                        "Govmnt.should.reduce.inc.differentials",
                        "Should.govt.do.more.or.less.",
                        "Govt.should.redistribute.wealth",
                        "Should.govt.help.pay.for.medical.care.")
social.questions <- c("Should.govt.aid.blacks.",
                      "Modern.science.does.more.harm.than.good",
                      "Believe.too.much.in.science..not.enough.faith",
                      "Husb.shld.work.wife.shld.look.after.home",
                      "Homosexual.sex.relations",
                      "Close.relative.marry.black",
                      "Ok.to.test.on.animals.to.save.humans")
environmental.questions <- c("Worry.too.much.about.envir..too.little.econ",
                             "Almost.everything.we.do.harms.envir",
                             "Accept.cut.in.living.stnds.to.help.envir.",
                             "Pay.higher.taxes.to.help.envir.",
                             "Pay.higher.prices.to.help.envir.",
                             "Econ.grwth.always.harms.envir",
                             "Amer.needs.econ.grwth.to.protect.envir",
                             "Modern.science.will.solve.envir.probs",
                             "Drive.less.for.envir.reasons",
                             "Greenhouse.effect.danger.to.envir",
                             "Nuke.power.danger.to.envir")
#################################
# DROP UNITS ACCORDING TO PAPER #
#################################
# delete units with less than 3 questions answered
enough.answers <- function(df, category, threshold = 3){
  t <- sapply(1:nrow(df), function (x) sum(!is.na(df1[x, category])))
  out <- which(t < threshold)
  return(out)
}
outlist.econ <- enough.answers(df1, economic.questions)
outlist.soci <- enough.answers(df1, social.questions)
outlist.envi <- enough.answers(df1, environmental.questions)
length(unique(c(outlist.econ, outlist.soci, outlist.envi)))
df2 <- df1[-unique(c(outlist.econ, outlist.soci, outlist.envi)), ]
# delete all units whose highest.year.of.school.completed is na
df2 = df2[!is.na(df2$Highest.year.of.school.completed), ]
#############################
# COMPUTE PROPENSITY SCORES #
#############################
df3 <- df2
covars = c("Race.of.respondent", "Rs.religious.preference", "Hispanic.specified")
# build the multinomial regression for propensity scores
df3$highest_degree = ordered(df3$highest_degree, c("no_high_school","high_school","bachelor","post_graduate"))
library(VGAM)
ordered.multinomial = vglm(highest_degree ~ .,
                           data=df3[ ,c("highest_degree", covars)], 
                           family=propodds)
c(deviance(ordered.multinomial), df.residual(ordered.multinomial))
predict(ordered.multinomial, newdata=df3, type="response")
library(nnet)
unordered.multinomial = multinom(highest_degree ~ .,
                                 data=df3[ ,c("highest_degree", covars)], 
                                 maxit=10000)
with(unordered.multinomial, c(deviance, edf))
# the nominal model actually outperforms the ordinal model in terms of deviance!
# probably because there are so many observations, so the order becomes irrelevant

# test for significance of interaction terms
# step(unordered.multinomial, direction="forward", scope = ~.^2)
# this takes too much computational power, so we'll have to stick to the linear predictors
# the deviance indicates that much of the variance is explained, so it's not too much of a worry to not include interaction

# unordered.multinomial has a lower deviance, so we'll use it 
prop.scores = predict(unordered.multinomial, newdata=df2[ ,covars], type="probs")
df4 = cbind(df3, prop.scores)

# delete units whose score in any category is lower than the 
scores.range = list()
for (i in levels(df4$highest_degree)) {
  scores.range[[i]] = apply(
    df4[df4$highest_degree==i, tail(colnames(df4),4)],
    2, range)
}
scores.range = as.data.frame(scores.range)

df5 = df4
# now, delete all units whose prop.score
for (i in levels(df5$highest_degree)) {
  max.min.score = max(scores.range[1,which(levels(df5$highest_degree)==i) + seq(0, by=4, length.out=4)])
  min.max.score = min(scores.range[2,which(levels(df5$highest_degree)==i) + seq(0, by=4, length.out=4)])
  df5 = df5[df5[ ,i] > max.min.score, ]
  df5 = df5[df5[ ,i] < min.max.score, ]
}

# only deleted 308 units!

# now, we have to recalculate the propensity scores, after the irrelevant units have been deleted
unordered.multinomial1 = multinom(highest_degree ~ .,
                                  data=df5[ ,c("highest_degree", covars)], maxit=10000)
prop.scores1 = predict(unordered.multinomial1, newdata=df5, type="probs")
df5$prop.scores <- prop.scores1
# save(df5, file = "./computed_ps.RData")
# load("./computed_ps.RData")
# matrix of linearized propensity scores
ps <- as.matrix(cbind(df5[, tail(colnames(df5), 4)]))
ps <- log(ps/(1 - ps))
df5[, tail(colnames(df5), 4)] <- ps
get.pval <- function(df, clusterID, ps){
  idx.clusterID <- df$clusterID == clusterID
  # There are only ncol(ps) - 1 linearly independent columns
  y <- as.matrix(ps[idx.clusterID, 1:(ncol(ps) - 1)])
  x <- as.factor(df[idx.clusterID, ]$highest_degree)
  fit <- manova(y ~ x)
  p.value <- summary(fit)$stats[1, 6]
  return(p.value)
}
# Attempt to code what we discussed today
df6 <- df5
df6$clusterID <- kmeans(ps, 2)$cluster
n.cluster <- max(df6$clusterID)
pv <- sapply(1:n.cluster, function(x) get.pval(x, df = df6, ps = ps))
sig.idx <- which(pv < 0.01)
ncl <- 2
while(length(sig.idx) > 0){
  ncl <- ncl + 1
  df6$clusterID <- kmeans(ps, ncl)$cluster
  pv <- sapply(1:ncl, function(x) get.pval(x, df = df6, ps = ps))
  sig.idx <- which(pv < 0.01)
  print(paste("Finished with fitting", ncl, "clusters"))
}
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
# df8 <- df5
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
# min.units <- 10 # min # of units in each category
# while(length(sig.idx) > 0 & count < 20){
#   offset <- 0
#   for(i in sig.idx) {
#     df_temp <- df9
#     df9 <- run_kmeans(df9, i + offset, ncl)
#     u <- sapply((i + offset):(i + offset + ncl - 1), function(x) 
#       table(df9[df9$clusterID == x, ]$highest_degree))
#     offset <- (ncl-1)
#     if(any(u < min.units)) {
#       df9 <- df_temp
#       offset <- 0
#     }
#   }
#   # Do we have enough units for each highest degree?
#   are.units.ok <- sapply(1:max(df9$clusterID), function(x) 
#     table(df9[df9$clusterID == x, ]$highest_degree))
#   are.units.ok <- sapply(1:max(df9$clusterID), 
#                          function(x) all(are.units.ok[, x] > min.units))
#   pv <- sapply(1:max(df9$clusterID), function(x) get.pval(x, df = df9, ps = ps))
#   sig.idx <- which(pv < 0.01 & are.units.ok)
#   count <- count + 1
#   print(paste("Finished with iteration", count))
# }
# get hisograms for propensity scores based on class
library(ggplot2)
library(gridExtra)
# Example with ncl=4 clusters
ncl <- 4 # clusters
bw <- 0.1 # binwidth
alpha <- 0.5 
df5$clusterID <- kmeans(ps, ncl)$cluster
df9 <- df5[, c("no_high_school", "high_school", "bachelor", 
               "post_graduate", "clusterID")]
# df9$clusterID <- as.factor(df9$clusterID)
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
extreme_attitude <- function(y) {
  x <- as.integer(y)
  max <- max(x, na.rm = TRUE)
  mid <- quantile(unique(x[!is.na(x)]), p = 0.5)
  out <- (x - mid)^2/(max - mid)^2
  return(out)
}
econ.ext <- sapply(economic.questions, function(x) extreme_attitude(df1[, x]))
soci.ext <- sapply(social.questions, function(x) extreme_attitude(df1[, x]))
envi.ext <- sapply(environmental.questions, 
                   function(x) extreme_attitude(df1[, x]))
mean(econ.ext[!is.na(econ.ext)])
mean(soci.ext[!is.na(soci.ext)])
mean(envi.ext[!is.na(envi.ext)])
sd(envi.ext[!is.na(envi.ext)])
# now, subclassify based on prop.scores
for (k in 4:10) {
  cluster.ids[[k]] = kmeans(df7[ ,tail(colnames(df5),4)], k)
  print(table(cbind(df5$highest_degree, as.data.frame(cluster.ids[[k]]$cluster))))
}

cluster.tot.withinss = c()
for (k in 4:10) {
  cluster.tot.withinss = rbind(cluster.tot.withinss, c(k, cluster.ids[[k]]$tot.withinss))
}
