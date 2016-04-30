setwd("C:/Users/Matteo/186_final_project/Project")
source("./helpers.R")
df1 = read.csv("./raw_data_t.csv")
# Some cleaning #
df1[df1=="Not applicable" | df1 == ""] <- NA
df1$Age.of.respondent <- as.integer(df1$Age.of.respondent)
# Recode Education
df1$Highest.year.of.school.completed[df1$Highest.year.of.school.completed == "Don\'t know" | 
                                       df1$Highest.year.of.school.completed == "No answer"] <- NA
# Recode after setting some levels to NA
df1$Highest.year.of.school.completed <- factor(df1$Highest.year.of.school.completed)
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
                        "Should.govt.help.pay.for.medical.care.")
social.questions <- c("Should.govt.aid.blacks.",
                      "Modern.science.does.more.harm.than.good",
                      "Believe.too.much.in.science..not.enough.faith",
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
covars = c("Race.of.respondent", "Rs.religious.preference", 
           "Hispanic.specified")
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
prop.scores = predict(unordered.multinomial, newdata=df3[ ,covars], type="probs")
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
df5[,tail(colnames(df5), 4)] <- prop.scores1
# matrix of linearized propensity scores
ps <- as.matrix(cbind(df5[, tail(colnames(df5), 4)]))
ps <- log(ps/(1 - ps))
df5[, tail(colnames(df5), 4)] <- ps
# Subclassify #
set.seed(24)
df8 <- df5
df8$clusterID <- kmeans(ps, 2)$cluster
n.cluster <- max(df8$clusterID)
pv <- sapply(1:n.cluster, function(x) get.pval(x, df = df8, ps = ps))
min.units <- 10
are.units.ok <- sapply(1:n.cluster , function(x) 
  table(df8[df8$clusterID == x, ]$highest_degree))
are.units.ok1 <- sapply(1:n.cluster, function(x) any(are.units.ok[, x] > min.units))
sig.idx <- which(pv < 0.01 & are.units.ok1)
count <- 0
df9 <- df8
ncl <- 2 # number of clusters to fit at each iteration
while(length(sig.idx) > 0 & count < 100){
  offset <- 0
  for(i in sig.idx) {
    df_temp <- df9
    df9 <- run_kmeans(df9, i + offset, ncl)
    u <- sapply((i + offset):(i + offset + ncl - 1), function(x) 
      table(df9[df9$clusterID == x, ]$highest_degree))
    offset <- (ncl-1)
    if(any(u < min.units)) {
      df9 <- df_temp
      offset <- 0
    }
  }
  # Do we have enough units for each highest degree?
  are.units.ok <- sapply(1:max(df9$clusterID), function(x) 
    table(df9[df9$clusterID == x, ]$highest_degree))
  are.units.ok1 <- sapply(1:max(df9$clusterID), 
                          function(x) all(are.units.ok[, x] > min.units))
  pv <- sapply(1:max(df9$clusterID), function(x) get.pval(x, df = df9, ps = ps))
  sig.idx <- which(pv < 0.05 & are.units.ok1)
  count <- count + 1
  print(paste("We fit", max(df9$clusterID), "clusters"))
}
# KIRANS METHOD #
df9k <- df9
df9k$clusterID <- kmeans(ps, 7)$cluster
#########################
# VISUALIZE THE BALANCE #
#########################
# get hisograms for propensity scores based on class
ncl <- max(df9$clusterID) # clusters
bw <- 0.1 # binwidth
alpha <- 0.5
# COVARIANCE BALANCE ON RAW DATA #
df12 <- df9[, c(covars, "highest_degree")]
pdf(paste("./balance_covariates.pdf"))
for(k in 1:(length(covars) - 1)){
  dat <- df12[, c(covars[k], "highest_degree")]
  names(dat) <- c("var", "highest_degree")
  df.new <- ddply(dat, .(highest_degree), summarise,
                  prop = as.numeric(prop.table(table(var))),
                  var = names(table(var)))
  # Decide number of rows for legend
  l.lvls <- length(levels(dat$var))
  if(l.lvls <= 3) nr.leg <- 1 
  if(l.lvls > 3 & l.lvls <= 15) nr.leg <- 2
  if(l.lvls > 15) nr.leg <- 3
  assign(paste0("p", k), ggplot(df.new, aes(x = highest_degree, y = prop,
                                            fill = var)) +
           geom_bar(stat="identity", position='dodge') +
           theme(axis.text=element_text(size=7)) + 
           labs(x = "", y = "Density") +
           theme(legend.direction = "horizontal", legend.key.size = unit(.5, "cm")) +
           guides(fill=guide_legend(title = "", nrow=nr.leg,byrow=TRUE)) +
           ggtitle(covars[k]))
}
dat <- df12[, c(covars[k+1], "highest_degree")]
names(dat) <- c("var", "highest_degree")
df.new <- ddply(dat,.(highest_degree),summarise,
                prop=as.numeric(prop.table(table(var))),
                var=names(table(var)))
l.lvls <- length(levels(dat$var))
if(l.lvls <= 3) nr.leg <- 1 
if(l.lvls > 3 & l.lvls <= 15) nr.leg <- 2
if(l.lvls > 15) nr.leg <- 3
p.last <-ggplot(df.new, aes(x = highest_degree, y = prop,
                            fill = var)) +
  geom_bar(stat="identity", position='dodge') +
  labs(x = "", y = "Density") + 
  theme(legend.direction = "horizontal", legend.key.size = unit(.5, "cm")) + 
  guides(fill=guide_legend(title = "", nrow=nr.leg,byrow=TRUE)) +
  theme(axis.text=element_text(size=7)) +
  ggtitle(covars[k+1])
grid.arrange(p1 + theme(legend.position = "none"), 
             g_legend(p1), 
             p2 + theme(legend.position = "none"),
             g_legend(p2),
             p.last + theme(legend.position = "none"),
             g_legend(p.last), nrow = 6, heights = c(6, 1, 6, 1, 6, 3),
             top=textGrob("Pre-subclassification covariates balance",
                          gp=gpar(fontsize=20,font=3)))
dev.off()
# Plot covariates by cluster
library(plyr)
meth <- "nicole"
if (meth == "kiran")  df11 <- df9k[, c(covars, "highest_degree", "clusterID")]
if (meth == "nicole") df11 <- df9[, c(covars, "highest_degree", "clusterID")]
for(k in covars){
  pdf(paste0("./balance_covariates_", meth, "_", k, ".pdf")) # CHANGE THIS
  for(i in 1:(ncl-1)){
    dat <- df11[df11$clusterID==i, c(k, "highest_degree")]
    names(dat) <- c("var", "highest_degree")
    df.new <- ddply(dat, .(highest_degree), summarise,
                  prop = as.numeric(prop.table(table(var))),
                  var = names(table(var)))
    # Decide number of rows for legend
    if(length(levels(dat$var)) == 3) nr.leg <- 1 else nr.leg <- 3
    assign(paste0("p", i), ggplot(df.new, aes(x = highest_degree, y = prop,
                                             fill = var)) +
             geom_bar(stat="identity", position='dodge') +
             theme(axis.text=element_text(size=7)) + 
             labs(x = "", y = "Density") + 
             ggtitle(paste("Cluster", i)))
  }
  dat <- df11[df11$clusterID==ncl, c(k, "highest_degree")]
  names(dat) <- c("var", "highest_degree")
  df.new <- ddply(dat,.(highest_degree),summarise,
                  prop=as.numeric(prop.table(table(var))),
                  var=names(table(var)))
  p.last <-ggplot(df.new, aes(x = highest_degree, y = prop,
                              fill = var)) +
    geom_bar(stat="identity", position='dodge') +
    labs(x = "", y = "Density") + 
    theme(legend.direction = "horizontal", legend.key.size = unit(.3, "cm")) + 
    guides(fill=guide_legend(nrow=nr.leg,byrow=TRUE)) +
    theme(axis.text=element_text(size=7)) +
    ggtitle(paste("Cluster", ncl))
  grid.arrange(arrangeGrob(p1 + theme(legend.position = "none"), 
                           p2 + theme(legend.position = "none"),
                           p3 + theme(legend.position = "none"),
                           p4 + theme(legend.position = "none"), 
                           p5 + theme(legend.position = "none"),
                           p.last + theme(legend.position = "none"), nrow = 3),
               g_legend(p.last), nrow = 2, heights = c(10, 1),
               top=textGrob(paste("Distribution of", k, 
                                  "\n after classification"),
                            gp=gpar(fontsize=20,font=3)))
  dev.off()
}
##################
# ANALYSIS PHASE #
##################
# Compute attitude extremity
econ.ext <- sapply(economic.questions, function(x) extreme_attitude(df9[, x]))
soci.ext <- sapply(social.questions, function(x) extreme_attitude(df9[, x]))
envi.ext <- sapply(environmental.questions, 
                   function(x) extreme_attitude(df9[, x]))
df9$econ.ext.att <- rowSums(econ.ext, na.rm = TRUE, dims = 1)
df9$soci.ext.att <- rowSums(soci.ext, na.rm = TRUE, dims = 1)
df9$envi.ext.att <- rowSums(envi.ext, na.rm = TRUE, dims = 1)
df9$pool.ext.att <- rowSums(cbind(econ.ext, soci.ext, envi.ext), na.rm = TRUE)
treat <- levels(df9$highest_degree)
avg.treat.effect <- matrix(NA, nrow = max(df9$clusterID), ncol = 4)
var.treat.effect <- nkj <- avg.treat.effect
colnames(avg.treat.effect) <-  colnames(var.treat.effect) <- 
  colnames(nkj) <- treat
rownames(avg.treat.effect) <-  rownames(var.treat.effect) <- 
  rownames(nkj) <- paste("cluster", 1:max(df9$clusterID))
for(i in 1:max(df9$clusterID)) {
  for(j in 1:length(treat)){
    idx <- df9$clusterID == i & df9$highest_degree == treat[j]
    nkj[i, j] <- sum(idx)
    avg.treat.effect[i, j] <- mean(df9[idx, "pool.ext.att"])
    var.treat.effect[i, j] <- sum((df9[idx, "pool.ext.att"] - 
                                    avg.treat.effect[i, j])^2)/(nkj[i, j] - 1)
  }
}
nj <- apply(nkj, 2, sum)
weights <- sweep(nkj, 2, nj, "/")
yj <- apply(avg.treat.effect*weights, 2, sum)
vyj <- apply(weights^2*var.treat.effect/nkj, 2, sum)
ci.lo <- yj + qnorm(0.025)*sqrt(vyj)
ci.hi <- yj + qnorm(0.975)*sqrt(vyj)
ci <- rbind(ci.lo, ci.hi)
rownames(ci) <- c("L", "U")
ci
# CHECK IF OUR DATA IS SIMILAR ENOUGH TO THAT IN THE PAPER #
mean(df9$econ.ext.avg)
mean(df9$soci.ext.avg)
mean(df9$envi.ext.avg)
sd(df9$econ.ext.avg)
sd(df9$soci.ext.avg)
sd(df9$envi.ext.avg)
