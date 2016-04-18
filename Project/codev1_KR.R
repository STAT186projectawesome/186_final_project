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

covars = c("Marital.status"
          ,"Age.of.respondent"
          ,"Race.of.respondent"
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

# unordered.multinomial has a lower deviance, so we'll use it 
prop.scores = predict(unordered.multinomial, newdata=df3, type="probs")
df4 = cbind(df3, prop.scores)

# delete units whose score in any category is lower than the 
scores.range = list()
for (i in levels(df4$highest_degree)) {
  scores.range[[i]] = apply(
                          df4[df4$highest_degree==i, tail(colnames(df4),4)],
                          2, range)
}
scores.range = as.data.frame(scores.range)

library("cluster")
df5 = df4
# now, delete all units whose prop.score
for (i in levels(df5$highest_degree)) {
  max.min.score = max(scores.range[1,which(levels(df5$highest_degree)==i) + seq(0, by=4, length.out=4)])
  min.max.score = min(scores.range[2,which(levels(df5$highest_degree)==i) + seq(0, by=4, length.out=4)])
  df5 = df5[df5[ ,i] > max.min.score, ]
  df5 = df5[df5[ ,i] < min.max.score, ]
}

# only deleted about 500 units!

# now, we have to recalculate the propensity scores, after the irrelevant units have been deleted
df6 = df5[ ,-(ncol(df5) - c(3:0))]
unordered.multinomial1 = multinom(highest_degree ~ .,
                                  data=df6[ ,c("highest_degree",covars)], maxit=10000)
prop.scores1 = predict(unordered.multinomial1, newdata=df6, type="probs")
df7 = cbind(df6, prop.scores1)

cluster.ids = list()
# now, subclassify based on prop.scores
for (k in 4:10) {
  cluster.ids[[k]] = kmeans(df7[ ,tail(colnames(df5),4)], k)
  print(table(cbind(df5$highest_degree, as.data.frame(cluster.ids[[k]]$cluster))))
}

cluster.tot.withinss = c()
for (k in 4:10) {
  cluster.tot.withinss = rbind(cluster.tot.withinss, c(k, cluster.ids[[k]]$tot.withinss))
}
plot(cluster.tot.withinss)

clusGap(df5[ ,tail(colnames(df5),4)], kmeans, K.max=10)
