df1 = read.csv("Google Drive/raw_data_readable.csv.xlt")
df1[df1=="Not applicable"] = NA

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

answered.enough.questions = function(unit, question.set, threshold=3) {
  return(as.numeric(sum(is.na(unit[question.set])) < 3))
}
df2 = df1
for (i in c("environment","social","economic")) {
  df2$newcol = apply(df1, 1, answered.enough.questions, question.set=get(paste(i,"questions",sep=".")))
  colnames(df2)[length(colnames(df2))] = paste("answered.enough",i,sep=".")
}

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

# build the multinomial regression for propensity scores
df4 = subset(df3, !is.na(Respondents.income))[ ,c(2:4,6,38)]
df4$highest_degree = ordered(df4$highest_degree, c("no_high_school","high_school","bachelor","post_graduate"))
df4$Age.of.respondent = as.numeric(df4$Age.of.respondent)
library(MASS)
ordered.multinomial = polr(highest_degree ~ . - Respondent.id.number,
                           data=df4, method="logistic")
with(ordered.multinomial, c(deviance, df.residual))
predict(ordered.multinomial, newdata=data.frame(df4))

df4 = subset(df3, !is.na(Respondents.income))[ ,c(3:7,38)]
library(nnet)
unordered.multinomial = multinom(highest_degree ~ . - Respondent.id.number,
                                 data=df4, maxit=10000)
with(unordered.multinomial, c(deviance, edf))
predict(unordered.multinomial, newdata=data.frame(df4), type="probs")