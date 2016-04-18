maindata = read.csv('raw_data_readable_numerical.csv', na.strings="")

summary(maindata$Should.govt.reduce.income.differences)

maindata$Should.govt.reduce.income.differences <- as.character(maindata$Should.govt.reduce.income.differences)
maindata$Should.govt.reduce.income.differences[maindata$Should.govt.reduce.income.differences=="Govt reduce diff"] = 2
maindata$Should.govt.reduce.income.differences[maindata$Should.govt.reduce.income.differences=="No govt action"] = 6
maindata$Should.govt.reduce.income.differences[maindata$Should.govt.reduce.income.differences=="Don't know"] = 4
maindata$Should.govt.reduce.income.differences[maindata$Should.govt.reduce.income.differences=="No answer"] = NA
maindata$Should.govt.reduce.income.differences[maindata$Should.govt.reduce.income.differences=="Not Applicable"] = NA
maindata$Should.govt.reduce.income.differences <- as.factor(maindata$Should.govt.reduce.income.differences)

summary(maindata$Govt.should.redistribute.wealth)
maindata$Govt.should.redistribute.wealth <- as.character(maindata$Govt.should.redistribute.wealth)
maindata$Govt.should.redistribute.wealth[maindata$Govt.should.redistribute.wealth=="Agree strongly"] = 1
maindata$Govt.should.redistribute.wealth[maindata$Govt.should.redistribute.wealth=="Agree"] = 2
maindata$Govt.should.redistribute.wealth[maindata$Govt.should.redistribute.wealth=="Disagree"] = 4
maindata$Govt.should.redistribute.wealth[maindata$Govt.should.redistribute.wealth=="Disagree strongly"] = 5
maindata$Govt.should.redistribute.wealth[maindata$Govt.should.redistribute.wealth=="Neither"] = 3
maindata$Govt.should.redistribute.wealth[maindata$Govt.should.redistribute.wealth=="No answer"] = 3
maindata$Govt.should.redistribute.wealth[maindata$Govt.should.redistribute.wealth=="Not applicable"] = NA
maindata$Govt.should.redistribute.wealth[maindata$Govt.should.redistribute.wealth=="Don't know"] = 3
maindata$Govt.should.redistribute.wealth <- as.factor(maindata$Govt.should.redistribute.wealth)




maindata$Should.govt.help.pay.for.medical.care. <- as.character(maindata$Should.govt.help.pay.for.medical.care.)
maindata$Should.govt.help.pay.for.medical.care.[maindata$Should.govt.help.pay.for.medical.care.=="Govt should help"] = 2
maindata$Should.govt.help.pay.for.medical.care.[maindata$Should.govt.help.pay.for.medical.care.=="People help selves"] = 4
maindata$Should.govt.help.pay.for.medical.care.[maindata$Should.govt.help.pay.for.medical.care.=="Agree with both"] = 3
maindata$Should.govt.help.pay.for.medical.care.[maindata$Should.govt.help.pay.for.medical.care.=="Don't know"] = 3
maindata$Should.govt.help.pay.for.medical.care.[maindata$Should.govt.help.pay.for.medical.care.=="No answer"] = 3
maindata$Should.govt.help.pay.for.medical.care.[maindata$Should.govt.help.pay.for.medical.care.=="Not applicable"] = 3
maindata$Should.govt.help.pay.for.medical.care. <- as.factor(maindata$Should.govt.help.pay.for.medical.care.)
summary(maindata$Should.govt.help.pay.for.medical.care.)

summary(maindata$Should.govt.improve.standard.of.living. )
maindata$Should.govt.improve.standard.of.living. <- as.character(maindata$Should.govt.improve.standard.of.living.)
maindata$Should.govt.improve.standard.of.living. [maindata$Should.govt.improve.standard.of.living. =="Govt action"] = 2
maindata$Should.govt.improve.standard.of.living. [maindata$Should.govt.improve.standard.of.living. =="People help selves"] = 4
maindata$Should.govt.improve.standard.of.living. [maindata$Should.govt.improve.standard.of.living. =="Agree with both"] = 3
maindata$Should.govt.improve.standard.of.living. [maindata$Should.govt.improve.standard.of.living. =="No answer"] = 3
maindata$Should.govt.improve.standard.of.living. [maindata$Should.govt.improve.standard.of.living. =="Don't know"] = 3
maindata$Should.govt.improve.standard.of.living. [maindata$Should.govt.improve.standard.of.living. =="Not applicable"] = NA
maindata$Should.govt.improve.standard.of.living. <- as.factor(maindata$Should.govt.improve.standard.of.living.)
summary(maindata$Should.govt.improve.standard.of.living. )

summary(maindata$Should.govt.do.more.or.less.)
maindata$Should.govt.do.more.or.less. <- as.character(maindata$Should.govt.do.more.or.less.)
maindata$Should.govt.do.more.or.less. [maindata$Should.govt.do.more.or.less. =="Govt do more"] = 2
maindata$Should.govt.do.more.or.less. [maindata$Should.govt.do.more.or.less. =="Govt does too much"] = 4
maindata$Should.govt.do.more.or.less. [maindata$Should.govt.do.more.or.less. =="Agree with both"] = 3
maindata$Should.govt.do.more.or.less. [maindata$Should.govt.do.more.or.less. =="No answer"] = 3
maindata$Should.govt.do.more.or.less. [maindata$Should.govt.do.more.or.less. =="Don't know"] = 3
maindata$Should.govt.do.more.or.less. [maindata$Should.govt.do.more.or.less. =="Not applicable"] = NA
maindata$Should.govt.do.more.or.less. <- as.factor(maindata$Should.govt.do.more.or.less.)
summary(maindata$Should.govt.do.more.or.less. )

summary(maindata$Number.of.immigrants.to.america.nowadays.should.be)
maindata$Number.of.immigrants.to.america.nowadays.should.be<-as.character(maindata$Number.of.immigrants.to.america.nowadays.should.be)
maindata$Number.of.immigrants.to.america.nowadays.should.be [maindata$Number.of.immigrants.to.america.nowadays.should.be =="Increased a lot"] = 1
maindata$Number.of.immigrants.to.america.nowadays.should.be [maindata$Number.of.immigrants.to.america.nowadays.should.be =="Increased a little"] = 2
maindata$Number.of.immigrants.to.america.nowadays.should.be [maindata$Number.of.immigrants.to.america.nowadays.should.be =="Cant choose"] = 3
maindata$Number.of.immigrants.to.america.nowadays.should.be [maindata$Number.of.immigrants.to.america.nowadays.should.be =="No answer"] = 3
maindata$Number.of.immigrants.to.america.nowadays.should.be [maindata$Number.of.immigrants.to.america.nowadays.should.be =="Remain the same as it is"] = 3
maindata$Number.of.immigrants.to.america.nowadays.should.be [maindata$Number.of.immigrants.to.america.nowadays.should.be =="Reduced a little"] = 4
maindata$Number.of.immigrants.to.america.nowadays.should.be [maindata$Number.of.immigrants.to.america.nowadays.should.be =="Reduced a lot"] = 5
maindata$Number.of.immigrants.to.america.nowadays.should.be [maindata$Number.of.immigrants.to.america.nowadays.should.be =="Not applicable"] = NA
maindata$Number.of.immigrants.to.america.nowadays.should.be <- as.factor(maindata$Number.of.immigrants.to.america.nowadays.should.be)
summary(maindata$Number.of.immigrants.to.america.nowadays.should.be )

summary(maindata$Govmnt.should.reduce.inc.differentials)
maindata$Govmnt.should.reduce.inc.differentials<-as.character(maindata$Govmnt.should.reduce.inc.differentials)
maindata$Govmnt.should.reduce.inc.differentials[maindata$Govmnt.should.reduce.inc.differentials=="Strongly agree"] = 1
maindata$Govmnt.should.reduce.inc.differentials[maindata$Govmnt.should.reduce.inc.differentials=="Agree"] = 2
maindata$Govmnt.should.reduce.inc.differentials[maindata$Govmnt.should.reduce.inc.differentials=="Disagree"] = 4
maindata$Govmnt.should.reduce.inc.differentials[maindata$Govmnt.should.reduce.inc.differentials=="Strongly disagree"] = 5
maindata$Govmnt.should.reduce.inc.differentials[maindata$Govmnt.should.reduce.inc.differentials=="Neither"] = 3
maindata$Govmnt.should.reduce.inc.differentials[maindata$Govmnt.should.reduce.inc.differentials=="Cant choose"] = 3
maindata$Govmnt.should.reduce.inc.differentials[maindata$Govmnt.should.reduce.inc.differentials=="No answer"] = 3
maindata$Govmnt.should.reduce.inc.differentials[maindata$Govmnt.should.reduce.inc.differentials=="Not applicable"] = NA
maindata$Govmnt.should.reduce.inc.differentials <- as.factor(maindata$Govmnt.should.reduce.inc.differentials)
summary(maindata$Govmnt.should.reduce.inc.differentials)

summary(maindata$Private.enterprise.will.solve.u.s..problems)
maindata$Private.enterprise.will.solve.u.s..problems<-as.character(maindata$Private.enterprise.will.solve.u.s..problems)
maindata$Private.enterprise.will.solve.u.s..problems[maindata$Private.enterprise.will.solve.u.s..problems=="Strongly agree"] = 1
maindata$Private.enterprise.will.solve.u.s..problems[maindata$Private.enterprise.will.solve.u.s..problems=="Agree"] = 2
maindata$Private.enterprise.will.solve.u.s..problems[maindata$Private.enterprise.will.solve.u.s..problems=="Disagree"] = 4
maindata$Private.enterprise.will.solve.u.s..problems[maindata$Private.enterprise.will.solve.u.s..problems=="Strongly disagree"] = 5
maindata$Private.enterprise.will.solve.u.s..problems[maindata$Private.enterprise.will.solve.u.s..problems=="Neither agree nor disagree"] = 3
maindata$Private.enterprise.will.solve.u.s..problems[maindata$Private.enterprise.will.solve.u.s..problems=="Don't know"] = 3
maindata$Private.enterprise.will.solve.u.s..problems[maindata$Private.enterprise.will.solve.u.s..problems=="No answer"] = 3
maindata$Private.enterprise.will.solve.u.s..problems[maindata$Private.enterprise.will.solve.u.s..problems=="Not applicable"] = NA
maindata$Private.enterprise.will.solve.u.s..problems<-as.factor(maindata$Private.enterprise.will.solve.u.s..problems)
summary(maindata$Private.enterprise.will.solve.u.s..problems)

#Social Issues

summary(maindata$Should.govt.aid.blacks.)
maindata$Should.govt.aid.blacks.<-as.character(maindata$Should.govt.aid.blacks.)
maindata$Should.govt.aid.blacks.[maindata$Should.govt.aid.blacks.=="Agree with both"] = 3
maindata$Should.govt.aid.blacks.[maindata$Should.govt.aid.blacks.=="Govt help blks"] = 2
maindata$Should.govt.aid.blacks.[maindata$Should.govt.aid.blacks.=="No special treatment"] = 5
maindata$Should.govt.aid.blacks.[maindata$Should.govt.aid.blacks.=="Don't know"] = 3
maindata$Should.govt.aid.blacks.[maindata$Should.govt.aid.blacks.=="No answer"] = 3
maindata$Should.govt.aid.blacks.[maindata$Should.govt.aid.blacks.=="Not applicable"] = NA
maindata$Should.govt.aid.blacks.<-as.factor(maindata$Should.govt.aid.blacks.)

summary(maindata$Modern.science.does.more.harm.than.good)
maindata$Modern.science.does.more.harm.than.good<-as.character(maindata$Modern.science.does.more.harm.than.good)
maindata$Modern.science.does.more.harm.than.good[maindata$Modern.science.does.more.harm.than.good=="Strongly agree"] = 1
maindata$Modern.science.does.more.harm.than.good[maindata$Modern.science.does.more.harm.than.good=="Agree"] = 2
maindata$Modern.science.does.more.harm.than.good[maindata$Modern.science.does.more.harm.than.good=="Disagree"] = 4
maindata$Modern.science.does.more.harm.than.good[maindata$Modern.science.does.more.harm.than.good=="Strongly disagree"] = 5
maindata$Modern.science.does.more.harm.than.good[maindata$Modern.science.does.more.harm.than.good=="Neither agree nor disagree"] = 3
maindata$Modern.science.does.more.harm.than.good[maindata$Modern.science.does.more.harm.than.good=="Don't know"] = 3
maindata$Modern.science.does.more.harm.than.good[maindata$Modern.science.does.more.harm.than.good=="No answer"] = 3
maindata$Modern.science.does.more.harm.than.good[maindata$Modern.science.does.more.harm.than.good=="Not applicable"] = NA
maindata$Modern.science.does.more.harm.than.good<-as.factor(maindata$Modern.science.does.more.harm.than.good)
summary(maindata$Modern.science.does.more.harm.than.good)

summary(maindata$Believe.too.much.in.science..not.enough.faith)
maindata$Believe.too.much.in.science..not.enough.faith<-as.character(maindata$Believe.too.much.in.science..not.enough.faith)
maindata$Believe.too.much.in.science..not.enough.faith[maindata$Believe.too.much.in.science..not.enough.faith=="Strongly agree"] = 1
maindata$Believe.too.much.in.science..not.enough.faith[maindata$Believe.too.much.in.science..not.enough.faith=="Agree"] = 2
maindata$Believe.too.much.in.science..not.enough.faith[maindata$Believe.too.much.in.science..not.enough.faith=="Disagree"] = 4
maindata$Believe.too.much.in.science..not.enough.faith[maindata$Believe.too.much.in.science..not.enough.faith=="Strongly disagree"] = 5
maindata$Believe.too.much.in.science..not.enough.faith[maindata$Believe.too.much.in.science..not.enough.faith=="Neither agree nor disagree"] = 3
maindata$Believe.too.much.in.science..not.enough.faith[maindata$Believe.too.much.in.science..not.enough.faith=="Don't know"] = 3
maindata$Believe.too.much.in.science..not.enough.faith[maindata$Believe.too.much.in.science..not.enough.faith=="No answer"] = 3
maindata$Believe.too.much.in.science..not.enough.faith[maindata$Believe.too.much.in.science..not.enough.faith=="Not applicable"] = NA
maindata$Believe.too.much.in.science..not.enough.faith<-as.factor(maindata$Believe.too.much.in.science..not.enough.faith)
summary(maindata$Believe.too.much.in.science..not.enough.faith)

summary(maindata$Husb.shld.work.wife.shld.look.after.home)
maindata$Husb.shld.work.wife.shld.look.after.home<-as.character(maindata$Husb.shld.work.wife.shld.look.after.home)
maindata$Husb.shld.work.wife.shld.look.after.home[maindata$Husb.shld.work.wife.shld.look.after.home=="Strongly agree"] = 1
maindata$Husb.shld.work.wife.shld.look.after.home[maindata$Husb.shld.work.wife.shld.look.after.home=="Agree"] = 2
maindata$Husb.shld.work.wife.shld.look.after.home[maindata$Husb.shld.work.wife.shld.look.after.home=="Disagree"] = 4
maindata$Husb.shld.work.wife.shld.look.after.home[maindata$Husb.shld.work.wife.shld.look.after.home=="Strongly disagree"] = 5
maindata$Husb.shld.work.wife.shld.look.after.home[maindata$Husb.shld.work.wife.shld.look.after.home=="Neither agree nor disagree"] = 3
maindata$Husb.shld.work.wife.shld.look.after.home[maindata$Husb.shld.work.wife.shld.look.after.home=="Cant choose"] = 3
maindata$Husb.shld.work.wife.shld.look.after.home[maindata$Husb.shld.work.wife.shld.look.after.home=="No answer"] = 3
maindata$Husb.shld.work.wife.shld.look.after.home[maindata$Husb.shld.work.wife.shld.look.after.home=="Not applicable"] = NA
maindata$Husb.shld.work.wife.shld.look.after.home<-as.factor(maindata$Husb.shld.work.wife.shld.look.after.home)
summary(maindata$Husb.shld.work.wife.shld.look.after.home)

summary(maindata$Homosexual.sex.relations)
maindata$Homosexual.sex.relations<-as.character(maindata$Homosexual.sex.relations)
maindata$Homosexual.sex.relations[maindata$Homosexual.sex.relations=="Not wrong at all"] = 1
maindata$Homosexual.sex.relations[maindata$Homosexual.sex.relations=="Sometimes wrong"] = 2
maindata$Homosexual.sex.relations[maindata$Homosexual.sex.relations=="Always wrong"] = 4
maindata$Homosexual.sex.relations[maindata$Homosexual.sex.relations=="Almst always wrg"] = 3
maindata$Homosexual.sex.relations[maindata$Homosexual.sex.relations=="Don't know"] = 2.5
maindata$Homosexual.sex.relations[maindata$Homosexual.sex.relations=="No answer"] = 2.5
maindata$Homosexual.sex.relations[maindata$Homosexual.sex.relations=="Not applicable"] = NA
maindata$Homosexual.sex.relations[maindata$Homosexual.sex.relations=="Other"] = NA
maindata$Homosexual.sex.relations<-as.factor(maindata$Homosexual.sex.relations)
summary(maindata$Homosexual.sex.relations)

summary(maindata$Close.relative.marry.black)
maindata$Close.relative.marry.black<-as.character(maindata$Close.relative.marry.black)
maindata$Close.relative.marry.black[maindata$Close.relative.marry.black=="Strongly favor"] = 1
maindata$Close.relative.marry.black[maindata$Close.relative.marry.black=="Favor"] = 2
maindata$Close.relative.marry.black[maindata$Close.relative.marry.black=="Oppose"] = 4
maindata$Close.relative.marry.black[maindata$Close.relative.marry.black=="Strongly oppose"] = 5
maindata$Close.relative.marry.black[maindata$Close.relative.marry.black=="Neither favor nor oppose"] = 3
maindata$Close.relative.marry.black[maindata$Close.relative.marry.black=="Dont know"] = 3
maindata$Close.relative.marry.black[maindata$Close.relative.marry.black=="No answer"] = 3
maindata$Close.relative.marry.black[maindata$Close.relative.marry.black=="Not applicable"] = NA
maindata$Close.relative.marry.black<-as.factor(maindata$Close.relative.marry.black)
summary(maindata$Close.relative.marry.black)

summary(maindata$Ok.to.test.on.animals.to.save.humans)
maindata$Ok.to.test.on.animals.to.save.humans<-as.character(maindata$Ok.to.test.on.animals.to.save.humans)
maindata$Ok.to.test.on.animals.to.save.humans[maindata$Ok.to.test.on.animals.to.save.humans=="Strongly agree"] = 1
maindata$Ok.to.test.on.animals.to.save.humans[maindata$Ok.to.test.on.animals.to.save.humans=="Agree"] = 2
maindata$Ok.to.test.on.animals.to.save.humans[maindata$Ok.to.test.on.animals.to.save.humans=="Disagree"] = 4
maindata$Ok.to.test.on.animals.to.save.humans[maindata$Ok.to.test.on.animals.to.save.humans=="Strongly disagree"] = 5
maindata$Ok.to.test.on.animals.to.save.humans[maindata$Ok.to.test.on.animals.to.save.humans=="Neither agree nor disagree"] = 3
maindata$Ok.to.test.on.animals.to.save.humans[maindata$Ok.to.test.on.animals.to.save.humans=="Don't know"] = 3
maindata$Ok.to.test.on.animals.to.save.humans[maindata$Ok.to.test.on.animals.to.save.humans=="No answer"] = 3
maindata$Ok.to.test.on.animals.to.save.humans[maindata$Ok.to.test.on.animals.to.save.humans=="Not applicable"] = NA
maindata$Ok.to.test.on.animals.to.save.humans<-as.factor(maindata$Ok.to.test.on.animals.to.save.humans)
summary(maindata$Ok.to.test.on.animals.to.save.humans)

# ENVIRONMENTAL ISSUES

summary(maindata$Worry.too.much.about.envir..too.little.econ)
maindata$Worry.too.much.about.envir..too.little.econ<-as.character(maindata$Worry.too.much.about.envir..too.little.econ)
maindata$Worry.too.much.about.envir..too.little.econ[maindata$Worry.too.much.about.envir..too.little.econ=="Strongly agree"] = 1
maindata$Worry.too.much.about.envir..too.little.econ[maindata$Worry.too.much.about.envir..too.little.econ=="Agree"] = 2
maindata$Worry.too.much.about.envir..too.little.econ[maindata$Worry.too.much.about.envir..too.little.econ=="Disagree"] = 4
maindata$Worry.too.much.about.envir..too.little.econ[maindata$Worry.too.much.about.envir..too.little.econ=="Strongly disagree"] = 5
maindata$Worry.too.much.about.envir..too.little.econ[maindata$Worry.too.much.about.envir..too.little.econ=="Neither agree nor disagree"] = 3
maindata$Worry.too.much.about.envir..too.little.econ[maindata$Worry.too.much.about.envir..too.little.econ=="Don't know"] = 3
maindata$Worry.too.much.about.envir..too.little.econ[maindata$Worry.too.much.about.envir..too.little.econ=="No answer"] = 3
maindata$Worry.too.much.about.envir..too.little.econ[maindata$Worry.too.much.about.envir..too.little.econ=="Not applicable"] = NA
maindata$Worry.too.much.about.envir..too.little.econ<-as.factor(maindata$Worry.too.much.about.envir..too.little.econ)
summary(maindata$Worry.too.much.about.envir..too.little.econ)

summary(maindata$Almost.everything.we.do.harms.envir)
maindata$Almost.everything.we.do.harms.envir<-as.character(maindata$Almost.everything.we.do.harms.envir)
maindata$Almost.everything.we.do.harms.envir[maindata$Almost.everything.we.do.harms.envir=="Strongly agree"] = 1
maindata$Almost.everything.we.do.harms.envir[maindata$Almost.everything.we.do.harms.envir=="Agree"] = 2
maindata$Almost.everything.we.do.harms.envir[maindata$Almost.everything.we.do.harms.envir=="Disagree"] = 4
maindata$Almost.everything.we.do.harms.envir[maindata$Almost.everything.we.do.harms.envir=="Strongly disagree"] = 5
maindata$Almost.everything.we.do.harms.envir[maindata$Almost.everything.we.do.harms.envir=="Neither agree nor disagree"] = 3
maindata$Almost.everything.we.do.harms.envir[maindata$Almost.everything.we.do.harms.envir=="Don't know"] = 3
maindata$Almost.everything.we.do.harms.envir[maindata$Almost.everything.we.do.harms.envir=="No answer"] = 3
maindata$Almost.everything.we.do.harms.envir[maindata$Almost.everything.we.do.harms.envir=="Not applicable"] = NA
maindata$Almost.everything.we.do.harms.envir<-as.factor(maindata$Almost.everything.we.do.harms.envir)
summary(maindata$Almost.everything.we.do.harms.envir)

summary(maindata$Accept.cut.in.living.stnds.to.help.envir.)
maindata$Accept.cut.in.living.stnds.to.help.envir.<-as.character(maindata$Accept.cut.in.living.stnds.to.help.envir.)
maindata$Accept.cut.in.living.stnds.to.help.envir.[maindata$Accept.cut.in.living.stnds.to.help.envir.=="Very willing"] = 1
maindata$Accept.cut.in.living.stnds.to.help.envir.[maindata$Accept.cut.in.living.stnds.to.help.envir.=="Fairly willing"] = 2
maindata$Accept.cut.in.living.stnds.to.help.envir.[maindata$Accept.cut.in.living.stnds.to.help.envir.=="Not at all willing"] = 4
maindata$Accept.cut.in.living.stnds.to.help.envir.[maindata$Accept.cut.in.living.stnds.to.help.envir.=="Not very willing"] = 5
maindata$Accept.cut.in.living.stnds.to.help.envir.[maindata$Accept.cut.in.living.stnds.to.help.envir.=="Neither willing nor unwill"] = 3
maindata$Accept.cut.in.living.stnds.to.help.envir.[maindata$Accept.cut.in.living.stnds.to.help.envir.=="Don't know"] = 3
maindata$Accept.cut.in.living.stnds.to.help.envir.[maindata$Accept.cut.in.living.stnds.to.help.envir.=="No answer"] = 3
maindata$Accept.cut.in.living.stnds.to.help.envir.[maindata$Accept.cut.in.living.stnds.to.help.envir.=="Not applicable"] = NA
maindata$Accept.cut.in.living.stnds.to.help.envir.<-as.factor(maindata$Accept.cut.in.living.stnds.to.help.envir.)
summary(maindata$Accept.cut.in.living.stnds.to.help.envir.)

summary(maindata$Pay.higher.taxes.to.help.envir.)
maindata$Pay.higher.taxes.to.help.envir.<-as.character(maindata$Pay.higher.taxes.to.help.envir.)
maindata$Pay.higher.taxes.to.help.envir.[maindata$Pay.higher.taxes.to.help.envir.=="Very willing"] = 1
maindata$Pay.higher.taxes.to.help.envir.[maindata$Pay.higher.taxes.to.help.envir.=="Fairly willing"] = 2
maindata$Pay.higher.taxes.to.help.envir.[maindata$Pay.higher.taxes.to.help.envir.=="Not at all willing"] = 4
maindata$Pay.higher.taxes.to.help.envir.[maindata$Pay.higher.taxes.to.help.envir.=="Not very willing"] = 5
maindata$Pay.higher.taxes.to.help.envir.[maindata$Pay.higher.taxes.to.help.envir.=="Neither willing nor unwill"] = 3
maindata$Pay.higher.taxes.to.help.envir.[maindata$Pay.higher.taxes.to.help.envir.=="Don't know"] = 3
maindata$Pay.higher.taxes.to.help.envir.[maindata$Pay.higher.taxes.to.help.envir.=="No answer"] = 3
maindata$Pay.higher.taxes.to.help.envir.[maindata$Pay.higher.taxes.to.help.envir.=="Not applicable"] = NA
maindata$Pay.higher.taxes.to.help.envir.<-as.factor(maindata$Pay.higher.taxes.to.help.envir.)
summary(maindata$Pay.higher.taxes.to.help.envir.)

summary(maindata$Pay.higher.prices.to.help.envir.)
maindata$Pay.higher.prices.to.help.envir.<-as.character(maindata$Pay.higher.prices.to.help.envir.)
maindata$Pay.higher.prices.to.help.envir.[maindata$Pay.higher.prices.to.help.envir.=="Very willing"] = 1
maindata$Pay.higher.prices.to.help.envir.[maindata$Pay.higher.prices.to.help.envir.=="Fairly willing"] = 2
maindata$Pay.higher.prices.to.help.envir.[maindata$Pay.higher.prices.to.help.envir.=="Not at all willing"] = 4
maindata$Pay.higher.prices.to.help.envir.[maindata$Pay.higher.prices.to.help.envir.=="Not very willing"] = 5
maindata$Pay.higher.prices.to.help.envir.[maindata$Pay.higher.prices.to.help.envir.=="Neither willing nor unwill"] = 3
maindata$Pay.higher.prices.to.help.envir.[maindata$Pay.higher.prices.to.help.envir.=="Don't know"] = 3
maindata$Pay.higher.prices.to.help.envir.[maindata$Pay.higher.prices.to.help.envir.=="No answer"] = 3
maindata$Pay.higher.prices.to.help.envir.[maindata$Pay.higher.prices.to.help.envir.=="Not applicable"] = NA
maindata$Pay.higher.prices.to.help.envir.<-as.factor(maindata$Pay.higher.prices.to.help.envir.)
summary(maindata$Pay.higher.prices.to.help.envir.)

summary(maindata$Econ.grwth.always.harms.envir)
maindata$Econ.grwth.always.harms.envir<-as.character(maindata$Econ.grwth.always.harms.envir)
maindata$Econ.grwth.always.harms.envir[maindata$Econ.grwth.always.harms.envir=="Strongly agree"] = 1
maindata$Econ.grwth.always.harms.envir[maindata$Econ.grwth.always.harms.envir=="Agree"] = 2
maindata$Econ.grwth.always.harms.envir[maindata$Econ.grwth.always.harms.envir=="Disagree"] = 4
maindata$Econ.grwth.always.harms.envir[maindata$Econ.grwth.always.harms.envir=="Strongly disagree"] = 5
maindata$Econ.grwth.always.harms.envir[maindata$Econ.grwth.always.harms.envir=="Neither agree nor disagree"] = 3
maindata$Econ.grwth.always.harms.envir[maindata$Econ.grwth.always.harms.envir=="Don't know"] = 3
maindata$Econ.grwth.always.harms.envir[maindata$Econ.grwth.always.harms.envir=="No answer"] = 3
maindata$Econ.grwth.always.harms.envir[maindata$Econ.grwth.always.harms.envir=="Not applicable"] = NA
maindata$Econ.grwth.always.harms.envir<-as.factor(maindata$Econ.grwth.always.harms.envir)
summary(maindata$Econ.grwth.always.harms.envir)

summary(maindata$Amer.needs.econ.grwth.to.protect.envir)
maindata$Amer.needs.econ.grwth.to.protect.envir<-as.character(maindata$Amer.needs.econ.grwth.to.protect.envir)
maindata$Amer.needs.econ.grwth.to.protect.envir[maindata$Amer.needs.econ.grwth.to.protect.envir=="Strongly agree"] = 1
maindata$Amer.needs.econ.grwth.to.protect.envir[maindata$Amer.needs.econ.grwth.to.protect.envir=="Agree"] = 2
maindata$Amer.needs.econ.grwth.to.protect.envir[maindata$Amer.needs.econ.grwth.to.protect.envir=="Disagree"] = 4
maindata$Amer.needs.econ.grwth.to.protect.envir[maindata$Amer.needs.econ.grwth.to.protect.envir=="Strongly disagree"] = 5
maindata$Amer.needs.econ.grwth.to.protect.envir[maindata$Amer.needs.econ.grwth.to.protect.envir=="Neither agree nor disagree"] = 3
maindata$Amer.needs.econ.grwth.to.protect.envir[maindata$Amer.needs.econ.grwth.to.protect.envir=="Don't know"] = 3
maindata$Amer.needs.econ.grwth.to.protect.envir[maindata$Amer.needs.econ.grwth.to.protect.envir=="No answer"] = 3
maindata$Amer.needs.econ.grwth.to.protect.envir[maindata$Amer.needs.econ.grwth.to.protect.envir=="Not applicable"] = NA
maindata$Amer.needs.econ.grwth.to.protect.envir<-as.factor(maindata$Amer.needs.econ.grwth.to.protect.envir)
summary(maindata$Amer.needs.econ.grwth.to.protect.envir)

summary(maindata$Modern.science.will.solve.envir.probs)
maindata$Modern.science.will.solve.envir.probs<-as.character(maindata$Modern.science.will.solve.envir.probs)
maindata$Modern.science.will.solve.envir.probs[maindata$Modern.science.will.solve.envir.probs=="Strongly agree"] = 1
maindata$Modern.science.will.solve.envir.probs[maindata$Modern.science.will.solve.envir.probs=="Agree"] = 2
maindata$Modern.science.will.solve.envir.probs[maindata$Modern.science.will.solve.envir.probs=="Disagree"] = 4
maindata$Modern.science.will.solve.envir.probs[maindata$Modern.science.will.solve.envir.probs=="Strongly disagree"] = 5
maindata$Modern.science.will.solve.envir.probs[maindata$Modern.science.will.solve.envir.probs=="Neither agree nor disagree"] = 3
maindata$Modern.science.will.solve.envir.probs[maindata$Modern.science.will.solve.envir.probs=="Don't know"] = 3
maindata$Modern.science.will.solve.envir.probs[maindata$Modern.science.will.solve.envir.probs=="No answer"] = 3
maindata$Modern.science.will.solve.envir.probs[maindata$Modern.science.will.solve.envir.probs=="Not applicable"] = NA
maindata$Modern.science.will.solve.envir.probs<-as.factor(maindata$Modern.science.will.solve.envir.probs)
summary(maindata$Modern.science.will.solve.envir.probs)

summary(maindata$Drive.less.for.envir.reasons)
maindata$Drive.less.for.envir.reasons<-as.character(maindata$Drive.less.for.envir.reasons)
maindata$Drive.less.for.envir.reasons[maindata$Drive.less.for.envir.reasons=="Always"] = 1
maindata$Drive.less.for.envir.reasons[maindata$Drive.less.for.envir.reasons=="Often"] = 2
maindata$Drive.less.for.envir.reasons[maindata$Drive.less.for.envir.reasons=="Sometimes"] = 4
maindata$Drive.less.for.envir.reasons[maindata$Drive.less.for.envir.reasons=="Never"] = 3
maindata$Drive.less.for.envir.reasons[maindata$Drive.less.for.envir.reasons=="Don't know"] = 2.5
maindata$Drive.less.for.envir.reasons[maindata$Drive.less.for.envir.reasons=="No answer"] = 2.5
maindata$Drive.less.for.envir.reasons[maindata$Drive.less.for.envir.reasons=="Not applicable"] = NA
maindata$Drive.less.for.envir.reasons[maindata$Drive.less.for.envir.reasons=="No car, dont drive"] = 2.5
maindata$Drive.less.for.envir.reasons<-as.factor(maindata$Drive.less.for.envir.reasons)
summary(maindata$Drive.less.for.envir.reasons)

summary(maindata$Greenhouse.effect.danger.to.envir)
maindata$Greenhouse.effect.danger.to.envir<-as.character(maindata$Greenhouse.effect.danger.to.envir)
maindata$Greenhouse.effect.danger.to.envir[maindata$Greenhouse.effect.danger.to.envir=="Extremely dangerous"] = 1
maindata$Greenhouse.effect.danger.to.envir[maindata$Greenhouse.effect.danger.to.envir=="Very dangerous"] = 2
maindata$Greenhouse.effect.danger.to.envir[maindata$Greenhouse.effect.danger.to.envir=="Somewhat dangerous"] = 3
maindata$Greenhouse.effect.danger.to.envir[maindata$Greenhouse.effect.danger.to.envir=="Not very dangerous"] = 4
maindata$Greenhouse.effect.danger.to.envir[maindata$Greenhouse.effect.danger.to.envir=="Not dangerous"] = 5
maindata$Greenhouse.effect.danger.to.envir[maindata$Greenhouse.effect.danger.to.envir=="Don't know"] = 3
maindata$Greenhouse.effect.danger.to.envir[maindata$Greenhouse.effect.danger.to.envir=="No answer"] = 3
maindata$Greenhouse.effect.danger.to.envir[maindata$Greenhouse.effect.danger.to.envir=="Not applicable"] = NA
maindata$Greenhouse.effect.danger.to.envir<-as.factor(maindata$Greenhouse.effect.danger.to.envir)
summary(maindata$Greenhouse.effect.danger.to.envir)

summary(maindata$Nuke.power.danger.to.envir)
maindata$Nuke.power.danger.to.envir<-as.character(maindata$Nuke.power.danger.to.envir)
maindata$Nuke.power.danger.to.envir[maindata$Nuke.power.danger.to.envir=="Extremely dangerous"] = 1
maindata$Nuke.power.danger.to.envir[maindata$Nuke.power.danger.to.envir=="Very dangerous"] = 2
maindata$Nuke.power.danger.to.envir[maindata$Nuke.power.danger.to.envir=="Somewhat dangerous"] = 3
maindata$Nuke.power.danger.to.envir[maindata$Nuke.power.danger.to.envir=="Not very dangerous"] = 4
maindata$Nuke.power.danger.to.envir[maindata$Nuke.power.danger.to.envir=="Not dangerous"] = 5
maindata$Nuke.power.danger.to.envir[maindata$Nuke.power.danger.to.envir=="Don't know"] = 3
maindata$Nuke.power.danger.to.envir[maindata$Nuke.power.danger.to.envir=="No answer"] = 3
maindata$Nuke.power.danger.to.envir[maindata$Nuke.power.danger.to.envir=="Not applicable"] = NA
maindata$Nuke.power.danger.to.envir<-as.factor(maindata$Nuke.power.danger.to.envir)
summary(maindata$Nuke.power.danger.to.envir)

write.csv(maindata, 'raw_data_t.csv')
