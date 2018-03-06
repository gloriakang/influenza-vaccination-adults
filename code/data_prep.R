#' Data prep:
#' 1. Create new column names for multi-column questions
#' 2. Re-order factors and levels
#' 2.1. Respondent characteristics
#' 2.2. Question responses
#' 3. Household section

rm(list = ls(all.names = TRUE))
library(car)
library(dplyr)

##########' 1. Create new column names for multi-column questions ##########

# load data and set NA
dat <- read.csv("data/surveydata.csv", na = c("#NULL!", "", "Refused", "NA"))

# rename sub-columns
dat_rename <- dat %>%
  # Q7
  rename("Q7_1_Bus" = Q7_1,
         "Q7_2_Carpool" = Q7_2,
         "Q7_3_Subway" = Q7_3,
         "Q7_4_Train" = Q7_4,
         "Q7_5_Taxi" = Q7_5,
         "Q7_6_Airplane" = Q7_6,
         "Q7_7_Other" = Q7_7,
         "Q7_8_Refused" = Q7_8) %>%
  # Q8
  rename("Q8_1_Work" = Q8_1,
         "Q8_2_School" = Q8_2,
         "Q8_3_Shopping" = Q8_3,
         "Q8_4_Visiting.people" = Q8_4,
         "Q8_5_Recreation" = Q8_5,
         "Q8_6_Other" = Q8_6,
         "Q8_7_Refused"= Q8_7) %>%
  # Q10
  rename("Q10_1_Bus" = Q10_1,
         "Q10_2_Carpool" = Q10_2,
         "Q10_3_Subway" = Q10_3,
         "Q10_4_Train" = Q10_4,
         "Q10_5_Taxi" = Q10_5,
         "Q10_6_Airplane" = Q10_6,
         "Q10_7_Don_t.know" = Q10_7,
         "Q10_8_Other" = Q10_8,
         "Q10_9_Refused" = Q10_9) %>%
  # Q11
  rename("Q11_1_Work" = Q11_1,
         "Q11_2_Schools" = Q11_2,
         "Q11_3_Day.care" = Q11_3,
         "Q11_4_Stores" = Q11_4,
         "Q11_5_Restaurants" = Q11_5,
         "Q11_6_Libraries" = Q11_6,
         "Q11_7_Hospitals" = Q11_7,
         "Q11_8_Doctor_s.office" = Q11_8,
         "Q11_9_Public.transportation" = Q11_9,
         "Q11_10_Family.or.friends" = Q11_10,
         "Q11_11_Other" = Q11_11) %>%
  # Q12
  rename("Q12_1_Avoid.touching.my.eyes" = Q12_1,
         "Q12_2_Avoid.touching.my.nose" = Q12_2,
         "Q12_3_Avoid.touching.my.mouth" =	Q12_3,
         "Q12_4_Wash.my.hands.with.soap.more.often"= Q12_4,
         "Q12_5_Use.hand.sanitizers" = Q12_5,
         "Q12_6_Clean.the.surfaces.in.my.home" =	Q12_6,
         "Q12_7_Clean.the.surfaces.at.work" = Q12_7,
         "Q12_8_Eat.nutritious.food" = Q12_8,
         "Q12_9_Get.adequate.rest" = Q12_9,
         "Q12_10_Get.recommended.vaccine" =	Q12_10,
         "Q12_11_Take.preventive.medicine" = Q12_11,
         "Q12_12_Cover.my.nose.and.mouth.with.a.surgical.mask" = Q12_12,
         "Q12_13_Avoid.contact.with.people.who.are.sick" = Q12_13,
         "Q12_14_Avoid.crowded.places" = Q12_14,
         "Q12_15_Other" = Q12_15) %>%
  # Q18
  rename("Q18_1_The.vaccine.costs.too.much" = Q18_1,
         "Q18_2_The.vaccine.is.not.very.effective.in.preventing.influenza" = Q18_2,
         "Q18_3_I.am.not.likely.to.get.influenza" = Q18_3,
         "Q18_4_Do.not.know.where.to.get.vaccine" = Q18_4,
         "Q18_5_The.side.effect.of.the.vaccine.are.too.risky" = Q18_5,
         "Q18_6_I.am.allergic.to.some.of.the.ingredients.in.the.vaccine" = Q18_6,
         "Q18_7_I.do.not.like.shots" = Q18_7,
         "Q18_8_I.just.don_t.get.around.to.doing.it" = Q18_8,
         "Q18_9_I.have.to.travel.too.far.to.get.vaccine" = Q18_9,
         "Q18_10_Other" = Q18_10,
         "Q18_11_Refused" = Q18_11) %>%
  # Q22
  rename("Q22_1_Go.to.a.doctor_s.office.or.medical.clinic" = Q22_1,
         "Q22_2_Decide.on.treatment.without.consulting.a.health.practitioner" = Q22_2,
         "Q22_3_Search.the.internet.for.a.treatment" = Q22_3,
         "Q22_4_Get.adequate.sleep" = Q22_4,
         "Q22_5_Eat.nutritious.food" = Q22_5,
         "Q22_6_Take-over-counter.medication.for.symptoms" = Q22_6,
         "Q22_7_Take.an.antiviral.medicine" = Q22_7,
         "Q22_8_Take.no.action.to.treat.the.illness" = Q22_8,
         "Q22_9_Other" = Q22_9) %>%
  # Q23
  rename("Q23_1_Stand.away.from.people" = Q23_1,
         "Q23_2_Avoid.public.places" = Q23_2,
         "Q23_3_Avoid.public.transportation" = Q23_3,
         "Q23_4_Stay.at.home" = Q23_4,
         "Q23_5_Wash.my.hands.with.soap.more.often" = Q23_5,
         "Q23_6_Use.hand.sanitizers" = Q23_6,
         "Q23_7_Clean.the.surfaces.in.my.home" = Q23_7,
         "Q23_8_Clean.the.surfaces.I.use.at.work" = Q23_8,
         "Q23_9_Cover.my.nose.and.mouth.with.a.surgical.mask" = Q23_9,
         "Q23_10_Cover.my.nose.and.mouth.when.I.sneeze.or.cough" = Q23_10,
         "Q23_11_Other" = Q23_11) %>%
  # Q24
  rename("Q24_1_Print.media.such.as.newspapers.and.magazines" = Q24_1,
         "Q24_2_Traditional.media.such.as.television.and.radio" = Q24_2,
         "Q24_3_Social.media.such.as.internet.and.blogs" = Q24_3,
         "Q24_4_Word.of.mouth" = Q24_4,
         "Q24_5_None" = Q24_5,
         "Q24_6_Other" = Q24_6,
         "Q24_7_Refused" = Q24_7) %>%
  # Q25
  rename("Q25_1_Stand.away.from.people" = Q25_1,
         "Q25_2_Avoid.public.places" = Q25_2,
         "Q25_3_Avoid.public.transportation" = Q25_3,
         "Q25_4_Stay.at.home" = Q25_4,
         "Q25_5_Wash.my.hands.with.soap.more.often" = Q25_5,
         "Q25_6_Use.hand.sanitizers" = Q25_6,
         "Q25_7_Clean.the.surfaces.in.my.home" = Q25_7,
         "Q25_8_Clean.the.surfaces.I.use.at.work" = Q25_8,
         "Q25_9_Cover.my.nose.and.mouth.with.a.surgical.mask" = Q25_9,
         "Q25_10_Cover.my.nose.and.mouth.when.I.sneeze.or.cough" = Q25_10,
         "Q25_11_Other" = Q25_11) %>%
  # Q27
  rename("Q27_1_Keep.the.child.away.from.the.others.in.the.residence" = Q27_1,
         "Q27_2_Keep.the.child.out.of.school-daycare" = Q27_2,
         "Q27_3_Stop.child_s.social.activities.like.play.dates" =	Q27_3,
         "Q27_4_Other" = Q27_4) %>%
  # Q29
  rename("Q29_1_A.parent.brings.the.child.to.work" = Q29_1,
         "Q29_2_A.parent.stays.home" = Q29_2,
         "Q29_3_Another.adult.stays.home" = Q29_3,
         "Q29_4_Send.the.child.to.school.sick" = Q29_4,
         "Q29_5_Take.the.child.to.a.relative.or.friends" =	Q29_5,
         "Q29_6_Other" = Q29_6) %>%
  # Q30
  rename("Q30_1_I.bring.the.child.to.work" = Q30_1,
         "Q30_2_I.stay.home" = Q30_2,
         "Q30_3_Another.adult.stays.home" = Q30_3,
         "Q30_4_Send.the.child.to.school.sick" = Q30_4,
         "Q30_5_Take.the.child.to.a.relative.or.friends" = Q30_5,
         "Q30_6_Other" = Q30_6)

# save lists of names
old_names <- names(dat)
new_names <- names(dat_rename)





##########' 2. Re-order factors and levels ##########

data <- dat # copy original
dataf <- dat  # new dataframe for factors


##' 2.1. List and sort demographic factors

# age cat = 7
summary(dataf$ppagecat)
levels(dataf$ppagecat)

# age cat = 4
summary(dataf$ppagect4)
levels(dataf$ppagect4)

# education cat = 14
summary(dataf$PPEDUC)
levels(dataf$PPEDUC)
PPEDUC.lab <- c("No formal education", "1st, 2nd, 3rd, or 4th grade", "5th or 6th grade",
                "7th or 8th grade", "9th grade", "10th grade",
                "11th grade", "12th grade NO DIPLOMA",
                "HIGH SCHOOL GRADUATE - high school DIPLOMA or the equivalent (GED)",
                "Some college, no degree", "Associate degree", "Bachelors degree",
                "Masters degree", "Professional or Doctorate degree")
dataf$PPEDUC <- factor(data$PPEDUC, levels = PPEDUC.lab)

# education cat = 4
summary(dataf$PPEDUCAT)
levels(dataf$PPEDUCAT)
PPEDUCAT.lab <- c("Less than high school", "High school", "Some college", "Bachelor_s degree or higher")
dataf$PPEDUCAT <- factor(data$PPEDUCAT, levels = PPEDUCAT.lab)

# ethnicity cat = 5
summary(dataf$PPETHM)
levels(dataf$PPETHM)
PPETHM.lab <- c("White, Non-Hispanic", "Black, Non-Hispanic",
                "Hispanic", "Other, Non-Hispanic", "2+ Races, Non-Hispanic")
dataf$PPETHM <- factor(data$PPETHM, levels = PPETHM.lab)

# gender
summary(dataf$PPGENDER)

# head of household
summary(dataf$PPHHHEAD)

# house type
summary(dataf$PPHOUSE)
levels(dataf$PPHOUSE)

# income cat = 19
summary(dataf$PPINCIMP)
PPINCIMP.lab <- c("Less than $5,000", "$5,000 to $7,499", "$7,500 to $9,999",
                  "$10,000 to $12,499", "$12,500 to $14,999", "$15,000 to $19,999",
                  "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
                  "$35,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                  "$60,000 to $74,999", "$75,000 to $84,999", "$85,000 to $99,999",
                  "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $174,999",
                  "$175,000 or more")
dataf$PPINCIMP <- ordered(data$PPINCIMP, levels = PPINCIMP.lab)

# marital status cat = 6
summary(dataf$PPMARIT)
PPMARIT.lab <- c("Never married", "Living with partner", "Married",
                 "Separated", "Divorced", "Widowed")
dataf$PPMARIT <- factor(data$PPMARIT, levels = PPMARIT.lab)

# metro status
summary(dataf$PPMSACAT)

# geographic region cat = 4
summary(dataf$PPREG4)

# geographic region cat = 9
summary(dataf$ppreg9)

# rent status
summary(dataf$PPRENT)
levels(dataf$PPRENT)

# state of residence
summary(dataf$PPSTATEN)
levels(dataf$PPSTATEN)

# employment status
summary(dataf$PPWORK)
levels(dataf$PPWORK)

# internet status
summary(dataf$PPNET)





##########' 2.2. Relevel question factors ##########

# generic labels
yesnodk.lab <- c("Yes", "No", "Don_t know")
always.lab <- c("Always", "Sometimes", "Never")
likely.lab <- c("Yes, more likely", "No, no effect", "No, less likely")

# relevel yes = 1, no = 2
str(dataf$Q1)
dataf$Q1 <- relevel(data$Q1, "Yes")
dataf$Q2 <- relevel(data$Q2, "Yes")

str(dataf$Q3)
dataf$Q3 <- factor(data$Q3, levels = yesnodk.lab)
dataf$Q4 <- relevel(data$Q4, "Yes")
dataf$Q5 <- relevel(data$Q5, "Yes")
dataf$Q6 <- relevel(data$Q6, "Yes")

dataf$Q7_1 <- relevel(data$Q7_1, "Yes")
dataf$Q7_2 <- relevel(data$Q7_2, "Yes")
dataf$Q7_3 <- relevel(data$Q7_3, "Yes")
dataf$Q7_4 <- relevel(data$Q7_4, "Yes")
dataf$Q7_5 <- relevel(data$Q7_5, "Yes")
dataf$Q7_6 <- relevel(data$Q7_6, "Yes")
dataf$Q7_7 <- relevel(data$Q7_7, "Yes")
dataf$Q7_8 <- relevel(data$Q7_8, "Yes")

dataf$Q8_1 <- relevel(data$Q8_1, "Yes")
dataf$Q8_2 <- relevel(data$Q8_2, "Yes")
dataf$Q8_3 <- relevel(data$Q8_3, "Yes")
dataf$Q8_4 <- relevel(data$Q8_4, "Yes")
dataf$Q8_5 <- relevel(data$Q8_5, "Yes")
dataf$Q8_6 <- relevel(data$Q8_6, "Yes")
dataf$Q8_7 <- relevel(data$Q8_7, "Yes")

levels(dataf$Q9)
dataf$Q9 <- factor(data$Q9, levels = yesnodk.lab)

levels(dataf$Q10_9)
dataf$Q10_1 <- relevel(data$Q10_1, "Yes")
dataf$Q10_2 <- relevel(data$Q10_2, "Yes")
dataf$Q10_3 <- relevel(data$Q10_3, "Yes")
dataf$Q10_4 <- relevel(data$Q10_4, "Yes")
dataf$Q10_5 <- relevel(data$Q10_5, "Yes")
dataf$Q10_6 <- relevel(data$Q10_6, "Yes")
dataf$Q10_7 <- relevel(data$Q10_7, "Yes")
dataf$Q10_8 <- relevel(data$Q10_8, "Yes")
dataf$Q10_9 <- relevel(data$Q10_9, "Yes")

levels(dataf$Q11_11)
q11.lab <- c("High Risk, Very Likely", "Medium Risk, Somewhat Likely",
             "Low Risk, Not Likely", "Don_t Know")
dataf$Q11_1 <- factor(data$Q11_1, levels = q11.lab)
dataf$Q11_2 <- factor(data$Q11_2, levels = q11.lab)
dataf$Q11_3 <- factor(data$Q11_3, levels = q11.lab)
dataf$Q11_4 <- factor(data$Q11_4, levels = q11.lab)
dataf$Q11_5 <- factor(data$Q11_5, levels = q11.lab)
dataf$Q11_6 <- factor(data$Q11_6, levels = q11.lab)
dataf$Q11_7 <- factor(data$Q11_7, levels = q11.lab)
dataf$Q11_8 <- factor(data$Q11_8, levels = q11.lab)
dataf$Q11_9 <- factor(data$Q11_9, levels = q11.lab)
dataf$Q11_10 <- factor(data$Q11_10, levels = q11.lab)
dataf$Q11_11 <- factor(data$Q11_11, levels = q11.lab)

levels(dataf$Q12_15)
dataf$Q12_1 <- factor(data$Q12_1, levels = always.lab)
dataf$Q12_2 <- factor(data$Q12_2, levels = always.lab)
dataf$Q12_3 <- factor(data$Q12_3, levels = always.lab)
dataf$Q12_4 <- factor(data$Q12_4, levels = always.lab)
dataf$Q12_5 <- factor(data$Q12_5, levels = always.lab)
dataf$Q12_6 <- factor(data$Q12_6, levels = always.lab)
dataf$Q12_7 <- factor(data$Q12_7, levels = always.lab)
dataf$Q12_8 <- factor(data$Q12_8, levels = always.lab)
dataf$Q12_9 <- factor(data$Q12_9, levels = always.lab)
dataf$Q12_10 <- factor(data$Q12_10, levels = always.lab)
dataf$Q12_11 <- factor(data$Q12_11, levels = always.lab)
dataf$Q12_12 <- factor(data$Q12_12, levels = always.lab)
dataf$Q12_13 <- factor(data$Q12_13, levels = always.lab)
dataf$Q12_14 <- factor(data$Q12_14, levels = always.lab)
dataf$Q12_15 <- factor(data$Q12_15, levels = always.lab)

dataf$Q13 <- factor(data$Q13, levels = c("Yes, every year", "Yes, some years", "No, never"))
dataf$Q14 <- factor(data$Q14, levels = c("$0", "Less than $30", "$30 to $60", "More than $60", "Don_t know"))
dataf$Q15 <- factor(data$Q15, levels = likely.lab)
dataf$Q16 <- factor(data$Q16, levels = likely.lab)

levels(dataf$Q18_11)
dataf$Q18_1 <- relevel(data$Q18_1, "Yes")
dataf$Q18_2 <- relevel(data$Q18_2, "Yes")
dataf$Q18_3 <- relevel(data$Q18_3, "Yes")
dataf$Q18_4 <- relevel(data$Q18_4, "Yes")
dataf$Q18_5 <- relevel(data$Q18_5, "Yes")
dataf$Q18_6 <- relevel(data$Q18_6, "Yes")
dataf$Q18_7 <- relevel(data$Q18_7, "Yes")
dataf$Q18_8 <- relevel(data$Q18_8, "Yes")
dataf$Q18_9 <- relevel(data$Q18_9, "Yes")
dataf$Q18_10 <- relevel(data$Q18_10, "Yes")
dataf$Q18_11 <- relevel(data$Q18_11, "Yes")
dataf$Q19 <- relevel(data$Q19, "Yes")

dataf$Q20 <- factor(data$Q20, levels = c("Very effective", "Somewhat effective", "It varies from season to season", "Not effective", "Don_t know"))

dataf$Q21 <- factor(data$Q21, levels = c("Yes, the full cost is paid", "Yes, but only part of the cost is paid", "No", "Don_t know"))

levels(dataf$Q22_9)
dataf$Q22_1 <- factor(data$Q22_1, levels = always.lab)
dataf$Q22_2 <- factor(data$Q22_2, levels = always.lab)
dataf$Q22_3 <- factor(data$Q22_3, levels = always.lab)
dataf$Q22_4 <- factor(data$Q22_4, levels = always.lab)
dataf$Q22_5 <- factor(data$Q22_5, levels = always.lab)
dataf$Q22_6 <- factor(data$Q22_6, levels = always.lab)
dataf$Q22_7 <- factor(data$Q22_7, levels = always.lab)
dataf$Q22_8 <- factor(data$Q22_8, levels = always.lab)
dataf$Q22_9 <- factor(data$Q22_9, levels = always.lab)

levels(dataf$Q23_11)
dataf$Q23_1 <- factor(data$Q23_1, levels = always.lab)
dataf$Q23_2 <- factor(data$Q23_2, levels = always.lab)
dataf$Q23_3 <- factor(data$Q23_3, levels = always.lab)
dataf$Q23_4 <- factor(data$Q23_4, levels = always.lab)
dataf$Q23_5 <- factor(data$Q23_5, levels = always.lab)
dataf$Q23_6 <- factor(data$Q23_6, levels = always.lab)
dataf$Q23_7 <- factor(data$Q23_7, levels = always.lab)
dataf$Q23_8 <- factor(data$Q23_8, levels = always.lab)
dataf$Q23_9 <- factor(data$Q23_9, levels = always.lab)
dataf$Q23_10 <- factor(data$Q23_10, levels = always.lab)
dataf$Q23_11 <- factor(data$Q23_11, levels = always.lab)

levels(dataf$Q24_7)
dataf$Q24_1 <- relevel(data$Q24_1, "Yes")
dataf$Q24_2 <- relevel(data$Q24_2, "Yes")
dataf$Q24_3 <- relevel(data$Q24_3, "Yes")
dataf$Q24_4 <- relevel(data$Q24_4, "Yes")
dataf$Q24_5 <- relevel(data$Q24_5, "Yes")
dataf$Q24_6 <- relevel(data$Q24_6, "Yes")
dataf$Q24_7 <- relevel(data$Q24_7, "Yes")

levels(dataf$Q25_11)
dataf$Q25_1 <- factor(data$Q25_1, levels = always.lab)
dataf$Q25_2 <- factor(data$Q25_2, levels = always.lab)
dataf$Q25_3 <- factor(data$Q25_3, levels = always.lab)
dataf$Q25_4 <- factor(data$Q25_4, levels = always.lab)
dataf$Q25_5 <- factor(data$Q25_5, levels = always.lab)
dataf$Q25_6 <- factor(data$Q25_6, levels = always.lab)
dataf$Q25_7 <- factor(data$Q25_7, levels = always.lab)
dataf$Q25_8 <- factor(data$Q25_8, levels = always.lab)
dataf$Q25_9 <- factor(data$Q25_9, levels = always.lab)
dataf$Q25_10 <- factor(data$Q25_10, levels = always.lab)
dataf$Q25_11 <- factor(data$Q25_11, levels = always.lab)

dataf$Q26 <- relevel(data$Q26, "Yes")

levels(dataf$Q27_4)
dataf$Q27_1 <- factor(data$Q27_1, levels = always.lab)
dataf$Q27_2 <- factor(data$Q27_2, levels = always.lab)
dataf$Q27_3 <- factor(data$Q27_3, levels = always.lab)
dataf$Q27_4 <- factor(data$Q27_4, levels = always.lab)

dataf$Q28 <- relevel(data$Q28, "Yes")

levels(dataf$Q29_6)
dataf$Q29_1 <- factor(data$Q29_1, levels = always.lab)
dataf$Q29_2 <- factor(data$Q29_2, levels = always.lab)
dataf$Q29_3 <- factor(data$Q29_3, levels = always.lab)
dataf$Q29_4 <- factor(data$Q29_4, levels = always.lab)
dataf$Q29_5 <- factor(data$Q29_5, levels = always.lab)
dataf$Q29_6 <- factor(data$Q29_6, levels = always.lab)

levels(dataf$Q30_6)
dataf$Q30_1 <- factor(data$Q30_1, levels = always.lab)
dataf$Q30_2 <- factor(data$Q30_2, levels = always.lab)
dataf$Q30_3 <- factor(data$Q30_3, levels = always.lab)
dataf$Q30_4 <- factor(data$Q30_4, levels = always.lab)
dataf$Q30_5 <- factor(data$Q30_5, levels = always.lab)
dataf$Q30_6 <- factor(data$Q30_6, levels = always.lab)





##########' 3. Household section ##########

#View(dataf[188:288])

levels(dataf$Q39)
dataf$Q39 <- factor(data$Q39, levels = c("Never", "Less than once per year", "Once per year", "More than once per year", "Don_t know"))

levels(dataf$Q40)
dataf$Q40 <- factor(data$Q40, levels = c("Never", "1 to 2 times", "3 to 5 times", "6 to 10 times", "More than 10", "Don_t know"))

levels(dataf$Q41)
dataf$Q41 <- factor(data$Q41, levels = c("Never", "Once", "2 times", "3 times", "More than 3", "Don_t know"))

levels(dataf$Q42)
dataf$Q42 <- factor(data$Q42, levels = c("Yes, always", "Yes, sometimes", "No, never", "Don_t know"))

# Q43 - Q46

levels(dataf$Q47)
dataf$Q47 <- factor(data$Q47, levels = c("Never", "Less than once per year", "Once per year", "More than once per year", "Don_t know"))

levels(dataf$Q48)
dataf$Q48 <- factor(data$Q48, levels = c("Never", "1 to 2 times", "3 to 5 times", "6 to 10 times", "More than 10", "Don_t know"))

levels(dataf$Q49)
dataf$Q49 <- factor(data$Q49, levels = c("Never", "Once", "2 times", "3 times", "More than 3", "Don_t know"))

levels(dataf$Q50)
dataf$Q50 <- factor(data$Q50, levels = c("Yes, always", "Yes, sometimes", "No, never", "Don_t know"))





## dat = original data
## data = copy of original data
## dataf = refactored and releveled data

# save as .RData
save(data, dataf, old_names, new_names, file = "data/data_prep.RData")
