
data <- read.table(file = "StudentsPerformance.csv", sep = ',', header = TRUE)

maleMathScores <- data[data$gender == 'male',]$"math.score"
femaleMathScores <- data[data$gender == 'female',]$"math.score"
plot(density(maleMathScores), ylim = c(0,0.04), xlim = c(0,120), col = 'blue', main = paste('Male and Female Math Scores'))
lines(density(femaleMathScores), col = 'red')

malereadingScores <- data[data$gender == 'male',]$"reading.score"
femalereadingScores <- data[data$gender == 'female',]$"reading.score"
plot(density(malereadingScores), ylim = c(0,0.04), xlim = c(0,120), col = 'blue', main = paste('Male and Female reading Scores'))
lines(density(femalereadingScores), col = 'red')

malewritingScores <- data[data$gender == 'male',]$"writing.score"
femalewritingScores <- data[data$gender == 'female',]$"writing.score"
plot(density(malewritingScores), ylim = c(0,0.04), xlim = c(0,120), col = 'blue', main = paste('Male and Female writing Scores'))
lines(density(femalewritingScores), col = 'red')

maleaverageScores <- data[data$gender == 'male',]$"average.score"
femaleaverageScores <- data[data$gender == 'female',]$"average.score"
plot(density(maleaverageScores), ylim = c(0,0.04), xlim = c(0,120), col = 'blue', main = paste('Male and Female average Scores'))
lines(density(femaleaverageScores), col = 'red')

groupAAverageScore <- data[data$race.ethnicity == "group.A",]$average.score
groupBAverageScore <- data[data$race.ethnicity == "group.B",]$average.score
groupCAverageScore <- data[data$race.ethnicity == "group.C",]$average.score
groupDAverageScore <- data[data$race.ethnicity == "group.D",]$average.score
groupEAverageScore <- data[data$race.ethnicity == "group.E",]$average.score

plot(density(groupAAverageScore), ylim = c(0,0.04), xlim = c(0,120), col = 'red', main = paste('Average Scores Among Races'))
lines(density(groupBAverageScore), col = 'blue')
lines(density(groupCAverageScore), col = 'black')
lines(density(groupDAverageScore), col = 'green')
lines(density(groupEAverageScore), col = 'purple')

groupTestPrepAverageScore <- data[data$test.preparation.course == "completed",]$average.score
plot(density(groupTestPrepAverageScore), ylim = c(0,0.04), xlim = c(0,120), col = 'red', main = paste('Average Scores By Test Prep'))
groupNoTestPrepAverageScore <- data[data$test.preparation.course == "none",]$average.score
lines(density(groupNoTestPrepAverageScore), col = 'blue')

groupStandardLunchAverageScore <- data[data$lunch == "standard",]$average.score
plot(density(groupStandardLunchAverageScore), ylim = c(0,0.04), xlim = c(0,120), col = 'red', main = paste('Average Scores By Lunch'))
groupFreeLunchAverageScore <- data[data$lunch == 'free/reduced',]$average.score
lines(density(groupFreeLunchAverageScore), col = 'blue')

print(t.test(maleMathScores,femaleMathScores))
print(t.test(malereadingScores,femalereadingScores))
print(t.test(malewritingScores,femalewritingScores))

print(t.test(groupTestPrepAverageScore,groupNoTestPrepAverageScore))

groupParentBachelorAverageScore <- data[data$parental.level.of.education == "bachelors.degree",]$average.score
groupParentMasterAverageScore <- data[data$parental.level.of.education == "masters.degree",]$average.score
groupParentAssociateAverageScore <- data[data$parental.level.of.education == "associates.degree",]$average.score
groupParentSomeCollegeAverageScore <- data[data$parental.level.of.education == "some.college",]$average.score
groupParentHighSchoolAverageScore <- data[data$parental.level.of.education == "high.school",]$average.score
groupParentSomeHighSchoolAverageScore <- data[data$parental.level.of.education == "some.high.school",]$average.score

#groupParentLessThanBachelorAverageScore <- data[data$parental.level.of.education == "associates.degree" || data$parental.level.of.education == "some.college" || data$parental.level.of.education == "high.school"|| data$parental.level.of.education == "some.high.school",]$average.score
groupParentLessThanBachelorAverageScore <- data[data$parental.level.of.education %in% c("associates.degree","some.college","high.school","some.high.school" ),]$average.score


plot(density(groupParentMasterAverageScore), ylim = c(0,0.04), xlim = c(0,120), col = 'purple', main = paste('Average Scores Parent Education'))
lines(density(groupParentBachelorAverageScore), col = 'blue')
lines(density(groupParentAssociateAverageScore), col = 'green')
lines(density(groupParentSomeCollegeAverageScore), col = 'yellow')
lines(density(groupParentHighSchoolAverageScore), col = 'orange')
lines(density(groupParentSomeHighSchoolAverageScore), col = 'red')

plot(density(groupParentMasterAverageScore), ylim = c(0,0.04), xlim = c(0,120), col = 'purple', main = paste('Average Scores Parent Education'))
lines(density(groupParentBachelorAverageScore), col = 'blue')
lines(density(groupParentLessThanBachelorAverageScore), col = 'red')


print(t.test(groupStandardLunchAverageScore,groupFreeLunchAverageScore))
print(t.test(groupTestPrepAverageScore,groupNoTestPrepAverageScore))

