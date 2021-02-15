## HEADER ####
## TidyTuesday Week 7
## Florence Galliers
## 2021-02-10

install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2021-02-09')

student <- tuesdata$student_debt

summary(student)
# year, race, loan_debt and loan_debt_pct
# year between 1989 and 2016
# race is character variable
# loan debt between 793 and 14224
# loan debt pct is the percentage of families with student loan debt
