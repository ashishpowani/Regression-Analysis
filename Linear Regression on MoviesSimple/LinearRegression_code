#Get the forecast library
library (forecast)
Movies <-read.csv(file.choose())

#Display the first 6 rows from the document chosen
head(Movies)
View(Movies)

FamousMovies <- subset(Movies, votes>500)
View(FamousMovies)

FamousRecentMovies<-subset(FamousMovies, year>1980)
FamousRecentBudgetMovies<-subset(FamousRecentMovies, budget>0)
FRBM <-FamousRecentBudgetMovies

unique(FRBM$year)

#Aggregate by calculating mean for all the budget values for each year
#Formula used is to model budget as a function of year
MoviesOfInterest<-aggregate(budget~year, data=FRBM, FUN=mean)
View(MoviesOfInterest)

#Fitting Linear model on budget as a function of year from the data "MoviesofInterest"
BudgetRegression= lm(budget~year, data=MoviesOfInterest)
summary(BudgetRegression)
summary(BudgetRegression$coefficients)

plot(MoviesOfInterest$year, MoviesOfInterest$budget)

abline(lm(MoviesOfInterest$budget~MoviesOfInterest$year))
#abline(lm(budget~year,data=MoviesofInterest))

lm(formula = budget ~ year, data = MoviesOfInterest)

MovieModel = lm(formula = budget ~ year, data = MoviesOfInterest)
summary(MovieModel)

