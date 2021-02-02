# College-Football-Bowl-Predictions-Model

This project scraps 2007-2020 college football data from sports-reference.com and creates a logistic regression and random forest model to predict bowl game outcomes. 

The data sources used (2020 link provided) : 
Offense (https://www.sports-reference.com/cfb/years/2020-team-offense.html)
Defense (https://www.sports-reference.com/cfb/years/2020-team-defense.html)
Special Team (https://www.sports-reference.com/cfb/years/2020-special-teams.html)
Conference Summary (https://www.sports-reference.com/cfb/years/2020.html) 
Ratings (https://www.sports-reference.com/cfb/years/2020-ratings.html)
Standings (https://www.sports-reference.com/cfb/years/2020-standings.html)
Scores (https://www.sports-reference.com/cfb/years/2020-schedule.html)

Steps for the project: 
1. Scrap the data using a years for loop (the year is only thing that changes in the url).
2. Set up team 1 as the team with the higher SRS (Simple Rating System) value. A team with a higher SRS value is generally considered the better team. More specifically,
the team with the higher SRS value win 70%+ of the time. If team 1 is not reassigned, this column will be the winner in the score data set. This is not predictive to future match ups. 
3. Clean, revalue, and rename variables/data
4. Join tables to create a master file 
5. Difference variables (in most cases team1-team2)
6. Create training and test datasets 
- (2007-2017, 2018)
- (2007-2008, 2019)
- (2007-2019, 2020) 
7. Exploratory analysis
8. Create logistic regression and random forest models to predict bowl game outcomes (predicts win probability of team 1) 

The final model uses strength of schedule, strength of record, winning percentage, conference winning percentage, pre-season and high of the AP poll, and the
average rushing yards allowed by team 1 compared to average rushing yards gained by team 2. 

The teams that win bowls games tend to have: 
-Higher SRS (Simple Rating System) 
-Higher SOS (Strength of Schedule) 
-Higher winning percentage
-Ranked higher in the Pre-Season AP poll 
-Allow on average less rushing yards than the opponent’s offense averages

There is one variable that is included in the model that has a non-intuitive interpretation.
The variable is conference winning percentage and in this bowl game model, teams that tend to win bowl games have a lower conference winning percentage. 
I do not necessary think is the case between two Power 5 opponents, but a Power 5 vs. non-Power 5 matchup. A Power 5 team’s conference schedule is typically
harder than a non-Power 5's so they could be a better team with a lower conference winning percentage. 

In general, the random forest model is slightly more accurate (89% in 2018, 80% in 2019, and 84% in 2020) compared to the logistic regression model (84% in 2018, 85% in 2019, 
and 80% in 2020). 

Note: For 2021, I will update the code to include the new bowl games and season summary statistics so a new user can create their own models. 
