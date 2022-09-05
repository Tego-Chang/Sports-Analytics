# Evalauting-NBA-players-value-in-the-free-agency
(https://github.com/Tego-Chang/Evalauting-NBA-players-value-in-the-free-agency/files/7657428/proposal.pdf)

## Motivation

For general managers in NBA teams, one of the major tasks is to decide what contracts their teams shall provide for the players they are interested in based on their values from and off the court. As the budget for each team is regulated by the league, called the salary cap, pursuing the players they want with a reasonable or even economical salary has been more critical than ever. The purpose of this project is to build a statistical model to predict an NBA player’s deserved salary and highlight the players that could be overpaid or lowballed. The general managers and the front office of each team are considered as the target audience for this project. 

## Response, Predictors, and Methodology

In this project, we define the response variable, the result we are interested in, as the average annual salary for each evaluated player. As the statistical model we build becomes more mature, we could further extend the coverage of response to include the contract length, number of years, which we know that could also be important information for general managers' decision-making in the real free-agent market.

Predictors, or say the factors, considered for the model include:

- Year: the year of the player's statistics. We plan to collect as many years of data as possible and based on the player's past performance to predict the value of the player in the upcoming year. 
- Age: the age of the evaluated player
- Health: the percentage of games the player has been able to play.
- Traditional statistics: points, rebound, and assist per game (PPG, RPG, and APG), steals, turnovers, etc.
- Advanced statistics: plus-minus (PM), BPM, offensive rating, defensive rating, net rating, PER, win-share (WS)
- Whether his team gets into the playoffs
- How far did the team stop in the playoffs
- Whether the player is in a winning team

Due to the nature of the response variable, we will first build a multiple linear regression model (MLR) for this topic. As there are 30 teams in the NBA, we then plan to apply "team" as a group predictor and transform this model into hierarchical MLR to predict the player's value based on each team's market size, the principle of constructing its lineup, and the corresponding financial policy. 

## Plan

There will be several stages for this project, and the ultimate goal is to provide a statistical model practical enough for the bussiness decision-making in professional sports. 

- Data collection and cleaning
- Iterations of modeling and result interpretation 
- Presentation (to managers in the industry if possible)
- Report 



