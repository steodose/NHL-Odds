# NHL-Odds
The NHL Odds App shows the latest playoff and Stanley Cup chances for all NHL teams. 

How this works
============

**Based on Neil Paine's Elo Ratings model at FiveThirtyEight.com**

Playoff odds are derived by using Elo ratings to simulate the remainder of the NHL schedule 1,000 times. For regular-season games, Elo differential is used to generate the odds of each team gaining 2, 1 or 0 points in a matchup (i.e., a win or regulation loss, or an overtime/shootout loss) via logistic regression. Simulations are run using the 2021 NHL season structure, with the assumption that teams with fewer than 56 games will be ranked and seeded according to Points Percentage. In the Stanley Cup Semifinals, the winners of each division bracket are re-seeded by regular-season points percentage with wins (per game) as the tie-breaker.

***

What is Elo?
------------

The Elo rating system is a method for calculating the relative skill levels of players (or teams in this case) in zero-sum games like hockey, according to Wikipedia. It is named after its creator Arpad Elo, a Hungarian-American physics professor, who developed it to rate the relative strength of chess players.

Historical Elo
------------

Elo ratings measure a team's strength over time, accounting for the strength of opponents, locations of games and margin of victory. In hockey an Elo rating of about 1500 is considered average. Ratings update after each game in proportion to both how unlikely the result is and the margin of victory.

Data
------------

The data comes from Neil Paine's [Github repository](https://github.com/NeilPaine538/NHL-Player-And-Team-Ratings)
, and is refreshed once per day.

Learn More
------------

You can read more about the app at my blog [Between the Pipes](https://betweenpipes.wordpress.com/)
