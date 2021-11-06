# Statistical-Honours-Research-Project

## Project Title
Spatio-Temporal Methods of Analysing Threat in Football Matches

## Abstract
In football, metrics such as: goals, assists and shots usually dictate how
threatening a team is. Furthermore, a team’s attacking attributes are usually
rewarded to players who score, and/or assist. One way of representing how
threatening a team is, is by making use of the expected goal (xG) metric,
which measures the quality of shots based on several features. xG ranges
from 0 to 1, where higher xG values translate to higher chances of a shot
becoming a goal. Most features in xG models include: distance & angle of the
shot taker to goal, the body part used to take the shot, and the technique used.
Thanks to more in-depth data released in recent years, additional features can
be generated from any given shot. Using StatsBomb’s publicly available data,
the aim of this paper is to build xG models that incorporates features that have
been explored in existing literature. Furthermore, incorporate spatio-temporal
features that have not been investigated, specifically engineer a feature this
paper describes as Attacking Dominance that quantifies the space controlled
by the attacking team at the moment a shot is taken, by making use of Voronoi
diagrams. Using three different machine learning algorithms, this paper will
compare the models to that of StatsBomb’s xG model. The results show that
spatio-temporal features improve the xG metric, and further research should
be done in capturing other spatial aspects of football in order to present a
more holistic model.
