# Hog Game Strategy
***
## Implementation of Hog, a variation of the dice game Pig.
##### Topics: Stochastic Dynamic Programming, Markov chains, Artificial Intelligence, Game Theory




Pig is a seemingly innocuous game - it is very simple to play and requires a single dice between two players. However, it has become a pedagogical example to help people understand advanced topics such as probability, machine learning/AI, probability, programming, and Markov decision processes.

The rules are very simple:

>The objective of the jeopardy dice game Pig is to be the first player to reach 100
points. Each player’s turn consists of repeatedly rolling a die. After each roll,
the player is faced with two choices: roll again, or hold (decline to roll again).

>• If the player rolls a 1, the player scores nothing and it becomes the opponent’s
turn.

>• If the player rolls a number other than 1, the number is added to the player’s
turn total and the player’s turn continues.

>• If the player holds, the turn total, the sum of the rolls during the turn, is
added to the player’s score, and it becomes the opponent’s turn.

There is actually very thorough [analysis](https://pdfs.semanticscholar.org/50b2/d628c3a03cfe2594a052a99da627f875ee48.pdf) on the internet about the game, and that specific paper outlines that an edge can be gained with optimal play. The paper also outlines how a *heuristic* of holding at 20 is not the optimal strategy at every case, which makes sense: a player should be more aggressive in scoring points if he or she is many points behind the opponent (but try to do so without maximizing the probability of *'pigging-out'*, which is rolling a 1 after not deciding to hold) "since risking points is not the same  as risking the probability of winning".

Another question that needs to be raised after this as well is when would one know to invoke a strategy that goes against a heuristic? This concept is difficult to realize in nature, but can be modeled effectively within this game. To explain succinctly, the optimal strategy of play depends on evaluating whether to hold or roll again. The probability of winning given two player's scores are also dependent on the difference of each one's score as well as how far each one is from winning. Probabilities are more realized near the end of the game (for example, if a player has the turn and has a score of 96 chance of winning is 50% as the expected value of the dice for a valid score is 4. The probability of the player winning with a current score of 92 depends on the probability they can arrive to a higher score - therefore outcomes are dependent of the probabilities of winning with higher turn totals.

Several variations of the game Pig exists, but this code focuses on a combination of two flavors:

- Progressive Pig - in that *pigging-out* would score a person a single point instead of zero.
- [Hog](http://personal.vu.nl/h.c.tijms/morfismospaper.pdf) - any person can roll *n* amount of dice, and the person can retain the sum of all values from every dice as long as no dice shows up with a 1. Otherwise, it becomes the opponent's turn.


The first component allows the game to have a finite set of rounds (if a *pig-out* does not add to a person's score, then theoretically the game could cycle infinitely). The benefit of this condition also makes it such that a dynamic programming solution can be created as there will be no cyclic dependencies and allow the game to be computed in stages.
Hog adds complexity to this game in that a person can actually throw more dice to increase their score - changing the heuristic. Now in order to play optimally, one must actually estimate the probability of rolling *n* dice with each roll not containing a value of 1.

![](https://latex.codecogs.com/gif.latex?P(p-2,n,s)&space;=&space;(1/6)^{n}\sum_{k=0}^{\lfloor&space;(p-n)/s&space;\rfloor)}(-1)^{k}{n&space;\choose&space;k}{p-sk-1&space;\choose&space;n-1})

Where *p* is the value, *n* is the number of dice, and *s* is the number of sides to a single dice. To exclude all possibilities such that any dice comes up to one, I subtract 2 from the value and calculate the probability as such.

I establish the limit of dice to be 50, being that the minimum sum of all dice after being rolled without any dice showing up as a 1 would be 100 which automatically constitutes a win. However, the likelihood of such a scenario is: ![](https://latex.codecogs.com/gif.latex?5/6^{50}&space;=&space;0.000109884) , which might be a naive move. If the expected value of throwing a dice is 4 without *pigging-out*, then *maybe* it might be better to throw 25. Only after maximizing the probability of winning at all states reveal the optimal decisions.


After evaluating the optimal decision strategy, two notable highlights came up:

- if two opponents are playing optimally, the person who goes first has a 53.39% chance of winning.
- turns out that the most one should ever throw given these rules is 16 dice. The maximal probability of winning the game anyways whenever that is the optimal decision is 3.19%, however. 


The biggest takeaways for me regarding this evaluation are that there still can be an optimal strategy even when there is randomness; and that the best decision to make changes depending on context, sometimes drastically against the heuristic.



