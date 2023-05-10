# ca208
Assignment for CA208 Logic module for DCU Computer Science

The teams is a sports competition are divided into six groups with five teams in each group. Each team plays the other teams in its group once only. Write a Prolog predicate schedule(S) that is true if S is a valid schedule. A schedule lists the fixtures for each day of the competition. The only facts your Prolog program should have are which teams are in which group. For a fixture (A,B, day), team A is the home team and team B is the away team. A schedule is valid if:

there are no more than three matches on any day,
each team has the same number of home and away fixtures in the whole schedule.

The schedule S should be as short as possible, and the predicate schedule should be able to generate multiple valid schedules.
