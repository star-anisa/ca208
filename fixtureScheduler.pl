% Anti-Plagarism Statement.
% I declare all the material that I am presenting for submission 
% are entirely my own work, save for the work that has been 
% cited and acknowlodeged as not my own.

%facts
%group(group_number, [team1, team2, team3, team4, team5])

group(1, [a1, a2, a3, a4, a5]).
group(2, [b1, b2, b3, b4, b5]).
group(3, [c1, c2, c3, c4, c5]).
group(4, [d1, d2, d3, d4, d5]).
group(5, [e1, e2, e3, e4, e5]).
group(6, [f1, f2, f3, f4, f5]).


% Predicates

schedule(PairsWithDays) :-
    all_pairs(AllPairs),  % get the list of pairs
	% built-in predicate that randomly permutates the elements of AllPairs and stores it in Pairs
    random_permutation(AllPairs, Pairs),
	three_matches_a_day(Pairs, 1, [], PairsWithDays).

/**********************/

% if the list of Pairs is empty and the last two inputs are equal then
% the function is finished
three_matches_a_day([], _, PairsWithDays, PairsWithDays).

three_matches_a_day([Pair|Pairs], CurrentDay, PairsWithDaysSoFar, PairsWithDays) :-
	% Count how many pairs already have CurrentDay assigned
	count_pairs_with_day(PairsWithDaysSoFar, CurrentDay, Count),
	Count < 3, % If less than 3, assign to current day
    assign_day([Pair], CurrentDay, PairWithDay),
    append(PairsWithDaysSoFar, PairWithDay, NewPairsWithDaysSoFar),
	three_matches_a_day(Pairs, CurrentDay, NewPairsWithDaysSoFar, PairsWithDays).

three_matches_a_day([Pair | Pairs], CurrentDay, PairsWithDaysSoFar, PairsWithDays) :-
	% If 3 pairs already have CurrentDay assigned, try the next day
	count_pairs_with_day(PairsWithDaysSoFar, CurrentDay, Count),
	Count >= 3,
	NextDay is CurrentDay + 1,
	three_matches_a_day([Pair | Pairs], NextDay, PairsWithDaysSoFar, PairsWithDays).
	
/**********************/

% A helper function to count how many pairs in a list have a given day assigned
count_pairs_with_day([], _, 0).

count_pairs_with_day([(_, _, Day) | T], Day, Count) :-
    count_pairs_with_day(T, Day, TempCount),
    Count is TempCount + 1.

count_pairs_with_day([(_, _, D) | T], Day, Count) :-
    Day \= D, % dont count pairs with a different day assigned
    count_pairs_with_day(T, Day, Count).

/**********************/

assign_day([], _, []).
% adds the appropriate day to the end of the tuple
assign_day([(A, B)], Day, [(A, B, Day)]).

/**********************/

all_pairs(ValidPairs) :-
    all_groups(ValidGroups), % get the list of groups

    % generates a list of tuples, containing pairs in a match in the same group, with dupliactes removed
	% ie no (A,B) present when there is (B,A).
    findall((A, B), (member(Group, ValidGroups), % iterates over each group in ValidGroups
        member(A, Group), % iterates over each team in Group
        select(A, Group, R), % selects team A from Group and creates a new
        member(B, R), A @< B % https://www.cse.unsw.edu.au/~billw/dictionaries/prolog/comparison.html
    ), AllPairs), % final pairs stored in AllPairs
	filter_two_home_two_away(AllPairs, [], ValidPairs).

/****************/

filter_two_home_two_away([], ValidPairs, ValidPairs).

filter_two_home_two_away([(A, B) | Pairs], ValidPairsSoFar, ValidPairs) :-
	check_home_games(ValidPairsSoFar, A, CountHome),
	% for the number of teams in each division, for home and away to be equal
	% each team needs 2 home and 2 away teams
	CountHome < 2, % if a team has had less than 2 home games append the team with A as the home team
	append(ValidPairsSoFar, [(A, B)], NewValidPairsSoFar),
	filter_two_home_two_away(Pairs, NewValidPairsSoFar, ValidPairs).

filter_two_home_two_away([(A, B) | Pairs], ValidPairsSoFar, ValidPairs) :-
	check_home_games(ValidPairsSoFar, _, CountHome),
	CountHome >= 2, % if a team has had 2 or more home games, append the team with A as the away team
	append(ValidPairsSoFar, [(B, A)], NewValidPairsSoFar),
	filter_two_home_two_away(Pairs, NewValidPairsSoFar, ValidPairs).

/*******************/

check_home_games([], _, 0).

check_home_games([(A, _) | T], A, CountHome) :-
	check_home_games(T, A, TempCount),
	% When team A occurs in the first position of the tuple, increment CountHome
	CountHome is TempCount + 1.

check_home_games([(W, _) | T], A, CountHome) :-
	A \== W, % If A is not equal to W, do not increment count, skip this tuple and move on with recursive call
	check_home_games(T, A, CountHome).

/**********************/

% create a list of lists of all the groups within the same division
all_groups(ValidGroups) :-
    findall(Group, group(_, Group), ValidGroups).

/**********************/