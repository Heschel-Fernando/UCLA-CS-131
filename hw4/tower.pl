% tower/3
% N: a nonnegative integer specifying the size of the square grid.
% T: a list of N lists, each representing a row of the square grid. 
%    Each row is represented by a list of N distinct integers from 1 through N. 
%    The corresponding columns also contain all the integers from 1 through N.
% C: a structure with function symbol counts and arity 4. 
%    Its arguments are all lists of N integers, and represent 
%    the tower counts for the top, bottom, left, and right edges, 
%    respectively.
tower(N, T, C) :- 
	% C is correctly formated
	C = counts(Top, Bottom, Left, Right),
	% count size limits
	len_row(Top, N),
	len_row(Bottom, N),
	len_row(Left, N),
	len_row(Right, N),
	% array size limits
	len_row(T, N),
	len_col(T, N),
	% domain limits
	within_domain(T, N),
	% every element is different for every row
	maplist(fd_all_different, T),
	% every element is different for every column
	transpose(T, TT),
	maplist(fd_all_different, TT),
	% range to values
	maplist(fd_labeling, T),
	% top counts are correct
	visible(TT, Top),
	% bottom counts are correct
	reverse_2d(TT, RTT),
	visible(RTT, Bottom),
	% left counts are correct
	visible(T, Left),
	% right counts are correct
	reverse_2d(T, RT),
	visible(RT, Right).

len_row(X, N) :-
    length(X, N).

len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

within_domain([], _).
within_domain([HD | TL], N) :-
    % http://www.gprolog.org/manual/html_node/gprolog057.html fd_domain(Vars, Lower, Upper)
    fd_domain(HD, 1, N),
    within_domain(TL, N).

% This is SWI-prolog's old implementation
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
	transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
	lists_firsts_rests(Ms, Ts, Ms1),
	transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
	lists_firsts_rests(Rest, Fs, Oss).

reverse_2d(X, RX) :-
	maplist(reverse, X, RX).

% non_decreasing/2
% Modification of non_increasing from hint code
% This can count number of visible towers
non_decreasing([HD|TL], ND) :- non_decreasing(TL, HD, [HD], ND).
non_decreasing([], _, Sublist, Sublist).
non_decreasing([HD|TL], MinVal, TmpSublist, ND) :-
	MinVal =< HD,
	append(TmpSublist, [HD], NextSublist),
	non_decreasing(TL, HD, NextSublist, ND).
non_decreasing([HD|TL], MinVal, TmpSublist, ND) :-
	HD < MinVal,
	non_decreasing(TL, MinVal, TmpSublist, ND).

% visible/2
% Count the number of towers visible from left
% M: matrix to check the counts
% L: counts
visible([], []).
visible([MHD|MTL], [LHD|LTL]) :-
	non_decreasing(MHD, ND),
	len_row(ND, LHD),
	visible(MTL, LTL).

% plain_tower/3
% N: a nonnegative integer specifying the size of the square grid.
% T: a list of N lists, each representing a row of the square grid. 
%    Each row is represented by a list of N distinct integers from 1 through N. 
%    The corresponding columns also contain all the integers from 1 through N.
% C: a structure with function symbol counts and arity 4. 
%    Its arguments are all lists of N integers, and represent 
%    the tower counts for the top, bottom, left, and right edges, 
%    respectively.
plain_tower(N, T, C) :- 
	% C is correctly formated
	C = counts(Top, Bottom, Left, Right),
	% count size limits
	len_row(Top, N),
	len_row(Bottom, N),
	len_row(Left, N),
	len_row(Right, N),
	% array size limits
	len_row(T, N),
	len_col(T, N),
	% grid is sudoku and satisfies Left and Right count constraints
	tower_2d(N, T, Left, Right, []),
	% Top and Bottom counts are valid
	transpose(T, TT),
	visible(TT, Top),
	reverse_2d(TT, RTT),
	visible(RTT, Bottom).

	
% unique_list/2
% L is unique list of size N
% N: size of list
% L: unique list
unique_list(N, L) :-
	length(L, N),
	maplist(between(1, N), L),
	unique_row(L).

% unique_row/1
% every element in the row is unique
% L: list
unique_row([]).
unique_row([H|T]) :- 
	\+ member(H, T),
	unique_row(T).

% tower_1d/3
% L: 1d tower
% Count: number of towers visible
tower_1d(L, Count) :-
	non_decreasing(L, ND),
	len_row(ND, Count).

% tower_2d
% 1. grid is within range
% 2. grid is unique row-wise and column-wise (i.e. sudoku)
% 3. Left and Right counts are correct
% 
% N: size of grid
% T: grid
% Counts: visible numbers of towers
% RCounts: visible numbers of towers from opposite side
% TmpT: temporary result of grid
tower_2d(_, _, [], [], TmpT) :- unique_cols(TmpT).
tower_2d(N, [THD|TTL], [CountHD|CountTL], [RCountHD|RCountTL], TmpT) :-
	% tmp grid is sudoku
	unique_cols(TmpT),
	unique_list(N, THD),
	% tmp grid has valid counts 
	tower_1d(THD, CountHD),
	reverse(THD, RTHD),
	tower_1d(RTHD, RCountHD),
	append(TmpT, [THD], NewTmpT),
	tower_2d(N, TTL, CountTL, RCountTL, NewTmpT).

% unique_rows/2
% matrix is unique row-wise
% M: matrix
unique_rows([]).
unique_rows([HD|TL]) :-
	unique_row(HD),
	unique_rows(TL).

unique_cols(M) :- 
	transpose(M, TM),
	unique_rows(TM).

% tower_performance_test/1
% https://stackoverflow.com/questions/34970061/display-the-execution-times-for-each-goal-of-a-predicate-clause
% http://www.gprolog.org/manual/html_node/gprolog048.html
% Takes about 276 ms
% CPUTime: time taken to complete test case
tower_performance_test(CPUTime) :-
	statistics(cpu_time, [Start|_]),
	tower(
		5, 
		_, 
		counts(
			[3, 2, 2, 2, 1],
			[1, 3, 2, 2, 5],
			[3, 3, 2, 3, 1],
			[1, 2, 2, 3, 3]
		)
	),
	!,
	statistics(cpu_time, [Stop|_]),
	CPUTime is Stop - Start.

% plain_tower_performance_test/1
% Takes about 424 ms
% CPUTime: time taken to complete test case
plain_tower_performance_test(CPUTime) :-
	statistics(cpu_time, [Start|_]),
	plain_tower(
		5, 
		_, 
		counts(
			[3, 2, 2, 2, 1],
			[1, 3, 2, 2, 5],
			[3, 3, 2, 3, 1],
			[1, 2, 2, 3, 3]
		)
	),
	!,
	statistics(cpu_time, [Stop|_]),
	CPUTime is Stop - Start.

% speedup/1
% Takes about 762 ms
% Ratio: plain_tower_performance_test time / tower_performance_test time
speedup(Ratio) :-
	plain_tower_performance_test(PlainTowerTime),
	tower_performance_test(TowerTime),
	!,
	Ratio is PlainTowerTime / TowerTime.

% ambiguous/4
% N: size of grid
% C: counts
% T1: grid that satisfies C
% T2: another grid that satisfies C
ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.

