:- begin_tests(utils).
:- use_module(utils, [atom_list_concat/2]).

test(atom_list_concat) :- atom_list_concat([], '').
test(atom_list_concat) :- atom_list_concat([x, +, y], 'x+y').
test(atom_list_concat) :-
  atom_list_concat([xy, z, '', z, yx], xyzzyx).
test(atom_list_concat) :-
  atom_list_concat(["xyz", -, "zyx"], 'xyz-zyx').

:- end_tests(utils).
