:- module(utils, [atom_list_concat/2]).


% Concatenate a list of atoms into a single atom
atom_list_concat(List, Atom) :- atom_list_concat(List, Atom, '').
atom_list_concat([], Atom, Atom).
atom_list_concat([H|T], Atom, Acc) :- atom_concat(Acc, H, Acci), atom_list_concat(T, Atom, Acci).
