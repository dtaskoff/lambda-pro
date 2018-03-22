:- module(utils, [atom_list_concat/2]).


% Concatenate a list of atoms into a single atom
atom_list_concat(List, Atom) :-
  foldl([X, Acc]>>call(atom_concat, Acc, X), List, '', Atom).
