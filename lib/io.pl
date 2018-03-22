:- module(io, [set_prompt/0, read_input/1, print_output/1, read_file/2]).


% Set the user's prompt
set_prompt :- prompt(_, 'λ-προ: ').

% Read a line from the user's input
read_input(In) :- read_string(user_input, String, "\n"),
  atom_string(In, String).

read_string(Stream, String, Sep) :- read_string(Stream, Sep, "\t", _, String).

% Otherwise it'd be rewl
print_output(Out) :- writeln(Out).

% Read the contents of a file
read_file(F, Out) :- open(F, read, Stream),
  read_string(Stream, Content, ''),
  close(Stream),
  split_string(Content, '\n', '', Lines),
  convlist([L, A]>>(L \= "", atom_string(A, L)), Lines, Out).
