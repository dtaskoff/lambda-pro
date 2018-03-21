:- module(test_terms, [term/4]).


term(i, normal, atom, 'x. x').
term(i, de_bruijn, atom, 'λ 0').
term(i, _, term, lambda(x, x-0)).

term(k, normal, atom, 'x. y. x').
term(k, de_bruijn, atom, 'λ λ 1').
term(k, _, term, lambda(x, lambda(y, x-1))).

term(s, normal, atom, 'x. y. z. (x z) (y z)').
term(s, de_bruijn, atom, 'λ λ λ (2 0) (1 0)').
term(s, _, term, lambda(x, lambda(y, lambda(z,
  application(parentheses(application(x-2, z-0)),
              parentheses(application(y-1, z-0))))))).

term(42, normal, atom, 'x. y').
term(42, de_bruijn, atom, 'λ 43').
term(42, _, term, lambda(x, y-43)).

term(c0, normal, atom, 'x. y. y').
term(c0, de_bruijn, atom, 'λ λ 0').
term(c0, _, term, lambda(x, lambda(y, y-0))).

term(c1, normal, atom, 'x. y. x y').
term(c1, de_bruijn, atom, 'λ λ 1 0').
term(c1, normal, term, lambda(x, lambda(y, application(x-1, y-0)))).
