
:- dynamic semantic_error/3.
:- dynamic context/6.

%%
%% Answers
%%

agree_msg('Yes.').
disagree_msg('No.').
sintax_error_msg('Sintax error.').
semantic_error_msg(Subj,Adj,Sug,R) :- string_concat('\'',Subj,R0),
                                  string_concat(R0,'\' is not',R1),
                                  string_concat(R1,' in the category ',R2),
                                  string_concat(R2,Adj,R3),
                                  string_concat(R3,'. Do you mean \'',R4),
                                  string_concat(R4,Sug,R5),
                                  string_concat(R5,'\'?',R),
                                  retractall(semantic_error(_,_,_)).

%%%%%

%%%
%% Affirmative Sentences

%%sentence(T,D,Ac,Subj,Obj) --> noun_phrase1(N,Subj), verb_phrase(N,T,D,Ac,Obj,Subj).
sentence(T,D,Ac,Subj,Obj) --> noun_phrase(N,Subj), verb_phrase(N,T,D,Ac,Obj,Subj).
sentence(T,D,Ac,Subj,Obj) --> noun_phrase2(N,Ac,_,Subj), verb_phrase(N,T,D,Verb,Obj,_).
noun_phrase(N,Subj) --> noun_phrase1(N,Subj).
noun_phrase(N,Subj) --> noun_phrase2(N,Subj).
noun_phrase1(N,Subj) --> determinant(N,_), noun(N,Subj).

noun_phrase2(p,[Subj1|Subj2]) --> noun(s,Subj1), conjunction(ag), noun_phrase2(p,Subj2).
noun_phrase2(p,Subj) --> noun(s,Subj).

noun_phrase2(N,Ac,Obj,Subj) -->  noun_phrase1(N,Adj), preposition(Ac), noun(N,Obj),
                            {(Ac = in),((Pred=..[be,Subj,Adj], Pred); (be(Subj,Sug), assert(semantic_error(Subj,Adj,Sug)), fail))}.
noun_phrase2(N,Ac,_,Subj) -->  noun_phrase1(N,Ac), preposition(Prep), noun(N,Subj).

noun_phrase2(N,Ac,_,Subj) -->  noun_phrase1(N,Ac), preposition(of), noun(N,Subj).

noun_phrase2(N,Ac,Obj,Subj) -->  preposition(Ac), noun(N,Obj).
noun_phrase2(N,Noun) --> noun(N,Noun).

verb_phrase(N,T,D,Ac,Obj,Subj) --> verb(N,T,be,_), noun_phrase2(N,Ac,Obj,Subj).
verb_phrase(N,T,D,Ac,Obj,Subj) --> verb(N,_,be,_), negation(T), noun_phrase2(N,Ac,Obj,Subj).

verb_phrase(N,T,D,Ac,Obj,Subj) --> verb(p,T,Ac,_), noun(p,Obj).
verb_phrase(N,T,D,Ac,Obj,Subj) --> verb(p,_,Ac,_), negation(T), noun(p,Obj).
verb_phrase(N,T,D,Ac,Obj,Subj) --> verb(N,T,Ac,_), determinant(N1,D), noun(N1,Obj).
verb_phrase(N,T,D,Ac,Obj,Subj) --> verb(N,_,Ac,_), negation(T), determinant(N1,D), noun(N1,Obj).

verb_phrase(N,T,D,Ac,Obj,Subj) --> verb(N,T,Verb,_), object(N,Obj).
verb_phrase(N,T,D,Ac,Obj,Subj) --> verb(N,_,Verb,_), negation(T), object(N,Obj).

%% Comparative

verb_phrase(N,T,Comp,Ac,Obj,Subj) --> verb(N,T,Verb,_), comparative(Comp), noun(_,Ac), conjunction(C), noun(N,Obj).
verb_phrase(N,T,Comp,Ac,Obj,Subj) --> verb(N,_,Verb,_), negation(T), comparative(Comp), noun(_,Ac), conjunction(C), noun(N,Obj).

%%%
%% Questions

verb_phrase(N,T,_,Ac,_,Subj) --> verb(N,T,Verb,_), determinant(_,the), noun2(a,Subj), noun(p,Ac).

noun2(a,[Subj1|Subj2]) --> noun(a,Subj1), conjunction(ag), noun2(a,Subj2).
noun2(a,Subj) --> noun(a,Subj).

question(T,Q,_,At,Ac,Obj,Subj) --> verb(N,T,Ac,_), noun_phrase2(N,Subj), noun_phrase1(N,Obj).
question(T,Q,_,At,Ac,Obj,Subj) --> verb(N,T,be,_), noun_phrase2(N,Subj), preposition(Ac), noun(N,Obj).
%%%question(T,Q,At,Ac,Obj,Subj) --> verb(N,T,Ac,_), noun_phrase2(N,Subj), determinant(N1,D), noun(N1,Obj).
question(T,Q,_,At,Ac,_,Subj) --> int_phrase(N,Q,At), verb_phrase(N,T,_,Ac,_,Subj).

question(T,Q,Comp,At,Ac,Obj,Subj) --> int_phrase(N,Q,Subj,At), verb_phrase(N,T,Comp,Ac,Obj,_).

int_phrase(N,Q,_) --> interrogative_pronoun(N,Q).
int_phrase(N,Q,At) --> interrogative_pronoun(N,Q), int_nom_phrase(N,At).
int_phrase(N,Q,Subj,At) --> interrogative_pronoun(N,Q), int_nom_phrase(a,Subj), noun(N,At).

int_nom_phrase(N,At) --> noun(N,At).
int_nom_phrase(N,At) --> noun2(a,At).

%int_nom_phrase(N,At) --> determinant(N,_), noun(N,At), pronoun(_,that).

%% Context

question(T,Q,Comp,At,Ac,Obj,Subj) --> conjunction(ag), noun(_,Subj), {context(T,Q,Comp,At,Ac,Obj)}.

%% Comparative

verb_phrase(N,T,Comp,Ac,Obj,Subj) --> verb(N,T,Verb,_), comparative(Comp), conjunction(C), object(_,Obj), noun(_,Ac).

%%%

check_sentence(WordList, R) :-
                 (retractall(semantic_error(_,_,_));true),
                 (( question(T,Q,Comp,At,Ac,Obj,Subj,WordList,[]),
                 answer(T,Q,Comp,At,Ac,Obj,Subj,L,N,R),!)
                 ;
                 ( sentence(t,D,Ac,Subj,Obj,WordList,[]),
                 ((check_objects(D,Ac,Subj,Obj), agree_msg(R),!) ; disagree_msg(R),!))
                 ;
                 ( sentence(f,D,Ac,Subj,Obj,WordList,[]),
                 ((check_objects(D,Ac,Subj,Obj), disagree_msg(R),!) ; agree_msg(R),!))
                 ;
                 ( semantic_error(Subj,Adj,Sug), semantic_error_msg(Subj,Adj,Sug,R),! )
                 ;
                 sintax_error_msg(R)),
                 (retractall(context(_,_,_,_,_,_));true),
                 (assert(context(T,Q,Comp,At,Ac,Obj));true),!.

compare_objects(Ac,Comp,Obj1,Obj2) :-
                 (Pred1=..[Ac,Obj1,X], Pred1, Pred2=..[Ac,Obj2,Y], Pred2, integer(X), integer(Y),
                 ((Comp = sup, X > Y) ; (Comp = inf, X < Y)))
                 ;
                 (count(Ac,Obj1,X1), count(Ac,Obj2,Y1),
                 ((Comp = sup, X1 > Y1) ; (Comp = inf, X1 < Y1))).

find_objects(T,Q,Comp,At,Ac,Obj,Subj,L,N,R) :-
             (Pred1=..[At,Subj,Object1],
             Pred2=..[Ac,Object1,X],
             findall(Object1,(Pred1, Pred2, ((Comp = sup, X > Obj) ; (Comp = inf, X < Obj))),L),
             (Q=ql,!, R = L; length(L,N), R = N))
             ;
             (Pred3=..[At,Subj,Object2],
             findall(Object2,(Pred3, count(Ac,Object2,X1), ((Comp = sup, X1 > Obj) ; (Comp = inf, X2 < Obj))),L),
             (Q=ql,!, R = L; length(L,N), R = N)).

count(Obj,Subj,D) :-
                 Pred=..[Obj,Subj,X], findall(X,Pred,L), length(L,D).

check_objects(D,Ac,Subj,Obj) :- integer(D), count(Obj,Subj,D).
check_objects(D,Ac,[Subj|T],Obj) :- check_objects(D,Ac,Subj,Obj), check_objects(D,Ac,T,Obj).
check_objects(D,Ac,Subj,Obj) :- Pred=..[Ac,Subj,Obj], Pred.
check_objects(Comp,Ac,Subj,Obj) :- nonvar(Comp), comparative(Comp,_,_), compare_objects(Ac,Comp,Subj,Obj).

%answer(T,Q,Comp,At,Ac,Obj,[Subj|Tail],L,N,R) :- nonvar(Comp),
%                                                answer(T,Q,Comp,At,Ac,Obj,Subj,L1,N,R1),
%                                                answer(T,Q,Comp,At,Ac,Obj,Subj,L2,N,R2),
                                                

answer(T,Q,Comp,At,Ac,Obj,[Subj|Tail],L,N,R) :- answer(T,Q,Comp,At,Ac,Obj,Subj,L1,N1,R1),
                                           answer(T,Q,Comp,At,Ac,Obj,Tail,L2,N2,R2),
                                           ((Q=qt, R is R1 + R2);
                                           (Q=ql, append(R1, R2, R))).
                                           
answer(T,Q,Comp,At,Ac,Obj,Subj,L,N,R) :- nonvar(Comp),
                                         find_objects(T,Q,Comp,At,Ac,Obj,Subj,L,N,R).

answer(T,Q,_,At,Ac,Obj,Subj,L,N,R) :-
                      var(At), var(Obj),
                      Pred=..[Ac,Subj,Obj],
                      findall(Obj,Pred,L),
                      (Q=ql,!, R = L;
                      length(L,N), R = N).

answer(T,Q,_,At,Ac,Obj,Subj,L,N,R) :-
                      nonvar(At), var(Obj),
                      Pred=..[Ac,Subj,Obj],
                      findall(Obj,(Pred, be(Obj,At)),L),
                      (Q=ql,!, R = L;
                      length(L,N), R = N).

%% y/n

answer(T,Q,_,At,Ac,Obj,Subj,L,N,R) :-
                      nonvar(Obj),
                      ((T = t, ((check_objects(D,Ac,Subj,Obj), agree_msg(R),!) ; disagree_msg(R),!));
                      (T = f, ((check_objects(D,Ac,Subj,Obj), disagree_msg(R),!) ; agree_msg(R),!))).


%%%
% grammar
%

interrogative_pronoun(p,ql)-->[which].
interrogative_pronoun(_,ql)-->[what].

interrogative_pronoun(p,qt)-->[howmany].

pronoun(_,that)-->[that].

determinant(s,a)-->[a].
determinant(_,the)-->[the].
determinant(s,N)-->[N],{integer(N), N = 1}.
determinant(p,N)-->[N],{integer(N), N > 1}.

preposition(of)-->[of].
preposition(in)-->[in].

indefinite_article(s,a)-->[a].

comparative(sup) --> [more].
comparative(inf) --> [less].

superlative(sup) --> [most].
superlative(inf) --> [least].

conjunction(cp) --> [than].
conjunction(ch) --> [or].
conjunction(ag) --> [and].

noun(s,continent)-->[continent].
noun(p,continent)-->[continents].
noun(s,country)-->[country].
noun(p,country)-->[countries].

noun(s,africa)-->[africa].
noun(a,africa)-->[african].
noun(s,america)-->[america].
noun(a,america)-->[american].
noun(s,asia)-->[asia].
noun(a,asia)-->[asian].
noun(s,europe)-->[europe].
noun(a,europe)-->[european].
noun(s,oceania)-->[oceania].
noun(a,oceania)-->[oceanian].

noun(s,portugal)-->[portugal].
noun(a,portugal)-->[portuguese].
noun(s,turkey)-->[turkey].
noun(a,turkey)-->[turkish].
noun(s,spain)-->[spain].
noun(a,spain)-->[spanish].

noun(s,people)-->[population].
noun(s,people)-->[person].
noun(p,people)-->[people].
noun(s,river)-->[river].
noun(p,river)-->[rivers].
noun(s,capital)-->[capital].
noun(p,capital)-->[capitals].

noun(s,ankara)-->[ankara].
noun(s,lisbon)-->[lisbon].
noun(s,madrid)-->[madrid].

noun(s,tagus)-->[tagus].
noun(s,douro)-->[douro].

%%verb(number,true/false,verb,subject)
verb(s,t,be,_)-->[is].
verb(p,t,be,_)-->[are].
verb(s,t,have,_)-->[has].
verb(p,t,have,_)-->[have].

object(_,Obj)-->[Obj].

%%
%% Database
%%

continent(continent).
continent(africa).
continent(america).
continent(asia).
continent(europe).
continent(oceania).

continent([H|T]):- continent(H),continent(T).
be(X,continent):- continent(X).

%% Countries

continent(portugal, europe).
continent(turkey, europe).
continent(spain, europe).
continent(china, asia).

country(portugal).
country(turkey).
country(spain).
country(asia).

country(Continent, Country) :- continent(Country, Continent).
be(X,country) :- country(X).


%% Cities

city(portugal, oporto).
city(portugal, lisbon).

city(Continent, City) :- continent(Continent, Country), city(Country, City).

%% Capitals

capital(portugal, lisbon).
capital(turkey, ankara).
capital(spain, madrid).
capital(china, beijing).

be(X,capital) :- capital(_,X).

%% Rivers

river(lisbon, tagus).
river(oporto, douro).

%%river(spain, tagus).
%%river(spain, douro).

river(Country, River) :- city(Country, City), river(City, River).
river(Continent, River) :- continent(Country, Continent), river(Country, River).
be(X,river) :- river(_,X).

%% Population

   % Continents

people(africa, 1022234000).
people(america, 934611000).
people(asia, 4164252000).
people(europe, 738199000).
people(oceania, 6895889000).

   % Countries

people(portugal, 10562178).
people(turkey, 75627384).
people(china, 1338612968).

%% Negations

negation(f)-->[not].

%% Trees

in(Country, Continent) :- continent(Country, Continent).
in(City, Continent) :- city(Country, City), continent(Country, Continent).
in(River, Continent) :- river(Country, River), continent(Country, Continent).
in(River, City) :- river(City, River).
in(River, Country) :- river(City, River), city(Country, City).
in(River, Continent) :- river(City, River), city(Country, City), continent(Country, Continent).