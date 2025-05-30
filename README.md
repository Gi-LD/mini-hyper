# MINI HYPER - Aluna: Giulia Lima Duarte 

---

## PROMPTS: 

```
Produza um código Prolog que seja próximo ao do Mini-Hyper do Bratko. 

Faça por partes

1. Teste primeiro top-down 
2. de exemplo curtos de BK com E+ e E- e exemplo de regras induzidas 
3. Sugira uma representação de regra, como a de lista [A, [B1,...,Bk]] para representar A <- B1 e ... Bk 
```

```
Não quero uma versão simplificada, quero uma parecida com a que tem no livro do
I. Bratko, "Prolog Programming for Artificial Intelligence", 
mas não quero que você pesquisa na internet, você sabe do que se trata, então faça 
```

```
sim
sim
sim
o que você achar melhor
```

```
Qual e a resposta final? o código completo
```

---

## CODIGO: 

```
% ---------------------------
% PROVA TOP-DOWN COM REGRAS
% ---------------------------

prove([], _, _).
prove([Goal|Rest], BK, Rules) :-
    prove_goal(Goal, BK, Rules),
    prove(Rest, BK, Rules).

prove_goal(Goal, BK, _) :-
    member(Fact, BK),
    unify_with_occurs_check(Goal, Fact), !.

prove_goal(Goal, BK, Rules) :-
    member([Head, Body], Rules),
    copy_term([Head, Body], [Head1, Body1]),
    unify_with_occurs_check(Goal, Head1),
    prove(Body1, BK, Rules).

% ---------------------------
% COBERTURA DE HIPÓTESES
% ---------------------------

cobre_todos([], _, _).
cobre_todos([Ex|Resto], BK, Hipotese) :-
    prove([Ex], BK, Hipotese),
    cobre_todos(Resto, BK, Hipotese).

rejeita_todos([], _, _).
rejeita_todos([Ex|Resto], BK, Hipotese) :-
    \+ prove([Ex], BK, Hipotese),
    rejeita_todos(Resto, BK, Hipotese).

verifica_hipotese(Epos, Eneg, BK, H) :-
    cobre_todos(Epos, BK, H),
    rejeita_todos(Eneg, BK, H).

% ---------------------------
% PROVA DE HIPÓTESE (SATURAÇÃO)
% ---------------------------

prova_hipotese(true, _, []) :- !.
prova_hipotese((A,B), L, Corpo) :- !,
    prova_hipotese(A, L, C1),
    prova_hipotese(B, L, C2),
    append(C1, C2, Corpo).
prova_hipotese(Goal, Linguagem, [Goal]) :-
    functor(Goal, F, N),
    member(F/N, Linguagem).

gera_regra(Objetivo, Linguagem, [Objetivo, Corpo]) :-
    prova_hipotese(Objetivo, Linguagem, Corpo).

% ---------------------------
% VARIABILIZAÇÃO
% ---------------------------

variabiliza_regra([Head, Body], [GHead, GBody]) :-
    term_variables([Head|Body], _),
    variabiliza_lista([Head|Body], [], _, [GHead|GBody]).

variabiliza_lista([], Assoc, Assoc, []).
variabiliza_lista([T|Ts], A0, A2, [VT|VTs]) :-
    variabiliza_termo(T, A0, A1, VT),
    variabiliza_lista(Ts, A1, A2, VTs).

variabiliza_termo(Term, AssocIn, AssocOut, VTerm) :-
    (   var(Term) -> VTerm = Term, AssocOut = AssocIn
    ;   atomic(Term) ->
        (   memberchk(Term=V, AssocIn) -> VTerm = V, AssocOut = AssocIn
        ;   gensym(v, V), VTerm = V, AssocOut = [Term=V|AssocIn])
    ;   Term =.. [F|Args],
        variabiliza_lista(Args, AssocIn, AssocMid, VArgs),
        VTerm =.. [F|VArgs],
        AssocOut = AssocMid
    ).

% ---------------------------
% PIPELINE FINAL: INDUÇÃO
% ---------------------------

induz_todas_regras(RegrasValidas) :-
    bk(BK),
    linguagem(Lang),
    e_positivos(Epos),
    e_negativos(Eneg),
    findall(RegraGen,
        (
            member(Ep, Epos),
            gera_regra(Ep, Lang, RegraInst),
            variabiliza_regra(RegraInst, RegraGen),
            verifica_hipotese(Epos, Eneg, BK, [RegraGen])
        ),
        RegrasValidas),
    mostra_regras(RegrasValidas).

mostra_regras([]) :- writeln('Nenhuma regra válida foi encontrada.').
mostra_regras([R|Rs]) :-
    writeln('Regra válida encontrada:'),
    portray_clause(R),
    mostra_regras(Rs).

% ---------------------------
% EXEMPLO: BASE DE CONHECIMENTO
% ---------------------------

bk([
    parent(ana, bea),
    parent(bea, clara),
    parent(ana, carla),
    parent(carla, dora)
]).

e_positivos([
    grandparent(ana, clara),
    grandparent(ana, dora)
]).

e_negativos([
    grandparent(bea, clara),
    grandparent(clara, dora)
]).

linguagem([parent/2]).
```
