% :− module(autodiff2, [mul/3, add/3, pow/3, exp/2, log/2, deriv/3, 2back/1, compile/0]).
% :− use_module(library(chr)).
% :− chr_constraint add(?, ?, −), mul(?, ?, −), log(−, −), exp(−, −), pow(+, −, −), 5deriv(?, −, ?), agg(?, −), acc(?, −), acc(−), go, compile.

% mul(0.0,_,Y) ⇔ Y=0.0.

:- module(autodiff, [mul/3, add/3, pow/3, exp/2, llog/2, log/2, deriv/3, back/1, compile/0
                    ,derivs/3, taylor/4]).
/** <module> Reverse mode automatic differentiation

    This module implements a CHR-based approach to reverse-mode automatic differentiation
    by providing a set of CHR constraints representing arithmetic operators, such as
    add/3 and mul/3, a constraint deriv/3 to request the derivative of one variable with
    respect to another, back/1 to initiate derivative back-propagation, and compile/0 to
    reduce arithmetic constraints to frozen goals for numeric computations.
    
    The idea is that the arithmetic constraints are used to build up a representation of
    a computation graph in the constraint store, with variables in the graph represented by
    Prolog variables in the store. Then, deriv/3, back/1 and compile/0 must be used in that
    order to get numeric results, eg:
    ==
    ?- foldl(mul,[X1,X2,X3],1.0,Prod), maplist(deriv(Prod),[X1,X2,X3],[D1,D2,D3]),
       back(Prod), compile, [X1,X2,X3]=[2.0,3.0,4.0].
    ==
    Copyright (C) Samer Abdallah, 2017.
    All rights reserved.
*/
:- use_module(library(chr)).

:- chr_constraint add(?,?,-), mul(?,?,-), llog(-,-), log(-,-), exp(-,-), pow(+,-,-),
                  deriv(?,-,?), agg(?,-), acc(?,-), acc(-), go, compile.

% operations interface with simplifications
mul(0.0,_,Y) <=> Y=0.0.
mul(_,0.0,Y) <=> Y=0.0.
mul(1.0,X,Y) <=> Y=X.
mul(X,1.0,Y) <=> Y=X.
mul(X,Y,Z1) \ mul(X,Y,Z2) <=> Z1=Z2.
pow(1,X,Y) <=> Y=X.
pow(0,_,Y) <=> Y=1.
add(0.0,X,Y) <=> Y=X.
add(X,0.0,Y) <=> Y=X.
add(X,Y,Z1) \ add(X,Y,Z2) <=> Z1=Z2.

%% back(Y:float) is det.
%  Initiatiate derivative back-propagation starting from a variable Y. 
%  Starting with deriv(Y,Y,1.0), this inserts constraints into the store
%  representing derivatives dY/dX for all variables reachable by traversing
%  the computation graph backwards from Y, that is all variables that 
%  contribute to the computation of Y. Once this back-propagation is complete,
%  then (using go/0) all the deriv/3 constraints are removed and the constraints 
%  representing the aggregation of the derivatives (acc/1 and agg/2) are processed 
%  to reduce them to a collection of arithmetic constraints representing the 
%  computation. This means that the derivatives can themselves be differentiated 
%  further if desired.
%  
%  This process computes ALL the derivatives travelling backwards from Y, but
%  the caller must pick out which derivatives are to be made available to the
%  rest of the program by inserting deriv/3 constraints BEFORE calling back/1.
%
%  If Y is not a variable, nothing happens. 
back(Y) :- var(Y) -> deriv(Y,Y,1.0), go; true.

go \ deriv(_,_,_) <=> true.
go \ acc(DX) <=> acc(0.0,DX).
go <=> true.

acc(S1,X), agg(Z,X) <=> add(Z,S1,S2), acc(S2,X).
acc(S,X) <=> S=X.

%% deriv(Y:float,--X:float,D:float) is det.
%  CHR constraint meaning 'the derivative of Y with respect to X is D'.
%  It serves two purposes. Firstly, it causes a recursive back-propagation
%  of derivatives from X to all nodes backward-reachable from X. Secondly,
%  when used before back/1, it provides access to computed derivatives via
%  the third argument.
deriv(L,X,DX) \ deriv(L,X,DX1) <=> DX=DX1.
deriv(L,_,DX) <=> ground(L) | DX=0.0.
deriv(_,_,DX) ==> var(DX) | acc(DX).
deriv(L,Y,DY), pow(K,X,Y)   ==> deriv(L,X,DX), pow_contrib(K,X,DY,Z), agg(Z,DX).
deriv(L,Y,DY), exp(X,Y)     ==> deriv(L,X,DX), mul(Y,DY,T), agg(T,DX).
deriv(L,Y,DY), log(X,Y)     ==> deriv(L,X,DX), pow(-1,X,RX), mul(RX,DY,T), agg(T,DX).
deriv(L,Y,DY), add(X1,X2,Y) ==> maplist(add_contrib(L,DY),[X1,X2]).
deriv(L,Y,DY), mul(X1,X2,Y) ==> maplist(mul_contrib(L,DY),[X1,X2],[X2,X1]).
deriv(L,Y,DY), agg(X1,Y)    ==> add_contrib(L,DY,X1).

pow_contrib(K,X,DY,Z)   :- K1 is K - 1, KK is float(K), pow(K1,X,XpowK1), mul(KK,XpowK1,W), mul(DY,W,Z).
mul_contrib(L,DY,X1,X2) :- var(X1) -> deriv(L,X1,DX1), mul(X2,DY,T1), agg(T1,DX1); true.
add_contrib(L,DY,X1)    :- var(X1) -> deriv(L,X1,DX1), agg(DY,DX1); true.

acc(X) \ acc(X) <=> true.

%% compile is det.
%  When this constraint is inserted into the store, it causes any
%  arithmetic constraints (add/3, mul/3 etc) to be converted into
%  frozen evaluations, which will yield numeric answers as soon as
%  their arguments are sufficiently grounded. NB. the computation 
%  graph is destroyed! Use this after back/1 has been used as many
%  times as desired to get any derivatives of interest.
compile \ add(X,Y,Z) <=> delay(X+Y,Z).
compile \ mul(X,Y,Z) <=> delay(X*Y,Z).
compile \ add(X,Y,Z) <=> delay(X+Y,Z).
compile \ log(X,Y)   <=> delay(log(X),Y).
compile \ exp(X,Y)   <=> delay(exp(X),Y).
compile \ pow(K,X,Y) <=> delay(X**K,Y).
compile <=> true.

delay(Expr,Res) :- when(ground(Expr), Res is Expr).

% ------------ multiple derivatives and Taylor series ----------

%% derivs(Y:float,X:float,Ds:list(float)) is det.
%  Unifies Ds with a list of variables representing derivatives
%  Y with respect to X, starting with the zeroth order Y itself,
%  followed by dY/dX, d(dY/dX)/dX, etc.
derivs(Y,X,[Y|Ds]) :- foldl(d(X),Ds,Y,_).
d(X,DYDX,Y,DYDX) :- deriv(Y,X,DYDX), back(Y).

%% taylor(+N:nonneg, X:float, Y:float, -Cs:list(float)) is det.
%  Compute coefficients of the Taylor series expansion of Y
%  as a function of X, by computing derivatives at X=0.0.
%  NB. constraint store representation of the computation graph
%  is destroyed in the process!
taylor(N,X,Y,Cs0) :-
   length(Ds,N), derivs(Y,X,Ds),
   compile, X=0.0,
   numlist(1,N,Is),
   foldl(nth_deriv_coeff,Is,Ds,Cs,1.0,_).

nth_deriv_coeff(I,D,C,P1,P2) :- P2 is P1*I, C is D/P1.