:- dynamic s0/0, s1/0, s2/0, s3/0, s4/0, s5/0, s6/0, s7/0, s8/0, s9/0, s10/0,
s11/0, s12/0, s13/0, s14/0.

initially(s0).

incompatible(_,L) :- length(L,1).

terminal :- s1.
terminal :- s2.
terminal :- s3.
terminal :- s4.
terminal :- s5.
terminal :- s6.
terminal :- s7.
terminal :- s8.
terminal :- s9.
terminal :- s10.
terminal :- s11.
terminal :- s12.
terminal :- s13.
terminal :- s14.

% terminal :- \+s0.
