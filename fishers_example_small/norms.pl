[
    name: firstInTime,
    predicates: [won_race/1],
    incompatibles: [won_race(F)],
    initially: [initially(consecutiveDefection(F,0)):-role(F,fisher)],
    rules: [
        boundary: [],
        position: [],
        choice: [
            if role(A,fisher) then ~can(A,leave) where [at(A,S),won_race(A),fishing_spot(S)],
            if role(A,fisher) then ~can(A,stay) where [at(A,S),won_race(B),B\=A,fishing_spot(S)]
        ],
        control: [
        if does(F1,go_to_spot(S)) and does(F2,go_to_spot(S))
        then [won_race(F1) withProb P1,won_race(F2) withProb P2]
        where [F1@<F2,fishing_spot(S),speed(F1,X1),speed(F2,X2),{P1=X1/(X1+X2)},{P2=X2/(X1+X2)}]
        ]
    ]
].

[
    name: firstToAnnounce,
    predicates: [announced/2],
    incompatibles: [],
    initially: [],
    rules: [
        boundary: [],
        position: [
            if participates(alice) then role(alice,announcer) where [findall(X,participates(X),L),random_member(A,L)]
        ],
        choice: [
            if role(A,announcer) then can(A,announce_spot(S)) where [at(A,shore),at(B,shore),A\=B,fishing_spot(S)],
            if role(A,announcer) then ~can(A,announce_spot(S)) where [announced(A,_),fishing_spot(S)],
            if role(A,fisher) then ~can(A,go_to_spot(S)) where [\+announced(_,_),fishing_spot(S)],
            if role(A,announcer) then ~can(A,leave) where [at(A,S),fishing_spot(S),announced(A,S)],
            if role(F,fisher) then ~can(F,stay) where [at(F,S),fishing_spot(S),announced(_,S),\+role(F,announcer)]
        ],
        control: [
            if does(F,announce_spot(S)) then [announced(F,S) withProb 1] where []
        ]
    ]
].
