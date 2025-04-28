:- module(core, [
    effectorValue/2,
    sensor/2,
    sensorValue/2,
    preference/4,
    inside/1,
    outside/1,
    effectorHistory/3,
    sensor_range/3,
    replace_existing_fact/2,
    remove_existing_fact/1
]).

% Dynamic predicate declarations
:- dynamic effectorValue/2.
:- dynamic sensor/2.
:- dynamic sensorValue/2.
:- dynamic preference/4.
:- dynamic inside/1.
:- dynamic outside/1.
:- dynamic effectorHistory/3.
:- dynamic sensor_range/3.

% -----------------------
% Helper predicates for core functionality
% -----------------------
replace_existing_fact(OldFact, NewFact) :-
    call(OldFact),
    !,
    retract(OldFact),
    assertz(NewFact).

remove_existing_fact(OldFact) :-
    call(OldFact),
    retract(OldFact).