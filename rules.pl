% Replace or remove facts
replace_existing_fact(OldFact, NewFact) :-
    call(OldFact),
    !,
    retract(OldFact),
    assertz(NewFact).

remove_existing_fact(OldFact) :-
    call(OldFact),
    retract(OldFact).

% Check if a sensor is outside (not inside)
outside(Id) :-
    \+ inside(Id).

% Set effectors
setEffectors([], _).
setEffectors([H|T], Value) :-
    replace_existing_fact(effectorValue(H, _), effectorValue(H, Value)),
    open('logActions.txt', append, Stream),
    write(Stream, 'setEffector('), write(Stream, H), write(Stream, ','), write(Stream, Value), write(Stream, ').'), nl(Stream),
    close(Stream),
    setEffectors(T, Value).

% Filter inside effectors
extractInsideEffectors([], L, L).
extractInsideEffectors([H|T], Acc, Result) :-
    inside(H),
    extractInsideEffectors(T, [H|Acc], Result).
extractInsideEffectors([_|T], Acc, Result) :-
    extractInsideEffectors(T, Acc, Result).

% Filter outside effectors
extractOutsideEffectors([], L, L).
extractOutsideEffectors([H|T], Acc, Result) :-
    outside(H),
    extractOutsideEffectors(T, [H|Acc], Result).
extractOutsideEffectors([_|T], Acc, Result) :-
    extractOutsideEffectors(T, Acc, Result).

% Set inside effectors
setInsideEffectors(Effectors, Value) :-
    extractInsideEffectors(Effectors, [], Filtered),
    setEffectors(Filtered, Value).

% Set outside effectors
setOutsideEffectors(Effectors, Value) :-
    extractOutsideEffectors(Effectors, [], Filtered),
    setEffectors(Filtered, Value).

% Helper: manage temperature effectors based on difference
setInsideEffectors_temp(Current, Desired) :-
    (Current < Desired ->
        setEffectors([r], Desired), setEffectors([ac], 0)
    ;
        setEffectors([ac], Desired), setEffectors([r], 0)
    ).

% Rule: Lighting control
set(Action, light) :-
    preference(Action, light, Desired, Effectors),
    sensor(Sensor, light),
    sensorValue(Sensor, Current),
    outside(Sensor),
    (Current >= Desired ->
        setOutsideEffectors(Effectors, Desired), setInsideEffectors(Effectors, 0)
    ;
        setOutsideEffectors(Effectors, 0), setInsideEffectors(Effectors, Desired)
    ).

% Rule: Temperature control
set(Action, temp) :-
    preference(Action, temp, Desired, Effectors),
    sensor(SensorIn, temp), inside(SensorIn), sensorValue(SensorIn, TempIn),
    sensor(SensorOut, temp), outside(SensorOut), sensorValue(SensorOut, TempOut),
    sensor(SensorWind, wind), sensorValue(SensorWind, WindVal),
    preference(Action, wind, WindThresh, _),
    (TempIn < Desired, TempOut > Desired ->
        (WindVal =< WindThresh ->
            sensor(SensorRain, rain), sensorValue(SensorRain, RainVal),
            (RainVal =:= 0 ->
                setOutsideEffectors(Effectors, 1), setInsideEffectors(Effectors, 0)
            ;
                setOutsideEffectors(Effectors, 0), setInsideEffectors_temp(TempIn, Desired)
            )
        ;
            setOutsideEffectors(Effectors, 0), setInsideEffectors_temp(TempIn, Desired)
        )
    ;
    TempIn < Desired, TempOut < Desired ->
        setOutsideEffectors(Effectors, 0), setInsideEffectors_temp(TempIn, Desired)
    ;
    TempIn > Desired, TempOut > Desired ->
        setOutsideEffectors(Effectors, 0), setInsideEffectors_temp(TempIn, Desired)
    ;
    TempIn > Desired, TempOut < Desired ->
        (WindVal > WindThresh ->
            setOutsideEffectors(Effectors, 0), setInsideEffectors_temp(TempIn, Desired)
        ;
            sensor(SensorRain, rain), sensorValue(SensorRain, RainVal),
            (RainVal =:= 0 ->
                setOutsideEffectors(Effectors, 1), setInsideEffectors(Effectors, 0)
            ;
                setOutsideEffectors(Effectors, 0), setInsideEffectors_temp(TempIn, Desired)
            )
        )
    ;
        TempIn =:= Desired -> true
    ).

% Rule: Noise control
set(Action, noise) :-
    preference(Action, noise, Threshold, Effectors),
    sensor(SensorNoise, noise), sensorValue(SensorNoise, Val),
    Val > Threshold,
    sensor(SensorTemp, temp), inside(SensorTemp), sensorValue(SensorTemp, Temp),
    preference(Action, temp, DesiredTemp, _),
    (Temp \== DesiredTemp ->
        setOutsideEffectors(Effectors, 0), setInsideEffectors_temp(Temp, DesiredTemp)
    ;
        setOutsideEffectors(Effectors, 0)
    ).

% Generic fallback
set(_, _) :- true.
