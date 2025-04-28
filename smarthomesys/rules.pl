:- module(rules, [
    setEffectors/2,
    setInsideEffectors/2,
    setOutsideEffectors/2,
    setInsideEffectors_temp/2,
    extractInsideEffectors/3,
    extractOutsideEffectors/3,
    set/2,
    run_rules/0
]).

:- use_module(core).
:- use_module(sensors).
:- use_module(utils).

% -----------------------
% Helper predicates for rules
% -----------------------
setEffectors([], _).
setEffectors([H|T], Value) :-
    get_time(Time),
    stamp_date_time(Time, DateTime, 'UTC'),
    effectorValue(H, OldVal),
    (OldVal \= Value ->
        replace_existing_fact(effectorValue(H, _), effectorValue(H, Value)),
        log_effector_change(H, OldVal, Value, DateTime, 'Effector value updated due to rule trigger')
    ;
        log_effector_unchanged(H, OldVal, DateTime)
    ),
    setEffectors(T, Value).

extractInsideEffectors([], L, L).
extractInsideEffectors([H|T], Acc, Result) :-
    inside(H),
    extractInsideEffectors(T, [H|Acc], Result).
extractInsideEffectors([_|T], Acc, Result) :-
    extractInsideEffectors(T, Acc, Result).

extractOutsideEffectors([], L, L).
extractOutsideEffectors([H|T], Acc, Result) :-
    outside(H),
    extractOutsideEffectors(T, [H|Acc], Result).
extractOutsideEffectors([_|T], Acc, Result) :-
    extractOutsideEffectors(T, Acc, Result).

setInsideEffectors(Effectors, Value) :-
    extractInsideEffectors(Effectors, [], Filtered),
    setEffectors(Filtered, Value).

setOutsideEffectors(Effectors, Value) :-
    extractOutsideEffectors(Effectors, [], Filtered),
    setEffectors(Filtered, Value).

setInsideEffectors_temp(Current, Desired) :-
    (Current < Desired ->
        setEffectors([r], Desired), setEffectors([ac], 0)
    ;
        setEffectors([ac], Desired), setEffectors([r], 0)
    ).

% -----------------------
% Automation Rules
% -----------------------
set(Action, light) :-
    preference(Action, light, Desired, Effectors),
    sensor(Sensor, light),
    safe_sensor_value(Sensor, Current),
    outside(Sensor),
    get_time(Time),
    stamp_date_time(Time, DateTime, 'UTC'),
    open('logActions.txt', append, Stream),
    format(Stream, '[~w] Lighting Rule Triggered | Current: ~w, Desired: ~w~n', [DateTime, Current, Desired]),
    close(Stream),
    (Current >= Desired ->
        setOutsideEffectors(Effectors, Desired), setInsideEffectors(Effectors, 0)
    ;
        setOutsideEffectors(Effectors, 0), setInsideEffectors(Effectors, Desired)
    ).

set(Action, temp) :-
    preference(Action, temp, Desired, Effectors),
    sensor(SensorIn, temp), inside(SensorIn), safe_sensor_value(SensorIn, TempIn),
    sensor(SensorOut, temp), outside(SensorOut), safe_sensor_value(SensorOut, TempOut),
    sensor(SensorWind, wind), safe_sensor_value(SensorWind, WindVal),
    preference(Action, wind, WindThresh, _),
    get_time(Time),
    stamp_date_time(Time, DateTime, 'UTC'),
    open('logActions.txt', append, Stream),
    format(Stream, '[~w] Temperature Rule Triggered | Inside: ~w, Outside: ~w, Desired: ~w, Wind: ~w (Thresh: ~w)~n',
           [DateTime, TempIn, TempOut, Desired, WindVal, WindThresh]),
    close(Stream),
    (TempIn < Desired, TempOut > Desired ->
        (WindVal =< WindThresh ->
            sensor(SensorRain, rain), safe_sensor_value(SensorRain, RainVal),
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
            sensor(SensorRain, rain), safe_sensor_value(SensorRain, RainVal),
            (RainVal =:= 0 ->
                setOutsideEffectors(Effectors, 1), setInsideEffectors(Effectors, 0)
            ;
                setOutsideEffectors(Effectors, 0), setInsideEffectors_temp(TempIn, Desired)
            )
        )
    ;
        TempIn =:= Desired -> true
    ).

set(Action, noise) :-
    preference(Action, noise, Threshold, Effectors),
    sensor(SensorNoise, noise), safe_sensor_value(SensorNoise, Val),
    sensor(SensorTemp, temp), inside(SensorTemp), safe_sensor_value(SensorTemp, Temp),
    preference(Action, temp, DesiredTemp, _),
    get_time(Time),
    stamp_date_time(Time, DateTime, 'UTC'),
    open('logActions.txt', append, Stream),
    format(Stream, '[~w] Noise Rule Triggered | Current: ~w, Threshold: ~w | Temp: ~w, Desired: ~w~n',
           [DateTime, Val, Threshold, Temp, DesiredTemp]),
    close(Stream),
    (Val > Threshold ->
        (Temp \== DesiredTemp ->
            setOutsideEffectors(Effectors, 0), setInsideEffectors_temp(Temp, DesiredTemp)
        ;
            setOutsideEffectors(Effectors, 0)
        )
    ; true).

set(_, _) :- true.

% Run Automation Rules
run_rules :-
    writeln('Running Smart Home Automation Rules...'),
    writeln('Processing temperature conditions...'),
    set(comfort, temp),
    writeln('Processing lighting conditions...'),
    set(comfort, light),
    writeln('Processing noise conditions...'),
    set(comfort, noise),
    get_time(Time),
    stamp_date_time(Time, DateTime, 'UTC'),
    log_summary(DateTime),
    writeln('All conditions processed. Summary logged.'),
    press_any_key.