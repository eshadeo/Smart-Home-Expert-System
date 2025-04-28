:- module(sensors, [
    register_sensor/3,
    register_effector/2,
    safe_sensor_value/2
]).

:- use_module(core).

% -----------------------
% Modular Sensor Registration
% -----------------------
register_sensor(Id, Type, Location) :-
    assertz(sensor(Id, Type)),
    (Location = inside -> assertz(inside(Id)) ; assertz(outside(Id))).

register_effector(Id, InitialValue) :-
    assertz(effectorValue(Id, InitialValue)).

% Helper predicate to safely get sensor values
safe_sensor_value(Sensor, Value) :-
    sensorValue(Sensor, Raw),
    Value is Raw.