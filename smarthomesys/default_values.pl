:- module(default_values, [
    initialize_system/0,
    reset_system/0
]).

:- use_module(core).
:- use_module(utils).

% -----------------------
% Default values
% -----------------------
initialize_system :-
    % --- Sensor Definitions ---
    assertz(sensor(s1, temp)),
    assertz(sensor(s2, temp)),
    assertz(sensor(s3, wind)),
    assertz(sensor(s4, rain)),
    assertz(sensor(s5, light)),
    assertz(sensor(s6, noise)),

    % --- Sensor Values ---
    assertz(sensorValue(s1, 18)),     % Inside temperature
    assertz(sensorValue(s2, 28)),     % Outside temperature 
    assertz(sensorValue(s3, 4)),      % Wind speed
    assertz(sensorValue(s4, 0)),      % Rain (0 = no rain)
    assertz(sensorValue(s5, 200)),    % Light
    assertz(sensorValue(s6, 80)),     % Noise

    % --- Sensor Location ---
    assertz(inside(s1)),
    assertz(inside(s6)),
    assertz(outside(s2)),
    assertz(outside(s3)),
    assertz(outside(s4)),
    assertz(outside(s5)),

    % --- User Preferences ---
    assertz(preference(comfort, temp, 24, [r, ac, win])),
    assertz(preference(comfort, wind, 5, [r, ac, win])),
    assertz(preference(comfort, light, 400, [bulb, lamp, tube])),
    assertz(preference(comfort, noise, 60, [curtains, door])),

    % --- Effector Values ---
    assertz(effectorValue(r, 0)),
    assertz(effectorValue(ac, 0)),
    assertz(effectorValue(win, 0)),
    assertz(effectorValue(bulb, 0)),
    assertz(effectorValue(lamp, 0)),
    assertz(effectorValue(tube, 0)),
    assertz(effectorValue(curtains, 0)),
    assertz(effectorValue(door, 0)),

    % --- Sensor Ranges (for validation) ---
    assertz(sensor_range(light, 0, 1000)),
    assertz(sensor_range(temp, -10, 50)),
    assertz(sensor_range(wind, 0, 100)),
    assertz(sensor_range(rain, 0, 1)),
    assertz(sensor_range(noise, 0, 150)).

% Reset System
reset_system :-
    writeln('Warning: This will reset all values to default.'),
    write('Are you sure? (y/n): '),
    read_line_to_codes(user_input, Codes),
    string_codes(Input, Codes),
    downcase_atom(Input, Response),
    (Response = "y" ->
        retractall(sensor(_, _)),
        retractall(sensorValue(_, _)),
        retractall(preference(_, _, _, _)),
        retractall(effectorValue(_, _)),
        retractall(inside(_)),
        retractall(outside(_)),
        retractall(sensor_range(_, _, _)),
        initialize_system,
        writeln('System has been reset to default values.')
    ;
        writeln('Reset cancelled.')
    ),
    press_any_key.