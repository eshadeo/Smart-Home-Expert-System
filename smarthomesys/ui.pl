:- module(ui, [
    main_menu/0,
    display_sensors/0,
    display_effectors/0,
    display_preferences/0
]).

:- use_module(core).
:- use_module(rules).
:- use_module(utils).
:- use_module(default_values).

% -----------------------
% Menu System
% -----------------------

% Main menu display
display_main_menu :-
    writeln('+---------------------------------------+'),
    writeln('|     SMART HOME MANAGEMENT SYSTEM      |'),
    writeln('+---------------------------------------+'),
    writeln('| 1. View System Status                 |'),
    writeln('| 2. Modify Sensor Values               |'),
    writeln('| 3. Modify User Preferences            |'),
    writeln('| 4. Run Automation Rules               |'),
    writeln('| 5. View Log File                      |'),
    writeln('| 6. Reset System to Default            |'),
    writeln('| 0. Exit                               |'),
    writeln('+---------------------------------------+').

% Main loop for the application
main_menu :-
    display_main_menu,
    write('Select an option: '),
    read_line_to_codes(user_input, Codes),
    string_codes(Input, Codes),
    (catch(number_string(Option, Input), _, fail) ->
        handle_main_option(Option)
    ;
        writeln('Invalid input. Please enter a number.'),
        main_menu
    ).

% Handle main menu options
handle_main_option(1) :- view_status_menu, main_menu.
handle_main_option(2) :- sensor_menu, main_menu.
handle_main_option(3) :- preference_menu, main_menu.
handle_main_option(4) :- run_rules, main_menu.
handle_main_option(5) :- view_log, main_menu.
handle_main_option(6) :- reset_system, main_menu.
handle_main_option(0) :- writeln('Exiting system. Goodbye!').
handle_main_option(_) :- 
    writeln('Invalid option. Please try again.'), 
    sleep(1),
    main_menu.

% Display status menu
display_status_menu :-
    writeln('+---------------------------------------+'),
    writeln('|           SYSTEM STATUS               |'),
    writeln('+---------------------------------------+'),
    writeln('| 1. View Sensor Values                 |'),
    writeln('| 2. View Effector States               |'),
    writeln('| 3. View User Preferences              |'),
    writeln('| 0. Back to Main Menu                  |'),
    writeln('+---------------------------------------+').

% Status menu handling
view_status_menu :-
    display_status_menu,
    write('Select an option: '),
    read_line_to_codes(user_input, Codes),
    string_codes(Input, Codes),
    (catch(number_string(Option, Input), _, fail) ->
        handle_status_option(Option)
    ;
        writeln('Invalid input. Please enter a number.'),
        view_status_menu
    ).

handle_status_option(1) :- display_sensors, press_any_key.
handle_status_option(2) :- display_effectors, press_any_key.
handle_status_option(3) :- display_preferences, press_any_key.
handle_status_option(0).
handle_status_option(_) :- 
    writeln('Invalid option. Please try again.'), 
    sleep(1),
    view_status_menu.

% Sensor menu handling
sensor_menu :-
    writeln('+---------------------------------------+'),
    writeln('|          MODIFY SENSORS               |'),
    writeln('+---------------------------------------+'),
    display_sensors,
    writeln('|                                       |'),
    writeln('| Enter sensor ID to modify (e.g., s1)  |'),
    writeln('| or 0 to return to main menu           |'),
    writeln('+---------------------------------------+'),
    write('Select an option: '),
    read_line_to_codes(user_input, Codes),
    string_codes(Input, Codes),
    atom_string(SensorId, Input),
    handle_sensor_option(SensorId).

handle_sensor_option('0').
handle_sensor_option(SensorId) :-
    sensor(SensorId, Type),
    sensor_range(Type, Min, Max),
    format('Enter new value for ~w (~w) [~w-~w]: ', [SensorId, Type, Min, Max]),
    read_line_to_codes(user_input, ValCodes),
    string_codes(ValStr, ValCodes),
    (catch(number_string(NewValue, ValStr), _, fail) ->
        (NewValue >= Min, NewValue =< Max ->
            replace_existing_fact(sensorValue(SensorId, _), sensorValue(SensorId, NewValue)),
            format('Sensor ~w updated to ~w~n', [SensorId, NewValue])
        ;
            format('Error: Value must be between ~w and ~w~n', [Min, Max])
        )
    ;
        writeln('Invalid input. Please enter a number.')
    ),
    press_any_key.
handle_sensor_option(_) :-
    writeln('Invalid sensor ID. Please try again.'),
    press_any_key.

% Preference menu handling
display_preference_menu :-
    writeln('+---------------------------------------+'),
    writeln('|       MODIFY USER PREFERENCES         |'),
    writeln('+---------------------------------------+'),
    display_preferences,
    writeln('|                                       |'),
    writeln('| Options:                              |'),
    writeln('| 1. Change Temperature Preference      |'),
    writeln('| 2. Change Light Preference            |'),
    writeln('| 3. Change Wind Threshold              |'),
    writeln('| 4. Change Noise Threshold             |'),
    writeln('| 0. Back to Main Menu                  |'),
    writeln('+---------------------------------------+').

preference_menu :-
    display_preference_menu,
    write('Select an option: '),
    read_line_to_codes(user_input, Codes),
    string_codes(Input, Codes),
    (catch(number_string(Option, Input), _, fail) ->
        handle_preference_option(Option)
    ;
        writeln('Invalid input. Please enter a number.'),
        preference_menu
    ).

handle_preference_option(1) :- modify_preference(temp), preference_menu.
handle_preference_option(2) :- modify_preference(light), preference_menu.
handle_preference_option(3) :- modify_preference(wind), preference_menu.
handle_preference_option(4) :- modify_preference(noise), preference_menu.
handle_preference_option(0).
handle_preference_option(_) :- 
    writeln('Invalid option. Please try again.'), 
    sleep(1),
    preference_menu.

modify_preference(Type) :-
    preference(comfort, Type, CurrentValue, Effectors),
    sensor_range(Type, Min, Max),
    format('Current ~w preference: ~w [~w-~w]~n', [Type, CurrentValue, Min, Max]),
    format('Enter new value for ~w preference: ', [Type]),
    read_line_to_codes(user_input, ValCodes),
    string_codes(ValStr, ValCodes),
    (catch(number_string(NewValue, ValStr), _, fail) ->
        (NewValue >= Min, NewValue =< Max ->
            replace_existing_fact(preference(comfort, Type, _, Effectors), 
                                preference(comfort, Type, NewValue, Effectors)),
            format('~w preference updated to ~w~n', [Type, NewValue])
        ;
            format('Error: Value must be between ~w and ~w~n', [Min, Max])
        )
    ;
        writeln('Invalid input. Please enter a number.')
    ),
    press_any_key.

% View Log File
view_log :-
    writeln('+---------------------------------------+'),
    writeln('|             LOG FILE                  |'),
    writeln('+---------------------------------------+'),
    (exists_file('logActions.txt') ->
        writeln('Contents of logActions.txt:'),
        writeln('----------------------------'),
        open('logActions.txt', read, Stream),
        read_file(Stream),
        close(Stream)
    ;
        writeln('Log file does not exist yet.')
    ),
    writeln('+---------------------------------------+'),
    press_any_key.

% Display Functions
display_sensors :-
    writeln('CURRENT SENSOR VALUES:'),
    writeln('---------------------------------------'),
    writeln('ID | Type | Location | Value'),
    writeln('---------------------------------------'),
    forall(sensor(ID, Type), (
        (inside(ID) -> Location = 'Inside' ; Location = 'Outside'),
        sensorValue(ID, Value),
        format('~w | ~w | ~w | ~w~n', [ID, Type, Location, Value])
    )),
    writeln('---------------------------------------').

display_effectors :-
    writeln('CURRENT EFFECTOR STATES:'),
    writeln('---------------------------------------'),
    writeln('ID | Value'),
    writeln('---------------------------------------'),
    forall(effectorValue(ID, Value), (
        format('~w | ~w~n', [ID, Value])
    )),
    writeln('---------------------------------------').

display_preferences :-
    writeln('CURRENT USER PREFERENCES:'),
    writeln('---------------------------------------'),
    writeln('Type | Value | Controlled Effectors'),
    writeln('---------------------------------------'),
    forall(preference(comfort, Type, Value, Effectors), (
        atomic_list_concat(Effectors, ', ', EffStr),
        format('~w | ~w | ~w~n', [Type, Value, EffStr])
    )),
    writeln('---------------------------------------').