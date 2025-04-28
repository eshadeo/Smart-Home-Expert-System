% Main entry point for the Smart Home Management System

:- use_module(core).
:- use_module(ui).
:- use_module(default_values).

% Run the System
start :-
    initialize_system,
    writeln('System initialized with default values.'),
    main_menu.

:- initialization(start).