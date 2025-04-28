:- module(utils, [
    log_effector_change/5,
    log_effector_unchanged/3,
    log_summary/1,
    read_file/1,
    get_line/2,
    press_any_key/0
]).

:- use_module(core).

% -----------------------
% Logging Functions
% -----------------------
log_effector_change(Effect, OldVal, NewVal, DateTime, Reason) :-
    open('logActions.txt', append, Stream),
    format(Stream, '[~w] CHANGED: ~w from ~w to ~w | Reason: ~w~n', [DateTime, Effect, OldVal, NewVal, Reason]),
    close(Stream).

log_effector_unchanged(Effect, Val, DateTime) :-
    open('logActions.txt', append, Stream),
    format(Stream, '[~w] UNCHANGED: ~w remains at ~w~n', [DateTime, Effect, Val]),
    close(Stream).

log_summary(DateTime) :-
    open('logActions.txt', append, Stream),
    writeln(Stream, '--------------------------------------------------'),
    format(Stream, '[~w] SYSTEM SUMMARY:~n', [DateTime]),
    forall(effectorValue(ID, Val), format(Stream, 'Effector ~w final state: ~w~n', [ID, Val])),
    writeln(Stream, '--------------------------------------------------'),
    close(Stream).

% File reading utilities
read_file(Stream) :-
    \+ at_end_of_stream(Stream),
    !,
    get_line(Stream, Line),
    writeln(Line),
    read_file(Stream).
read_file(_).

get_line(Stream, Line) :-
    read_line_to_codes(Stream, Codes),
    atom_codes(Line, Codes).

% Utility function for UI
press_any_key :-
    writeln('Press Enter to continue...'),
    read_line_to_codes(user_input, _).