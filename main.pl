:- [facts].       % Load facts
:- [rules].       % Load rules

run :-
    writeln('Smart Home System Running...'),
    set(comfort, temp),
    set(comfort, light),
    set(comfort, noise),
    writeln('All conditions processed.').
