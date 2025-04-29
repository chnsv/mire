#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(socket)).
:- use_module(library(random)).
:- use_module(library(readutil)).

main :-
    sleep(5),
    client(localhost, 3333).

client(Host, Port) :-
    setup_call_cleanup(
        tcp_connect(Host:Port, Stream, []),
        bot(Stream),
        close(Stream)
    ).

bot(Stream) :-
    % Read the initial message from the server

    format(Stream,'~s~n',"bot"),
    flush_output(Stream),

    % Main loop for sending commands
    repeat,
    (   % Randomly choose a direction to move
        random_between(1, 4, Direction),
        (   Direction = 1 -> Command = "move north";
            Direction = 2 -> Command = "move south";
            Direction = 3 -> Command = "move east";
            Command = "move west"
        ),
        format(Stream, '~s~n', [Command]),
        flush_output(Stream),
        format('Sent command: ~s~n', [Command]),
        sleep(2),
        flush_output(Stream),
        fail  % Continue the loop
    ).
