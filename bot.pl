#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(socket)).
:- use_module(library(random)).
:- use_module(library(thread)).
:- use_module(library(lists)).

:- dynamic server_messages/1.
:- dynamic room_info/1.

main :-
    format("Starting bot client...~n"),
    sleep(2),
    client('localhost', 3333).

client(Host, Port) :-
    format("Attempting to connect to ~w:~w...~n", [Host, Port]),
    (catch(
        setup_call_cleanup(
            tcp_connect(Host:Port, Stream, [timeout(5)]),
            (format("Connection established!~n"),
             thread_create(reader_thread(Stream), _, [detached(true)]),
             bot(Stream)
            ),
            close(Stream)
        ),
        Error,
        (format("Connection error: ~w~n", [Error]),
         sleep(3),
         client(Host, Port))
    ).

% Инициализация хранилища сообщений
:- assertz(server_messages([])).

% Поток для чтения сообщений
reader_thread(Stream) :-
    repeat,
    (catch(
        read_line_to_string(Stream, Line),
        Error,
        (format("Read error: ~w~n", [Error]), fail)
     ),
     (Line == end_of_file
      -> !, true
      ; (retract(server_messages(Current)),
        (Current == [] -> Messages = [Line] ; append(Current, [Line], Messages)),
        assertz(server_messages(Messages)),
        fail
     )
    ).

% Получение сообщений от сервера
get_server_messages(Messages) :-
    server_messages(Messages).

% Получение последнего сообщения
get_last_message(Last) :-
    server_messages(Messages),
    (Messages = [] -> Last = "" ; last(Messages, Last)).

% Очистка сообщений
clear_server_messages :-
    retractall(server_messages(_)),
    assertz(server_messages([])).

% Анализ информации о комнате
parse_room_info(Messages, Enemies) :-
    reverse(Messages, RevMessages),
    (member(Line, RevMessages),
     sub_string(Line, _, _, _, "Enemies")
     -> parse_enemies(Line, Enemies)
     ;  Enemies = []
    ).

% Парсинг информации о врагах
parse_enemies(Line, Enemies) :-
    split_string(Line, ":", "", Parts),
    (length(Parts, 2)
     -> split_string(Parts.2, ", ", "", RawEnemies),
        exclude(==(""), RawEnemies, Enemies)
     ;  Enemies = []
    ).

% Основная логика бота
bot(Stream) :-
    % Регистрация
    format(Stream, 'bot~n', []),
    flush_output(Stream),
    sleep(2),
    
    % Главный цикл
    repeat,
    (look_around(Stream),
     process_room(Stream),
     random_move(Stream),
     sleep(3),
     fail).

% Осмотр комнаты
look_around(Stream) :-
    format(Stream, 'look~n', []),
    flush_output(Stream),
    sleep(1).

% Обработка информации о комнате
process_room(Stream) :-
    get_server_messages(Messages),
    parse_room_info(Messages, Enemies),
    (Enemies = []
     -> format("Room is clear.~n", [])
     ;  random_member(Target, Enemies),
        format("Attacking ~w!~n", [Target]),
        attack(Stream, Target)
    ),
    clear_server_messages.

% Атака врага
attack(Stream, Target) :-
    format(Stream, 'kill ~w~n', [Target]),
    flush_output(Stream),
    sleep(1).

% Случайное перемещение
random_move(Stream) :-
    random_between(1, 4, Direction),
    direction_command(Direction, Command),
    format("Moving ~w~n", [Command]),
    format(Stream, '~w~n', [Command]),
    flush_output(Stream),
    sleep(1).

% Преобразование направления в команду
direction_command(1, 'move north').
direction_command(2, 'move south').
direction_command(3, 'move east').
direction_command(4, 'move west').
