#!/usr/bin/env swipl
 
:- initialization(main, main).
:- use_module(library(socket)).
:- use_module(library(random)).
:- use_module(library(thread)).
:- use_module(library(lists)).
:- dynamic server_messages/1.
:- dynamic room_info/1. % Теперь храним только врагов
 
main :-
    sleep(5),
    client(localhost, 3333).
 
client(Host, Port) :-
    setup_call_cleanup(
        tcp_connect(Host:Port, Stream, []),
        (   thread_create(reader_thread(Stream), _, [detached(true)]),
            bot(Stream)
        ),
        close(Stream)
    ).
 
% Инициализация хранилища сообщений
:- assertz(server_messages([])).
 
% Поток для чтения сообщений от сервера
reader_thread(Stream) :-
    repeat,
    (   read_line_to_string(Stream, Line),
        (   Line == end_of_file
        ->  true, !
        ;   retract(server_messages(Current)),
            append(Current, [Line], New),
            assertz(server_messages(New)),
            fail
        )
    ).
 
get_server_messages(Messages) :-
    server_messages(Messages).
 
get_last_message(Last) :-
    server_messages(Messages),
    (   Messages = [] -> Last = "";
        last(Messages, Last)
    ).
 
clear_server_messages :-
    retractall(server_messages(_)),
    assertz(server_messages([])).
 
% Парсим только информацию о врагах из последней строки
parse_room_info(Messages, Enemies) :-
    last(Messages, LastLine),
    (   sub_string(LastLine, _, _, _, "Enemies"),
        parse_enemies(LastLine, Enemies)
    ->  true
    ;   Enemies = [] % Если строка с врагами не найдена
    ).
 
parse_enemies(Line, Enemies) :-
    % Заменяем все ":" кроме первого на пустую строку
    replace_all_but_first(Line, ":", "", CleanLine),
    
    % Разбиваем строку по оставшемуся ":"
    split_string(CleanLine, ":", "", Parts),
    
    length(Parts, L), 
    (   L > 1
    ->  nth1(2, Parts, EnemiesStr),
        split_string(EnemiesStr, ", ", "", EnemiesList),
        exclude(==(""), EnemiesList, Enemies)
    ;   format('Debug: Not enough parts after splitting~n', []),
        Enemies = []
    ).
 
% Вспомогательный предикат для замены всех вхождений подстроки кроме первого
replace_all_but_first(String, Sub, Replacement, Result) :-
    string_length(Sub, SubLen),
    replace_all_but_first(String, Sub, SubLen, Replacement, false, [], Result).
 
replace_all_but_first(String, Sub, SubLen, Replacement, FoundFirst, Acc, Result) :-
    (sub_string(String, Before, SubLen, After, Sub)
    ->  sub_string(String, 0, Before, _, Prefix),
        (   FoundFirst == false
        ->  % Первое вхождение - оставляем как есть
            append(Acc, [Prefix, Sub], NewAcc),
            RemainingPos is Before + SubLen,
            sub_string(String, RemainingPos, After, 0, Rest),
            replace_all_but_first(Rest, Sub, SubLen, Replacement, true, NewAcc, Result)
        ;   % Последующие вхождения - заменяем
            append(Acc, [Prefix, Replacement], NewAcc),
            RemainingPos is Before + SubLen,
            sub_string(String, RemainingPos, After, 0, Rest),
            replace_all_but_first(Rest, Sub, SubLen, Replacement, true, NewAcc, Result)
        )
    ;   % Больше вхождений не найдено
        append(Acc, [String], NewAcc),
        atomic_list_concat(NewAcc, Result)
    ).
 
bot(Stream) :-
    format(Stream,'~s~n',["bot"]),
    flush_output(Stream),
    sleep(5),
    
    repeat,
    (   
        % Осматриваем комнату
        format('Bot is looking around...~n', []),
        format(Stream, 'look~n', []),
        flush_output(Stream),
        sleep(1),
        get_server_messages(Messages),
        parse_room_info(Messages, Enemies),
        retractall(room_info(_)),
        assertz(room_info(Enemies)),
        
        % Выводим информацию о врагах
        (   Enemies = []
        ->  format('No enemies detected.~n', [])
        ;   format('Enemies detected: ~w~n', [Enemies])
        ),
        clear_server_messages,
        
        % Если есть враги - атакуем случайного
        (   Enemies = []
        ->  true
        ;   random_member(Target, Enemies),
            format('Attacking: ~w~n', [Target]),
            format(Stream, 'kill ~w~n', [Target]),
            flush_output(Stream),
            sleep(1),
            clear_server_messages
        ),
        
        % Пытаемся двигаться
        random_between(1, 4, Direction),
        (   Direction = 1 -> Command = "move north";
            Direction = 2 -> Command = "move south";
            Direction = 3 -> Command = "move east";
            Command = "move west"
        ),
        format('Moving: ~s~n', [Command]),
        format(Stream, '~s~n', [Command]),
        flush_output(Stream),
        
        % Ждем ответа сервера
        sleep(1),
        (   get_last_message(Last),
            Last = "> You can't go that way."
        ->  format('Cannot move that way.~n', [])
        ;   true
        ),
        clear_server_messages,
        sleep(5),
        fail
    ).
