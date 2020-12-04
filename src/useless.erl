-module(useless).

-export([
    add/2,
    hello/0,
    greet_and_add_two/1,
    greet/2,
    help_me/1,
    insert/2
]).

add(A, B) ->
    A + B.

%% Shows greetings
hello() ->
    io:format("Hello, world!~n").

greet(male, Name) ->
    io:format("Hello Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello ~s!", [Name]).

help_me(Animal) ->
    Talk = if Animal == cat -> "meow";
              Animal == cow -> "moo";
              Animal == dog -> "bark";
              Animal == lion -> "roar"
           end,
    {Animal, "says " ++ Talk ++ "!"}.

insert(X, []) ->
    [X];
insert(X, Set) ->
    case lists:member(X,Set) of
        true -> Set;
        false -> [X|Set]
    end.

greet_and_add_two(X) ->
    hello(),
    add(X, 2).
