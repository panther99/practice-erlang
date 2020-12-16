-module(kitchen).
-author("Nikola Stojaković").

-export([
   fridge1/0,
   fridge2/1,
   store/2,
   take/2,
   start/1,
   important/0,
   normal/0
]).

% First fridge has no way to store the food - therefore it's useless!

fridge1() ->
    receive
        {From, {store, _Food}} ->
            From ! {self(), ok},
            fridge1();
        {From, {take, _Food}} ->
            From ! {self(), not_found},
            fridge1();
        terminate ->
            ok
    end.

% Second fridge maintains a FoodList which has content of the fridge. When we want to
% take stuff we just have to check if the item exists and remove it from the list. When
% we want to add new stuff we just append an item in the FoodList.

fridge2(FoodList) ->
    receive
        {From, {store, Food}} ->
            From ! {self(), ok},
            fridge2([Food|FoodList]);
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true ->
                    From ! {self(), {ok, Food}},
                    fridge2(lists:delete(Food, FoodList));
                false ->
                    From ! {self(), not_found},
                    fridge2(FoodList)
            end;
        terminate ->
            ok
    end.

% In previous examples, user of the functions had to know the protocol for storing and taking
% stuff. It's a cumbersome burden - we'll create two methods which will allow us to make a
% simple interface for user. We'll also stop waiting for the message after 3 seconds.

store(Pid, Food) ->
    Pid ! {self(), {store, Food}},
    receive
        {Pid, Msg} -> Msg
    after 3000 ->
        timeout
    end.

take(Pid, Food) ->
    Pid ! {self(), {take, Food}},
    receive
        {Pid, Msg} -> Msg
    after 3000 ->
        timeout
    end.

% We'll add a function start which will allow user to easily create new fridge process

start(FoodList) ->
    spawn(?MODULE, fridge2, [FoodList]).

% We'll accept only important messages in the important list and push less important ones
% to the normal list

important() ->
    receive
        {Priority, Message} when Priority > 10 ->
            [Message | important()]
    after 0 ->
        normal()
    end.

normal() ->
    receive
        {_, Message} ->
            [Message | normal()]
    after 0 ->
        []
    end.
