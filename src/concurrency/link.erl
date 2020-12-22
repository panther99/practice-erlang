-module(link).
-author("Nikola Stojaković").

-export([
   spawn_parent_proc/0,
   spawn_parent_proc_atomically/0
]).

% A process which will wait 5 seconds before dying.

proc() ->
    timer:sleep(5000),
    exit(reason).

% A method for spawning process above.

spawn_parent_proc() ->
    Pid = spawn(fun proc/0),
    link(Pid).

% Same as above but safer (linking is done atomically so we avoid
% situations like process dying before it's even linked)

spawn_parent_proc_atomically() ->
    Pid = spawn_link(fun proc/0),
    link(Pid).
