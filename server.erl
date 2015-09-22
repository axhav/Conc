-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{users = []}.

%% ---------------------------------------------------------------------------

loop(St = #server_st{users = Users}, Message) ->
    io:format("server stuff"),
    case Message of
        {connect,CId,Nick} ->
            Temp = lists:any(fun({X,_}) -> X ==CId end,Users),
            Temp2= lists:any(fun({_,X}) -> X==Nick end,Users),
            if
                Temp ->
                    CId ! {error, user_already_connected,"failure"},
                    {error, user_already_connected,"failure"};
                Temp2 ->
                    CId ! {error, nick_already_in_use,"failure"},
                    {error,nick_already_in_use,"failure"};
                true ->
                    NUsers = lists:append(Users,[{CId,Nick}]),
                    CId ! {ok, self()},
                    NSt = St#server_st{users = NUsers},
                    {ok,NSt}
            end;
        {disconnect,CId} ->
            NUsers = lists:filter(fun({X,_}) -> X /= CId end , Users),
            CId ! {ok, self()},
            NSt = St#server_st{users = NUsers},
            {ok,NSt}
    end.
    

    %{not_implemented, St}.
