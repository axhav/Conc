-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{users = []}.

%% ---------------------------------------------------------------------------

loop(St = #server_st{users = Users}, Message) ->
    io:format("server stuff ~p~n",[Message]),
    case Message of
        {connect,Nick} ->
            Temp2= lists:any(fun({X}) -> X==Nick end,Users),
            if
                Temp2 ->
                    {error,St};
                true ->
                    NUsers = lists:append(Users,[{Nick}]),
                    NSt = St#server_st{users = NUsers},
                    {ok,NSt}
            end;
        {disconnect,Nick} ->
            NUsers = lists:filter(fun({X}) -> X /= Nick end , Users),
            NSt = St#server_st{users = NUsers},
            {ok,NSt}
    end.
    

    %{not_implemented, St}.
