-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{users = [],channels = []}.

%% ---------------------------------------------------------------------------

loop(St = #server_st{users = Users,channels = Channels}, Message) ->
    io:format("server stuff ~p~n",[Message]),
    case Message of
        {connect,Nick} ->
            Temp2= lists:any(fun({X}) -> X==Nick end,Users),
            if
                Temp2 ->
                    {error,St};
                true ->
                    NUsers = lists:append(Users,[Nick]),
                    NSt = St#server_st{users = NUsers},
                    {ok,NSt}
            end;
        {disconnect,Nick} ->
            NUsers = lists:filter(fun(X) -> X /= Nick end , Users),
            NSt = St#server_st{users = NUsers},
            {ok,NSt};
        {join,Nick,Pid,Channel} -> 
            Temp2 = lists:any(fun({X,_}) -> X==Channel end, Channels),
            if
                Temp2 == false -> 
                    
                    genserver:start(list_to_atom(Channel),initial_cstate(Channel),fun chatroom/2),
                    io:format("server stuff in join~n"),
                    genserver:request(list_to_atom(Channel),{join,Pid,Nick}),
                    io:format("server stuff in join~n"),
                    NChannels =lists:append(Channels,[Channel]),
                    NSt = St#server_st{channels = NChannels},
                    {whereis(list_to_atom(Channel)),NSt};
                true -> 
                    {whereis(list_to_atom(Channel)),St}
            end
    end.
    

initial_cstate(ChatName) ->
    #chat_st{name = ChatName,users = []}.
    
chatroom(St = #chat_st{name = ChatName,users = Users}, Message) ->
    io:format("server stuff in channel~n"),
    case Message of
        {join,Pid,Nick} ->
            NSt = lists:append(Users,[{Pid,Nick}]),
            {ok,NSt};
        {leave,Nick} ->
            NSt = lists:delete(fun({_,X}) -> X == Nick end, Users),
            {ok,NSt};
        {send,Nick,Msg} ->
            [if U /= Nick -> genserver:request(P,{incoming_msg, ChatName, Nick, Msg}) end || {P,U} <- Users],
            {ok,St}
    end.
    
    
    
    
    
    
                    %NChannels = [if 
                    %    C == Channel -> {C,lists:append(U,Nick)};
                    %   true -> {C,U} end 
                    %    || {C,U} <- Channels];