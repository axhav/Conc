-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName,users = [],channels = []}.

%% ---------------------------------------------------------------------------

loop(St = #server_st{users = Users,channels = Channels}, Message) ->
    %io:format("server stuff ~p~n",[Message]),
    case Message of
        {connect,Nick} ->
            Temp2= lists:any(fun(X) -> X==Nick end,Users),
            if
                Temp2 ->
                    {error,St};
                true ->
                    NUsers = lists:append(Users,[Nick]),
                    NSt = St#server_st{users = NUsers},
                    %io:format("server stuff ~p~n",NUsers),
                    {ok,NSt}
            end;
        {disconnect,Nick} ->
            NUsers = lists:filter(fun(X) -> X /= Nick end , Users),
            NSt = St#server_st{users = NUsers},
            {ok,NSt};
        {join,Pid,Nick,Channel} -> 
            Temp2 = lists:any(fun(X) -> X==Channel end, Channels),
            if
                Temp2 == false -> 
                    genserver:start(list_to_atom(Channel),initial_cstate(Channel),fun chatroom/2),
                    %io:format("server stuff in join~n"),
                    Result = genserver:request(list_to_atom(Channel),{join,Pid,Nick}),
                    %io:format("server stuff in join~n"),
                    NChannels =lists:append(Channels,[Channel]),
                    NSt = St#server_st{channels = NChannels},
                    case Result of
                        ok -> 
                            {ok,NSt};
                        _ ->
                            {error,NSt}
                    end;
                true -> 
                    Result = genserver:request(list_to_atom(Channel),{join,Pid,Nick}),
                    case Result of
                        ok -> 
                            {ok,St};
                        _ ->
                            {error,St}
                    end
            end;
        {leave,Nick,Channel} ->
            %io:format("server stuff in leave enter ~p~n",Channels),
            Temp2 = lists:any(fun(X) -> X==Channel end, Channels),
            if
                Temp2 ->
                   % io:format("server stuff in leave~p~n",Channels),
                    Result = genserver:request(list_to_atom(Channel),{leave,Nick}),
                    case Result of
                        ok -> 
                            NUsers = lists:filter(fun(X) -> X /= Nick end,Users),
                            NSt = St#server_st{users = NUsers},
                            {ok,NSt};
                        _ -> 
                            {error,St}
                        end;
                true ->
                    {error, St}
            end;
        {msg,Nick, Channel, Msg} ->
            genserver:request(list_to_atom(Channel),{send,Nick,Msg}),
            {ok,St}
    end.
    

initial_cstate(ChatName) ->
    #chat_st{name = ChatName,users = []}.
    
chatroom(St = #chat_st{name = ChatName,users = Users}, Message) ->
    %io:format("server stuff in channel~n"),
    case Message of
        {join,Pid,Nick} ->
            Temp2 = lists:any(fun({_,X}) -> X==Nick end, Users),
            if
                Temp2 -> 
                    {error,St};
                true ->
                    NUsers = lists:append(Users,[{Pid,Nick}]),
                    NSt = St#chat_st{users = NUsers},
                    {ok,NSt}
                    
            end;
        {leave,Nick} ->
            NUsers = lists:filter(fun({_,X}) -> X /= Nick end, Users),
            NSt = St#chat_st{users = NUsers},
            L1 = length(Users),
            L2 = length(NUsers),
            %io:format("server stuff in if ~p~p~n",[Users,NUsers]),
            if
                L2 == L1 ->
                    {error,St};
                true ->
                    {ok,NSt}
            end;
        {send,Nick,Msg} ->
            [ genserver:request(P,{incoming_msg, ChatName, Nick, Msg})||  {P,U} <- Users,U /= Nick ],
            {ok,St}  
    end.
    
    
    
    
                    %NChannels = [if 
                    %    C == Channel -> {C,lists:append(U,Nick)};
                    %   true -> {C,U} end 
                    %    || {C,U} <- Channels];