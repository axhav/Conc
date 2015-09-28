-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, nick = Nick, connected = false,channels = [] }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St = #client_st{nick = Nick,connected = Connected}, {connect, Server}) ->
    if
        Connected /= false ->
            {{error, user_already_connected, "User already connected to a server"},St};
        true ->
            SPid = whereis(list_to_atom(Server)),
            if 
                SPid /= undefined ->
                    %io:format("stuff ~p~n ~p~n",[SPid,self()]),
                    Result = genserver:request(SPid,{connect,Nick}),
                    case Result of
                        ok ->
                            St1 = St#client_st{connected = SPid},
                            {ok,St1};
                        _ ->
                            {{error,user_already_connected,"Some one with that nick is already connected"},St}
                    end;
                true ->
                    {{error,server_not_reached ,"Server not reached"},St}
            end
    end;
    

%% Disconnect from server
loop(St = #client_st{nick = Nick,connected = SPid, channels = Chan}, disconnect) ->
    if
        SPid /= false -> 
            L1 = length(Chan),
            if 
                L1 == 0 ->
                    Result = genserver:request(SPid,{disconnect,Nick}),
                    case Result of
                        ok -> 
                            St1 = St#client_st{connected = false},
                            {ok,St1};
                        true ->
                            {{error,server_not_reached,"Server timeout"},St}
                    end;
                true ->
                    {{error,leave_channels_first,"Leave all channels before disconnecting" },St}
            end;
        true ->
            {{error,user_not_connected,"User not connected to a server"},St}
    end;

            
        
    %{{error, not_implemented, "Not implemented"}, St} ;

% Join channel
loop(St = #client_st{nick = Nick,connected = SPid,channels = Chan}, {join, Channel}) ->
    Result = genserver:request(SPid,{join,self(),Nick,Channel}),
    case Result of
        ok -> 
            NChannel = lists:append(Chan,[Channel]),
            NSt = St#client_st{channels = NChannel},
            {ok, NSt};
        _ -> 
            {{error,user_already_joined,"User already joined the channel"},St}
    end;
    %{{error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
loop(St = #client_st{nick = Nick,connected = SPid,channels = Chan}, {leave, Channel}) ->
    Result = genserver:request(SPid,{leave,Nick,Channel}),
    case Result of
        ok -> 
            NChannel = lists:filter(fun(X) -> X/=Channel end,Chan),
            NSt = St#client_st{channels = NChannel},
            {ok, NSt};
        _ ->
            {{error,user_not_joined,"User not connected to a channel"},St}
    end;
    %{{error, not_implemented, "Not implemented"}, St} ;

% Sending messages
loop(St = #client_st{nick = Nick,connected = SPid,channels = Chan}, {msg_from_GUI, Channel, Msg}) ->
    Temp = lists:any(fun(X) -> X ==Channel end,  Chan),
    if 
        Temp -> 
            Result = genserver:request(list_to_atom(Channel),{send,Nick,Msg}),
            {ok, St} ;
        true -> 
            {{error,user_not_joined,"User is not connected to that channel"},St}
    end;
    % {{error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
loop(St = #client_st{nick = Nick}, whoami) ->
    {Nick, St};


%% Change nick
loop(St = #client_st{connected = Connected}, {nick, Nick}) ->
    if 
        Connected == true ->
            {{error,user_already_connected , "Cant change nick while connected"},St} ;
        true -> 
            St1 = St#client_st{nick = Nick},
            {ok, St1}
    end;
    

%% Incoming message
loop(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
