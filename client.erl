-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, nick = Nick, connected = false }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St = #client_st{nick = Nick,connected = Connected}, {connect, Server}) ->
    if
        Connected /= false ->
            {error, user_already_connected, "User already connected to a server"};
        true ->
            SPid = whereis(list_to_atom(Server)),
            io:format("stuff ~p~n ~p~n",[SPid,self()]),
            Result = genserver:request(SPid,{connect,Nick}),
            case Result of
                ok ->
                    St1 = St#client_st{connected = SPid},
                    {ok,St1};
                true ->
                    {error,server_not_reached,"Server timeout"}
            end
    end;

%% Disconnect from server
loop(St = #client_st{nick = Nick,connected = SPid}, disconnect) ->
    Result = genserver:request(SPid,{disconnect,Nick}),
    %SPid ! {request,self(),self(),{disconnect, self()}},
    case Result of
        ok -> 
            St1 = St#client_st{connected = false},
            {ok,St1};
        true ->
            {error,server_not_reached,"Server timeout"}
    end;
    %{{error, not_implemented, "Not implemented"}, St} ;

% Join channel
loop(St, {join, Channel}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
loop(St, {leave, Channel}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
loop(St = #client_st{nick = Nick}, whoami) ->
    {Nick, St};


%% Change nick
loop(St = #client_st{connected = Connected}, {nick, Nick}) ->
    if 
        Connected == true ->
            {error,user_already_connected , "Cant change nick while connected"} ;
        true -> 
            St1 = St#client_st{nick = Nick},
            {ok, St1}
    end;
    

%% Incoming message
loop(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
