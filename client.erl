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
        Connected == true ->
            {error, user_already_connected, "User already connected to a server"};
        true ->
            SPid = whereis(list_to_atom(Server)),
            io:format("stuff ~p~n ~p~n",[SPid,self()]),
            %{request, From, Ref, Data} 
            SPid ! {request,self(),self(),{connect, self(), Nick}},
            %SPid ! {connect, self(), Nick},
            receive
                {ok, SPid } -> 
                    St1 = St#client_st{connected = SPid},
                    {ok,St1};
                {ok, Test} ->
                    io:format("second case"),
                    {ok,St}
            after 
                5000 ->
                    {error,server_not_reached,"Server timeout"}
            end
    end;

%% Disconnect from server
loop(St = #client_st{connected = SPid}, disconnect) ->
    SPid ! {request,self(),self(),{disconnect, self()}},
    receive
        {ok, SPid } -> 
            St1 = St#client_st{connected = false},
                {ok,St1}
    after 
        5000 ->
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
