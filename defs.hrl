% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   nick: the nick of the user
%   connected: the server information, if not connected its false
%   channels: the information for all the channels/chatroom that the user is connected to
-record(client_st, {gui,nick,connected,channels}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
%   name: server name
%   users: a list of all connected users
%   channels: a list of all created channel/chatroom processes
-record(server_st, {name, users = [nick],channels = [channel]}).

% Record to define the structure of the chatroom process.
%   name: the name of the chatroom
%   users: a list of user nicks and their pids
-record(chat_st, {name,users = [{cpid,nick}]}).
