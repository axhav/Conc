% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   nick: the nick of the user
-record(client_st, {gui,nick}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
-record(server_st, {}).
