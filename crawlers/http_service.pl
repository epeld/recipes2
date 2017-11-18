:- module(http_service, [get_html/2]).

:- use_module(library(http/http_client)).

user_agent('Mozilla/5.0 (X11; Linux x86_64; rv:56.0) Gecko/20100101').


get_html(Url, Contents) :-
  call_with_stream(http_get_stream(Url), load_html_stream(Contents)).


% Helper for working with streams
call_with_stream(WriteGoal, ReadGoal) :-
  new_memory_file(Handle),
  % Write
  open_memory_file(Handle, write, W, [encoding(octet)]),
  call(WriteGoal, W),
  close(W),

  % Read
  open_memory_file(Handle, read, R, [encoding(octet)]),
  call(ReadGoal, R),
  close(R),

  free_memory_file(Handle).



% Helper
http_get_stream(Url, WriteStream) :-
  user_agent(Agent),
  http_get(Url, _Data, [to(stream(WriteStream)), user_agent(Agent)]).

% Helper
load_html_stream(Contents, ReadStream) :-
  load_html(stream(ReadStream), Contents, []).
