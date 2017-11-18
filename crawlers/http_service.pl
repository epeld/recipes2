:- module(http_service, [get_html/2]).

:- use_module(library(http/http_client)).
:- use_module(stream_service, [call_with_stream/2]).

:- use_module(library(tabling)).
:- table get_html/2.

user_agent('Mozilla/5.0 (X11; Linux x86_64; rv:56.0) Gecko/20100101').


get_html(Url, Contents) :-
  call_with_stream(http_get_stream(Url), load_html_stream(Contents)).



% Helper
http_get_stream(Url, WriteStream) :-
  user_agent(Agent),
  http_get(Url, _Data, [to(stream(WriteStream)), user_agent(Agent)]).

% Helper
load_html_stream(Contents, ReadStream) :-
  load_html(stream(ReadStream), Contents, []).
