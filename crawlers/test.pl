:- use_module(library(xpath)).
:- use_module(library(http/http_client)).

:- use_module(recept, []).

recipe_url('http://recept.se/recept/tacopaj').

user_agent('Mozilla/5.0 (X11; Linux x86_64; rv:56.0) Gecko/20100101').

load_recipe_url(Recipe) :-
  recipe_url(Url),
  load_recipe_url(Url, Recipe).



parse_recipe_test(Recipe) :-
  load_html('test.html', Contents, []),
  recept:parse_recipe_dom(Contents, Recipe).


load_recipe_url(Url, Recipe) :-
  recipe_url(Url),
  load_html('test.html', Contents, []),
  %call_with_stream(http_get_stream(Url), load_html_stream(Contents)),
  !,
  parse_related_recipes(Contents, Recipe).
%parse_recipe_dom(Contents, Recipe).





% Helper
http_get_stream(Url, WriteStream) :-
  user_agent(Agent),
  http_get(Url, _Data, [to(stream(WriteStream)), user_agent(Agent)]).

% Helper
load_html_stream(Contents, ReadStream) :-
  load_html(stream(ReadStream), Contents, []).




  

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
