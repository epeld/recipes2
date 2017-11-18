:- module(main, []).

:- use_module(library(http/http_client)).

:- use_module(recept, []).


base_url('http://recept.se').

recipe_url('/content/fishnchips').
%recipe_url('/recept/ost-och-skinkfyllda-kycklingfileer').

search_url('/recept').

user_agent('Mozilla/5.0 (X11; Linux x86_64; rv:56.0) Gecko/20100101').


list_links :-
  base_url(Base),
  search_url(Rel),
  full_url(Base, Rel, Url),

  load_links_url(Url, Links),
  writeq(Links).

run :-
  stream_property(Stream, alias(user_output)),
  base_url(Base),
  recipe_url(Relative),
  
  !,
  run(Base, Relative, [], Stream).

run(BaseUrl, RecipeUrl, Visited, Stream) :-
  length(Visited, N),
  N < 5,
  
  full_url(BaseUrl, RecipeUrl, Url),
  load_recipe_url(Url, Recipe),
  
  writeq(Stream, Recipe), !,

  % Find next recipe
  recipe_related(Recipe, Links),
  member(other(RecipeUrl2), Links),
  \+ member(RecipeUrl2, Visited),
  !,

  % Recurse
  run(BaseUrl, RecipeUrl2, [RecipeUrl | Visited], Stream).


full_url(BaseUrl, Relative, Url) :-
  string_concat(BaseUrl, Relative, Url).

recipe_related(recipe(_,_,_,R), R).


load_page_content(Url, Contents) :-
  call_with_stream(http_get_stream(Url), load_html_stream(Contents)).

load_recipe_url(Url, Recipe) :-
  %load_html('test.html', Contents, []),
  load_page_content(Url, Contents),
  !,
  recept:parse_recipe_dom(Contents, Recipe).

load_links_url(Url, Recipe) :-
  %load_html('test.html', Contents, []),
  load_page_content(Url, Contents),
  !,
  recept:parse_links(Contents, Recipe).





% Helper
http_get_stream(Url, WriteStream) :-
  user_agent(Agent),
  http_get(Url, _Data, [to(stream(WriteStream)), user_agent(Agent)]).

% Helper
load_html_stream(Contents, ReadStream) :-
  load_html(stream(ReadStream), Contents, []).


  
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
