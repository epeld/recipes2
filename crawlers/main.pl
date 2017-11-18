:- module(main, []).


:- use_module(recept_service, [parse_recipe_dom/2]).


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


load_recipe_url(Url, Recipe) :-
  %load_html('test.html', Contents, []),
  get_html(Url, Contents),
  !,
  parse_recipe_dom(Contents, Recipe).

