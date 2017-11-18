:- module(main, []).


:- use_module(recipe_model, [recipe_related/2]).
:- use_module(recept_service, [parse_recipe_dom/2,
                               base_url/1,
                               recipe_url/1]).


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
  get_html(Url, DOM),

  !,
  parse_recipe_dom(DOM, Recipe),
  writeq(Stream, Recipe),
  !,

  next_recipe_url(Recipe, Visited, RecipeUrl2),
  !,

  % Recurse
  run(BaseUrl, RecipeUrl2, [RecipeUrl | Visited], Stream).


full_url(BaseUrl, Relative, Url) :-
  string_concat(BaseUrl, Relative, Url).


next_recipe_url(Recipe, Visited, RecipeUrl) :-
  recipe_related(Recipe, Links),
  member(other(RecipeUrl), Links),
  \+ member(RecipeUrl, Visited).



% Util
chain(Goal, Arg, Result) :-
  Goal = (Goal1, Goal2) *->
    call(Goal1, Arg, Arg2),
    chain(Goal2, Arg2, Result)
  
  ; call(Goal, Arg, Result).
