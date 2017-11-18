:- module(recept_service, [parse_recipe_dom/2, parse_links/2]).

:- use_module(library(xpath)).

parse_recipe_dom(Contents, recipe(Name, Instructions, Ingredients, Related)) :-
  recipe_name(Contents, Name),
  all_recipe_instructions(Contents, Instructions),
  all_recipe_ingredients(Contents, Ingredients),
  all_related_recipes(Contents, Related).
  

parse_links(Contents, Links) :-
  all_linked_recipes(Contents, Links).



% Find links
linked_recipe(Content, Link) :-
  xpath(Content, //a(@href = Link), _),
  starts_with("/recept/", Link).

all_linked_recipes(Content, Links) :-
  setof(Link, linked_recipe(Content, Link), Links).

starts_with(Prefix, String) :-
  sub_string(String, 0, _, _, Prefix).

all_related_recipes(Contents, Hrefs) :-
  bagof(Href, related_recipe(Contents, Href), Hrefs).

related_recipe(Contents, other(Href)) :-
  xpath(Contents, //div(@class='view-content')/div(a(@href=Href)), _).

related_recipe(Contents, self(Href)) :-
  xpath(Contents, //form(@class='fivestar-widget',@action = Href), _).


% Find name
recipe_name(Contents, Name) :-
  xpath(Contents, //h1(text), Name).


% Find Instructions
recipe_instruction(Contents, Instruction) :-
  xpath(Contents, //ol(@class='instructions',text), Instruction).

all_recipe_instructions(Contents, Instructions) :-
  bagof(I, recipe_instruction(Contents, I), Instructions).


% Find Ingredients
recipe_ingredient(Contents, Ingredient) :-
  xpath(Contents, //div(@class='ingredient',normalize_space), Ingredient0),
  split_string(Ingredient0, " ", "", Parts),
  exactly_once(ingredient_parts(Parts, Ingredient)).


ingredient_parts([Name], ingredient(Name)).
ingredient_parts([Amount, Name], ingredient(Name, Amount)).
ingredient_parts([Amount, Unit, Name], ingredient(Name, Amount, Unit)).
ingredient_parts([Amount, _Name0, "eller", Name], ingredient(Name, Amount)).

ingredient_parts([Amount, Unit | Names], ingredient(Name, Amount, Unit)) :-
  \+ member("eller", Names),
  string_join(Names, Name).

ingredient_parts([Amount, Unit | Names], ingredient(Name, Amount, Unit)) :-
  Parts = [_Part1, ["eller"], Part2],
  append(Parts, Names),
  member("eller", Names),
  string_join(Part2, Name).




all_recipe_ingredients(Contents, Ingredients) :-
  bagof(I, recipe_ingredient(Contents, I), Ingredients).



% Helper that matches its goal exactly once (throws if fails)
exactly_once(Goal) :-
  once(Goal), ! ; throw(failed(Goal)).



string_concat_space(S, "", S).
string_concat_space(S1, S2, S3) :-
  \+ S2 = "",
  string_concat(S1, " ", S1_0),
  string_concat(S1_0, S2, S3).

string_join(Strings, String) :-
  reverse(Strings, Strings0),
  foldl(string_concat_space, Strings0, "", String).
