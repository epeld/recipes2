:- module(recipe_model, [recipe_props/2,
                         recipe_related/2]).


recipe_props(Recipe, Props) :-
  length(Props, 4),
  member(related(_), Props),
  member(instructions(_), Props),
  member(name(_), Props),
  member(ingredients(_), Props),
  Recipe = Props. 


recipe_name(Recipe, Name) :-
  recipe_prop(Recipe, name, Name).

recipe_ingredients(Recipe, Ingredients) :-
  recipe_prop(Recipe, ingredients, Ingredients).

recipe_instructions(Recipe, Instructions) :-
  recipe_prop(Recipe, instructions, Instructions).

recipe_related(Recipe, Related) :-
  recipe_prop(Recipe, related, Related).


recipe_prop(Recipe, Name, Value) :-
  recipe_props(Recipe, Props),
  
  functor(F, Name, 1),
  arg(1, F, Value),
  
  member(F, Props).
