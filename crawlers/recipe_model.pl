:- module(recipe_model, [recipe_props/2,
                         recipe_related/2]).


recipe_props(Recipe, Props) :-
  Recipe = Props. 


recipe_related(Recipe, Related) :-
  recipe_props(Recipe, Props),
  member(related(Related), Props).
