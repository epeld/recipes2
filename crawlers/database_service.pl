:- module(database_service, []).

:- use_module(library(persistency)).


:- persistent
   recipe_instructions(name:text, instructions:text),
   recipe_ingredient(recipe_name:text, ingredient_name:text, amount:number, unit:text, subgroup:text),
   recipe_related(name:text, related_name:text).


attach_recipe_db(File) :-
  db_attach(File).

detach_recipe_db :-
  db_detach.


persist_recipe(Recipe) :-
  recpe_model:recipe_name(Recipe, Name),
  recpe_model:recipe_instructions(Recipe, Instructions),
  recpe_model:recipe_related(Recipe, Related),
  recpe_model:recipe_ingredients(Recipe, Ingredients),
  
  persist_recipe_instructions(Name, Instructions),
  forall(member(Ingredient, Ingredients),
         persist_recipe_ingredients(Name, )).


persist_recipe_ingredients(Recipe) :-
  recpe_model:recipe_name(Recipe, Name),
  recpe_model:recipe_ingredients(Recipe, Ingredients),
  
  forall(member(Ingredient, Ingredients),
         persist_recipe_ingredient(Name, Ingredient)).


persist_recipe_ingredient_model(RecipeName, ingredient(Name)) :-
  assert_recipe_ingredient(RecipeName, Name, none, none, none).

