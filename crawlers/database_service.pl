:- module(database_service, []).

:- use_module(library(persistency)).


:- persistent
   recipe_instructions(name:text, instructions:text),
   recipe_ingredient(recipe_name:text, ingredient_name:text, amount:any, unit:text, subgroup:text),
   recipe_related(name:text, related_name:text).


attach_recipe_db(File) :-
  db_attach(File).

detach_recipe_db :-
  db_detach.


persist_recipe(Recipe) :-
  persist_recipe_instructions(Recipe),
  persist_recipe_ingredients(Recipe).


persist_recipe_instructions(Recipe) :-
  recpe_model:recipe_name(Recipe, Name),
  recpe_model:recipe_instructions(Recipe, Instructions),

  retractall_recipe_instructions(Name, _),
  assert_recipe_instructions(Name, Instructions).


persist_recipe_ingredients(Recipe) :-
  recpe_model:recipe_name(Recipe, RecipeName),
  recpe_model:recipe_ingredients(Recipe, Ingredients),

  with_mutex(database_service,
             (
               retractall_recipe_ingredient(RecipeName, _, _, _, _),
               forall(member(Ingredient, Ingredients),
                      persist_recipe_ingredient_model(Recipe, Ingredient))
             )).


persist_recipe_ingredient_model(Recipe, Ingredient) :-
  recipe_model:recipe_name(Recipe, RecipeName),
  
  ingredient_model:ingredient_name(Ingredient, IngredientName),
  ingredient_model:ingredient_amount(Ingredient, Amount),
  ingredient_model:ingredient_subgroup(Ingredient, Subgroup),
  ingredient_model:ingredient_unit(Ingredient, Unit),
  
  assert_recipe_ingredient(RecipeName, IngredientName, Amount, Unit, Subgroup).

