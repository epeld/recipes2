:- module(recipe_model, [make_recipe/2,
                         default_recipe/1,
                         recipe_name/2,
                         recipe_instructions/2,
                         recipe_ingredients/2,
                         recipe_related/2
                        ]).

:- use_module(library(record)).

:- record
   recipe(name:text, instructions:text, ingredients:list, related:list).

