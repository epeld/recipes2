:- module(ingredient_model, [make_ingredient/2,
                             default_ingredient/1,
                             ingredient_name/2,
                             ingredient_subgroup/2,
                             ingredient_unit/2,
                             ingredient_amount/2
                            ]).

:- use_module(library(record)).


:- record ingredient(name:text,
                     amount:any,
                     unit:text = none,
                     subgroup:text = none).
