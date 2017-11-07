CREATE TABLE IF NOT EXISTS recipes (
       id INT NOT NULL AUTO_INCREMENT,
       name VARCHAR(100) NOT NULL UNIQUE,
       description VARCHAR(300),
       PRIMARY KEY (id)
       );


CREATE TABLE IF NOT EXISTS ingredients (
       id INT NOT NULL AUTO_INCREMENT,
       name VARCHAR(100) NOT NULL,
       PRIMARY KEY (id),
       UNIQUE (name)
       );


CREATE TABLE IF NOT EXISTS recipe_ingredients (
       id INT NOT NULL AUTO_INCREMENT,
       recipe_id INT NOT NULL,
       ingredient_id INT NOT NULL,
       amount DECIMAL NOT NULL,
       unit VARCHAR(50),
       FOREIGN KEY (recipe_id) REFERENCES recipes(id) ON DELETE CASCADE,
       FOREIGN KEY (ingredient_id) REFERENCES ingredients(id) ON DELETE CASCADE,
       PRIMARY KEY (id),
       UNIQUE (recipe_id, ingredient_id));


--
-- Procedure Definitions
--
DELIMITER $$
DROP PROCEDURE IF EXISTS add_recipe $$
CREATE PROCEDURE add_recipe (IN name VARCHAR(100), IN description VARCHAR(300))
BEGIN
INSERT INTO recipes (name, description) VALUES (name, description);
END $$


DROP PROCEDURE IF EXISTS add_ingredient $$
CREATE PROCEDURE add_ingredient (IN recipe_id INT, IN name VARCHAR(100), IN amount DECIMAL, IN unit VARCHAR(50))
BEGIN
SET @foo = name;
INSERT IGNORE INTO ingredients (name) VALUES (name);
SET @ingredient = (SELECT id FROM ingredients WHERE ingredients.name = @foo FOR UPDATE);
INSERT INTO recipe_ingredients (recipe_id, ingredient_id, amount, unit) VALUES (recipe_id, @ingredient, amount, unit);
END $$


DROP PROCEDURE IF EXISTS full_recipes $$
CREATE procedure full_recipes ()
BEGIN
SELECT recipes.id, recipes.name, recipes.description, GROUP_CONCAT(ingredients.name) as ingredients
FROM recipe_ingredients INNER JOIN recipes ON recipes.id = recipe_ingredients.recipe_id INNER JOIN ingredients ON ingredients.id = recipe_ingredients.ingredient_id
GROUP BY recipe_id;
END $$

DROP PROCEDURE IF EXISTS drop_tables $$
CREATE procedure drop_tables ()
BEGIN
DROP TABLE recipe_ingredients, ingredients, recipes;
END $$

DELIMITER ;


--
-- Some Sample Recipes
--
CALL add_recipe('Pancakes', 'Fry ''em');
SET @recipe = LAST_INSERT_ID();
CALL add_ingredient(@recipe, 'Eggs', 2, NULL);
CALL add_ingredient(@recipe, 'Milk', 2, 'dl');
CALL add_ingredient(@recipe, 'Flour', 1, 'dl');

CALL add_recipe('Meatballs', 'Roll ''em, then fry ''em');
SET @recipe = LAST_INSERT_ID();
CALL add_ingredient(@recipe, 'Eggs', 1, NULL);
CALL add_ingredient(@recipe, 'Minced Meat', 400, 'g');
CALL add_ingredient(@recipe, 'Onion', 1, NULL);

CALL add_recipe('Pasta', 'Boil it in water');
SET @recipe = LAST_INSERT_ID();
CALL add_ingredient(@recipe, 'Pasta', 200, 'g');
CALL add_ingredient(@recipe, 'Water', 1, 'L');

-- CALL drop_tables();

-- CALL full_recipes();
