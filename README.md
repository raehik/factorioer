# Factorioer
**Main goal:** Model some of Factorio's subsystems.

  * Crafting
  * World: tiles, placing things
  * Belt mechanics

## Documentation
### Crafting
In Factorio, different crafters function differently:

  * Players can select a recipe to craft from the inventory screen.
  * Assembler machines are configured to craft a single recipe.
  * Furnaces have a set recipe to craft for each individual valid input.

In the Factorioer model, there are two primary groups of Crafters: Players and
CraftMachines.

#### Players
Players are boring and simple.

#### CraftMachines
Depending on the recipes available to it in the world (not just the unlocked
ones), a CraftMachine chooses a *mode*:

  * If for every recipe the CraftMachine can craft, any given state of input can
    map to only either 0 or 1 recipe, it sets itself to **auto configure mode**.
  * Else, it sets itself to **manual configure mode**. The player
    must select one recipe for the CraftMachine to craft, which is saved with
    its state.

This way, expected Factorio furnace behaviour emerges naturally through the fact
that furnaces do 1 thing with their inputs only. It also extends furnaces to
support multiple inputs (with some consideration from inserters).

#### Recipes
Recipes are defined separately from Items. They're pretty simple:

  * Ingredients and products
  * 
