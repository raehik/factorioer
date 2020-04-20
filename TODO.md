## Crafting
### Flexibility for Factorio compatibility
  * recipe name even with just one product (e.g. "Basic oil processing")
    * recipe API has been written with this in mind, wouldn't be hard to add a
      `recipeName` field in to replace the current function (which
      retrieves/derives it from the `recipeProduct` field)
  * crafters, may have to tinker with them to get expected behaviour
