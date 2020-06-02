## Next
### Clean up base ingr. algorithm
  * Later: make a safe algorithm which detects cycles
    * should be very easy, treat item as base ingredient if one of its
      ingredients is an item we've already seen
    * test with `MagicPutty <-> FormedMagicPutty`

Fiiiix!

  * State throughout the algorithm: seen items, byproducts.

### Finish the UI model for recipes
Will also include finishing item model: image/sprites mainly.

How to encode **items having various properties for their various uses?** Like
some items connecting power poles, some being belts, etc. In the current model,
that information must be accessible from the item, or requestable from the world
using the item. Thinking that typeclasses may *not* help here. Maybe need to
rewrite `Item` as a typeclass, and then allow adding items which implement
certain properties, and are an instance of `Item`?

Also, distinguish between placeable and non-placeable items. **All items are
droppable.**, not all can be placed. (If you hold a non-placeable item in your
hand in Factorio, the cursor continues to work as an placeable selector.)

Note that for proper emulation, the UI needs access to the world so it if all
recursive ingredients for an item are made only one way, it can tell you the raw
resources (ones which have no recipe) required.

## Crafting
### Flexibility for Factorio compatibility
  * recipe name even with just one product (e.g. "Basic oil processing")
    * recipe API has been written with this in mind, wouldn't be hard to add a
      `recipeName` field in to replace the current function (which
      retrieves/derives it from the `recipeProduct` field)
  * crafters, may have to tinker with them to get expected behaviour
