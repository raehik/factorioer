Main function should be `bestNextTurns` or something? That prints a list of best
next turns (for the next player, not necessarily you) to take according to
player knowledge.

Main knowledge should be simply a list of queries! Some queries will be from
you, so you'll have full info regarding the shown card. Others you'll only be
able to mark off "maybes". This object can be converted into a knowledge graph
at any time - find some clean way to cache it later if needed.

Should try to ask about cards which have least info.
