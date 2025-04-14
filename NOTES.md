Figuring our If a player is within 8 chunks of an Action
===

Make an ordered Set of all of the cnages (with the key being the chunk coordinates of any changes), then check anything above -9, -9 of the players current chunk coordinates using somthing like select/2, then send all of these changes to the player.

Do a similar thing for chekinbg what chunks need to be loaded by each player

