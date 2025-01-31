Things to do
- [x] when getting message from tcp implement a check to make sure you both have the entire message as well as only that message
- [x] change the payer manager to be a seperate gen server that managers all players.
- [x] make the listen socket retry to connect if fails
- [x] change the clock so its sends to all players

- [ ] for arrays of data in the get by packet id, have a list with [type of packets in array, length of array, array]

- [ ] need to add array of x, optional x, enum x, decode varlong, entity metadata, byte array to decoder.

- [ ] uuid needs to be gotten from microsofts servers.

- [ ] implement an EID system

- [ ] chunk generation

- [ ] add a tick every 1/20 of a second which updates the chunk manager and payer manager.

- [ ] finish the serverbound messages. 

- [ ] change the state update after handshake to set state of play to state if needed

- [ ] handle the state of play state messages


