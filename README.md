# Civ IV 🗺️
## INSTALL
1. Navigate to the `civ` directory and build with `dune build`
2. To play the game, type `make play`

## OVERVIEW AND GOAL OF THE GAME
- Civ4 is a turn-based strategy and terminal-based game. 
- You and your computer opponent will take turns to either claim or build on tiles. 
- Points are gained and lost during the game. 
- At the end of 20 rounds, the player with the highest score is proclaimed the winner. 

## GAME PLAY
### Initializing the game
1. Start by entering your name. 
2. Then, choose your civilization, a leader of your civilization, and a starting technology for your civilization. 
  - Civilization options: 
    - Russia 
    - China
    - England
    - America
    - France
  - Note that the starting technology you choose will limit the type of tile that you can build infrastructure on later on in the game. 
3. Once you're done initializing your civilization, your opponent will do the same. 
4. At round 0, you automatically claim tile `(0, 0)`. Next, at round 1, your opponent automatically claims the diagonally opposite title `(10, 10)`.

### Claiming a tile
- You can claim tile `T` if `T` is claimed by neither of the players in the game and `T` is edge adjacent to a tile you have claimed in past rounds. 
- The reward for claiming a tile is 10. This is a one time reward which you will gain immediately after claiming.

### Building on a tile
- You can build on tile `T` if `T` has been claimed by you in a past round and you have the right technology to build on the terrain of `T`. 
- The cost of building on a tile is **50**. This will be deducted immediately when you decide to build. 
- The reward of building on a tile is **20 per round** after the round that you decided to build.

### Important Commands
- `quit`→ exits the game session. Equivalent to `Ctrl+C`
- `Claim`→ Claims a tile on the game map. Player will then be prompted to enter `x` and `y` coordinates.
- `Build`→ Builds a structure upon a player's already claimed tile. Player will then be prompted to enter `x` and `y` coordinates to select where on the map to build. 
- `Skip`→ Player skips their turn. Computer opponent will make a turn and the round will increase. 

## Map Keys 
### Emojis that represent each terrain type: 
- Grassland: 🌾
- Desert: 🐪
- Ocean: 🌊
- Mountains: 🗻
- Forest: 🌲
### Technology needed to build on each terrain type: 
- Grassland: Agriculture
- Desert: The Wheel
- Ocean: Fishing
- Mountains: Mining
- Forest: Hunting
### When a tile is built upon, their display changes according to the rule below:
- Grassland🌾 → 🚜
- Desert 🐫 → 🛣️
- Ocean 🌊 → 🎣
- Mountains 🗻 → 🏭
- Forest🌲→ 🪵
### Tile color codes: The background color of the tiles represent which player, if any, has claimed that space
- White → Unclaimed ⬜ `#ffffff`
- Purple → American ![#d78df2](https://placehold.co/15x15/d78df2/d78df2.png) `#d78df2`
- Orange → Chinese ![#ff5500](https://placehold.co/15x15/ff5500/ff5500.png) `#ff5500`
- Dark Green → English ![#0f5e0b](https://placehold.co/15x15/0f5e0b/0f5e0b.png) `#0f5e0b`
- Navy → French ![#082785](https://placehold.co/15x15/082785/082785.png) `#082785`
- Brown-Green → Russian ![#334220](https://placehold.co/15x15/334220/334220.png) `#334220`


## Reference table
| Country | Leaders                        | Legal Build Tiles (depending on technology) | Technologies             | Claimed Tile Color                                       |
|---------|--------------------------------|---------------------------------------------|--------------------------|----------------------------------------------------------|
| Russia  | Catherine, Peter, Stalin       | Mountain 🗻, Forest🌲                         | Mining 🏭, Hunting 🪵      | ![#334220](https://placehold.co/15x15/334220/334220.png) |
| America | Lincoln, Washington, Roosevelt | Grassland 🌾, Ocean 🌊                        | Agriculture 🚜, Fishing 🎣 | ![#d78df2](https://placehold.co/15x15/d78df2/d78df2.png) |
| China   | Mao Zedong, Qin Shi Huang      | Mountain 🗻, Grassland 🌾                     | Agriculture 🚜, Mining 🏭  | ![#ff5500](https://placehold.co/15x15/ff5500/ff5500.png) |
| England | Elizabeth, Churchill, Victoria | Mountain 🗻, Ocean 🌊                         | Fishing 🎣, Mining 🏭      | ![#0f5e0b](https://placehold.co/15x15/0f5e0b/0f5e0b.png) |
| France  | Louis XIV, De Gaulle, Napoleon | Grassland 🌾, Desert 🐪                       | Agriculture 🚜, Roads 🛣️   | ![#082785](https://placehold.co/15x15/082785/082785.png) |

Full project commit contributions available upon request.
