# ZigZag — Word Search Game

A terminal-based word search game. The game places hidden words on a 5×5 board and challenges the player to find them by guessing the word, its starting coordinates, and direction.

## How It Works

- The board is 5×5 and filled with random uppercase letters.
- Words defined in `src/givenWords.txt` are embedded in the board at fixed positions.
- The player guesses a word, its starting cell (x, y), and the direction it reads in.
- The game tracks the number of tries and correct finds.

## Controls

| Input                                 | Action                 |
| ------------------------------------- | ---------------------- |
| Type a word + coordinates + direction | Attempt to find a word |
| `N`                                   | Start a new game       |
| `R`                                   | Show the rules again   |
| `Q`                                   | Quit                   |

**Directions:** `north`, `northeast`, `east`, `southeast`, `south`, `southwest`, `west`, `northwest`

## Project Structure

```
src/
├── ZigZag.scala       # Entry point and main game loop
├── ZigZagUtils.scala  # Board logic, file parsing, display functions
├── MyRandom.scala     # Custom LCG random number generator
└── givenWords.txt     # Words and their board coordinates
```

## Requirements

- Java 21+
- Scala 2.13.12
- IntelliJ IDEA (recommended — project includes `.idea/` config)

## Running the Game

### IntelliJ IDEA

1. Open the project folder in IntelliJ.
2. Make sure the Scala plugin and SDK 2.13.12 are configured.
3. Run the `ZigZag` object.

### Command Line (Scala CLI)

Run directly from the project root — no separate compile step needed:

```bash
scala run src/ZigZag.scala src/ZigZagUtils.scala src/MyRandom.scala
```

> **Note:** Must be run from the project root so `src/givenWords.txt` is found correctly.

## Adding Words

Edit `src/givenWords.txt` following this format:

```
WORD
(row,col)
(row,col)
...

NEXTWORD
(row,col)
...
```

Each coordinate is a cell the word passes through, in order. Rows and columns are 0-indexed (0–4).
