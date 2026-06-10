# ZigZag — Word Search

A 5×5 word search game in Scala. Find hidden words by guessing the word, starting cell, and direction.

## Requirements

- Java 21+
- Scala CLI

## Run

```bash
make run   # terminal UI
```

Must be run from the project root so `src/givenWords.txt` is found.

## Controls (TUI)

| Key | Action |
|-----|--------|
| Word + coordinates + direction | Guess a word |
| `N` | New game |
| `R` | Show rules |
| `Q` | Quit |

Directions: `north`, `northeast`, `east`, `southeast`, `south`, `southwest`, `west`, `northwest`

## Adding Words

Edit `src/givenWords.txt`:

```
WORD
(row,col)
(row,col)

NEXTWORD
(row,col)
```

Coordinates are 0-indexed (0–4), one per letter, in order.
