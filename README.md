# Functional and Logical Programming (PFL) Project 2

## Group T06_G09 - Replica_7

| Name                                           | E-mail            | Contribution | Tasks Developed                                             |
| ---------------------------------------------- | ----------------- | -----------: | ----------------------------------------------------------- |
| Bruno Ricardo Soares Pereira de Sousa Oliveira | up202208700@up.pt |          50% | Development and documentation of **board** and **game** logic       |
| Rodrigo Albergaria Coelho e Silva              | up202205188@up.pt |          50% | Development and documentation of **input** and **interface** system |

## Installation and Execution

```sh
sicstus -l src/game.pl
```

## Description of the Game

## Considerations for Game Extensions

## Game Logic

### Game Configuration Representation

### Internal Game State Representation

### Move Representation

### User Interaction

> The numbers are read as the resulting integer from concatenating all the digits in the input string (i.e. skipping all the junk that may be put there)

> The strings are the concatenation of all the chars with ASCII codes between 32-127, which includes spaces, numbers, letters and a bunch of symbols (the rest of characters is skipped)

> The coordinates are referenced like a spreadsheet with input where containing a sequence of letters and numbers. Letters refer to the column and numbers to the row, letters that don't exist in the range are skipped automatically and the first valid letter is considered. The same applies for numbers. For boards larger than 26, it is assumed spreadsheet-like index continuing from AA, AB, AC...  
> As an example, all of the following coordinates represent the same position in an 8x8 board: `a8`, `az8`, `ai8`, `8a`, `89za`, ` 8 _ a`.

## Conclusions

## References
