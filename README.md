# rps-plutus-v2
The first proof-of-concept Plutus Puzzle.
It is a rudimentary (and intentionally buggy) version of Rock Paper Scissors, where in order to spend the validator, the redeemer's sign has to
beat the datum's sign.
E.g.: If the datum is Paper, the redeemer has to be scissors for the validator to return true.

You are encouraged to play around with the validator by building transactions on cardano-cli.

You will find that it doesn't quite work like Rock Paper Scissors. Try to fix the validator logic to make it actually follow the rules
of Rock Paper Scissors.

Hope you have fun!

Feel free to get in touch if you want to be involved in developing Plutus Puzzles as a learning tool alongside PPBL!


Use the choice JSONs and Plutus Script from the output folder to build your transaction. Rebuild the script each time you change business logic!