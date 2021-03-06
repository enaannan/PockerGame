# Poker Hand Strength Evaluator

An algorithm for sorting poker hands according to their strength.

### Poker Games Supported

#### Texas hold'em

Supports edge case where multiple highCards have the same rank eg: `10c8s7s6h4d` and `10d8d7s6c4c` . They will have
hands being equal i.e `6h4d=6c4c`
assuming hand cards are `6c4c` and `6h4d`. My algorithm for handling this situation could be improved by implementing a
sorting algo that makes use of card ranks value in Int rather than introducing a rank value of String to make use of the
already existing sorting algo for strings

#### Omaha Hold'em

Current Implementation fails when board cards have 4 or more cards of the same suit. A proposed solution which I started
to write was to create dedicated methods for Omaha Hold'em which returns all possible combinations of a card value eg
all possible combinations of a flush and return the best omaha value

#### Five Card Draw
