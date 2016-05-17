### Beginner Script
**Beginner.fsx** is obviously aimed at beginners.  Absolute beginners will need help with running scripts,
and perhaps syntax here and there.

### Intermediate Script
**Intermediate.fsx** gives the result of following the beginner script, and beginners can refer there if they
have any questions, but should obviously be encouraged to not rely on it for more than just spot guidance. The
intermediate tasks are still under construction, but if people wish, they can refer to my work-in-progress
**Final.fsx** (found in the Organizer section).  I am still "perfecting" this script, because like most new
concepts, ROP feels clunky if only halfway done, or if not done quite right -- I think I'm still trying to push
a square peg through a round hole in places.

### Final Script
**Final.fsx** has a _Tools module_ near the bottom which deals primarily (and clumbsily) with generating the
hands.txt file.

The final script also gives my answers to the first two challenges -- I ran out of time before starting the
third. Refer to the _Challenges module_ at the bottom of the script.  All 10,000 hands are scored and sorted
in descending order.  The top 25 hand scores in the file are as follows:

1. 5C jC jS jH jD: **FourKind (Jack,Five)**
1. 5C 5S 5H 5D aS: **FourKind (Five,Ace)**
1. kS kD aC aS aD: **FullHouse (Ace,King)**
1. qC qD aS aH aD: **FullHouse (Ace,Queen)**
1. 4C 4D kC kS kD: **FullHouse (King,Four)**
1. jC jS jH kS kD: **FullHouse (Jack,King)**
1. 8H 8D 9C 9S 9H: **FullHouse (Nine,Eight)**
1. 7H 7D 9C 9S 9H: **FullHouse (Nine,Seven)**
1. 6H 6D 9C 9S 9D: **FullHouse (Nine,Six)**
1. 6C 6H 6D kS kD: **FullHouse (Six,King)**
1. 2S 2H 6C 6S 6H: **FullHouse (Six,Two)**
1. 5C 5S 5D tS tH: **FullHouse (Five,Ten)**
1. 3C 3S 3H aH aD: **FullHouse (Three,Ace)**
1. 3C 3S 3H jS jD: **FullHouse (Three,Jack)**
1. 2S 2H 2D 3C 3D: **FullHouse (Two,Three)**
1. 2C 2S 2D 3C 3H: **FullHouse (Two,Three)**
1. 3D 5D 7D 8D aD: **Flush Ace**
1. 2S 6S tS kS aS: **Flush Ace**
1. 2C 4C 5C 6C aC: **Flush Ace**
1. 4D 6D 8D kD aD: **Flush Ace**
1. 2H 5H 7H kH aH: **Flush Ace**
1. 2S 4S 5S 8S aS: **Flush Ace**
1. 2H 3H 7H 8H kH: **Flush King**
1. 7S 8S 9S qS kS: **Flush King**
1. 3H 5H 6H tH kH: **Flush King**
