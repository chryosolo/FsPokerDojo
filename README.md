# Dojo "Poker Hand Scoring"
This dojo is about validating and scoring a poker hand.  This dojo is suitable for **novice F# programmers**,
and should be doable in **two hours**.  There are other dojos for this out there (see [KataPokerHands](http://codingdojo.org/cgi-bin/index.pl?KataPokerHands) which is generic, and [Poker-Dojo](https://github.com/gstamp/poker-dojo) for Clojure).  Beginners will learn about F# types, Discriminated Unions, pattern matching, and unit testing.  More experienced programmers get a look at [Railway Oriented Programming](http://fsharpforfunandprofit.com/rop/) and [Property Based Testing](http://fsharpforfunandprofit.com/pbt/).

At the most basic level, [Poker](https://en.wikipedia.org/wiki/Poker) is a game using a standard deck of playing cards
where players attempt to build a hand with the highest score amongst all other players.  Fundamental to the game, then,
is understanding how a poker hand is scored.  This dojo will have participants validating and scoring poker hands
according to the below rules.  First some terminology for those unfamiliar with the game:

### Terminology
* A **card** is one of a set with a combination of two properties:
  * a **rank** is a numerical value:
    * 2-9 - value is as stated
    * 10 (T) - value is 10
    * Jack (J) - value is 11
    * Queen (Q) - value is 12
    * King (K) - value is 13
    * Ace (A) - value is 14 (high) _(for this dojo, we'll say "Aces are high", but specific game rules may say Aces are
    low (value of 1), or either, depending on what the player wishes at that immediate moment.)_
  * and a **suit**:
    * Diamonds (♦ - Unicode x2666)
    * Hearts (♥ - Unicode x2665)
    * Clubs (♣ - Unicode x2663)
    * Spades (♠ - Unicode x2660)
  * for example, the card 5♥ would be called the "5 of hearts"
* A **deck** of cards is the set of all possible 13 ranks and 4 suites (13 * 4 = 52).
* A **hand** is a combination of cards used by a single player at a moment in time
* A hand is a **straight** if all card ranks (suits ignored) are adjacent and unique.  Said differently, if the hand
  is sorted by rank, and has no duplicate ranks, the high rank minus the low rank is always 4.
  * 8, 9, T, J, Q form a straight -- no ranks skipped, all ranks unique
  * 8, 9, T, Q, K do NOT form a straight because Jacks are skipped
  * 8, 9, T♦, T♣, J do NOT form a straight because of the duplicate 10
* A hand is a **flush** if all card suits are the same.
* A hand with cards sharing the same rank is usually a good thing:
  * two cards sharing the same rank is called a **pair**.  Note a 5-card hand could have two pairs.
  * three cards sharing the same rank is called **three of a kind**.
  * four cards sharing the same rank is called **four of a kind**.

### Scoring
* Card sequence does not matter -- 6♥ J♥ 2♥ 9♥ 4♥ is the same as 2♥ 4♥ 6♥ 9♥ J♥
* A hand is scored by primarily by **category**.  Choosing 5 cards from a single deck gives 2,598,960
  combinations, and poker has nine categories based on how rare the category is.
* Inside a category, a hand is **ranked** by the highest relevant card.  Of two hands in the same category, the
  hand ranked higher will win.  Of two hands with the same category and ranking, a **kicker** rank can break ties.  
  * For example, between two hands of the same _One Pair_ category, 4♣ 4♦ 8♥ T♠ K♥ and 5♣ 5♦ 8♥ T♠ K♥,
    the pair of 5's is _ranked_ higher than the pair of 4's.
  * Two hands with the same _One Pair_ category and _rank_ of 4, 4♣ 4♦ 8♥ T♠ K♥ and 4♥ 4♠ 8♥ T♠ Q♥,
    the King _kicker_ beats the Queen.
* A higher scoring category with a low card will always beat a lower scoring category with a high card.
* If two hands are in the same scoring category and have the same rank and kicker, the hands are a **tie**.

### Categories (from [List of Poker Hands](https://en.wikipedia.org/wiki/List_of_poker_hands))
A hand should be scored using the category with the strictest rules (rarest hand).  For example, a hand with four
of a kind will also have a three of a kind and two of a kind, but would only be considered to be _four of a kind_.
* (Score of 10) **Royal Flush**
  * Hand is both a **straight** AND a **flush**, and the high card is an Ace, there is no kicker
  * Example: T♥ J♥ Q♥ K♥ [A]♥ ( no cards are ignored in this category )
  * The Ace is the relevant rank.
  * There are 4 possible, one for each suit, for a likelihood of 0.00015%
* (Score of 9) **Straight Flush**
  * Hand is both a **straight** AND a **flush**
  * Example: 4♥ 5♥ 6♥ 7♥ [8]♥ ( no cards are ignored in this category )
  * The highest card value is the rank, and there is no kicker
  * There are 40 possible, 4 suits * 10 possible high cards, for a likelihood of 0.0015%
* (Score of 8) **Four of a Kind**:
  * Hand has four cards with the same value
  * Example: [4]♣ 4♦ 4♥ 4♠ ( {2}♥ ) -- the card with the non-duplicated value is ignored
  * The duplicated value is the rank, and the ignored card is the kicker
  * There are 624 possible hands, for a likelihood of 0.024%
* (Score of 7) **Full house**:
  * Hand has three cards with one value as well as two cards with a second value
  * Example: [2]♣ 2♦ 2♥ {4}♠ 4♥ ( no cards are ignored in this category )
  * The value of the three of a kind is the rank, and the kicker is the two of a kind
  * There are 3,744 possible hands, for a likelihood of 0.1441%
* (Score of 6) **Flush**:
  * Hand has cards of only a single suit
  * Example: 2♥ 4♥ 6♥ 9♥ [J]♥ ( no cards are ignored in this category )
  * The highest card value is the rank, and remaining ranks are kickers
  * There are 5,108 possible hands, for a likelihood of 0.196%
* (Score of 5) **Straight**:
  * Hand has card values that are all unique and are all adjacent
  * Example: 2♣ 3♦ 4♥ 5♠ [6]♥ () -- no cards are ignored in this category
  * The highest card value is the rank and there is no kicker
  * There are 10,200 possible hands, for a likehood of 0.39%
* (Score of 4) **Three of a Kind**:
  * Hand has three cards with the same value
  * Example: [4]♣ 4♦ 4♥ ( 6♠ {J}♥ ) -- the cards with non-duplicated values are ignored
  * The duplicated value is the rank and the highest ignored card is the kicker
  * There are 54,912 possible hands, for a likelihood of 2.11%
* (Score of 3) **Two Pair**:
  * Hand has two cards with the same value as well as two cards which share a second value
  * Example: [8]♥ 8♠ {4}♣ 4♦ ( {J}♥ ) -- the card with the non-duplicated values is ignored
  * The highest pair is the rank and the lowest pair is the first kicker while the ignored card is the second kicker
  * There are 123,552 possible hands, for a likelihood of 4.75%
* (Score of 2) **One Pair**:
  * Hand has two cards with the same value
  * Example: [4]♣ 4♦ ( {K}♥ {T}♠ {8}♥ ) -- the cards with non-duplicated values are ignored
  * The duplicated value is the rank, and the remaining cards in descending order are kickers if needed.
  * There are 1,098,240 possible hands, for a likelihood of 42.26%
* (Score of 1) **High Card**:
  * Hand with absolutely nothing special about it at all
  * Example: [K]♣ ( {Q}♥ {T}♠ {8}♥ {4}♦ ) -- the cards without the highest value are ignored
  * The highest value is the rank, and the remaining cards in descending order are kickers if needed.
  * There are 1,302,540 possible hands, for a likelihood of 50.11%
