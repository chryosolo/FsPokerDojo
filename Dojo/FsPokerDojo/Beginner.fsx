(* Style of dojo taken from https://github.com/c4fsharp/Dojo-Markov-Bot

All source material for this dojo can be found at:
https://github.com/chryosolo/FsPokerDojo.git
It's MIT licensed - use it, share it, modify it. *)

(*
# Introduction
################################################################################
The goal of the dojo is to take a valid poker hand and put it in the correct
scoring category.  To achieve this, we will break the task down into manageable
pieces, and then suggest directions to explore further! 

The BEGINNER script is a guide, walking the user step by step towards a basic,
working solution, emphasizing notes about F# syntax and concepts.

The INTERMEDIATE script takes over from there -- the beginner program is simply
given, already working, and steps will work toward the improvement of the
program by giving better error handling using computational expressions and
Railway Oriented Programming, and better testing using FsCheck and xUnit.
*)

// --> delete this error when you understand to look for tasks! :)
failwith( "Any time you see '-->', this is a task for you!" )

(*
Freebie: Simple testing function -- we'll get more in-depth later
################################################################################
It is hugely helpful to be able to test code while it is still being written.
The function testFunc exists to help us test our other functions as we're
writing them.  It will print out a test description, whether or not the test
passed, and give the result, or the error as appropriate.

It takes three parameters:
* desc is a string, and simply prints out to show what test is being performed
* parseFunc is the parsing function to call
* input is what to pass to the parsing function

It does the call to the function inside a try/with, so the parseFunc can
'failwith', and the failure message will be printed out.
*)
let testFunc desc parseFunc input =
   try
      let actual = parseFunc input
      printfn "%s: OK with value of %A" desc actual
   with
   | Failure(message) -> printfn "%s: FAIL with %s" desc message

// we'll test it right away -- should say 'testFunc - OK: OK with value of 7'
testFunc "testFunc - OK" (fun (x,y) -> x + y ) (3,4)
// should say 'testFunc - failure: FAIL with sample failure'
testFunc "testFunc - failure" (fun () -> failwith "sample failure") ()


(*
Chapter 1: Representing a card
################################################################################
Your goal here is to take a two character string input, like "5C", and return a
strongly-typed card (Five, Clubs), or throw an error on invalid input.

We start with defining types which hopefully will make impossible holding
incorrect data.

F# has a Discriminated Union (usually just DU), which define a list of values
and optionally allows each to have data of different types.  At its simplest,
DU choices do not have to have any data, and are like an unnumbered enum.

   type EmptyDu =
      | Red
      | Green
      | Blue

There are four suits: Clubs, Spades, Hearts, and Diamonds. *)

// -----------------------------------------------------------------------------
// Suits as an isolated concept
// -----------------------------------------------------------------------------
type Suit =
   | Clubs
// --> Fix and complete the Suit DU.
   | Suit1
   | Suit2

(*
Enum syntax is like the empty DU, except each holds an integer value.

   type ValuedEnum =
      | Squared1 = 1
      | Squared2 = 4
      | Squared3 = 9

Even though ranks will be numeric, we will use defined names for easier future
programming. There are thirteen Ranks: Two, Three, Four, Five, Six, Seven,
Eight, Nine, Ten, Jack, Queen, King, Ace
*)

// -----------------------------------------------------------------------------
// Rank as an isolated concept
// -----------------------------------------------------------------------------
type Rank =
   | Two = 2
// --> Fix and complete the Rank enum.
   | Squared1 = 1
   | Squared2 = 4
   | Squared3 = 9

(*
A card has both a Suit and a Value, never just one or the other.  This is a
perfect time to use a Tuple, which requires all portions to be used together.
A tuple is like a class, except properties are not accessed by name, but by
position.  A tuple type DEFINITION has portion types separated by an asterisk:

   type Date = int * int * int * DayOfWeek

However, when USED, tuple portions are separated by a comma:

   let petBirthDate = 2010, 12, 14, DayOfWeek.Tuesday

Note the Date has three integer values and a DayOfWeek.  It is up to the
programmer to remember that the first int is a Month, the second is a Day, and
the third is a Year.  This is a weakness of Tuples, but there are ways to help
get around this:

* Records are like a class, and allow by-name access:
   
   type Date = { Year:int; Month:int; Day:int; DayOfWeek:DayOfWeek }
   let petBirthDate = { Year:2010; Month=12; Day=14;
                        DayOfWeek=DayOfWeek.Tuesday }

* Single-case DUs label an intent of a basic type and can help avoid "primitive
  obsession":  see
  http://blog.ploeh.dk/2015/01/19/from-primitive-obsession-to-domain-modelling/

   type Year = Year of int
   type Month = Month of int
   type Day = Day of int
   type Date = Year * Month * Day * DayOfWeek
   let petBirthDate = Year 2010, Month 12, Day 14, DayOfWeek.Tuesday
   // or if you had another empty DU...
   let petBirthDate = Year 2010, December, Day 14, DayOfWeek.Tuesday

However, for us, our tuple portions aren't of the same type, so we don't need to
worry about it! :) Our card type will be a tuple with a Rank and a Suit.
*)

// -----------------------------------------------------------------------------
// Card is a Rank and Suit
// -----------------------------------------------------------------------------

// --> Fix the Card type definition
type Card = int * float * bool

(*
Now we'll have a function which will parse a character and return the valid
suit, or throw an error.  Pattern matching is a strong feature of F#.  It works
really well for DUs, because the compiler requires us to deal with all cases
which eliminates all bugs about forgetting to handle a corner case.

   let isFavoriteColor color =
      match color with
      | EmptyDu.Red -> true
      | EmptyDu.Blue -> false
   // throws a compile-time error because EmptyDu.Green wasn't handled

Pattern matching also work for enums, but here's the interesting thing... In F#,
you can dynamically create new enum values at runtime, so the list you give in
code can never be considered "complete", so you MUST use a wildcard to catch
unspecified values.
See https://fsharpforfunandprofit.com/posts/enum-types/ for more info.

let isFavoriteRank rank =
   match rank with
   | Rank.Ace -> false
   | Rank.Two -> true
   ...
   | Rank.King -> false
   // gives a warning that you haven't covered all cases, like 0.  ?!?  There
   // IS NO ZERO enum!!!  Oh, right, someone may MAKE ONE at runtime...
   | _ -> failwith "I haven't handled that enum yet..."

We will use C for clubs, S for spades, H for hearts, and D for diamonds.
*)

// -----------------------------------------------------------------------------
// Parse a character representation of a suit into a Suit.  Valid suits are:
// 'C', 'S', 'H', 'D'
// -----------------------------------------------------------------------------
let parseSuit charSuit =
   match charSuit with
   | 'c'
   | 'C' -> Suit.Clubs
// --> fix and complete the function
   | 'q'                 // not handling 'q' means it is handled below
   | 'Q' -> Suit.Suit1   // 'Q' (and 'q') are handled here
   | 'W' -> Suit.Suit2
   | _ -> failwith "Invalid suit." // wildcard match throws an error

testFunc "parseSuit success" parseSuit 'd'
testFunc "parseSuit invalid" parseSuit 'Q'

(*
We will use 2-9 for Two..Nine, T for Ten, J for Jack, Q for Queen, K for King,
and A for Ace
*)

// -----------------------------------------------------------------------------
// Parse a character representation of a rank into a Rank.  Valid values are:
// '2' .. '9', 'T', 'J', 'Q', 'K', 'A'
// -----------------------------------------------------------------------------
let parseRank charRank =
   match charRank with
   | '2' -> Rank.Two
// --> fix and complete the parseRank function
   | '1' -> Rank.Squared3
   | _ -> failwith "Invalid rank."

testFunc "parseRank success numeric" parseRank '5'
testFunc "parseRank success face" parseRank 'Q'
testFunc "parseRank invalid" parseRank '1'

(*
We'll put it together here and return a card or throw an error.  We'll use
pattern matching again, this time on the size of an array.  If we're given
two chars, we'll assume they are a value followed by a suit.  Anything else
is invalid.
*)

// -----------------------------------------------------------------------------
// Parse a two-character string representation of a card into a Card. Is NOT
// case sensitive.
// -----------------------------------------------------------------------------
let parseCard (strCard:string) =
// --> add a ToUpper so you can simplify above parsing!
   let chars = strCard.   ToCharArray()
   match chars with
   // here pattern matching is seeing if the chars array can be placed into a
   // two-item array.  If it can, it will name them charValue and charSuit,
   // which you can use when it's appropriate!  Handy!
   | [| charRank; charSuit |] ->
// --> call the parseValue and parseSuit functions
      let rank = Rank.Squared1
      let suit = Suit.Suit2
      rank,suit
   // array isn't two items long
   | _ -> failwith "Invalid card syntax."

testFunc "parseCard success UPPER lower" parseCard "Tc"
testFunc "parseCard success lower UPPER" parseCard "aS"
testFunc "parseCard invalid card syntax" parseCard "123"
testFunc "parseCard invalid rank" parseCard "1s"
testFunc "parseCard invalid suit" parseCard "4K"




(*
Chapter 2: Representing a hand
################################################################################
In chapter two, we will be taking a string of five cards separated by spaces,
like "2C 3C 4C 5C 6C" and converting it into a Hand, which will be a 5-tuple of
Cards.
*)

// --> make sure your Card type from above matches, and then delete this line
//     and switch all future Code from Card' to just Card
type Card' = Rank * Suit

// -----------------------------------------------------------------------------
// A hand is five cards
// -----------------------------------------------------------------------------
// --> fix the hand type
type Hand = bool * double

(*
F# has several built in collection types including :
* Arrays -- random access by index
* List -- sequential access via a singly linked list
* Sequence -- "like a list" -- is lazily evaluated and compatible with rest of
              .NET but doesn't fully support pattern matching, and poor
              performance in certain areas
Note -- see https://fsharpforfunandprofit.com/posts/list-module-functions
for distinction between the collection types, and when you might choose each.

We will need to be able to tell if an array of cards contains any duplicates.
We'll use the GROUP BY function which gives an array of buckets where each
bucket is "named" by a key, and the bucket holds all values to which that key
applies.  In our case, we're generating the key to be the card itself, and that
key is the same for all identical cards -- duplicate cards will go into the
same bucket while unique cards will go into different buckets.  Given this, if
the length of the array of buckets is less than the input length, there must be
a duplicate!
*)

// -----------------------------------------------------------------------------
// Return if the given array of cards has any duplicates.  Puts cards into
// groups, and if number of groups is less than number of cards, there is a
// duplicate.
// -----------------------------------------------------------------------------
let areDups cardArray =
   let uniqueCards = Array.groupBy id cardArray
// --> functions return their last expression.  Here, we should return whether
//     the uniqueCards length is less than the cardArray length
   true
   
testFunc "areDups all unique" areDups [| (Rank.Squared1,Suit.Suit1);
                                         (Rank.Squared1,Suit.Suit2) |]
testFunc "areDups duplicates" areDups [| (Rank.Squared1,Suit.Suit1);
                                         (Rank.Squared1,Suit.Suit1) |]

(*
You can have functions which take a "Generic" type.  For instance -- if you
have a function which returns the length of an array, it is not important
whether you have an array of string or an array of dates, or an array of a huge
complex object structure, you simply count the number of them.

In our case, we take an array of SOMETHING (cards), and if there are 5 items,
return a 5-tuple of SOMETHING, otherwise we throw an error.  Tuples are written
with a comma separating the pieces:

let tuple3 = "item1", false, 14

Array data is accessed with a "." and the index number in square braces:
let firstItem = myArray.[0]
let secondItem = myArray.[1]

There are two main ways to branch in F#.  See
https://fsharpforfunandprofit.com/posts/control-flow-expressions/ for more.

The simpler is if-then-else:

   let isEven x = if x % 2 = 0 then true else false

However, F# is a functional language, so we don't have an if STATEMENT which
controls flow.  Instead, we have an if EXPRESSION which returns either one value
or another of the same type.  In the above example, a value is assigned to the
RESULT of the if expression.

The same pattern matching we saw above is the other way common way to branch,
and is a much better fit for branching in a functional language.  We'll do the
same test as the if above, to see the syntax, then drastically improve it:

   let isEven x = match x % 2 = 0 with
                  | true -> true
                  | _ -> false

We're taking the input value, in this case the boolean result of the modulus
of 2 equalling 0, and testing it against patterns, and following the path of
the first matching pattern.  In this case, true, yielding true.  But, we can
do better by matching possible results instead of whether the results equal 0:

   let isEven x = match x % 2 with
                  | 0 -> true
                  | _ -> false

This is better because we often want to use values created in the conditional:

   let stringToInt str = match System.Int32.TryParse( str ) with
                         | true, x -> Some x    // just got x and now use it
                         | false, _ -> None

What does TryParse return?  It's a boolean return plus an int out parameter.  In
F#, this is simply returned as a tuple, which we match against, then directly
use the value.  Very slick!
*)

// -----------------------------------------------------------------------------
// Turn the array of 5 cards into a 5-tuple of cards, or fail if not 5.
// -----------------------------------------------------------------------------
let cardsToHand (cards:'a[]) =
   match cards.Length with
// --> add case when length is 5, return a 5-tuple of cards
   | _ -> failwith "Invalid hand size."

testFunc "cardsToHand success" cardsToHand [| (Rank.Squared1, Suit.Suit1); 
                                              (Rank.Squared1, Suit.Suit2);
                                              (Rank.Squared2, Suit.Suit1);
                                              (Rank.Squared2, Suit.Suit2);
                                              (Rank.Squared3, Suit.Suit1); |]
testFunc "cardsToHand invalid size"
         cardsToHand [| (Rank.Squared1, Suit.Suit1); 
                        (Rank.Squared2, Suit.Suit2); |]

(*
We can use String.Split to get an array of card tokens, and just send them to
our existing parseCard function.  Since we're expecting a single deck, there
cannot be any duplicate cards.

A common collection function is MAP, which is a function to take the current
item and turn it into something else.  For example, squaresStr takes a list of
the integers 0 through 10 and MAPS each to the string representation of that
number squared:

   let squaresStr = [0..10] |> List.map (fun x -> sprintf "%i" (x * x) )

For our needs, we'll be taking a collection of card tokens and MAPPING each
into an actual card.

Another common function is SORT. Hand analysis will be easier if we sort the
hand by card.  Note that you get sorting for free in F#, first by Value, which
is just an int, and then by Suit, which is an empty DU, which is turned into
int cases by the compiler, so they're sortable too.
*)

// -----------------------------------------------------------------------------
// Parse the hand string into a valid hand, or fail with a reason why the hand
// string was invalid.
// -----------------------------------------------------------------------------
let parseHand (strHand:string) =
   let cardTokens = strHand.Split[| ' ' |]
// --> change the mapping to call our parseCard function
   let cards = cardTokens
               |> Array.map (fun token -> Rank.Squared1, Suit.Suit1 )
               |> Array.sort
   // the above |> is a pipe, and it passes the result forward to be the final
   // parameter of the next expression.  This instance is the equivalent of:
   // let temp1 = Array.map (fun) cardTokens
   // let cards = Array.sort temp1

// --> pattern check result of the areDups function:
   //     if duplicates, fail with duplicates
   //     if all unique, return cardsToHand of cards
   failwith "Duplicate cards found"

testFunc "parseHand success" parseHand "AD 2C 3D 6S KC"
testFunc "parseHand card parse error" parseHand "1D 2C 3D 6S KC"
testFunc "parseHand invalid hand size" parseHand "2C 3D 6S KC"
testFunc "parseHand duplicate card" parseHand "2C 4H 3D 6S 2C"




(*
Chapter 3: Scoring a hand
################################################################################
In chapter three, we will be taking a valid poker hand, analyzing it in several
ways, and scoring it.  We should then be able to take many hand scores and
simply ask F# to sort them to figure out which is best!

Remember that DUs cases can have different data types.  A RoyalFlush has no High
Card, but a StraightFlush does.  As the categories get lower value we must
consider more and more "non-category" cards for a proper comparison.  In the
lowest value category, HighCard, each card rank is stored for comparison.

We will declare our categories in ascending order, so a Max or Min statement
will return the highest or lowest accordingly.
*)

// -----------------------------------------------------------------------------
// Possible hand scoring categories
// -----------------------------------------------------------------------------
type HandCategory = 
   // --> HighCard is a 5-tuple of Rank
   | HighCard of int
   // --> OnePair is a 4-tuple of Rank
   | OnePair of int
   // --> TwoPair is a 3-tuple of Rank
   | TwoPair of int
   // --> ThreeKind is a 3-tuple of Rank
   | ThreeKind of int
   // --> Straight and Flush are just a Rank
   | Straight of int
   | Flush of int
   // --> FullHouse and FourKind are a 2-tuple of Rank
   | FullHouse of int
   | FourKind of int
   // --> StraightFlush is just a Rank
   | StraightFlush of int
   | RoyalFlush

(*
Most functions below will need to be able to access cards out of an array
instead of the tuple.  To facilitate this, we'll one of F#'s features which
deconstructs a complex data structure into its component parts.

   let (tupleItem1, tupleItem2) = tuple
   // record syntax feels backwards to me, but meh...
   let {Year=theYear; Month=aMonth; Day=someDay; DayOfWeek=whatDay} = date
*)

// -----------------------------------------------------------------------------
// Return the hand (5-tuple of cards) as an array of cards
// -----------------------------------------------------------------------------
let toArray hand =
   // --> deconstruct the hand into 5 cards named c1 through c5
   let (fst,snd) = hand
   // --> return all five cards in order in an array
   [|fst;snd|]

testFunc "toArray success" toArray <| parseHand "AD 2C 3D 6S KC"

(*
Determining if a hand is a straight is as easy as checking each consecutive pair
of cards in a hand and if each has the property that the second card is the next
rank after the first card, then the hand is a straight.

We will be turning an enum value to its corresponding int, which is easy:

   let intThree = int Rank.Three
*)

// -----------------------------------------------------------------------------
// Return if the second rank is exactly one more than the first rank
// -----------------------------------------------------------------------------
let isNextRank (ranks : Rank * Rank) =
// --> use deconstruction to pull rank1 and rank2 out from ranks

// --> return "rank2 as an int" equals "rank1 as an int" plus 1
   false

testFunc "isNextRank true: two,three" isNextRank (parseRank '2', parseRank '3')
testFunc "isNextRank true: king,ace" isNextRank (parseRank 'K', parseRank 'A')
testFunc "isNextRank false: six,ten" isNextRank (parseRank '6', parseRank 'T')


(*
Now we'll finish the check for if a hand is a straight.  We'll use the
Seq.pairwise function, which takes a sequence 1;2;3;... and returns a sequence
of tuples of consecutive pairs: (1,2);(2,3);(3,4);...  We will also use the
Seq.forall function, which returns true if the function returns true for all
items in the sequence. The only problem is our hands are in an array. All F#
collections let you easily convert from one collection type to another:

   let array = [|1;2;3;4;5|]
   let list = Array.toList array
   let seq = List.toSeq list
   let list2 = List.ofArray array
   let seq2 = Seq.ofList list2

Also notice complex functions often just pipe data through simple functions:

   let complexOutput =
      input
      |> pipedToStepOne
      |> thenToStepTwo
      |> thenStepThree
      |> finallytoStepFour
*)

// -----------------------------------------------------------------------------
// Return whether or not the given hand is a straight (sequential ranks)
// Assumes the hand is valid and already sorted by Rank ascending.
// -----------------------------------------------------------------------------
let isStraight hand =
// --> start pipelining with simply the given hand instead of this mess :)
   [|(parseRank '2',0),(parseRank '3',0)|]
// --> turn the hand into an array of cards using our toArray function

   |> Array.toSeq
// --> turn the sequence of cards into a sequence of card pairs

// --> return whether for all pairs the second rank is 'next' after the first
   |> Seq.forall( fun( (r1, _), (r2,_ ) ) -> false )

testFunc "isStraight true" isStraight ( parseHand "2C 3D 4D 5S 6H" )
testFunc "isStraight false" isStraight ( parseHand "2C 3D 4D 5S 5H" )


(*
For checking whether the given hand is a flush, we will simply take our array of
cards and use the Array.forall function to make sure each card has the same suit
as the first card in the array.

You'll see the term "lambda" everywhere when talking about functional concepts.
Basically, this is just a function, but you're not formally declaring it and
giving it to a name -- you're just declaring it inline and using it:

   // no lambda
   let isOdd anInt = anInt % 2 = 1
   let isThreeOdd = isOdd 3

   // rewritten using a lambda
   let isThreeOdd = 3 |> (fun anInt -> anInt % 2 = 1)

Why use them?  Sometimes you won't.  Functions that are too long or complicated
or that you'll use again and again should not be lambdas.  Many functions are
necessary as wrappers around given core and library functions, and these are
usually specific to your data and situation, and are perfect candidates for
lambdas.  In the parseCard function above, we wrap a call to Array.map with the
actual transformation handled by a lambda:

   let cards = cardTokens
               |> Array.map ( fun token -> parseCard token )
               |> Array.sort
*)

// -----------------------------------------------------------------------------
// Return whether or not the given hand is a flush (single suit)
// Assumes the hand is valid (such as from parseHand)
// -----------------------------------------------------------------------------
let isFlush hand =
// --> deconstruct the hand so the first card's suit is saved
   let ( _, _, (_,thirdSuit), (fourthRank,_), _ ) = hand
// --> start pipelining with simply the hand
   [|1;2;3|]
// --> turn the hand into an array of cards using our toArray function

// --> Array.forall should call a lambda which takes a card and returns whether
//     the card's suit equals the remembered first suit
   |> Array.forall( fun anInt -> anInt % 2 = 1 )

testFunc "isFlush true" isFlush ( parseHand "2D 6D tD jD aD" )
testFunc "isFlush false" isFlush ( parseHand "2C 3D 4D 5S 5H" )


(*
One of the biggest ways to analyze a poker hand is by the count of distinct
ranks.  For example, four of a kind would have 4 of one rank with 1 of another.
We don't even need to know what rank they are to identify the score category. We
won't throw away the actual rank, however, because we'll need them for breaking
most ties with two hands of the same category.  We will use Array.countBy which
groups, but doesn't return an array of groups, just an array of the tuple of the
group value and the number of items in the group.

   [|1;1;1;2;2;5|] |> Array.countBy id
   // gives [|(1,3);(2,2);(5,1)|] -- '1' appears 3 times, etc.

We will also turn our array result into a list, because pattern matching against
a list is extremely powerful.  Lists can deconstruct using the list cons
operator '::' which let you use arbitrary lengths, which is extremely useful for
algorithms which don't care about the rest of the list, or recursive algorithms
which break off pieces to process, then recurse until finished:

   let firstNum :: secondNum :: theEntireRemainder = listOfNumbers

   let print content = printfn "%A" content
   let rec printList (list:int list) =
      match list with
      | [] -> () // done, return unit
      | head :: rest ->
         print head
         printList rest
*)

// -----------------------------------------------------------------------------
// Return list of counts * Rank, sorted descending.
// Ex: 2_ 3_ 3_ T_ J_ returns [(2,3);(1,J);(1,T);(1;2)]
// -----------------------------------------------------------------------------
let getRankCounts hand =
// --> start pipelining with the given hand then turn it into an array
   [|(Rank.Squared1,Suit.Suit1)|]

   |> Array.countBy( fun (r,_) -> r ) // count by only rank (can ignore suit)
// --> Array.map (value,count) into (count,value)

// --> pipe to Array.sort then to Array.rev

   |> Array.toList

testFunc "getRankCounts 4kind" getRankCounts <| parseHand "2D 6D 2H 2S 2C"
testFunc "getRankCounts fh" getRankCounts <| parseHand "3C 2D 2H 3S 2C"
testFunc "getRankCounts 3kind" getRankCounts <| parseHand "2D 6D 2H 2S 5C"
testFunc "getRankCounts 2pair" getRankCounts <| parseHand "2D 6D 2H 6H 3C"
testFunc "getRankCounts 1pair" getRankCounts <| parseHand "2D 6D 4H 6H 3C"
testFunc "getRankCounts hc" getRankCounts <| parseHand "2D 3H 4S 5C 8H"


(*
The final function!  Actually analyzing a poker hand!  We will pattern match on
a three-tuple of the hand properties:  isStraight, isFlush, rankCounts.  From
these properties, we can get everything we need not only for determining score
category, but also all high cards and kickers.


*)
// -----------------------------------------------------------------------------
// Determine the correct HandScore for the given hand.
//   Assumes the hand is valid and already sorted by Rank ascending (such as
//   from parseHand).
//   Algorithm is to determine three properties of the hand:
//     1) It is a straight (T/F)
//     2) It is a flush (T/F)
//     3) We have a list of the rank counts
//   We can correctly score the hand by matching these properties
// -----------------------------------------------------------------------------
let analyzeHand hand = 
   // gather hand properties
// --> call our isStraight, isFlush functions
   let isStraight = false
   let isFlush = true
// --> call our getRankCounts functions
   let rankCounts = [(5,Rank.Squared3)]
   // match pattern of properties to determine HandScore
   match (isStraight, isFlush, rankCounts) with
   // matches with straight and/or flush
// --> RoyalFlush is straight, flush, and the high card is an Ace
   | (_, _, (1,Rank.Squared2) :: _ ) -> RoyalFlush
// --> StraightFlush is true, true, and not ace (deconstruct to hold high and
//     pass that as value of StraightFlush)
   | (_, _, (1,Rank.Squared1) :: _ ) -> StraightFlush -37
// --> Flush is NOT straight, flush, remember high card
   
// --> Straight is straight, NOT flush, remember high card

   // matches based on count only, so throw away flush and straight
   | (false, false, _ ) ->
      match rankCounts with
// --> switch value of FourKind to be (rank,kick)
      | [(4,rank); (1,kick)] -> FourKind -37
// --> FullHouse is 3,2 instead of 4,1

// --> ThreeKind is 3,1,something instead of 4,1
      
// --> TwoPair is 2(rank high),2(rank low),1(kicker) instead of 4,1

// --> OnePair is 2(rank),1(kick 1),1(kick 2),1(kick 3) instead of 4,1
      
// --> HighCard is 1(high),1(k1),1(k2),1(k3),1(k4) instead of 4,1
      
      | _ -> failwith "Should be unreachable, merely completes matching."
   | _ -> failwith "Should be unreachable, merely completes matching."

testFunc "analyzeHand Royal" analyzeHand <| parseHand "tC jC qC kC aC"
testFunc "analyzeHand StrFlush" analyzeHand <| parseHand "9C tC jC qC kC"
testFunc "analyzeHand Flush" analyzeHand <| parseHand "3C tC jC qC kC"
testFunc "analyzeHand Straight" analyzeHand <| parseHand "9S tS jC qC kC"
testFunc "analyzeHand FourKind" analyzeHand <| parseHand "kS kD kC kH 2C"
testFunc "analyzeHand FullHouse" analyzeHand <| parseHand "kS kD kC 2H 2C"
testFunc "analyzeHand ThreeKind" analyzeHand <| parseHand "kS kD kC 4H 2C"
testFunc "analyzeHand TwoPair" analyzeHand <| parseHand "kS kD qC qH 2C"
testFunc "analyzeHand OnePair" analyzeHand <| parseHand "kS kD qC 4H 2C"
testFunc "analyzeHand HighCard" analyzeHand <| parseHand "kS qD tC 4H 2C"

(*
You did it!  At this point, you can take hands as shown in the tests above and
easily compare their analyses to determine which hand is better -- F# gives the
comparison for free using the max function, or any of the collection Sort
functions.  F# also gives for free the ability to print the analysis, as seen
in the above analyzeHand tests.

Hopefully you had fun and learned something about F#.  When you're ready,
compare your changes against the intermediate script.  The only change there is
that all tests have been moved into a Test module at the end of the script,
because one of the intermediate script tasks is to improve testing capability.


Challenges
################################################################################

Challenge 1: Create a function which takes two string hand representations and
prints out the better scoring hand.

Challenge 2: The solution includes a file, hands.txt which lists 10,000 hands.
Find the top 25 hands.  Compare against the Organizer's Notes.

Challenge 3: Give a better hand-analysis reporting function which when given
two hands, only prints out enough of the analysis as is necessary.
  e.g. Comparing "aH kH qH jH tH" and "2C 3H 4D 7S 7H" would say
                 RoyalFlush beats OnePair
  e.g. Comparing "aH aD 2H 3H 4H" and "aC aS 2C 3C 5C" would say
                 OnePair Ace with Kicker of Five beats
                 OnePair Ace with Kicker of Four
*)
