(* Style of dojo taken from https://github.com/c4fsharp/Dojo-Markov-Bot

All source material for this dojo can be found at:
https://github.com/chryosolo/FsPokerDojo.git
It's MIT licensed - use it, share it, modify it. *)

(*
Introduction
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
The goal of the dojo is to take a valid poker hand and put it in the correct
scoring category.

To achieve this, we will break the task down into manageable pieces, and then
suggest directions to explore further! *)

// Quick test function which catches exceptions
let testParse desc func param =
   try
      let actual = func param
      printfn "%s: OK with value of %A" desc actual
   with
   | Failure(message) -> printfn "%s: FAIL with %s" desc message

(*
Chapter 1: Representing a card
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Your goal here is to take a two character string input, and return a
strongly-typed card, or throw an error on invalid input.

We start with defining types which hopefully will make impossible to hold
incorrect data.

F# has enums, which define a list of allowed values:

type NonValuedEnum =
   | Red
   | Green
   | Blue

There are four suits: Clubs, Spades, Hearts, and Diamonds. *)

// --> Fix and complete the Suit enum.
type Suit =
   | Suit1
   | Suit2

(*
Enum values can also map to an optional integer value, which we will use to
associate card value names to a consistent integer value.

type ValuedEnum =
   | Squared1 = 1
   | Squared2 = 4
   | Squared3 = 9

Even though values will be numeric, we will use defined names for easier future
programming. There are thirteen Values: Two, Three, Four, Five, Six, Seven,
Eight, Nine, Ten, Jack, Queen, King, Ace
*)

// --> Fix and complete the Value enum.
type Value =
   | Squared1 = 1
   | Squared2 = 4
   | Squared3 = 9

(*
A card has both a Suit and a Value, never just one or the other.  This is a
perfect time to use a Tuple, which requires all portions to be used together.
A tuple type has portion types separated by an asterisk:

type Date = int * int * int * DayOfWeek

Note the Date has three integer values and a DayOfWeek.  It is up to the
programmer to remember that the first int is a Month, the second is a Day,
and the third is a Year.  This is a weakness of Tuples, but there are ways
to help get around this which we'll see later.

Our card type will have a Value and a Suit
*)

// --> Fix the Card type definition
type Card = int * int * float

(*
Now we'll have a function which will parse a character and return the valid
suit, or throw an error.  Pattern matching is a strong feature of F#.  It
requires us to deal with all cases which eliminates a whole category of bugs.

let myEnumLikes valueEnum =
   match food with
   | ValuedEnum.Squared1 -> true
   | ValuedEnum.Squared3 -> false
// throws a compile-time error because ValuedEnum.Squared2 wasn't handled

We are matching against all possible chars, and converting to only four suits.
So we don't have to give a definitive list of chars (which ironically sidesteps
the completeness benefit given to us above), we'll use a wildcard match to
catch everything which we don't handle.

We will use C for clubs, S for spades, H for hearts, and D for diamonds.
*)

// --> fix and complete the function
let parseSuit charSuit =
   match charSuit with
   | 'q'                 // not handling 'q' means it is handled below
   | 'Q' -> Suit.Suit1   // 'Q' (and 'q') are handled here
   | 'W' -> Suit.Suit2
   | _ -> failwith "Invalid suit." // wildcard match throws an error

testParse "Should be Clubs" parseSuit 'c'
testParse "Should fail" parseSuit 'Q'

(*
We will use 2-9 for Two..Nine, T for Ten, J for Jack, Q for Queen, K for King,
and A for Ace
*)
// --> fix and complete the parseValue function
let parseValue charValue =
   match charValue with
   | '1' -> Value.Squared3
   | _ -> failwith "Invalid value."

testParse "Should be Five" parseValue '5'
testParse "Should be Queen" parseValue 'Q'
testParse "Should fail" parseValue '1'

(*
We'll put it together here and return a card or throw an error.  We'll use
pattern matching again, this time on the size of an array.  If we're given
two chars, we'll assume they are a value followed by a suit.  Anything else
is invalid.
*)
let parseCard (strCard:string) =
   let chars = strCard.ToCharArray()
   match chars with
   | [| charValue; charSuit |] ->
      let value = parseValue charValue
      let suit = parseSuit charSuit
      value,suit
   | _ -> failwith "Invalid card syntax."

testParse "Should be Ten of Clubs" parseCard "Tc"
testParse "Should be Ace of Spades" parseCard "aS"
testParse "Should fail card syntax" parseCard "123"
testParse "Should be invalid value" parseCard "1s"
testParse "Should be invalid suit" parseCard "4K"

