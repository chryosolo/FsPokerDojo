(* Style of dojo taken from https://github.com/c4fsharp/Dojo-Markov-Bot

All source material for this dojo can be found at:
https://github.com/chryosolo/FsPokerDojo.git
It's MIT licensed - use it, share it, modify it. *)

(*
Introduction
###############################################################################
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


(*
Give existing Beginner code, all fixed.  Take the following improvements:

* Switch Suit to enum of char, use that in matching?
  https://fsharpforfunandprofit.com/posts/enum-types/
* Pull in "ROP"
* parseHand does Option.map or "maybe" monad
* FsCheck

*)

// --> delete this error when you understand to look for tasks! :)
failwith( "Any time you see '-->', this is a task for you!" )


// ----------------------------------------------------------------------------
// Suits as an isolated concept
// ----------------------------------------------------------------------------
type Suit =
   | Clubs
   | Spades
   | Hearts
   | Diamonds


// ----------------------------------------------------------------------------
// Values as an isolated concept
// ----------------------------------------------------------------------------
type Value = 
   | Two = 2
   | Three = 3
   | Four = 4
   | Five = 5
   | Six = 6
   | Seven = 7
   | Eight = 8
   | Nine = 9
   | Ten = 10
   | Jack = 11
   | Queen = 12
   | King = 13
   | Ace = 14


// ----------------------------------------------------------------------------
// Card is a Value and Suit
// ----------------------------------------------------------------------------
type Card = Value * Suit


// ----------------------------------------------------------------------------
// Parse a character representation of a suit into a Suit.  Valid suits are:
// 'C', 'S', 'H', 'D' -- case sensitive!
// ----------------------------------------------------------------------------
let parseSuit charSuit =
   match charSuit with
   | 'C' -> Suit.Clubs
   | 'S' -> Suit.Spades
   | 'H' -> Suit.Hearts
   | 'D' -> Suit.Diamonds
   | _ -> failwith "Invalid suit." // wildcard match throws an error


// ----------------------------------------------------------------------------
// Parse a character representation of a value into a Value.  Valid values are:
// '2' .. '9', 'T', 'J', 'Q', 'K', 'A' -- case sensitive!
// ----------------------------------------------------------------------------
let parseValue charValue =
   match charValue with
   | '2' -> Value.Two
   | '3' -> Value.Three
   | '4' -> Value.Four
   | '5' -> Value.Five
   | '6' -> Value.Six
   | '7' -> Value.Seven
   | '8' -> Value.Eight
   | '9' -> Value.Nine
   | 'T' -> Value.Ten
   | 'J' -> Value.Jack
   | 'Q' -> Value.Queen
   | 'K' -> Value.King
   | 'A' -> Value.Ace
   | _ -> failwith "Invalid value." // wildcard match throws an error


// ----------------------------------------------------------------------------
// Parse a two-character string representation of a card into a Card. Is NOT
// case sensitive.
// ----------------------------------------------------------------------------
let parseCard (strCard:string) =
   // --> add a ToUpper so you can simplify above parsing!
   let chars = strCard.ToUpper().ToCharArray()
   match chars with
   | [| charValue; charSuit |] ->
      // --> call the parseValue and parseSuit functions
      let value = parseValue charValue
      let suit = parseSuit charSuit
      value,suit
   // array isn't two items long
   | _ -> failwith "Invalid card syntax."


// ----------------------------------------------------------------------------
// A hand is five cards
// ----------------------------------------------------------------------------
type Hand = Card * Card * Card * Card * Card


// ----------------------------------------------------------------------------
// Return if the given array of cards has any duplicates.  Puts cards into
// groups, and if number of groups is less than number of cards, there is a
// duplicate.
// ----------------------------------------------------------------------------
let areDups cardArray =
   let uniqueCards = Array.groupBy id cardArray
   uniqueCards.Length < cardArray.Length
   

// ----------------------------------------------------------------------------
// Return if the given array of cards has any duplicates.  Puts cards into
// groups, and if number of groups is less than number of cards, there is a
// duplicate.
// ----------------------------------------------------------------------------
let cardsToHand (cards:'a[]) =
   match cards.Length with
   // --> add case when length is 5, return a 5-tuple of cards
   | 5 -> cards.[0], cards.[1], cards.[2], cards.[3], cards.[4]
   | _ -> failwith "Invalid hand size."


// ----------------------------------------------------------------------------
// Return if the given array of cards has any duplicates.  Puts cards into
// groups, and if number of groups is less than number of cards, there is a
// duplicate.
// ----------------------------------------------------------------------------
let parseHand (strHand:string) =
   let cardTokens = strHand.Split[| ' ' |]
   // --> change the mapping to call our parseCard function
   let cards = cardTokens
               |> Array.map parseCard
               |> Array.sort

   // --> pattern check result of the areDups function:
   //     if duplicates, fail with duplicates
   //     if all unique, return cardsToHand of cards
   match areDups cards with
   | true -> failwith "Duplicate cards found"
   | false -> cardsToHand cards


// ----------------------------------------------------------------------------
// Possible hand scoring categories
// ----------------------------------------------------------------------------
type HandCategory = 
   | HighCard of Value * Value * Value * Value * Value
   | OnePair of Value * Value * Value * Value
   | TwoPair of Value * Value * Value
   | ThreeKind of Value * Value
   | Straight of Value
   | Flush of Value
   | FullHouse of Value * Value
   | FourKind of Value * Value
   | StraightFlush of Value
   | RoyalFlush
   

// ----------------------------------------------------------------------------
// Return the hand (5-tuple of cards) as an array of cards
// ----------------------------------------------------------------------------
let toArray hand =
   let (c1, c2, c3, c4, c5) = hand
   [|c1;c2;c3;c4;c5|]


// ----------------------------------------------------------------------------
// Return if the second value is exactly one more than the first value
// ----------------------------------------------------------------------------
let isNextValue (values : Value * Value) =
   let (val1,val2) = values
   int val2 = int val1 + 1


// ----------------------------------------------------------------------------
// Return whether or not the given hand is a straight (sequential values)
// Assumes the hand is valid and already sorted by Value ascending.
// ----------------------------------------------------------------------------
let isStraight hand = 
   hand
   |> toArray
   |> Array.toSeq
   |> Seq.pairwise
   |> Seq.forall( fun( (v1, _), (v2,_ ) ) -> isNextValue (v1,v2) )


// ----------------------------------------------------------------------------
// Return whether or not the given hand is a flush (single suit)
// Assumes the hand is valid (such as from parseHand)
// ----------------------------------------------------------------------------
let isFlush hand =
   let cardArray = hand |> toArray
   cardArray |> Array.forall( fun card -> snd card = snd cardArray.[0] )


// ----------------------------------------------------------------------------
// Return list of counts * Values, sorted descending.
// Ex: 2_ 3_ 3_ T_ J_ returns [(2,3);(1,J);(1,T);(1;2)]
// ----------------------------------------------------------------------------
let getValueCounts hand = 
   hand
   |> toArray
   |> Array.countBy( fun (v,_) -> v )     // count by only values
   |> Array.map( fun (value,count) -> (count,value) ) // swap count and value
   |> Array.sort
   |> Array.rev
   |> Array.toList

// -------------------------------------------------------------------------------------------------
// Determine the correct HandScore for the given hand.
//   Assumes the hand is valid and already sorted by Value ascending (such as from parseHand)
//   Algorithm is to determine three properties of the hand:
//     1) It is a straight (T/F)
//     2) It is a flush (T/F)
//     3) We have a list of the value counts
//   We can correctly score the hand by matching these properties
// -------------------------------------------------------------------------------------------------
let analyzeHand hand = 
   // gather hand properties
   let isStraight = isStraight hand
   let isFlush = isFlush hand
   let valueCounts = getValueCounts hand
   // match pattern of properties to determine HandScore
   match (isStraight, isFlush, valueCounts) with
   // matches with straight and/or flush
   | (true, true, (1,Value.Ace) :: _ ) -> RoyalFlush
   | (true, true, (1,high) :: _ ) -> StraightFlush high
   | (false, true, (1,high) :: _ ) -> Flush high
   | (true, false, (1,high) :: _ ) -> Straight high
   // matches based on count only
   | (false, false, _ ) ->
      match valueCounts with
      | [(4,v); (1,kick)] -> FourKind (v,kick)
      | [(3,v3); (2,v2)] -> FullHouse (v3,v2)
      | [(3,v); (1,kick); _] -> ThreeKind (v,kick)
      | [(2,vh); (2,vl); (1,kick)] -> TwoPair (vh,vl,kick)
      | [(2,v); (1,k1); (1,k2); (1,k3)] -> OnePair (v,k1,k2,k3)
      | [(1,h); (1,k1); (1,k2); (1,k3); (1,k4)] -> HighCard (h,k1,k2,k3,k4)
      | _ -> failwith "Should be unreachable, merely completes matching."
   | _ -> failwith "Should be unreachable, merely completes matching."


module Tests =
   // -------------------------------------------------------------------------
   // Test function for simple REPL checking
   // -------------------------------------------------------------------------
   let testFunc desc parseFunc input =
      try
         let actual = parseFunc input
         printfn "%s: OK with value of %A" desc actual
      with
      | Failure(message) -> printfn "%s: FAIL with %s" desc message

   // test it right away -- should say 'testFunc - OK: OK with value of 7'
   testFunc "testFunc - OK" (fun (x,y) -> x + y ) (3,4)
   // should say 'testFunc - failure: FAIL with sample failure'
   testFunc "testFunc - failure" (fun () -> failwith "sample failure") ()

   // ----- parseSuit -----
   testFunc "parseSuit success" parseSuit 'C'
   testFunc "parseSuit invalid" parseSuit 'Q'

   // ----- parseValue -----
   testFunc "parseValue success numeric" parseValue '5'
   testFunc "parseValue success face" parseValue 'Q'
   testFunc "parseValue invalid" parseValue '1'

   // ----- parseCard -----
   testFunc "parseCard success UPPER lower" parseCard "Tc"
   testFunc "parseCard success lower UPPER" parseCard "aS"
   testFunc "parseCard invalid card syntax" parseCard "123"
   testFunc "parseCard invalid value valid suit" parseCard "1s"
   testFunc "parseCard valid value invalid suit" parseCard "4K"

   // ----- areDups -----
   testFunc "areDups all unique" areDups [| (Value.Ten, Suit.Clubs);
                                            (Value.Jack, Suit.Hearts) |]
   testFunc "areDups duplicates" areDups [| (Value.Ten, Suit.Diamonds);
                                            (Value.Ten, Suit.Diamonds) |]

   // ----- cardsToHand -----
   testFunc "cardsToHand success" cardsToHand [| (Value.Three, Suit.Clubs); 
                                                 (Value.Four, Suit.Hearts);
                                                 (Value.Five, Suit.Spades);
                                                 (Value.Six, Suit.Diamonds);
                                                 (Value.Seven, Suit.Clubs); |]
   testFunc "cardsToHand invalid size" cardsToHand
            [| (Value.Three, Suit.Clubs); 
               (Value.Six, Suit.Clubs); |]

   // ----- parseHand -----
   testFunc "parseHand success" parseHand "AD 2C 3D 6S KC"
   testFunc "parseHand card parse error" parseHand "1D 2C 3D 6S KC"
   testFunc "parseHand invalid hand size" parseHand "2C 3D 6S KC"
   testFunc "parseHand duplicate card" parseHand "2C 4H 3D 6S 2C"

   // ----- isNextValue -----
   testFunc "isNextValue true - two,three" isNextValue (Value.Two, Value.Three)
   testFunc "isNextValue true - king,ace" isNextValue (Value.King, Value.Ace)
   testFunc "isNextValue false - six,ten" isNextValue (Value.Six, Value.Ten)

   // ----- isStraight -----
   testFunc "isStraight true" isStraight ( parseHand "2C 3D 4D 5S 6H" )
   testFunc "isStraight false" isStraight ( parseHand "2C 3D 4D 5S 5H" )

   // ----- isFlush -----
   testFunc "isFlush true" isFlush ( parseHand "2D 6D tD jD aD" )
   testFunc "isFlush false" isFlush ( parseHand "2C 3D 4D 5S 5H" )

   // ----- getValueCounts -----
   testFunc "getValueCounts 4kind" getValueCounts <| parseHand "2D 6D 2H 2S 2C"
   testFunc "getValueCounts fh" getValueCounts <| parseHand "3C 2D 2H 3S 2C"
   testFunc "getValueCounts 3kind" getValueCounts <| parseHand "2D 6D 2H 2S 5C"
   testFunc "getValueCounts 2pair" getValueCounts <| parseHand "2D 6D 2H 6H 3C"
   testFunc "getValueCounts 1pair" getValueCounts <| parseHand "2D 6D 4H 6H 3C"
   testFunc "getValueCounts hc" getValueCounts <| parseHand "2D 3H 4S 5C 8H"

   // ----- analyzeHand -----
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