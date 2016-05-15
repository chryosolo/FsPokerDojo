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

The FINAL script shows my personal implementation of all intermediate tasks.
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
// Rank as an isolated concept
// ----------------------------------------------------------------------------
type Rank = 
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
// Card is a Rank and Suit
// ----------------------------------------------------------------------------
type Card = Rank * Suit


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
// Parse a character representation of a rank into a Rank.  Valid values are:
// '2' .. '9', 'T', 'J', 'Q', 'K', 'A' -- case sensitive!
// ----------------------------------------------------------------------------
let parseRank charRank =
   match charRank with
   | '2' -> Rank.Two
   | '3' -> Rank.Three
   | '4' -> Rank.Four
   | '5' -> Rank.Five
   | '6' -> Rank.Six
   | '7' -> Rank.Seven
   | '8' -> Rank.Eight
   | '9' -> Rank.Nine
   | 'T' -> Rank.Ten
   | 'J' -> Rank.Jack
   | 'Q' -> Rank.Queen
   | 'K' -> Rank.King
   | 'A' -> Rank.Ace
   | _ -> failwith "Invalid rank." // wildcard match throws an error


// ----------------------------------------------------------------------------
// Parse a two-character string representation of a card into a Card. Is NOT
// case sensitive.
// ----------------------------------------------------------------------------
let parseCard (strCard:string) =
   // --> add a ToUpper so you can simplify above parsing!
   let chars = strCard.ToUpper().ToCharArray()
   match chars with
   | [| charRank; charSuit |] ->
      // --> call the parseRank and parseSuit functions
      let rank = parseRank charRank
      let suit = parseSuit charSuit
      rank,suit
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
// Turn the array of 5 cards into a 5-tuple of cards, or fail if not 5.
// ----------------------------------------------------------------------------
let cardsToHand (cards:'a[]) =
   match cards.Length with
   // --> add case when length is 5, return a 5-tuple of cards
   | 5 -> cards.[0], cards.[1], cards.[2], cards.[3], cards.[4]
   | _ -> failwith "Invalid hand size."


// ----------------------------------------------------------------------------
// Parse the hand string into a valid hand, or fail with a reason why the hank
// string was invalid.
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
   | HighCard of Rank * Rank * Rank * Rank * Rank
   | OnePair of Rank * Rank * Rank * Rank
   | TwoPair of Rank * Rank * Rank
   | ThreeKind of Rank * Rank
   | Straight of Rank
   | Flush of Rank
   | FullHouse of Rank * Rank
   | FourKind of Rank * Rank
   | StraightFlush of Rank
   | RoyalFlush
   

// ----------------------------------------------------------------------------
// Return the hand (5-tuple of cards) as an array of cards
// ----------------------------------------------------------------------------
let toArray hand =
   let (c1, c2, c3, c4, c5) = hand
   [|c1;c2;c3;c4;c5|]


// ----------------------------------------------------------------------------
// Return if the second rank is exactly one more than the first rank
// ----------------------------------------------------------------------------
let isNextRank (r1 : Rank, r2 : Rank) =
   int r2 = int r1 + 1


// ----------------------------------------------------------------------------
// Return whether or not the given hand is a straight (sequential ranks)
// Assumes the hand is valid and already sorted by Rank ascending.
// ----------------------------------------------------------------------------
let isStraight hand = 
   hand
   |> toArray
   |> Array.toSeq
   |> Seq.pairwise
   |> Seq.forall( fun( (r1, _), (r2,_ ) ) -> isNextRank (r1,r2) )


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
let getRankCounts hand = 
   hand
   |> toArray
   |> Array.countBy( fun (r,_) -> r )     // count by only ranks
   |> Array.map( fun (rank, count) -> (count, rank) ) // swap count and rank
   |> Array.sort
   |> Array.rev
   |> Array.toList

// ----------------------------------------------------------------------------
// Determine the correct HandScore for the given hand.
//   Assumes the hand is valid and already sorted by Value ascending (such as from parseHand)
//   Algorithm is to determine three properties of the hand:
//     1) It is a straight (T/F)
//     2) It is a flush (T/F)
//     3) We have a list of the rank counts
//   We can correctly score the hand by matching these properties
// -------------------------------------------------------------------------------------------------
let analyzeHand hand = 
   // gather hand properties
   let isStraight = isStraight hand
   let isFlush = isFlush hand
   let rankCounts = getRankCounts hand
   // match pattern of properties to determine HandScore
   match (isStraight, isFlush, rankCounts) with
   // matches with straight and/or flush
   | (true, true, (1,Rank.Ace) :: _ ) -> RoyalFlush
   | (true, true, (1,high) :: _ ) -> StraightFlush high
   | (false, true, (1,high) :: _ ) -> Flush high
   | (true, false, (1,high) :: _ ) -> Straight high
   // matches based on count only
   | (false, false, _ ) ->
      match rankCounts with
      | [(4,rank); (1,kick)] -> FourKind (rank,kick)
      | [(3,rank3); (2,rank2)] -> FullHouse (rank3,rank2)
      | [(3,rank); (1,kick); _] -> ThreeKind (rank,kick)
      | [(2,rankH); (2,rankL); (1,kick)] -> TwoPair (rankH,rankL,kick)
      | [(2,rank); (1,k1); (1,k2); (1,k3)] -> OnePair (rank,k1,k2,k3)
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
   testFunc "parseRank success numeric" parseRank '5'
   testFunc "parseRank success face" parseRank 'Q'
   testFunc "parseRank invalid" parseRank '1'

   // ----- parseCard -----
   testFunc "parseCard success UPPER lower" parseCard "Tc"
   testFunc "parseCard success lower UPPER" parseCard "aS"
   testFunc "parseCard invalid card syntax" parseCard "123"
   testFunc "parseCard invalid rank" parseCard "1s"
   testFunc "parseCard invalid suit" parseCard "4K"

   // ----- areDups -----
   testFunc "areDups all unique" areDups [| (Rank.Ten, Suit.Clubs);
                                            (Rank.Jack, Suit.Hearts) |]
   testFunc "areDups duplicates" areDups [| (Rank.Ten, Suit.Diamonds);
                                            (Rank.Ten, Suit.Diamonds) |]

   // ----- cardsToHand -----
   testFunc "cardsToHand success" cardsToHand [| (Rank.Three, Suit.Clubs); 
                                                 (Rank.Four, Suit.Hearts);
                                                 (Rank.Five, Suit.Spades);
                                                 (Rank.Six, Suit.Diamonds);
                                                 (Rank.Seven, Suit.Clubs); |]
   testFunc "cardsToHand invalid size" cardsToHand
            [| (Rank.Three, Suit.Clubs); 
               (Rank.Six, Suit.Clubs); |]

   // ----- parseHand -----
   testFunc "parseHand success" parseHand "AD 2C 3D 6S KC"
   testFunc "parseHand card parse error" parseHand "1D 2C 3D 6S KC"
   testFunc "parseHand invalid hand size" parseHand "2C 3D 6S KC"
   testFunc "parseHand duplicate card" parseHand "2C 4H 3D 6S 2C"

   // ----- isNextValue -----
   testFunc "isNextRank true - two,three" isNextRank (Rank.Two, Rank.Three)
   testFunc "isNextRank true - king,ace" isNextRank (Rank.King, Rank.Ace)
   testFunc "isNextRank false - six,ten" isNextRank (Rank.Six, Rank.Ten)

   // ----- isStraight -----
   testFunc "isStraight true" isStraight ( parseHand "2C 3D 4D 5S 6H" )
   testFunc "isStraight false" isStraight ( parseHand "2C 3D 4D 5S 5H" )

   // ----- isFlush -----
   testFunc "isFlush true" isFlush ( parseHand "2D 6D tD jD aD" )
   testFunc "isFlush false" isFlush ( parseHand "2C 3D 4D 5S 5H" )

   // ----- getValueCounts -----
   testFunc "getRankCounts 4kind" getRankCounts <| parseHand "2D 6D 2H 2S 2C"
   testFunc "getRankCounts fh" getRankCounts <| parseHand "3C 2D 2H 3S 2C"
   testFunc "getRankCounts 3kind" getRankCounts <| parseHand "2D 6D 2H 2S 5C"
   testFunc "getRankCounts 2pair" getRankCounts <| parseHand "2D 6D 2H 6H 3C"
   testFunc "getRankCounts 1pair" getRankCounts <| parseHand "2D 6D 4H 6H 3C"
   testFunc "getRankCounts hc" getRankCounts <| parseHand "2D 3H 4S 5C 8H"

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