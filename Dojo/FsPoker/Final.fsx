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
//failwith( "Any time you see '-->', this is a task for you!" )


// ----------------------------------------------------------------------------
// Capturing of how the program can fail
// ----------------------------------------------------------------------------
type Failures =
   | Debug
   | InvalidHandSize
   | InvalidRank of char
   | InvalidSuit of char
   | InvalidCardSyntax of string
   | HandHasInvalidCards
   | HandHasDuplicates
   | MultiFail of Failures list


// ----------------------------------------------------------------------------
// Convert Failures to a human-readable string
// ----------------------------------------------------------------------------
let rec explainFailure failType = 
   match failType with
   | Debug -> "Debug Fail"
   | InvalidHandSize -> "Invalid Hand Size"
   | InvalidRank r -> sprintf "Invalid Rank of '%c'" r
   | InvalidSuit s -> sprintf "Invalid Suit of '%c'" s
   | InvalidCardSyntax s -> sprintf "Invalid Card Syntax of '%s'" s
   | HandHasInvalidCards -> "Hand has one or more invalid cards"
   | HandHasDuplicates -> "Hand has one or more duplicate cards"
   | MultiFail list -> (String.concat ", " (list |> List.map explainFailure))


// ----------------------------------------------------------------------------
// Result is either Success of a type, or a Fail of a specific Failures type
// ----------------------------------------------------------------------------
type Result<'a> = 
   | Success of 'a
   | Fail of Failures


// ----------------------------------------------------------------------------
// Either return the underlying type of a Result.Success or failwith if
// Result.Failure
// ----------------------------------------------------------------------------
let getSuccess result = 
   match result with
   | Success s -> s
   | Fail _ -> failwith "Expecting result to be a success"


// ----------------------------------------------------------------------------
// Convert a tuple of Results into a Result of a tuple.  Fails propagate.
//   ex: (Success 2, Success 5) -> Success (2,5)
//   ex: (Fail FailType, Success "foo") -> Fail FailType
//   ex: (Fail fail1, Fail fail2) -> MultiFail [Fail fail1; Fail fail2]
// ----------------------------------------------------------------------------
let mergeResult result1 result2 = 
   match (result1, result2) with
   | (Success r1, Success r2) -> Success (r1,r2)
   | (Success _, Fail r2) -> Fail r2
   | (Fail r1, Success _) -> Fail r1
   | (Fail r1, Fail r2) -> Fail(MultiFail [ r1; r2 ])


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
   | 'C' -> Success( Suit.Clubs )
   | 'S' -> Success( Suit.Spades )
   | 'H' -> Success( Suit.Hearts )
   | 'D' -> Success( Suit.Diamonds )
   | _ -> Fail( InvalidSuit charSuit )


// ----------------------------------------------------------------------------
// Parse a character representation of a rank into a Rank.  Valid ranks are:
// '2' .. '9', 'T', 'J', 'Q', 'K', 'A' -- case sensitive!
// ----------------------------------------------------------------------------
let parseRank charRank =
   match charRank with
   | '2' -> Success( Rank.Two )
   | '3' -> Success( Rank.Three )
   | '4' -> Success( Rank.Four )
   | '5' -> Success( Rank.Five )
   | '6' -> Success( Rank.Six )
   | '7' -> Success( Rank.Seven )
   | '8' -> Success( Rank.Eight )
   | '9' -> Success( Rank.Nine )
   | 'T' -> Success( Rank.Ten )
   | 'J' -> Success( Rank.Jack )
   | 'Q' -> Success( Rank.Queen )
   | 'K' -> Success( Rank.King )
   | 'A' -> Success( Rank.Ace )
   | _ -> Fail( InvalidRank charRank )


// ----------------------------------------------------------------------------
// Parse a two-character string representation of a card into a Card. Is NOT
// case sensitive.
// ----------------------------------------------------------------------------
let parseCard (strCard:string) : Result<Card> =
   // --> add a ToUpper so you can simplify above parsing!
   let chars = strCard.ToUpper().ToCharArray()
   match chars with
   | [| charRank; charSuit |] ->
      // --> call the parseValue and parseSuit functions
      let rank = parseRank charRank
      let suit = parseSuit charSuit
      mergeResult rank suit
   // array isn't two items long
   | _ -> Fail( InvalidCardSyntax strCard )


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
let cardsToHand (cards: Result<Card>[]) : Result<Hand> =
   match cards with
   | [| c0; c1; c2; c3; c4 |] ->
      let fails =
         cards
         |> Array.choose( fun c -> match c with
                                   | Success _ -> None
                                   | Fail f -> Some f )
         |> Array.toList
      match fails with
      | [] -> Success( c0, c1, c2, c3, c4 )
      | [ fail ] -> Fail( fail )
      | _ -> Fail( MultiFail fails )
   | _ -> Fail( InvalidHandSize )


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
   | true -> Fail( HandHasDuplicates )
   | false ->
      cards
      |> cardsToHand


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
//   Assumes the hand is valid and already sorted by Value ascending (such as
//   from parseHand)
//   Algorithm is to determine three properties of the hand:
//     1) It is a straight (T/F)
//     2) It is a flush (T/F)
//     3) We have a list of the rank counts
//   We can correctly score the hand by matching these properties
// ----------------------------------------------------------------------------
let analyzeHand hand = 
   // gather hand properties
   let isStraight = isStraight hand
   let isFlush = isFlush hand
   let rankCounts = getRankCounts hand
   // match pattern of properties to determine HandScore
   match (isStraight, isFlush, rankCounts) with
   // matches with straight and/or flush
   | (true, true, (1,Rank.Ace) :: _ ) -> Success RoyalFlush
   | (true, true, (1,high) :: _ ) -> Success( StraightFlush high )
   | (false, true, (1,high) :: _ ) -> Success( Flush high )
   | (true, false, (1,high) :: _ ) -> Success( Straight high )
   // matches based on count only
   | (false, false, _ ) ->
      match rankCounts with
      | [(4,rank); (1,kick)] -> Success( FourKind (rank,kick) )
      | [(3,rank3); (2,rank2)] -> Success( FullHouse (rank3,rank2) )
      | [(3,rank); (1,kick); _] -> Success( ThreeKind (rank,kick) )
      | [(2,rH); (2,rL); (1,kick)] -> Success( TwoPair (rH,rL,kick) )
      | [(2,r); (1,k1); (1,k2); (1,k3)] -> Success( OnePair (r,k1,k2,k3) )
      | [(1,h); (1,k1); (1,k2); (1,k3); (1,k4)] ->
         Success( HighCard (h,k1,k2,k3,k4) )
      | _ -> Fail( Debug )
   | _ -> Fail( Debug )


module Tests =
   // -------------------------------------------------------------------------
   // Basic test compares expected to actual and prints results and meta info
   // -------------------------------------------------------------------------
   let test compareOp desc expecting actual = 
      match compareOp expecting actual with
      | true -> printfn "(OK) %s" desc
      | false ->
         printfn "FAIL %s: expecting %A but was %A" desc expecting actual


   // -------------------------------------------------------------------------
   // Partial application for common testing types
   // -------------------------------------------------------------------------
   let assertEq desc = test (=) desc
   let assertSuccess desc expecting = assertEq desc (Success expecting)
   let assertTrue desc = assertEq desc true
   let assertFalse desc = assertEq desc false


   // -------------------------------------------------------------------------
   // Partial application for testing a Fail with explanations on printout
   // -------------------------------------------------------------------------
   let assertFail desc failType value = 
      let failExplained = desc + ": " + (explainFailure failType)
      test (fun exp act -> (Fail exp) = act) failExplained failType value


   // -------------------------------------------------------------------------
   // Partial application for testing a failwith with explanations on printout
   // -------------------------------------------------------------------------
   let assertFailWith desc op expFailType = 
      try 
         op() |> ignore
      with Failure msg -> assertEq desc expFailType msg

   // ----- getSuccess -----
   assertEq "getSuccess valid" 5 (getSuccess (Success 5))
   assertFailWith "getSuccess failure" (fun () -> getSuccess( Fail Debug ) )

   // ----- mergeResult -----
   assertEq "mergeResult success" ( Success( 1,2 ) )
         ( mergeResult (Success 1) (Success 2) )
   assertFail "mergeResult success,fail" Debug
         ( mergeResult (Success 1) (Fail Debug) )
   assertFail "mergeResult fail,success" Debug
         ( mergeResult (Fail Debug) (Success 1) )
   assertFail "mergeResult fail,fail" (MultiFail [Debug; Debug])
         (mergeResult (Fail Debug) (Fail Debug) )
   
   // ----- parseSuit -----
   assertSuccess "parseSuit success" (Suit.Hearts) (parseSuit 'H')
   assertFail "parseSuit invalid" (InvalidSuit 'Q') (parseSuit 'Q')

   // ----- parseRank -----
   assertSuccess "parseRank numeric" (Rank.Five) (parseRank '5')
   assertSuccess "parseRank face" (Rank.Queen) (parseRank 'Q')
   assertFail "parseRank fail" (InvalidRank '1') (parseRank '1')

   // ----- parseCard -----
   assertSuccess "parseCard success1" (Rank.Ten,Suit.Clubs) (parseCard "Tc")
   assertSuccess "parseCard success2" (Rank.Ace,Suit.Spades) (parseCard "aS")
   assertFail "parseCard invalid syntax" (InvalidCardSyntax "123")
         (parseCard "123")
   assertFail "parseCard invalid rank" (InvalidRank '1') (parseCard "1s")
   assertFail "parseCard invalid suit" (InvalidSuit 'K') (parseCard "4K")

   // ----- areDups -----
   assertFalse "areDups all unique" (areDups [| (Rank.Ten, Suit.Clubs);
                                                (Rank.Jack, Suit.Hearts) |] )
   assertTrue "areDups duplicates" (areDups [| (Rank.Ten, Suit.Diamonds);
                                               (Rank.Ten, Suit.Diamonds) |] )

   // ----- cardsToHand -----
   assertSuccess  "cardsToHand success"
                  (  (Rank.Three, Suit.Clubs), (Rank.Four, Suit.Hearts),
                     (Rank.Five, Suit.Spades), (Rank.Six, Suit.Diamonds),
                     (Rank.Seven, Suit.Clubs) )
                  (  cardsToHand [| Success (Rank.Three, Suit.Clubs);
                                    Success (Rank.Four, Suit.Hearts);
                                    Success (Rank.Five, Suit.Spades);
                                    Success (Rank.Six, Suit.Diamonds);
                                    Success (Rank.Seven, Suit.Clubs); |] )
   assertFail  "cardsToHand single fail" ( InvalidRank 'y' )
               (  cardsToHand [| Success (Rank.Three, Suit.Clubs);
                                 Fail ( InvalidRank 'y' );
                                 Success (Rank.Five, Suit.Spades);
                                 Success (Rank.Six, Suit.Diamonds);
                                 Success (Rank.Seven, Suit.Clubs); |] )
   assertFail  "cardsToHand multi fail"
               ( MultiFail [ InvalidRank 'y'; InvalidSuit '4' ] )
               (  cardsToHand [| Success (Rank.Three, Suit.Clubs);
                                 Fail ( InvalidRank 'y' );
                                 Fail ( InvalidSuit '4' );
                                 Success (Rank.Six, Suit.Diamonds);
                                 Success (Rank.Seven, Suit.Clubs); |] )
   assertFail  "cardsToHand invalid size" InvalidHandSize
               ( cardsToHand [| Success (Rank.Three, Suit.Clubs) |] )
   // ----- parseHand -----
   assertSuccess  "parseHand success"
                  (  (Rank.Two, Suit.Clubs), (Rank.Three, Suit.Diamonds),
                     (Rank.Six, Suit.Spades), (Rank.King, Suit.Clubs),
                     (Rank.Ace, Suit.Diamonds) )
                  ( parseHand "aD 2C 3D 6S kC" )
   assertFail  "parseHand card parse error" ( InvalidRank '1' )
               ( parseHand "1D 2C 3D 6S KC" )
   assertFail  "parseHand invalid hand size" InvalidHandSize
               ( parseHand "2C 3D 6S KC" )
   assertFail  "parseHand duplicate card" HandHasDuplicates
               ( parseHand "2C 4H 3D 6S 2C" )

   // ----- isNextValue -----
   assertTrue "isNextRank true - 2,3" ( isNextRank (Rank.Two, Rank.Three) )
   assertTrue "isNextRank true - K,A" ( isNextRank (Rank.King, Rank.Ace) )
   assertFalse "isNextRank false - 6,T" ( isNextRank (Rank.Six, Rank.Ten) )

   // ----- isStraight -----
   let parseHandVal = parseHand >> getSuccess
   assertTrue "isStraight true" ( parseHandVal "2C 3D 4D 5S 6H" |> isStraight )
   assertFalse "isStraight false"
               ( parseHandVal "2C 3D 4D 5S 5H" |> isStraight )

   // ----- isFlush -----
   assertTrue "isFlush true" ( parseHandVal "2D 6D tD jD aD" |> isFlush )
   assertFalse "isFlush false" ( parseHandVal "2C 3D 4D 5S 5H" |> isFlush )

   // ----- getRankCounts -----
   assertEq "getRankCounts 4kind" [ 4,Rank.Two; 1,Rank.Six ]
            ( parseHandVal "2D 6D 2H 2S 2C" |> getRankCounts )
   assertEq "getRankCounts fh" [ 3,Rank.Two; 2,Rank.Three ]
            ( parseHandVal "3C 2D 2H 3S 2C" |> getRankCounts )
   assertEq "getRankCounts 3kind" [ 3,Rank.Two; 1,Rank.Six; 1,Rank.Five ]
            ( parseHandVal "2D 6D 2H 2S 5C" |> getRankCounts )
   assertEq "getRankCounts 2pair" [ 2,Rank.Six; 2,Rank.Two; 1,Rank.Three ]
            ( parseHandVal "2D 6D 2H 6H 3C" |> getRankCounts )
   assertEq "getRankCounts 1pair" [ 2,Rank.Six; 1,Rank.Four;
                                    1,Rank.Three; 1,Rank.Two ]
            ( parseHandVal "2D 6D 4H 6H 3C" |> getRankCounts )
   assertEq "getRankCounts hc" [ 1,Rank.Eight; 1,Rank.Five; 1,Rank.Four;
                                 1,Rank.Three; 1,Rank.Two ]
            ( parseHandVal "2D 3H 4S 5C 8H" |> getRankCounts )

   // ----- analyzeHand -----
   assertSuccess  "analyzeHand Royal" RoyalFlush
                  ( parseHandVal "tC jC qC kC aC" |> analyzeHand )
   assertSuccess  "analyzeHand StrFlush" ( StraightFlush Rank.King )
                  ( parseHandVal "9C tC jC qC kC" |> analyzeHand )
   assertSuccess  "analyzeHand Flush" ( Flush Rank.King )
                  ( parseHandVal "3C tC jC qC kC" |> analyzeHand )
   assertSuccess  "analyzeHand Straight" ( Straight Rank.King )
                  ( parseHandVal "9S tS jC qC kC" |> analyzeHand )
   assertSuccess  "analyzeHand FourKind" ( FourKind (Rank.King,Rank.Two) )
                  ( parseHandVal "kS kD kC kH 2C" |> analyzeHand )
   assertSuccess  "analyzeHand FullHouse" ( FullHouse (Rank.King,Rank.Two) )
                  ( parseHandVal "kS kD kC 2H 2C" |> analyzeHand )
   assertSuccess  "analyzeHand ThreeKind" ( ThreeKind (Rank.King,Rank.Four ) )
                  ( parseHandVal "kS kD kC 4H 2C" |> analyzeHand )
   assertSuccess  "analyzeHand TwoPair"
                  ( TwoPair (Rank.King,Rank.Queen,Rank.Two ) )
                  ( parseHandVal "kS kD qC qH 2C" |> analyzeHand )
   assertSuccess  "analyzeHand OnePair"
                  ( OnePair (Rank.King,Rank.Queen,Rank.Four,Rank.Two ) )
                  ( parseHandVal "kS kD qC 4H 2C" |> analyzeHand )
   assertSuccess  "analyzeHand HighCard"
                  ( HighCard (Rank.Ace,Rank.Ten,Rank.Six,Rank.Five,Rank.Two ) )
                  ( parseHandVal "aS tD 6h 5c 2c" |> analyzeHand )
