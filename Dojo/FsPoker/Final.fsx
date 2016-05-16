(* Style of dojo taken from https://github.com/c4fsharp/Dojo-Markov-Bot

All source material for this dojo can be found at:
https://github.com/chryosolo/FsPokerDojo.git
It's MIT licensed - use it, share it, modify it. *)

(*
Introduction
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


// -----------------------------------------------------------------------------
// Capture all the ways the program can fail
// -----------------------------------------------------------------------------
type Failure =
   | Debug
   | InvalidHandSize
   | InvalidRank of char
   | InvalidSuit of char
   | InvalidCardSyntax of string
   | HandHasInvalidCards
   | HandHasDuplicates


// -----------------------------------------------------------------------------
// Convert a Failure to a human-readable string
// -----------------------------------------------------------------------------
let explainFailure failType = 
   match failType with
   | Debug -> "Requested Failure for Debug reasons"
   | InvalidHandSize -> "Invalid Hand Size"
   | InvalidRank r -> sprintf "Invalid Rank of '%c'" r
   | InvalidSuit s -> sprintf "Invalid Suit of '%c'" s
   | InvalidCardSyntax s -> sprintf "Invalid Card Syntax of '%s'" s
   | HandHasInvalidCards -> "Hand has one or more invalid cards"
   | HandHasDuplicates -> "Hand has one or more duplicate cards"


// -----------------------------------------------------------------------------
// Convert a Failure list to a human-readable string
// -----------------------------------------------------------------------------
let explainFail failList =
   String.concat ", " ( failList |> List.map explainFailure )


// -----------------------------------------------------------------------------
// Result is either Success of data, or a Fail of one or more specific Failures
// -----------------------------------------------------------------------------
type Result<'a> = 
   | Success of 'a
   | Fail of Failure list


// -----------------------------------------------------------------------------
// Turn the given "switch" function into a ROP two-track input.
// -----------------------------------------------------------------------------
let bind func =
   function // fun input -> match input with
   | Success s -> func s
   | Fail f -> Fail f


// -----------------------------------------------------------------------------
// Operator version of bind
// -----------------------------------------------------------------------------
let (>>=) input func = bind func input


// -----------------------------------------------------------------------------
// Turn the given "single track" function into a ROP two-track input.
// -----------------------------------------------------------------------------
let map func = bind ( func >> Success )


// -----------------------------------------------------------------------------
// Convert a tuple of Results into a Result of a tuple.  Fails propagate.
//   ex: (Success 2, Success 5) -> Success (2,5)
//   ex: (Fail FailType, Success "foo") -> Fail FailType
//   ex: (Fail fail1, Fail fail2) -> MultiFail [Fail fail1; Fail fail2]
// -----------------------------------------------------------------------------
let mergeResult =
   function // fun (result1,result2) -> match (result1,result2) with
   | (Success r1, Success r2) -> Success (r1,r2)
   | (Success _, Fail r2) -> Fail r2
   | (Fail r1, Success _) -> Fail r1
   | (Fail r1, Fail r2) -> Fail( List.append r1 r2 )


// -----------------------------------------------------------------------------
// Suits as an isolated concept
// -----------------------------------------------------------------------------
type Suit =
   | Clubs
   | Spades
   | Hearts
   | Diamonds


// -----------------------------------------------------------------------------
// Rank as an isolated concept
// -----------------------------------------------------------------------------
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


// -----------------------------------------------------------------------------
// Card is a Rank and Suit
// -----------------------------------------------------------------------------
type Card = Rank * Suit


// -----------------------------------------------------------------------------
// Parse a character representation of a suit into a Suit.  Valid suits are:
// 'C', 'S', 'H', 'D' -- case sensitive!
// -----------------------------------------------------------------------------
let parseSuit =
   function // fun charSuit -> match charSuit with
   | 'C' -> Success( Suit.Clubs )
   | 'S' -> Success( Suit.Spades )
   | 'H' -> Success( Suit.Hearts )
   | 'D' -> Success( Suit.Diamonds )
   | char -> Fail [ InvalidSuit char ]


// -----------------------------------------------------------------------------
// Parse a character representation of a rank into a Rank.  Valid ranks are:
// '2' .. '9', 'T', 'J', 'Q', 'K', 'A' -- case sensitive!
// -----------------------------------------------------------------------------
let parseRank =
   function // fun charRank = match charRank with
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
   | char -> Fail [ InvalidRank char ]


// -----------------------------------------------------------------------------
// Prepare the given string for parsing into a card.
// -----------------------------------------------------------------------------
let canonicalizeCardInput (strCard:string) = strCard.ToUpper().ToCharArray()


// -----------------------------------------------------------------------------
// Parse a two-character string representation of a card into a Card. Is NOT
// case sensitive.
// -----------------------------------------------------------------------------
let parseCard =
   map canonicalizeCardInput
   >> bind (
      function // success if 2 chars, fail if not two
      | [| charRank; charSuit |] -> Success (charRank, charSuit)
      | chars -> Fail [ InvalidCardSyntax ( new string( chars ) ) ] )
   >> bind (fun (charRank,charSuit) ->
      // --> call the parseValue and parseSuit functions
      let rank = parseRank charRank
      let suit = parseSuit charSuit
      mergeResult (rank,suit) )


// -----------------------------------------------------------------------------
// A hand is five cards
// -----------------------------------------------------------------------------
type Hand = Card * Card * Card * Card * Card


// -----------------------------------------------------------------------------
// If no duplicates, Success, otherwise Fail HandHasDuplicates
// -----------------------------------------------------------------------------
let validateNoDups cardList =
   let uniqueCards = List.groupBy id cardList
   if uniqueCards.Length = cardList.Length
      then Success cardList
      else Fail [HandHasDuplicates]


// -----------------------------------------------------------------------------
// Turn the list of 5 cards into a 5-tuple of cards, or fail if not 5.
// -----------------------------------------------------------------------------
let cardsToHand (cards: Result<Card> list) : Result<Hand> =
   match cards with
   // 5 card successes is a hand success
   | [ Success c0; Success c1; Success c2; Success c3; Success c4 ] ->
      Success( c0, c1, c2, c3, c4 )
   // 5 of anything else must have at least one Failure
   | [ _; _; _; _; _ ] ->
      let fails =
         cards
         |> List.choose( function | Success _ -> None | Fail f -> Some f )
         |> List.collect id
      Fail fails
   // other than 5 is an invalid hand size
   | _ -> Fail [ InvalidHandSize ]


// -----------------------------------------------------------------------------
// Tokenize the given string into a list of strings.
// -----------------------------------------------------------------------------
let tokenizeInput (strHand:string) =
   strHand.Split [|' '|]
   |> Array.toList


// -----------------------------------------------------------------------------
// Parse the hand string into a valid hand, or fail with a reason why the hank
// string was invalid.
// -----------------------------------------------------------------------------
let parseHand =
   map tokenizeInput
   >> map (fun cardTokens ->
      (cardTokens |> List.map ( Success >> parseCard )
                  |> List.sort ) )
   >> bind validateNoDups
   >> bind cardsToHand


// -----------------------------------------------------------------------------
// Possible hand scoring categories
// -----------------------------------------------------------------------------
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
   

// -----------------------------------------------------------------------------
// Return the hand (5-tuple of cards) as an array of cards
// -----------------------------------------------------------------------------
let toArray hand =
   let (c1, c2, c3, c4, c5) = hand
   [|c1;c2;c3;c4;c5|]


// -----------------------------------------------------------------------------
// Return if the second rank is exactly one more than the first rank
// -----------------------------------------------------------------------------
let isNextRank (r1 : Rank, r2 : Rank) =
   int r2 = int r1 + 1


// -----------------------------------------------------------------------------
// Return whether or not the given hand is a straight (sequential ranks)
// Assumes the hand is valid and already sorted by Rank ascending.
// -----------------------------------------------------------------------------
let isStraight = 
   toArray
   >> Array.toSeq
   >> Seq.pairwise
   >> Seq.forall( fun( (r1, _), (r2,_ ) ) -> isNextRank (r1,r2) )


// -----------------------------------------------------------------------------
// Return whether or not the given hand is a flush (single suit)
// Assumes the hand is valid (such as from parseHand)
// -----------------------------------------------------------------------------
let isFlush hand =
   let cardArray = hand |> toArray
   cardArray |> Array.forall( fun card -> snd card = snd cardArray.[0] )


// -----------------------------------------------------------------------------
// Return list of counts * Values, sorted descending.
// Ex: 2_ 3_ 3_ T_ J_ returns [(2,3);(1,J);(1,T);(1;2)]
// -----------------------------------------------------------------------------
let getRankCounts hand = 
   hand
   |> toArray
   |> Array.countBy( fun (r,_) -> r )     // count by only ranks
   |> Array.map( fun (rank, count) -> (count, rank) ) // swap count and rank
   |> Array.sort
   |> Array.rev
   |> Array.toList


// -----------------------------------------------------------------------------
// Determine the correct HandScore for the given hand.
//   Assumes the hand is valid and already sorted by Value ascending (such as
//   from parseHand)
//   Algorithm is to determine three properties of the hand:
//     1) It is a straight (T/F)
//     2) It is a flush (T/F)
//     3) We have a list of the rank counts
//   We can correctly score the hand by matching these properties
// -----------------------------------------------------------------------------
let analyzeHand (hand:Hand) = 
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
      | _ -> Fail [ Debug ]
   | _ -> Fail [ Debug ]


module Tests =
   let kickoff data = Success data
   let parse_card = Success >> parseCard
   let parse_hand = Success >> parseHand


   // --------------------------------------------------------------------------
   // Basic test compares expected to actual and prints results and meta info
   // --------------------------------------------------------------------------
   let test compareOp desc expecting actual = 
      match compareOp expecting actual with
      | true -> printfn "(OK) %s" desc
      | false ->
         printfn "FAIL %s: expecting %A but was %A" desc expecting actual


   // --------------------------------------------------------------------------
   // Partial application for common testing types
   // --------------------------------------------------------------------------
   let assertEq desc = test (=) desc
   let assertSuccess desc expecting = assertEq desc (Success expecting)
   let assertTrue desc = assertEq desc true
   let assertFalse desc = assertEq desc false


   // --------------------------------------------------------------------------
   // Partial application for testing a Fail with explanations on printout
   // --------------------------------------------------------------------------
   let assertFail desc failType value = 
      let failExplained = desc + ": " + (explainFail failType)
      test (fun exp act -> (Fail exp) = act) failExplained failType value


   // --------------------------------------------------------------------------
   // Partial application for testing a failwith with explanations on printout
   // --------------------------------------------------------------------------
   let assertFailWith desc op expFailType = 
      try 
         op() |> ignore
      with Failure msg -> assertEq desc expFailType msg


   // ----- mergeResult -----
   assertEq "mergeResult success" ( Success( 1,2 ) )
         ( mergeResult (Success 1,Success 2) )
   assertFail "mergeResult success,fail" [Debug]
         ( mergeResult (Success 1,Fail [Debug]) )
   assertFail "mergeResult fail,success" [Debug]
         ( mergeResult (Fail [Debug],Success 1) )
   assertFail "mergeResult fail,fail" [Debug; Debug]
         (mergeResult (Fail [Debug],Fail [Debug]) )
   
   // ----- parseSuit -----
   assertSuccess "parseSuit success" (Suit.Hearts) (parseSuit 'H')
   assertFail "parseSuit invalid" [InvalidSuit 'Q'] (parseSuit 'Q')

   // ----- parseRank -----
   assertSuccess "parseRank numeric" (Rank.Five) (parseRank '5')
   assertSuccess "parseRank face" (Rank.Queen) (parseRank 'Q')
   assertFail "parseRank fail" [InvalidRank '1'] (parseRank '1')

   // ----- parseCard -----
   assertSuccess "parseCard success1" (Rank.Ten,Suit.Clubs) (parse_card "Tc")
   assertSuccess "parseCard success2" (Rank.Ace,Suit.Spades) (parse_card "aS")
   assertFail "parseCard invalid syntax" [InvalidCardSyntax "123"]
         (parse_card "123")
   assertFail "parseCard invalid rank" [InvalidRank '1'] (parse_card "1s")
   assertFail "parseCard invalid suit" [InvalidSuit 'K'] (parse_card "4K")

   // ----- validateNoDups -----
   assertSuccess "validateNoDups all unique"
      [(Rank.Ten, Suit.Clubs);(Rank.Jack,Suit.Hearts)]
      (validateNoDups [(Rank.Ten, Suit.Clubs);(Rank.Jack, Suit.Hearts)] )
   assertFail "validateNoDups duplicates" [HandHasDuplicates]
      (validateNoDups [(Rank.Ten, Suit.Diamonds);(Rank.Ten, Suit.Diamonds)])

   // ----- cardsToHand -----
   assertSuccess  "cardsToHand success"
                  (  (Rank.Three, Suit.Clubs), (Rank.Four, Suit.Hearts),
                     (Rank.Five, Suit.Spades), (Rank.Six, Suit.Diamonds),
                     (Rank.Seven, Suit.Clubs) )
                  (  cardsToHand [  Success (Rank.Three, Suit.Clubs);
                                    Success (Rank.Four, Suit.Hearts);
                                    Success (Rank.Five, Suit.Spades);
                                    Success (Rank.Six, Suit.Diamonds);
                                    Success (Rank.Seven, Suit.Clubs); ] )
   assertFail  "cardsToHand fail" [InvalidRank 'y';InvalidSuit '4']
               (  cardsToHand [  Success (Rank.Three, Suit.Clubs);
                                 Fail [InvalidRank 'y'];
                                 Success (Rank.Five, Suit.Spades);
                                 Fail [InvalidSuit '4'];
                                 Success (Rank.Seven, Suit.Clubs); ] )
   assertFail  "cardsToHand invalid size" [InvalidHandSize]
               ( cardsToHand [ Success (Rank.Three, Suit.Clubs) ] )
   // ----- parseHand -----
   assertSuccess  "parseHand success"
                  (  (Rank.Two, Suit.Clubs), (Rank.Three, Suit.Diamonds),
                     (Rank.Six, Suit.Spades), (Rank.King, Suit.Clubs),
                     (Rank.Ace, Suit.Diamonds) )
                  ( parse_hand "aD 2C 3D 6S kC" )
   assertFail  "parseHand card parse error" [InvalidRank '1']
               ( parse_hand "1D 2C 3D 6S KC" )
   assertFail  "parseHand invalid hand size" [InvalidHandSize]
               ( parse_hand "2C 3D 6S KC" )
   assertFail  "parseHand duplicate card" [HandHasDuplicates]
               ( parse_hand "2C 4H 3D 6S 2C" )

   // ----- isNextValue -----
   assertTrue "isNextRank true - 2,3" ( isNextRank (Rank.Two, Rank.Three) )
   assertTrue "isNextRank true - K,A" ( isNextRank (Rank.King, Rank.Ace) )
   assertFalse "isNextRank false - 6,T" ( isNextRank (Rank.Six, Rank.Ten) )

   // ----- isStraight -----
   let getSuccess =  function
                     | Success s -> s
                     | Fail f -> failwith (explainFail f)
   let getHand = parse_hand >> getSuccess
   assertTrue "isStraight true" ( getHand "2C 3D 4D 5S 6H" |> isStraight )
   assertFalse "isStraight false"  ( getHand "2C 3D 4D 5S 5H" |> isStraight )

   // ----- isFlush -----
   assertTrue "isFlush true" ( getHand "2D 6D tD jD aD" |> isFlush )
   assertFalse "isFlush false" ( getHand "2C 3D 4D 5S 5H" |> isFlush )

   // ----- getRankCounts -----
   assertEq "getRankCounts 4kind" [ 4,Rank.Two; 1,Rank.Six ]
            ( getHand "2D 6D 2H 2S 2C" |> getRankCounts )
   assertEq "getRankCounts fh" [ 3,Rank.Two; 2,Rank.Three ]
            ( getHand "3C 2D 2H 3S 2C" |> getRankCounts )
   assertEq "getRankCounts 3kind" [ 3,Rank.Two; 1,Rank.Six; 1,Rank.Five ]
            ( getHand "2D 6D 2H 2S 5C" |> getRankCounts )
   assertEq "getRankCounts 2pair" [ 2,Rank.Six; 2,Rank.Two; 1,Rank.Three ]
            ( getHand "2D 6D 2H 6H 3C" |> getRankCounts )
   assertEq "getRankCounts 1pair" [ 2,Rank.Six; 1,Rank.Four;
                                    1,Rank.Three; 1,Rank.Two ]
            ( getHand "2D 6D 4H 6H 3C" |> getRankCounts )
   assertEq "getRankCounts hc" [ 1,Rank.Eight; 1,Rank.Five; 1,Rank.Four;
                                 1,Rank.Three; 1,Rank.Two ]
            ( getHand "2D 3H 4S 5C 8H" |> getRankCounts )

   // ----- analyzeHand -----
   assertSuccess  "analyzeHand Royal" RoyalFlush
                  ( getHand "tC jC qC kC aC" |> analyzeHand )
   assertSuccess  "analyzeHand StrFlush" ( StraightFlush Rank.King )
                  ( getHand "9C tC jC qC kC" |> analyzeHand )
   assertSuccess  "analyzeHand Flush" ( Flush Rank.King )
                  ( getHand "3C tC jC qC kC" |> analyzeHand )
   assertSuccess  "analyzeHand Straight" ( Straight Rank.King )
                  ( getHand "9S tS jC qC kC" |> analyzeHand )
   assertSuccess  "analyzeHand FourKind" ( FourKind (Rank.King,Rank.Two) )
                  ( getHand "kS kD kC kH 2C" |> analyzeHand )
   assertSuccess  "analyzeHand FullHouse" ( FullHouse (Rank.King,Rank.Two) )
                  ( getHand "kS kD kC 2H 2C" |> analyzeHand )
   assertSuccess  "analyzeHand ThreeKind" ( ThreeKind (Rank.King,Rank.Four ) )
                  ( getHand "kS kD kC 4H 2C" |> analyzeHand )
   assertSuccess  "analyzeHand TwoPair"
                  ( TwoPair (Rank.King,Rank.Queen,Rank.Two ) )
                  ( getHand "kS kD qC qH 2C" |> analyzeHand )
   assertSuccess  "analyzeHand OnePair"
                  ( OnePair (Rank.King,Rank.Queen,Rank.Four,Rank.Two ) )
                  ( getHand "kS kD qC 4H 2C" |> analyzeHand )
   assertSuccess  "analyzeHand HighCard"
                  ( HighCard (Rank.Ace,Rank.Ten,Rank.Six,Rank.Five,Rank.Two ) )
                  ( getHand "aS tD 6h 5c 2c" |> analyzeHand )
