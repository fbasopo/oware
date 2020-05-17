module Oware

type StartingPosition =
    | South
    | North
 type Player ={
      score : int
      houses : int*int*int*int*int*int
  }
  type Board ={
      state : string
      play1 : Player
      play2 : Player 
  }
    

let getSeeds n board = 
    let {play1 =myplayer1; play2 = myplayer2} = board
    let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myplayer1.houses,myplayer2.houses
    match n with
        |1 -> h1|2 -> h2|3 -> h3|4 -> h4|5 -> h5|6 -> h6|7 -> h7|8 -> h8|9 -> h9|10 -> h10|11 -> h11|_ -> h12 //find out how many seeds each house has 

//The actual playing of the game
let useHouse n board = 
    let {play1 =myplayer1; play2 = myplayer2} = board
    let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myplayer1.houses,myplayer2.houses
    let myBoardState = board.state
    let south_Total = h1+h2+h3+h4+h5+h6
    let north_Total = h7+h8+h9+h10+h11+h12
    let south_Score = myplayer1.score
    let north_Score = myplayer2.score
    

    let setSeeds myBoard currentHouse v  =  
        let {play1 =myplayer1; play2 = myplayer2} = myBoard
        let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myplayer1.houses,myplayer2.houses
        let south_Houses,north_Houses = 
          match currentHouse with
             |1 -> ((v,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12))|2 -> ((h1,v,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12))|3 -> ((h1,h2,v,h4,h5,h6),(h7,h8,h9,h10,h11,h12))
             |4 -> ((h1,h2,h3,v,h5,h6),(h7,h8,h9,h10,h11,h12))|5 -> ((h1,h2,h3,h4,v,h6),(h7,h8,h9,h10,h11,h12))|6 -> ((h1,h2,h3,h4,h5,v),(h7,h8,h9,h10,h11,h12))
             |7 -> ((h1,h2,h3,h4,h5,h6),(v,h8,h9,h10,h11,h12))|8 -> ((h1,h2,h3,h4,h5,h6),(h7,v,h9,h10,h11,h12))|9 -> ((h1,h2,h3,h4,h5,h6),(h7,h8,v,h10,h11,h12))
             |10 -> ((h1,h2,h3,h4,h5,h6),(h7,h8,h9,v,h11,h12))|11 -> ((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,v,h12))|_ -> ((h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,v))

        let new_player1 = { myplayer1 with houses = south_Houses }
        let new_player2 = { myplayer2 with houses = north_Houses }
        {myBoard with play1=new_player1;play2=new_player2} // Return updated board with given house set to specified value

    let examSide seed house player_Score =
    // Checks to see if a player is getting seeds from the other side
         let south_Total,_ = player_Score
         let _,north_Total = player_Score
         match seed with
             |"South's turn" -> match house,north_Total<>0,north_Total=0&&south_Total=0 with
                                 |_,_,true -> true // This results in a draw
                                 |7,true,_|8,true,_|9,true,_|10,true,_|11,true,_|12,true,_-> true // Player is at the other side so seeds can be taken
                                 |_ -> false // Unable to get seeds from opponent
             |_ -> match house,south_Total<>0,north_Total=0&&south_Total=0 with 
                    |_,_,true -> true // This results in a draw
                    |1,true,_|2,true,_|3,true,_|4,true,_|5,true,_|6,true,_ -> true // Player is at the other side so seeds can be taken
                    |_ -> false // Unable to get seeds from opponent

    let  seize_Seeds myBoard seize_House seed = 
    // Collects  seeds from opponent 
         let rec Seize myBoard score seize_House = 
            let {play1 =myplayer1; play2 = myplayer2} = myBoard
            let (h1,h2,h3,h4,h5,h6),(h7,h8,h9,h10,h11,h12) = myplayer1.houses,myplayer2.houses

            let south_Total,north_Total = (h1+h2+h3+h4+h5+h6),(h7+h8+h9+h10+h11+h12)
            let number_of_seeds = getSeeds seize_House myBoard // The number of seeds to collect

            let newsouth_Total,newnorth_Total =
             match seed with
             |"South's turn" -> (south_Total),(north_Total-number_of_seeds) // Take seeds from North side
             |_ -> (south_Total-number_of_seeds),(north_Total) // Take seed from South side

            match (examSide seed seize_House (newsouth_Total,newnorth_Total)),number_of_seeds=2||number_of_seeds=3 with  // Check if you can take some seeds
             |true,true -> Seize (setSeeds myBoard seize_House 0 ) (score+number_of_seeds) (seize_House-1)
             |_ -> myBoard,score // Returns new board and new score
                    
         Seize myBoard 0 seize_House

    let updateScore total myBoard=
    // Updates the game board
         let {play1 =myplayer1; play2 = myplayer2} = myBoard
         let state = myBoard.state

         let new_player1Score,new_player2score,newState = 
          match state with
             |"North's turn" -> south_Score,(north_Score+total),"South's turn" // gives North's score and switches to South's play
             |_ -> (south_Score+total),north_Score,"North's turn" // gives South's score and switches North's play
         //updates player scores

         let new_player1 = {myplayer1 with score = new_player1Score } 
         let new_player2 = {myplayer2 with score = new_player2score}   
         // update the games status and board

         { myBoard with play1 = new_player1;play2 = new_player2; state= newState } 

    let house12Check n =
    // check if at  house number 12
        match n with
         |12 -> 1  // Return to  1
         |_ -> n+1  // Move forward to next house
     
    let check_InitHouse n =  
        match n with 
         |0 -> 12  // Go to 12
         |_ -> n // Stay at current house
          

    let enough_Seeds myBoard house_num goal=
    //Sees if there are enough seeds to be sown
             let num_seed  = getSeeds (house_num) myBoard 
             match (house_num+num_seed)>=goal with
                 |true -> true // Seeds are enough
                 |_ -> false  // Seeds are not enough

    let rec add v myBoard presentHouse previousHouseSeeds = 
             match v = 0 with  // See if done sowing seeds
              |true ->  
                      let newBoard,theScores = seize_Seeds myBoard (check_InitHouse (presentHouse-1)) myBoardState  // returns board, score after capturing all seeds tha can be
                      updateScore theScores newBoard // updates score and board                           
              |_ -> 
                    match presentHouse,presentHouse=n with
                        |12,false -> 
                                let num_seeds = (getSeeds presentHouse myBoard)+1 // add one to the current house
                                add (v-1) (setSeeds myBoard presentHouse (num_seeds)) (1) (num_seeds) 
                        |_,false ->
                                let num_seeds = (getSeeds presentHouse myBoard)+1    // add one to the current house                        
                                add (v-1) (setSeeds myBoard presentHouse (num_seeds)) (presentHouse+1) (num_seeds)  
                        |_ -> add (v) (myBoard) (house12Check (presentHouse)) (previousHouseSeeds) //inital house we got the seeds from , ignore it and move to the next one

// Takes from certain house and sows and seizes
    let moveMyHouse houseNumber =
         match houseNumber with
            |12 -> add (getSeeds houseNumber board) (setSeeds board houseNumber 0) (1) (0) // if 12 start sowing from house 1 
            |_ -> add (getSeeds houseNumber board) (setSeeds board houseNumber 0) (houseNumber) (0)  // if its any other house start sowing from the next house

    let TemporaryBoard houseNumber =
            match (north_Total=0 && board.state = "South's turn"),(south_Total=0 && board.state = "North's turn") with // Checks if South or North has any seeds on their sides
             |true,false -> match enough_Seeds board houseNumber 7 with  
                             |true -> moveMyHouse houseNumber 
                             |_ -> board  
             |false,true -> match enough_Seeds board houseNumber 13 with  
                             |true -> moveMyHouse houseNumber  
                             |_ -> board 
             |_ -> moveMyHouse houseNumber  // Both sides have enough seeds to continue plsying the game
//get the seeds from house
    let theNewBoard  =
             let mySeeds = (getSeeds n board) 
             match myBoardState with  
             |"South's turn" ->  match n,mySeeds>0 with  // Continue playing you you're from the South and not empty
                             |1,true|2,true|3,true|4,true|5,true|6,true -> (TemporaryBoard n )  // Play the game
                             |_ -> { board with state = "South's turn" } // 
                            
             |_ -> match n,mySeeds>0 with // // Continue playing you you're from the South and not empty
                 |7,true|8,true|9,true|10,true|11,true|12,true -> (TemporaryBoard n) // Play the game
                 |_ -> {board with state = "North's turn" } // 
    theNewBoard // return the new updated game board

let start position = 
    let player1 = {score=0; houses= 4,4,4,4,4,4}
    let player2 = {score=0; houses= 4,4,4,4,4,4}

    let myState n = 
        match n with 
        | South -> "South's turn"
        | _ -> "North's turn"   
    let myboard = {state=myState position; play1=player1; play2=player2}
    (myboard) // return the new and updated  board


let score board = 
    let {play1 =myplayer1; play2 = myplayer2} = board
    (myplayer1.score,myplayer2.score) //returns the score

let gameState board = 
    let {play1 =myplayer1; play2 = myplayer2} = board
    let south_Score,north_Score = myplayer1.score,myplayer2.score

    match south_Score=24,north_Score = 24 with
       |true,true -> "Game ended in a draw"
       |_ -> match south_Score>=25,north_Score>=25 with
               |true,false -> "South won"
               |false,true -> "North won"
               |_ -> board.state


[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
