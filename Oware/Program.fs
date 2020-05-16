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
