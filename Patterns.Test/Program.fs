type Cell = 
    |Positive
    |Negative
    |Unknown

type Pattern =
    |BlackP
    |WhiteP
    |UnknownP
    |ZeroOrMore of Pattern
    |OneOrMore of Pattern
    |Exactly of int * Pattern
    |FewerThan of int * Pattern
    |Sequence of List<Pattern>
    |Either of Pattern * Pattern
    |Anything
    |EndOfCells



let rec patternMatch pattern cellList  :Cell list option=

    (*let rec orMore (lst:List<Cell>) pt = 
       let getIndex = match pt with 
                        |BlackP -> List.tryFindIndex (fun x -> not (x = Positive)) lst
                        |WhiteP -> List.tryFindIndex (fun x -> not (x = Negative)) lst
                        |UnknownP -> List.tryFindIndex (fun x -> not (x = Unknown)) lst
                        |Anything -> None
                        |ZeroOrMore a -> 
                                         let rec aResult pat resList count =
                                            let ptm = patternMatch pat resList
                                            let ptmLen = match ptm with 
                                                            |Some(v) -> List.length v
                                                            |None -> 0
                                            match ptm with
                                            |None -> Some(count)
                                            |Some(v) -> match List.isEmpty v with
                                                        |true -> Some(count)
                                                        |false -> aResult pat (List.skip (List.length v) resList) (count + (List.length v))
                                         aResult a lst 0
                        |EndOfCells -> match List.isEmpty lst with
                                        |true -> Some(0)
                                        |false -> None
                        |OneOrMore a -> let rec aResult pat resList count =
                                            let ptm = patternMatch pat resList
                                            let ptmLen = match ptm with 
                                                            |Some(v) -> List.length v
                                                            |None -> 0
                                            match ptm with
                                            |None -> Some(count)
                                            |Some(v) -> match List.isEmpty v with
                                                        |true -> Some(count)
                                                        |false -> aResult pat (List.skip (List.length v) resList) (count + (List.length v))
                                        match (aResult a lst 0) with 
                                        |Some(v) when v>0 ->Some(v)
                                        |_ -> None
                        |Exactly (n , a) -> let rec aResult pat resList count (num:int) =
                                                let ptm = patternMatch pat resList
                                                let ptmLen = match ptm with 
                                                                |Some(v) -> List.length v
                                                                |None -> 0
                                                match num = n with 
                                                |true -> Some(count)
                                                |false -> match ptm with
                                                            |None -> None
                                                            |Some(v) -> match List.isEmpty v with
                                                                        |true -> None
                                                                        |false -> aResult pat (List.skip (List.length v) resList) (count + (List.length v)) (num + 1)

                                            match n=0 with
                                            |true -> Some(0)
                                            |false -> aResult a lst 0 0
                        |FewerThan (n , a) -> match n=0 with 
                                                |true -> None
                                                |false -> let rec aResult pat resList count (num:int) =
                                                            let ptm = patternMatch pat resList
                                                            let ptmLen = match ptm with 
                                                                            |Some(v) -> List.length v
                                                                            |None -> 0
                                                            match (num + 1) = n with 
                                                            |true -> Some(count)
                                                            |false -> match ptm with
                                                                        |None -> Some(count)
                                                                        |Some(v) -> match List.isEmpty v with
                                                                                    |true -> Some(count)
                                                                                    |false -> aResult pat (List.skip (List.length v) resList) (count + (List.length v)) (num + 1)
                                                          aResult a lst 0 0

                        |Either (a ,b) -> let aRes = patternMatch a lst
                                          let bRes = patternMatch b lst
                                          let aLen = match aRes with 
                                                         |Some(v) -> List.length v
                                                         |None -> 0
                                          let bLen = match bRes with
                                                         |Some(v) -> List.length v
                                                         |None -> 0

                                          match ((aRes.IsNone) , (bRes.IsNone)) with
                                            |(true,true) -> None
                                            |(true,false) -> Some(bLen)
                                            |(false, true) -> Some(aLen)
                                            |(false,false) -> match ( aLen >= bLen) with
                                                                 |true -> Some(aLen)
                                                                 |false -> Some(bLen)

                        |Sequence a -> match List.isEmpty a with
                                        |true -> Some(0)
                                        |false-> let rec checkHeads patternList cellLis acc pass = 
                                                        match pass with
                                                        |false -> -1
                                                        |true -> match (patternList , cellLis) with
                                                                    |(pHead::pTail , cHead::cTail) -> let pRes = patternMatch pHead cellLis
                                                                                                      let pLen = match pRes with
                                                                                                                    |Some(v) -> List.length v
                                                                                                                    |None -> 0
                                                                                                      match pRes with
                                                                                                      |Some(v) -> checkHeads pTail (List.skip pLen cellLis) (pLen + acc) true
                                                                                                      |None -> checkHeads [] [] 0 false
                                                                                      
                                                                    |(pHead::pTail , []) -> match pHead with 
                                                                                            |EndOfCells -> acc
                                                                                            |_ -> checkHeads [] [] 0 false
                                                                            

                                                                    |([] , _ ) -> acc
                                                    
                                        

                                                 match (checkHeads a lst 0 true) = -1 with
                                                    |true -> None
                                                    |false -> Some(List.length (List.truncate (checkHeads a lst 0 true) lst))
       getIndex*)

    

    let rec getPattern pat lst = match pat with 
                                    |BlackP -> match lst with
                                                |h::t -> match h with 
                                                            |Positive -> Some([h])
                                                            |_ -> None
                                                |[] -> None

                                    |WhiteP -> match lst with
                                                |h::t -> match h with 
                                                            |Negative -> Some([h])
                                                            |_ -> None
                                                |[] -> None

                                    |UnknownP -> match lst with
                                                 |h::t -> match h with 
                                                            |Unknown -> Some([h])
                                                            |_ -> None
                                                 |[] -> None

                                    |ZeroOrMore a ->let rec aResult pat resList count =
                                                            let ptm = patternMatch pat resList
                                                            let ptmLen = match ptm with 
                                                                            |Some(v) -> List.length v
                                                                            |None -> 0
                                                            match ptm with
                                                            |None -> Some(count)
                                                            |Some(v) -> match List.isEmpty v with
                                                                        |true -> Some(count)
                                                                        |false -> aResult pat (List.skip (List.length v) resList) (count + (List.length v))
                                                    match aResult a lst 0 with 
                                                    |Some(v) -> Some(List.truncate (v) lst)
                                                    |None -> Some([])
                                                    
                                                        (*match (orMore lst a) with
                                                        |Some(v) -> Some(List.take v lst)
                                                        |None -> Some(lst)*)

                                    |OneOrMore a -> let rec aResult pat resList count =
                                                        let ptm = patternMatch pat resList
                                                        let ptmLen = match ptm with 
                                                                        |Some(v) -> List.length v
                                                                        |None -> 0
                                                        match ptm with
                                                        |None -> Some(count)
                                                        |Some(v) -> match List.isEmpty v with
                                                                    |true -> Some(count)
                                                                    |false -> aResult pat (List.skip (List.length v) resList) (count + (List.length v))
                                                    match (aResult a lst 0) with 
                                                    |Some(v) when v>0 ->Some(List.truncate (v) lst)
                                                    |_ -> None
                                                    (*match (orMore lst a) with
                                                    |None -> match lst with
                                                                |h::t -> Some(lst)
                                                                |[] -> None

                                                    |Some(v) -> match v with
                                                                    |0 -> None
                                                                    |_ -> Some(List.take v lst)*)
                                    |Exactly (n , a ) -> 
                                                          let rec aResult pat resList count (num:int) =
                                                                let ptm = patternMatch pat resList
                                                                let ptmLen = match ptm with 
                                                                                |Some(v) -> List.length v
                                                                                |None -> 0
                                                                match num = n with 
                                                                |true -> Some(List.truncate count lst)
                                                                |false -> match ptm with
                                                                            |None -> None
                                                                            |Some(v) -> match List.isEmpty v with
                                                                                        |true -> None
                                                                                        |false -> aResult pat (List.skip (List.length v) resList) (count + (List.length v)) (num + 1)

                                                          match n=0 with
                                                          |true -> Some([])
                                                          |false -> aResult a lst 0 0

                                                            (*match (orMore lst a) with
                                                            |Some(v) -> match v with
                                                                        //|x when x = n -> Some(List.truncate v cellList)
                                                                        |x when x >= n -> Some(List.truncate n lst)
                                                                        | _  -> None

                                                            |None -> match (n <= (List.length lst) ) with
                                                                        |true -> Some(List.truncate n lst)                          //old
                                                                        |false -> None*)
                                                        
                                    |FewerThan (n , a) -> match n=0 with 
                                                            |true -> None
                                                            |false -> let rec aResult pat resList count (num:int) =
                                                                        let ptm = patternMatch pat resList
                                                                        let ptmLen = match ptm with 
                                                                                        |Some(v) -> List.length v
                                                                                        |None -> 0
                                                                        match (num + 1) = n with 
                                                                        |true -> Some(count)
                                                                        |false -> match ptm with
                                                                                    |None -> Some(count)
                                                                                    |Some(v) -> match List.isEmpty v with
                                                                                                |true -> Some(count)
                                                                                                |false -> aResult pat (List.skip (List.length v) resList) (count + (List.length v)) (num + 1)
                                                                      match (aResult a lst 0 0) with 
                                                                      |Some(v) -> Some(List.truncate (v) lst)
                                                                      |None -> Some([])
                                                            (*match (n <= 0) with 
                                                            |true -> None
                                                            |false -> match (orMore lst a) with
                                                                        |Some(v) -> match v with
                                                                                    |x when x < n -> Some(List.truncate v lst)
                                                                                    | _  -> Some(List.truncate (n-1) lst)

                                                                        |None -> Some( List.truncate (n-1) lst)*)

                                    |Anything -> match (List.isEmpty lst) with
                                                    |true -> None
                                                    |false -> Some(List.truncate 1 lst)

                                    |EndOfCells -> match (List.isEmpty lst) with
                                                    |true -> Some(lst)
                                                    |false -> None

                                    |Either (a , b) -> let aRes = patternMatch a lst
                                                       let bRes = patternMatch b lst
                                                       let aLen = match aRes with 
                                                                    |Some(v) -> List.length v
                                                                    |None -> 0
                                                       let bLen = match bRes with
                                                                    |Some(v) -> List.length v
                                                                    |None -> 0
                                                       match ((aRes.IsNone) , (bRes.IsNone)) with
                                                       |(true,true) -> None
                                                       |(true,false) -> bRes
                                                       |(false, true) -> aRes
                                                       |(false,false) -> match ( aLen >= bLen) with
                                                                            |true -> aRes
                                                                            |false -> bRes

                                    |Sequence a -> match List.isEmpty a with
                                                    |true -> Some(List.truncate 0 lst)
                                                    |false-> let rec checkHeads patternList cellLis acc pass = 
                                                                    match pass with
                                                                    |false -> -1
                                                                    |true -> match (patternList , cellLis) with
                                                                                | ([],[]) -> acc
                                                                                |(pHead::pTail , cHead::cTail) -> let pRes = patternMatch pHead cellLis
                                                                                                                  let pLen = match pRes with
                                                                                                                                |Some(v) -> List.length v
                                                                                                                                |None -> 0
                                                                                                                  match pRes with
                                                                                                                  |Some(v) -> checkHeads pTail (List.skip pLen cellLis) (pLen + acc) true
                                                                                                                  |None -> checkHeads [] [] 0 false
                                                                                                                  
                                                                                |(pHead::pTail , []) -> match pHead with 
                                                                                                        |EndOfCells -> acc
                                                                                                        |FewerThan -> acc
                                                                                                        |_ -> checkHeads [] [] 0 false
                                                                                                        

                                                                                |([] , _ ) -> acc
                                                                                
                                                                    

                                                             match (checkHeads a lst 0 true) = -1 with
                                                                |true -> None
                                                                |false -> Some(List.truncate (checkHeads a lst 0 true) lst)
                                                             
    getPattern pattern cellList  


let find pattern cellList =
    let rec rFind pat lst pos =
        match lst with
        |[] -> None
        |head::tail -> match patternMatch pat lst with
                        |Some(v) -> Some(v , pos)
                        |None -> rFind pat tail (pos + 1)
    rFind pattern cellList 0

let map func pattern cellList = 
    let rec mapper rlst =
        match rlst with
        |h::t ->match find pattern rlst with
                |Some(v,n) -> (List.truncate n rlst) :: (func v) :: (mapper (List.skip ((List.length v) + n) rlst))
                |None -> [rlst]
        |[] -> []
    match find pattern cellList with
    |None -> cellList
    |Some(_ , _ ) -> List.collect id (mapper cellList)

let toCells input = List.map (fun x -> match x with
                                       |'b' -> Positive
                                       |'B' -> Positive
                                       |'w' -> Negative
                                       |'W' -> Negative
                                       |_ -> Unknown) (Seq.toList input)

let fromCells input = 
    let charList = List.map (fun x -> match x with
                                            |Positive -> "b"
                                            |Negative -> "w"
                                            |Unknown -> ".") input
    String.concat "" charList




module Program = let [<EntryPoint>] main _ = 0