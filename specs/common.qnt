
module common {
    type Time = int
    type Nonce = int
    type Party = int
    type SID = int
    type Counter = int    


    type MsgConfig = {
        party: Party,
        sid: SID,
        nonce: Nonce,
    }

  pure def listFilter(list: List[a], f: a => bool): List[a] = {
      list.foldl(List(), (r, e) =>   {
            if(f(e)) {
                r.append(e)
            } else {
                r
            }
      })
  }    

  pure def listHas(list: List[a], e: a): bool = {
      list.foldl(false, (r, x) => r or (x == e))
  }

  pure def setMax(a: Set[int],min: int): int = {
      a.fold(min, (r, x) => if (x > r) x else r)
  }


}