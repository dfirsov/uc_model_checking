
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
    
}