module g_clock_types {

    import uc.* from "./uc"
    import basicSpells.* from "../../spells/basicSpells"

    // define types of the system
    type Message =
        | RegisterRequest({nonce: Nonce, party: PartySID})
        | RegisterResponse({nonce: Nonce, party: PartySID})

        | TimeRequest({nonce: Nonce, party: PartySID})
        | TimeResponse({nonce: Nonce, party: PartySID, time: Time})

        | OkayRequest({nonce: Nonce, party: PartySID})
        | SIMOkayHold({nonce: Nonce, party: PartySID})
        | SIMOkayRelease({nonce: Nonce, party: PartySID})
        | OkayResponse({nonce: Nonce, party: PartySID})

        | SIMControl

    type SystemParams = {
        C : Set[Party] // corrupted parties
    }

    type StateVars = {
        TIME : Time,
        Reg : Set[PartySID],
        ok  : Set[PartySID],
    }

    type System = SystemPoly[SystemParams,StateVars,Message]


    pure def LogMessage(msg: Message) :Effect = LogMessagePoly(msg)
    pure def UpdateInput(msgs: Set[Message]) :Effect = UpdateInputPoly(msgs)
    pure def AppendHandle(msg: Message) :Effect = AppendHandlePoly(msg)
    pure def RemoveHandle(handle: Message) :Effect = RemoveHandlePoly(handle)

    type Effect = EffectPoly[Message] 
    type Transition = TransitionPoly[StateVars,Message]
    

    pure def get_register_request_messages(messages: Set[Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | RegisterRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_time_request_messages(messages: Set[Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | TimeRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_okay_request_messages(messages: Set[Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | OkayRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_okay_response_messages(messages: Set[Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | OkayResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_register_response_messages(messages: Set[Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | RegisterResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_time_response_messages(messages: Set[Message]): Set[{nonce: Nonce, party: PartySID, time: Time}] = {
        messages.filterMap(m => {
        match m {
            | TimeResponse(p) => Some(p)
            | _ => None
        }
        })
    }         

    pure def get_all_response_messages(messages: Set[Message]): Set[Message] = {
        messages.filterMap(m => {
        match m {
            | RegisterResponse(p) => Some(m)
            | TimeResponse(p) => Some(m)
            | OkayResponse(p) => Some(m)
            | _ => None
        }
        })
    }

    pure def get_sim_okay_release_messages(messages: Set[Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | SIMOkayRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_sim_okay_hold_messages(messages: Set[Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | SIMOkayHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def contains_sim_control(messages: Set[Message]): bool = {
        messages.exists(m => {
            match m {
                | SIMControl => true
                | _ => false
            }
        })
    }    
    
}