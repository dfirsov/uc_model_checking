module g_clock_types {

    //import uc.* from "../uc"
    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"    

    // define types of the system
    type PartySID = {party: Party, sid: SID}

    type GC_Message =
        | RegisterRequest({nonce: Nonce, party: PartySID})
        | RegisterResponse({nonce: Nonce, party: PartySID})

        | TimeRequest({nonce: Nonce, party: PartySID})
        | TimeResponse({nonce: Nonce, party: PartySID, time: Time})

        | OkayRequest({nonce: Nonce, party: PartySID})
        | SIMOkayHold({nonce: Nonce, party: PartySID})
        | SIMOkayRelease({nonce: Nonce, party: PartySID})
        | OkayResponse({nonce: Nonce, party: PartySID})

        | GC_SIMControl

    type GC_SystemParams = {
        C : Set[Party] // corrupted parties
    }



    type GC_StateVars = {
        TIME : Time,
        Reg : Set[PartySID],
        ok  : Set[PartySID],
    }


   
    type GC_System = {
        params : GC_SystemParams,
        state : GC_StateVars,
        input : Set[GC_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: GC_Message, state: GC_StateVars}],  
        SIM_handles : Set[GC_Message],
    }  
 
    type GC_Effect = 
      | GC_LogMessage(GC_Message)
      | GC_UpdateInput(Set[GC_Message])
      | GC_AppendHandle(GC_Message)
      | GC_RemoveHandle(GC_Message)      

    type GC_Transition = {
        post_state: GC_StateVars,
        effects: Set[GC_Effect]
    }    

    pure def g_clock_apply_effect(sys: GC_System, eff: GC_Effect): GC_System = {
        match eff {
        | GC_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg, state : sys.state})
            }
        | GC_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | GC_AppendHandle(msg) => 
            {...sys,
                SIM_handles: sys.SIM_handles.union(Set(msg)),
            }
        | GC_RemoveHandle(handle_to_remove) =>
            {...sys,
                SIM_handles: sys.SIM_handles.exclude(Set(handle_to_remove))
            }
        }
    }   

    pure def get_register_request_messages(messages: Set[GC_Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | RegisterRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_time_request_messages(messages: Set[GC_Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | TimeRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_okay_request_messages(messages: Set[GC_Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | OkayRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_okay_response_messages(messages: Set[GC_Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | OkayResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_register_response_messages(messages: Set[GC_Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | RegisterResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_time_response_messages(messages: Set[GC_Message]): Set[{nonce: Nonce, party: PartySID, time: Time}] = {
        messages.filterMap(m => {
        match m {
            | TimeResponse(p) => Some(p)
            | _ => None
        }
        })
    }         

    pure def gc_get_all_response_messages(messages: Set[GC_Message]): Set[GC_Message] = {
        messages.filterMap(m => {
        match m {
            | RegisterResponse(p) => Some(m)
            | TimeResponse(p) => Some(m)
            | OkayResponse(p) => Some(m)
            | _ => None
        }
        })
    }

    pure def get_sim_okay_release_messages(messages: Set[GC_Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | SIMOkayRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_sim_okay_hold_messages(messages: Set[GC_Message]): Set[{nonce: Nonce, party: PartySID}] = {
        messages.filterMap(m => {
        match m {
            | SIMOkayHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def contains_sim_control(messages: Set[GC_Message]): bool = {
        messages.exists(m => {
            match m {
                | GC_SIMControl => true
                | _ => false
            }
        })
    }    
    
}