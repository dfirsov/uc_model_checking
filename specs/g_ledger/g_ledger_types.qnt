module g_ledger_types{
    
    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    // define types of the system  
    type Pointer = int
    type TX = int
    type EventId = int
    type Block = int
    type ValidatorState = (List[Party], TX) => bool

    type LED_Message =

        | SubmitRequest({cfg: MsgConfig, tx: TX}) 
        | SubmitTimeRequestHold({cfg: MsgConfig, tx:TX})
        //| SubmitTimeRequestRelease({cfg: MsgConfig, tx:TX, t: Time})
        | SubmitSIMHold({cfg: MsgConfig, tx:TX, eID: EventId, t: Time})
        | SubmitSIMRelease({cfg: MsgConfig, tx:TX, eID: EventId, t: Time})
        | SubmitResponse({cfg: MsgConfig, tx:TX, eID: EventId, t: Time})

        | OKRequest({cfg: MsgConfig})
        | OKRequestWait({cfg: MsgConfig})
        | OKResponse({cfg: MsgConfig})

        | ReadRequest({cfg: MsgConfig})
        | ReadSIMHold({cfg: MsgConfig})
        | ReadSIMRelease({cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer})        
        | ReadTimeRequestHold({cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer })
        | ReadResponse({cfg: MsgConfig, result: List[TX]})

        | SIMControl      
    

    type LED_SystemParams = {
        P : Set[Party], 
        C : Set[Party], 
        SIDs: Set[SID], 
        delta_latency: Time,
        delta_slack: Time,
        genesis: Block,
    }    


    type LED_StateVars = {
        buffer: EventId -> {tx: TX, time: Time},
        eID: EventId,
        log: List[{tx:TX, time: Time}],
        ptr: Party -> Pointer,
        Val: ValidatorState,
    }


    type LED_System = {
        params : LED_SystemParams,
        state : LED_StateVars,
        input : Set[LED_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: LED_Message, time : Time, handles: Set[LED_Message],  }], // state: LED_StateVars
        handles : Set[LED_Message],
    }  


   type LED_Effect = 
      | LED_LogMessage(LED_Message)
      | LED_UpdateInput(Set[LED_Message])
      | LED_AppendHandle(LED_Message)
      | LED_RemoveHandle(LED_Message)   


    type LED_Transition = {
        post_state: LED_StateVars,
        effects: Set[LED_Effect]
    }    


    pure def led_get_all_response_messages(messages: Set[LED_Message]): Set[LED_Message] = {
        messages.filterMap(m => {
        match m {
            | SubmitResponse(p) => Some(SubmitResponse(p))
            | OKResponse(p) => Some(OKResponse(p))
            | ReadResponse(p) => Some(ReadResponse(p))  
            | _ => None
        }
        })
    }    


    pure def led_get_submit_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx: TX}] = {
        messages.filterMap(m => {
        match m {
            | SubmitRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_submit_time_request_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx:TX}] = {
        messages.filterMap(m => {
        match m {
            | SubmitTimeRequestHold(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def led_get_submit_time_request_release_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx:TX, t: Time}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SubmitTimeRequestRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def led_get_submit_sim_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx:TX, eID: EventId, t: Time}] = {
        messages.filterMap(m => {
        match m {
            | SubmitSIMHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_submit_sim_release_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx:TX, eID: EventId, t: Time}] = {
        messages.filterMap(m => {
        match m {
            | SubmitSIMRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_submit_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx:TX, eID: EventId, t: Time}] = {
        messages.filterMap(m => {
        match m {
            | SubmitResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_ok_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_ok_request_wait_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKRequestWait(p) => Some(p)
            | _ => None
        }
        })
    }



    pure def led_get_ok_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_read_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | ReadRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_read_sim_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | ReadSIMHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_read_sim_release_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer}] = {
        messages.filterMap(m => {
        match m {
            | ReadSIMRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_read_time_request_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer}] = {
        messages.filterMap(m => {
        match m {
            | ReadTimeRequestHold(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def led_get_read_time_request_release_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer, t: Time}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ReadTimeRequestRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def led_get_read_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, result: List[Block]}] = {
        messages.filterMap(m => {
        match m {
            | ReadResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def led_get_sim_control_messages(messages: Set[LED_Message]): Set[LED_Message] = {
        messages.filter(m => {
        match m {
            | SIMControl => true
            | _ => false
        }
        })
    }


    
}