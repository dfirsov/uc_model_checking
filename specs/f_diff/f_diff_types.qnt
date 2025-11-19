module f_diff_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    // define types of the system  
    type Data = int
    
    type DIF_Message =

        | DiffuseRequest({cfg: MsgConfig, m:Data}) 
        | SIMDiffuseHold({cfg: MsgConfig, ctr:Counter, t:Time, m:Data})
        | SIMDiffuseRelease({cfg: MsgConfig, ctr: Counter, t:Time, m : Data})
        | ClockRequest({cfg: MsgConfig, ctr:Counter, t: Time, m: Data})
        | ClockRequestWait({cfg: MsgConfig, ctr:Counter, t: Time, m: Data})
        | ClockResponse({cfg: MsgConfig, ctr:Counter, t: Time, m: Data})        
        | DiffuseResponse({cfg: MsgConfig, ctr:Counter, t: Time, m: Data})

        | FetchRequest({cfg: MsgConfig})
        | SIMFetchHold({cfg: MsgConfig, t_prim: Time, i_late: Set[Time]})
        | SIMFetchRelease({cfg: MsgConfig, t_prim: Time, i_late: Set[Time], i_early: Set[Time]})
        | FetchResponse({cfg: MsgConfig, t_prim: Time, data: Set[Data]})

        | OKRequest({cfg: MsgConfig})
        | OKRequestWait({cfg: MsgConfig})
        | OKResponse({cfg: MsgConfig})

        | InjectRequest( {cfg: MsgConfig, Q:Party, m: Data})
        | InjectResponse({cfg: MsgConfig, Q:Party, m: Data})

        | SIMControl  

    type DIF_SystemParams = {
        P : Set[Party], // parties who can use the functionality
        C : Set[Party], // corrupted parties
        delta: Time,
        SIDs: Set[SID], // TODO: is this correct?
    }

    type DIF_StateVars = SID -> 
        {
            L : {to: Party, ctr: Counter} -> {data: Data, time: Time},
            ctr : Counter,
            F_flag: Set[(Party, Time)],
            D_flag: Set[(Party, Time)],            
        }

    type DIF_System = {
        params : DIF_SystemParams,
        state : DIF_StateVars,
        input : Set[DIF_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: DIF_Message, time : Time, handles: Set[DIF_Message],  }], // state: DIF_StateVars
        SIM_handles : Set[DIF_Message],
    }  

    type DIF_Effect = 
      | DIF_LogMessage(DIF_Message)
      | DIF_UpdateInput(Set[DIF_Message])
      | DIF_AppendHandle(DIF_Message)
      | DIF_RemoveHandle(DIF_Message)      

    type DIF_Transition = {
        post_state: DIF_StateVars,
        effects: Set[DIF_Effect]
    }    

    pure def dif_get_inject_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, Q:Party, m:Data}] = {
        messages.filterMap(m => {
        match m {
            | InjectRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_inject_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, Q:Party, m:Data}] = {
        messages.filterMap(m => {
        match m {
            | InjectResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_diffuse_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, m: Data}] = {
        messages.filterMap(m => {
        match m {
            | DiffuseRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_diffuse_hold_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: Data}] = {
        messages.filterMap(m => {
        match m {
            | SIMDiffuseHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_diffuse_release_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: Data}] = {
        messages.filterMap(m => {
        match m {
            | SIMDiffuseRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_clock_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: Data}] = {
        messages.filterMap(m => {
        match m {
            | ClockRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_clock_request_wait_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: Data}] = {
        messages.filterMap(m => {
        match m {
            | ClockRequestWait(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_clock_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: Data}] = {
        messages.filterMap(m => {
        match m {
            | ClockResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_diffuse_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: Data}] = {
        messages.filterMap(m => {
        match m {
            | DiffuseResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_fetch_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | FetchRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_fetch_hold_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[Time]}] = {
        messages.filterMap(m => {
        match m {
            | SIMFetchHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_fetch_release_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[Time], i_early: Set[Time]}] = {
        messages.filterMap(m => {
        match m {
            | SIMFetchRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_fetch_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, t_prim: Time, data: Set[Data]}] = {
        messages.filterMap(m => {
        match m {
            | FetchResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_ok_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_ok_request_wait_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKRequestWait(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_ok_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_control_messages(messages: Set[DIF_Message]): Set[DIF_Message] = {
        messages.filterMap(m => {
        match m {
            | SIMControl => Some(SIMControl)
            | _ => None
        }
        })
    }

    
}