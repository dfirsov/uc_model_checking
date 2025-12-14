module f_diff_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import g_clock_types.* from "../g_clock/g_clock_types"

    // define types of the system  
    type DIF_Data = int
    type DIF_Index = int
    
    type DIF_Message =

        | DiffuseRequest({cfg: MsgConfig, m:DIF_Data}) 
        | SIMDiffuseHold({cfg: MsgConfig, ctr:Counter, t:Time, m:DIF_Data})
        | SIMDiffuseRelease({cfg: MsgConfig, ctr: Counter, t:Time, m : DIF_Data})
        | ClockRequest({cfg: MsgConfig, ctr:Counter, t: Time, m: DIF_Data})
        | ClockRequestWait({cfg: MsgConfig, ctr:Counter, t: Time, m: DIF_Data})
        | ClockResponse({cfg: MsgConfig, ctr:Counter, t: Time, m: DIF_Data})        
        | DiffuseResponse({cfg: MsgConfig, ctr:Counter, t: Time, m: DIF_Data})

        | FetchRequest({cfg: MsgConfig})
        | SIMFetchHold({cfg: MsgConfig, t_prim: Time, i_late: Set[DIF_Index]})
        | SIMFetchRelease({cfg: MsgConfig, t_prim: Time, i_late: Set[DIF_Index], i_early: Set[DIF_Index]})
        | FetchResponse({cfg: MsgConfig, t_prim: Time, data: Set[DIF_Data]})

        | OKRequest({cfg: MsgConfig})
        | OKRequestWait({cfg: MsgConfig})
        | OKResponse({cfg: MsgConfig})

        | InjectRequest( {cfg: MsgConfig, Q:Party, m: DIF_Data})
        | InjectResponse({cfg: MsgConfig, Q:Party, m: DIF_Data})

        | SIMControl  



    type DIF_SystemParams = {
        P : Set[Party], // parties who can use the functionality
        C : Set[Party], // corrupted parties
        SIDs: Set[SID], // TODO: is this correct?

        delta: Time,
    }

    type DIF_StateVars = SID -> 
        {
            L : {to: Party, ctr: Counter} -> {data: DIF_Data, time: Time},
            ctr : Counter,
            F_flag: Set[(Party, Time)],
            D_flag: Set[(Party, Time)],            
        }

    type DIF_System = {
        params : DIF_SystemParams,
        state : DIF_StateVars,
        input : Set[DIF_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: DIF_Message, time : Time, handles: Set[DIF_Message] }], // state: DIF_StateVars
        handles : Set[DIF_Message],
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


    pure def dif_apply_effect(sys: DIF_System, clock: GC_System, eff: DIF_Effect): DIF_System = {
        match eff {
        | DIF_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg,  time: clock.state.TIME, handles: sys.handles})
            }
        | DIF_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | DIF_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | DIF_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }

    pure def dif_get_all_response_messages(messages: Set[DIF_Message]): Set[DIF_Message] = {
        messages.filterMap(m => {
        match m {
            | DiffuseResponse(p) => Some(DiffuseResponse(p))
            | FetchResponse(p) => Some(FetchResponse(p))
            | InjectResponse(p) => Some(InjectResponse(p))  
            | OKResponse(p) => Some(OKResponse(p))
            | _ => None
        }
        })
    }      


    pure def dif_get_inject_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, Q:Party, m:DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | InjectRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_inject_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, Q:Party, m:DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | InjectResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_diffuse_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | DiffuseRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_diffuse_hold_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | SIMDiffuseHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_diffuse_release_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | SIMDiffuseRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_clock_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | ClockRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_clock_request_wait_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | ClockRequestWait(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_clock_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | ClockResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_diffuse_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: DIF_Data}] = {
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

    pure def dif_get_sim_fetch_hold_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[DIF_Index]}] = {
        messages.filterMap(m => {
        match m {
            | SIMFetchHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_fetch_release_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[DIF_Index], i_early: Set[DIF_Index]}] = {
        messages.filterMap(m => {
        match m {
            | SIMFetchRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_fetch_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, t_prim: Time, data: Set[DIF_Data]}] = {
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

    pure def dif_access_check(cfg: MsgConfig, params: DIF_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    }     

    
}