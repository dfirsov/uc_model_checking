module f_ac_types {

    import uc.* from "../uc"
    import basicSpells.* from "../../spells/basicSpells"

    // define types of the system
    type Data = int
    type Counter = int
/*
1. All state variables of a F-AC functionality are indexed by SID
2. F-AC accepts messages from parties at a particular SID (messages which are not in the "allowed set" are ignored)
3. The messages are always being sent by environment on behalf of a party
4. Responses are always being sent to the environment on behalf of a party
5. A particular configuration and constraints of who can send messages to whom are defined when instantiating the environment.

*/
    type MsgConfig = {
        party: Party,
        sid: SID,
        nonce: Nonce,
    }

    type Message =

        | SendRequest({cfg: MsgConfig, Q:Party, x:Data}) 
        | SIMSendHold({cfg: MsgConfig, ctr:Counter, Q:Party, t:Time, x:Data})
        | SIMSendRelease({cfg: MsgConfig, Q:Party, t:Time})
        | SendResponse({cfg: MsgConfig})

        | FetchRequest({cfg: MsgConfig})
        | SIMFetchHold({cfg: MsgConfig, t_prim: Time, i_late: Set[Time]})
        | SIMFetchRelease({cfg: MsgConfig, t_prim: Time, i_late: Set[Time], i_early: Set[Time]})
        | FetchResponse({cfg: MsgConfig, data: Set[Data]})

        | OKRequest({cfg: MsgConfig})
        | ClockRequest({cfg: MsgConfig, t: Time})
        | ClockRequestWait({cfg: MsgConfig, t: Time})
        | ClockResponse({cfg: MsgConfig, t: Time})
        | OKResponse({cfg: MsgConfig})

        | SIMControl  

    type SystemParams = {
        P : Set[Party], // parties who can use the functionality
        C : Set[Party], // corrupted parties
        delta: Time,
        SIDs: Set[SID],
    }

    type StateVars = SID -> 
        {
            L : Counter -> {from: Party, to: Party, time: Time, data: Data},
            ctr : Counter,
            done  : {from: Party, to: Party, time: Time} -> bool,
        }

    type System = SystemPoly[SystemParams,StateVars,Message]

    pure def LogMessage(msg: Message) :Effect = LogMessagePoly(msg)
    pure def UpdateInput(msgs: Set[Message]) :Effect = UpdateInputPoly(msgs)
    pure def AppendHandle(msg: Message) :Effect = AppendHandlePoly(msg)
    pure def RemoveHandle(handle: Message) :Effect = RemoveHandlePoly(handle)

    type Effect = EffectPoly[Message] 
    type Transition = TransitionPoly[StateVars,Message]
    

    pure def get_send_request_messages(messages: Set[Message]): Set[{cfg: MsgConfig, Q:Party, x:Data}] = {
        messages.filterMap(m => {
        match m {
            | SendRequest(p) => Some(p)
            | _ => None
        }
        })
    }            
     

    pure def get_sim_send_hold_messages(messages: Set[Message]): Set[{cfg: MsgConfig, ctr:Counter, Q:Party, t:Time, x:Data}] = {
        messages.filterMap(m => {
        match m {
            | SIMSendHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_sim_send_release_messages(messages: Set[Message]): Set[{cfg: MsgConfig, Q:Party, t:Time}] = {
        messages.filterMap(m => {
        match m {
            | SIMSendRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_send_response_messages(messages: Set[Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | SendResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_fetch_request_messages(messages: Set[Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | FetchRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_sim_fetch_hold_messages(messages: Set[Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[Time]}] = {
        messages.filterMap(m => {
        match m {
            | SIMFetchHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_sim_fetch_release_messages(messages: Set[Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[Time], i_early: Set[Time]}] = {
        messages.filterMap(m => {
        match m {
            | SIMFetchRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_fetch_response_messages(messages: Set[Message]): Set[{cfg: MsgConfig, data: Set[Data]}] = {
        messages.filterMap(m => {
        match m {
            | FetchResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_ok_request_messages(messages: Set[Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_clock_request_messages(messages: Set[Message]): Set[{cfg: MsgConfig, t: Time}] = {
        messages.filterMap(m => {
        match m {
            | ClockRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_clock_request_wait_messages(messages: Set[Message]): Set[{cfg: MsgConfig, t: Time}] = {
        messages.filterMap(m => {
        match m {
            | ClockRequestWait(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_clock_response_messages(messages: Set[Message]): Set[{cfg: MsgConfig, t: Time}] = {
        messages.filterMap(m => {
        match m {
            | ClockResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_ok_response_messages(messages: Set[Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_sim_control_messages(messages: Set[Message]): Set[bool] = {
        messages.filterMap(m => {
        match m {
            | SIMControl => Some(true)
            | _ => None
        }
        })
    }
 

   
    
}