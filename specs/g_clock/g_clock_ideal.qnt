module g_clock_ideal{

    import basicSpells.* from "../../spells/basicSpells"

    // define types of the system
    type Party = int
    type SID = int

    type Time = int
    type PartySID = {party: Party, sid: SID}
    type Nonce = int

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

    type LogEntry  = {idx:int, msg: Message, time: Time, Reg: Set[PartySID], ok: Set[PartySID]} 
    type SIMHandle = Message

    type SystemParams = {
        P : Set[Party], // all parties
        C : Set[Party] // corrupted parties
    }

    type StateVars = {
        TIME : Time,
        Reg : Set[PartySID],
        ok  : Set[PartySID],
    }

    type SystemPoly[s] = {
        params : SystemParams,
        state : s,
        input : Set[Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[LogEntry],  
        SIM_handles : Set[SIMHandle],

    }

    type Effect = 
      | LogMessage(Message)
      | UpdateInput(Set[Message])
      | AppendHandle(Message)
      | RemoveHandle(SIMHandle)

    type Transition = {
        post_state: StateVars,
        effects: Set[Effect]
    } 
  

    
    type System = SystemPoly[StateVars]
    var s : System

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

    pure def register(sys: System) :  Set[Transition] = { 
        sys.input.get_register_request_messages().map(msg => 
        {
            post_state : {...sys.state, 
                Reg : sys.state.Reg.union(Set(msg.party))
            },
            effects : Set(
                LogMessage(RegisterRequest({nonce: msg.nonce, party: msg.party})),
                UpdateInput(Set(RegisterResponse({nonce: msg.nonce, party: msg.party}))),
            )
        }
        )
    }

    pure def time(sys: System) : Set[Transition] = {
        sys.input.get_time_request_messages().map(msg =>
        {
            post_state : sys.state,
            effects : Set(
                LogMessage(TimeRequest({nonce: msg.nonce, party: msg.party})),
                UpdateInput(Set(TimeResponse({nonce: msg.nonce, party: msg.party, time: sys.state.TIME}))),
            )
        }
        )
    }

    pure def okay_1(sys: System) : Set[Transition] = {
        sys.input.get_okay_request_messages().map(msg => {
            if(sys.state.Reg.contains(msg.party)) {
                // Check if all registered parties are ready (including this one)
                val flag = sys.state.Reg.forall(rp => sys.state.ok.contains(rp) or rp == msg.party)
                val new_time = if (flag) sys.state.TIME + 1 else sys.state.TIME
                val new_ok = if (flag) Set() else sys.state.ok.setAdd(msg.party)
                
                {
                    post_state : {...sys.state, 
                        ok: new_ok,
                        TIME: new_time
                    },
                    effects : Set(
                        LogMessage(OkayRequest({nonce: msg.nonce, party: msg.party})),
                        UpdateInput(Set(SIMOkayHold({nonce: msg.nonce, party: msg.party}))),
                    )
                }
            } else {
                // Party not registered - send control message
                {
                    post_state : sys.state,
                    effects : Set(
                        LogMessage(OkayRequest({nonce: msg.nonce, party: msg.party})),
                        UpdateInput(Set()), // control goes to env
                    )
                }
            }
        })
    }

    pure def okay_2(sys: System) : Set[Transition] = {
        sys.input.get_sim_okay_release_messages().map(msg =>
        {
            post_state : sys.state,
            effects : Set(
                UpdateInput(Set(OkayResponse({nonce: msg.nonce, party: msg.party}))),
            )
        }
        )
    }

    pure def sim_okay_hold_reqs(sys: System) : Set[Transition] = {
        sys.input.get_sim_okay_hold_messages().map(msg => {
            val hold_msg = SIMOkayRelease({nonce: msg.nonce, party: msg.party})
            {
                post_state : sys.state,
                effects : Set(
                    AppendHandle(hold_msg),
                    UpdateInput(Set(SIMControl)),
                )
            }
        })
    }

    pure def sim_process_hold_reqs(sys: System) : Set[Transition] = {
        if(sys.SIM_handles.size() > 0 and sys.input.contains_sim_control()) {
            // Option 1: Release one of the stored hold requests
            val release_trans = sys.SIM_handles.map(release_request => {
                {
                    post_state : sys.state,
                    effects : Set(
                        UpdateInput(Set(release_request)),
                        RemoveHandle(release_request),
                    )
                }
            })
            
            // Option 2: Give control back to ENV (empty input)
            val give_control_trans = Set({
                post_state : sys.state,
                effects : Set(
                    UpdateInput(Set()),
                )
            })
            
            release_trans.union(give_control_trans)
        } else {
            Set()
        }
    }


    pure def apply_effect(sys: System, eff: Effect): System = {
        match eff {
        | LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg, time: sys.state.TIME, Reg: sys.state.Reg, ok: sys.state.ok})
            }
        | UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | AppendHandle(msg) => 
            {...sys,
                SIM_handles: sys.SIM_handles.union(Set(msg)),
            }
        | RemoveHandle(handle_to_remove) =>
            {...sys,
                SIM_handles: sys.SIM_handles.exclude(Set(handle_to_remove))
            }
        }
    }

    pure def main_listener(sys : System) : Set[Transition] = {
        Set(
            register(sys),
            time(sys),
            okay_1(sys),
            sim_okay_hold_reqs(sys),
            sim_process_hold_reqs(sys),
            okay_2(sys),
        ).flatten()
    }  

    pure def process_transitions(sys: System, transition: Transition) : System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => apply_effect(a1,a2)))
    }

    action step : bool =  { 
        nondet transition = main_listener(s).oneOf()
        s' = process_transitions(s, transition)
    }

    action init(params : SystemParams, Reg: Set[PartySID], time: Time) : bool = {
        s' = {
            params: {
                P : params.P,
                C : params.C,
            },
            state : {
                TIME : time,
                Reg : Reg,
                ok  : Set(),
            },
            nonce : 0,
            input : Set(),
            log : Set(),
            SIM_handles : Set()
        }
    }

}