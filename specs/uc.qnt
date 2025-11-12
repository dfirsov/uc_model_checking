module uc {
    import basicSpells.* from "../spells/basicSpells"

    type Time = int
    type Nonce = int
    type Party = int
    type SID = int
    type PartySID = {party: Party, sid: SID}
  
    type EffectPoly[m] = 
      | LogMessagePoly(m)
      | UpdateInputPoly(Set[m])
      | AppendHandlePoly(m)
      | RemoveHandlePoly(m)  


    type SystemPoly[params,state,msg] = {
        params : params,
        state : state,
        input : Set[msg], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: msg, state: state}],  
        SIM_handles : Set[msg],
    }    

    type TransitionPoly[state,msg] = {
        post_state: state,
        effects: Set[EffectPoly[msg]]
    }    

    pure def apply_effect(sys: SystemPoly[params,state,msg], eff: EffectPoly[msg]): SystemPoly[params,state,msg] = {
        match eff {
        | LogMessagePoly(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg, state : sys.state})
            }
        | UpdateInputPoly(msgs) => 
            {...sys, 
                input: msgs
            }
        | AppendHandlePoly(msg) => 
            {...sys,
                SIM_handles: sys.SIM_handles.union(Set(msg)),
            }
        | RemoveHandlePoly(handle_to_remove) =>
            {...sys,
                SIM_handles: sys.SIM_handles.exclude(Set(handle_to_remove))
            }
        }
    }


   // TODO: introducing this, breaks the typechecking
    // pure def process_transitions_poly(sys: SystemPoly[params,state,msg], transition: TransitionPoly[state,msg]) : SystemPoly[params,state,msg] = {
    //     val new_sys = {...sys, state: transition.post_state}
    //     transition.effects.fold(new_sys, ((a1,a2) => apply_effect(a1,a2)))
    // }         
    
}