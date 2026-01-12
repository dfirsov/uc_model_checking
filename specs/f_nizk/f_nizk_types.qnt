module f_nizk_types{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"    
    
    type NIZK_Witness = int
    type NIZK_Statement = str
    type NIZK_Proof = int    
    type NIZK_Relation = Set[(NIZK_Statement, NIZK_Witness)]


    type NIZK_Message =
        | ProveRequest({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}) 
        | SIMProveHold({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}) 
        | SIMProveRelease({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, pi: NIZK_Proof})
        | ProveResponse({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, pi: Option[NIZK_Proof]}) 

        | VerifyRequest({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof})
        | SIMVerifyHold({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof}) 
        | SIMVerifyRelease({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, w: NIZK_Witness}) 
        | VerifyResponse({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, b: bool})

        

        | SIMControl
  

    type NIZK_SystemParams = {
        P: Set[Party],
        C: Set[Party],
        SIDs: Set[SID], 

        ALL_WITNESSES: Set[NIZK_Witness],
        ALL_PROOFS: Set[NIZK_Proof],
    }        

    type NIZK_StateVars = SID -> 
        {
            Ver: Set[{x: NIZK_Statement, pi: NIZK_Proof, b: bool}], // verified proofs for statements with witness w
            Rel:  NIZK_Relation,
        }    

    type NIZK_System = {
        params : NIZK_SystemParams,
        state : NIZK_StateVars,
        input : Set[NIZK_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: NIZK_Message }], // state: NIZK_StateVars
        handles : Set[NIZK_Message],
    }        

    type NIZK_Effect = 
      | NIZK_LogMessage(NIZK_Message)
      | NIZK_UpdateInput(Set[NIZK_Message])
      | NIZK_AppendHandle(NIZK_Message)
      | NIZK_RemoveHandle(NIZK_Message)     


    type NIZK_Transition = {
        post_state: NIZK_StateVars,
        effects: Set[NIZK_Effect]
    }

    pure def nizk_access_check(cfg: MsgConfig, params: NIZK_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    }    

   pure def nizk_get_all_response_messages(messages: Set[NIZK_Message]): Set[NIZK_Message] = {
        messages.filterMap(m => {
        match m {
            | ProveResponse(p) => Some(ProveResponse(p))
            | VerifyResponse(p) => Some(VerifyResponse(p))  
            | _ => None
        }
        })
   }    

    pure def nizk_get_sim_control_messages(messages: Set[NIZK_Message]): Set[NIZK_Message] = {
        messages.filter(m => {
        match m {
            | SIMControl => true
            | _ => false
        }
        })
    }          

    pure def nizk_get_prove_request_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}] = {
        messages.filterMap(m => {
        match m {
            | ProveRequest(p) => Some(p)
            | _ => None
        }
        })
    }   

    pure def nizk_get_sim_prove_hold_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}] = {
        messages.filterMap(m => {
        match m {
            | SIMProveHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def nizk_get_sim_prove_release_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, pi: NIZK_Proof}] = {
        messages.filterMap(m => {
        match m {
            | SIMProveRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def nizk_get_prove_response_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, pi: Option[NIZK_Proof]}] = {
        messages.filterMap(m => {
        match m {
            | ProveResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def nizk_get_verify_request_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof}] = {
        messages.filterMap(m => {
        match m {
            | VerifyRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def nizk_get_sim_verify_hold_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof}] = {
        messages.filterMap(m => {
        match m {
            | SIMVerifyHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def nizk_get_sim_verify_release_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, w: NIZK_Witness}] = {
        messages.filterMap(m => {
        match m {
            | SIMVerifyRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def nizk_get_verify_response_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, b: bool}] = {
        messages.filterMap(m => {
        match m {
            | VerifyResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    def nizk_apply_effect(sys: NIZK_System, eff: NIZK_Effect): NIZK_System = {
        match eff {
        | NIZK_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg}) 
            }
        | NIZK_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | NIZK_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | NIZK_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }

    pure def nizk_Rel_sat(sys: NIZK_System, sid: SID, x: NIZK_Statement, w: NIZK_Witness): bool = {
        sys.state.get(sid).Rel.contains((x, w))
    }         


}