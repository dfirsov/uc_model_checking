  module f_sig_types {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"    
    
    type SIG_PublicKey = int
    type SIG_Document = str
    type SIG_Signature = int    
    type SIG_VK = int

    type SIG_Message =
        | GenRequest({cfg: MsgConfig}) 
        | SIMGenHold({cfg: MsgConfig}) 
        | SIMGenRelease({cfg: MsgConfig, vk: SIG_VK})
        | GenResponse({cfg: MsgConfig, vk: SIG_VK}) 

        | SignRequest({cfg: MsgConfig, m: SIG_Document}) 
        | SIMSignHold({cfg: MsgConfig, m: SIG_Document,})  
        | SIMSignRelease({cfg: MsgConfig, m: SIG_Document, sig: SIG_Signature}) // output of SIM
        | SignResponse({cfg: MsgConfig, sig: Option[SIG_Signature]})

        | VerifyRequest({cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature}) // input to FSig
        | SIMVerifyHold({cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature}) // input to SIM
        | SIMReleaseVerify({cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature, b: bool}) // output of SIM
        | VerifyResponse({cfg: MsgConfig, b: bool})

        | SIMControl
  

    type SIG_SystemParams = {
        P: Set[Party],
        C: Set[Party],
        SIDs: Set[SID], 
        S: Set[Party], 
    }        

    type SIG_StateVars = SID -> 
        {
            VK : Set[{party:Party, vk: SIG_VK}],
            Ver: Set[{vk: SIG_VK, m: SIG_Document, sig: SIG_Signature, b: bool}],
        }    

    type SIG_System = {
        params : SIG_SystemParams,
        state : SIG_StateVars,
        input : Set[SIG_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: SIG_Message }], // state: SIG_StateVars
        handles : Set[SIG_Message],
    }        

    type SIG_Effect = 
      | SIG_LogMessage(SIG_Message)
      | SIG_UpdateInput(Set[SIG_Message])
      | SIG_AppendHandle(SIG_Message)
      | SIG_RemoveHandle(SIG_Message)     


    type SIG_Transition = {
        post_state: SIG_StateVars,
        effects: Set[SIG_Effect]
    }          

    pure def sig_signer_access_check(cfg: MsgConfig, params: SIG_SystemParams): bool = {
            cfg.party.in(params.S) and cfg.sid.in(params.SIDs)
    }    

    pure def sig_verifier_access_check(cfg: MsgConfig, params: SIG_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    }    


   pure def sig_get_all_response_messages(messages: Set[SIG_Message]): Set[SIG_Message] = {
        messages.filterMap(m => {
        match m {
            | GenResponse(p) => Some(GenResponse(p))
            | SignResponse(p) => Some(SignResponse(p))
            | VerifyResponse(p) => Some(VerifyResponse(p))  
            | _ => None
        }
        })
   }


        pure def sig_get_gen_request_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig}] = {
            messages.filterMap(m => {
            match m {
                | GenRequest(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_gen_hold_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig}] = {
            messages.filterMap(m => {
            match m {
                | SIMGenHold(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_gen_release_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK}] = {
            messages.filterMap(m => {
            match m {
                | SIMGenRelease(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_gen_response_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK}] = {
            messages.filterMap(m => {
            match m {
                | GenResponse(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sign_request_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, m: SIG_Document}] = {
            messages.filterMap(m => {
            match m {
                | SignRequest(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_sign_hold_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, m: SIG_Document}] = {
            messages.filterMap(m => {
            match m {
                | SIMSignHold(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_sign_release_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, m: SIG_Document, sig: SIG_Signature}] = {
            messages.filterMap(m => {
            match m {
                | SIMSignRelease(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sign_response_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, sig: Option[SIG_Signature]}] = {
            messages.filterMap(m => {
            match m {
                | SignResponse(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_verify_request_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature}] = {
            messages.filterMap(m => {
            match m {
                | VerifyRequest(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_verify_hold_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature}] = {
            messages.filterMap(m => {
            match m {
                | SIMVerifyHold(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_release_verify_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature, b: bool}] = {
            messages.filterMap(m => {
            match m {
                | SIMReleaseVerify(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_verify_response_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, b: bool}] = {
            messages.filterMap(m => {
            match m {
                | VerifyResponse(p) => Some(p)
                | _ => None
            }
            })
        } 

        pure def sig_get_sim_control_messages(messages: Set[SIG_Message]): Set[SIG_Message] = {
            messages.filter(m => {
            match m {
                | SIMControl => true
                | _ => false
            }
            })
        }        

    def sig_apply_effect(sys: SIG_System, eff: SIG_Effect): SIG_System = {
        match eff {
        | SIG_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg}) 
            }
        | SIG_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | SIG_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | SIG_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }        

}