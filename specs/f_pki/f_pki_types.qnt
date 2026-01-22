module f_pki_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    // define types of the system  
    type Certificate = int
    type VK = int

    type PKI_Message =

        | PKI_RegisterRequest({cfg: MsgConfig, vk: VK}) 
        | PKI_SIMRegisterRequestHold({cfg: MsgConfig, vk: VK})
        | PKI_SIMRegisterRequestRelease({cfg: MsgConfig, vk: VK, cert: Certificate})     
        | PKI_RegisterResponse({cfg: MsgConfig, vk: VK, cert: Option[Certificate]})

        | PKI_RetrieveRequest({cfg: MsgConfig, Q: Party}) 
        | PKI_SIMRetrieveRequestHold({cfg: MsgConfig, Q: Party})
        | PKI_SIMRetrieveRequestRelease({cfg: MsgConfig, Q: Party})     
        | PKI_RetrieveResponse({cfg: MsgConfig, Q: Party, vk: Option[VK]})

        | PKI_CertVerifyRequest({cfg: MsgConfig, Q: Party, vk: VK, cert: Certificate})
        | PKI_CertVerifyResponse({cfg: MsgConfig, Q: Party, vk: VK, cert: Certificate, result: bool})

        | PKI_GetAllRequest({cfg: MsgConfig}) 
        | PKI_SIMGetAllRequestWait({cfg: MsgConfig})
        | PKI_GetAllResponse({cfg: MsgConfig, vks: Party -> VK})

        | PKI_SIMControl      

    type PKI_SystemParams = {
        P : Set[Party], // parties who can use the functionality
        SIDs: Set[SID], // TODO: is this correct?

        //SIM_ALL_CERTS: Set[Certificate], // for the simulator: all possible certificates
    }

    type PKI_StateVars = SID -> 
        {
            Ver : Set[{party: Party, vk: VK, cert: Certificate}],
            Reg: Set[{party: Party, vk: VK}]
        }

    type PKI_System = {
        params : PKI_SystemParams,
        state : PKI_StateVars,
        input : Set[PKI_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: PKI_Message, }], // state: PKI_StateVars
        SIM_handles : Set[PKI_Message],
    }  

    type PKI_Effect = 
      | PKI_LogMessage(PKI_Message)
      | PKI_UpdateInput(Set[PKI_Message])
      | PKI_AppendHandle(PKI_Message)
      | PKI_RemoveHandle(PKI_Message)      

    type PKI_Transition = {
        post_state: PKI_StateVars,
        effects: Set[PKI_Effect]
    }    

    pure def pki_get_get_all_request_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig}] = {
            messages.filterMap(m => {
            match m {
                | PKI_GetAllRequest(p) => Some(p)
                | _ => None
            }
            })
        }

    pure def pki_get_sim_get_all_request_wait_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | PKI_SIMGetAllRequestWait(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_get_all_response_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vks: Party -> VK}] = {
        messages.filterMap(m => {
        match m {
            | PKI_GetAllResponse(p) => Some(p)
            | _ => None
        }
        })
    }    

    pure def pki_id_trans(pki : PKI_System) : PKI_Transition = {
        post_state : pki.state,
        effects : Set(),
    } 

    pure def pki_skip_trans(pki: PKI_System) : PKI_Transition = {
        {
            post_state: pki.state,
            effects : Set(
                PKI_UpdateInput(Set())
            )
        }
    }    

    pure def pki_get_all_response_messages(messages: Set[PKI_Message]): Set[PKI_Message] = {
        messages.filterMap(m => {
        match m {
            | PKI_RegisterResponse(p) => Some(PKI_RegisterResponse(p))
            | PKI_RetrieveResponse(p) => Some(PKI_RetrieveResponse(p))
            | PKI_CertVerifyResponse(p) => Some(PKI_CertVerifyResponse(p))
            | _ => None
        }
        })
    }  

    pure def pki_get_register_request_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vk: VK}] = {
        messages.filterMap(m => {
        match m {
            | PKI_RegisterRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_register_request_hold_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vk: VK}] = {
        messages.filterMap(m => {
        match m {
            | PKI_SIMRegisterRequestHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_register_request_release_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vk: VK, cert: Certificate}] = {
        messages.filterMap(m => {
        match m {
            | PKI_SIMRegisterRequestRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_register_response_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vk: VK, cert: Option[Certificate]}] = {
        messages.filterMap(m => {
        match m {
            | PKI_RegisterResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_retrieve_request_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party}] = {
        messages.filterMap(m => {
        match m {
            | PKI_RetrieveRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_retrieve_request_hold_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party}] = {
        messages.filterMap(m => {
        match m {
            | PKI_SIMRetrieveRequestHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_retrieve_request_release_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party}] = {
        messages.filterMap(m => {
        match m {
            | PKI_SIMRetrieveRequestRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_retrieve_response_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party, vk: Option[VK]}] = {
        messages.filterMap(m => {
        match m {
            | PKI_RetrieveResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_cert_verify_request_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party, vk: VK, cert: Certificate}] = {
        messages.filterMap(m => {
        match m {
            | PKI_CertVerifyRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_cert_verify_response_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party, vk: VK, cert: Certificate, result: bool}] = {
        messages.filterMap(m => {
        match m {
            | PKI_CertVerifyResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_control_messages(messages: Set[PKI_Message]): Set[PKI_Message] = {
        messages.filter(m => {
        match m {
            | PKI_SIMControl => true
            | _ => false
        }
        })
    }
   
}