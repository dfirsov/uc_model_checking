module f_pki_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    // define types of the system  
    type Certificate = int
    type VK = int
    
    type PKI_Message =

        | RegisterRequest({cfg: MsgConfig, vk: VK}) 
        | SIMRegisterRequestHold({cfg: MsgConfig, vk: VK})
        | SIMRegisterRequestRelease({cfg: MsgConfig, vk: VK, cert: Certificate})     
        | RegisterResponse({cfg: MsgConfig, vk: VK, cert: Option[Certificate]})

        | RetrieveRequest({cfg: MsgConfig, Q: Party}) 
        | SIMRetrieveRequestHold({cfg: MsgConfig, Q: Party})
        | SIMRetrieveRequestRelease({cfg: MsgConfig, Q: Party})     
        | RetrieveResponse({cfg: MsgConfig, vk: Option[VK]})

        | CertVerifyRequest({cfg: MsgConfig, Q: Party, vk: VK, cert: Certificate})
        | CertVerifyResponse({cfg: MsgConfig, Q: Party, vk: VK, cert: Certificate, result: bool})

        | SIMControl      

    type PKI_SystemParams = {
        P : Set[Party], // parties who can use the functionality
        SIDs: Set[SID], // TODO: is this correct?

        SIM_ALL_CERTS: Set[Certificate], // for the simulator: all possible certificates
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


    pure def pki_get_all_response_messages(messages: Set[PKI_Message]): Set[PKI_Message] = {
        messages.filterMap(m => {
        match m {
            | RegisterResponse(p) => Some(RegisterResponse(p))
            | RetrieveResponse(p) => Some(RetrieveResponse(p))
            | CertVerifyResponse(p) => Some(CertVerifyResponse(p))
            | _ => None
        }
        })
    }  

    pure def pki_get_register_request_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vk: VK}] = {
        messages.filterMap(m => {
        match m {
            | RegisterRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_register_request_hold_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vk: VK}] = {
        messages.filterMap(m => {
        match m {
            | SIMRegisterRequestHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_register_request_release_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vk: VK, cert: Certificate}] = {
        messages.filterMap(m => {
        match m {
            | SIMRegisterRequestRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_register_response_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vk: VK, cert: Option[Certificate]}] = {
        messages.filterMap(m => {
        match m {
            | RegisterResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_retrieve_request_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party}] = {
        messages.filterMap(m => {
        match m {
            | RetrieveRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_retrieve_request_hold_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party}] = {
        messages.filterMap(m => {
        match m {
            | SIMRetrieveRequestHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_retrieve_request_release_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party}] = {
        messages.filterMap(m => {
        match m {
            | SIMRetrieveRequestRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_retrieve_response_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, vk: Option[VK]}] = {
        messages.filterMap(m => {
        match m {
            | RetrieveResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_cert_verify_request_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party, vk: VK, cert: Certificate}] = {
        messages.filterMap(m => {
        match m {
            | CertVerifyRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_cert_verify_response_messages(messages: Set[PKI_Message]): Set[{cfg: MsgConfig, Q: Party, vk: VK, cert: Certificate, result: bool}] = {
        messages.filterMap(m => {
        match m {
            | CertVerifyResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def pki_get_sim_control_messages(messages: Set[PKI_Message]): Set[PKI_Message] = {
        messages.filter(m => {
        match m {
            | SIMControl => true
            | _ => false
        }
        })
    }

    
}