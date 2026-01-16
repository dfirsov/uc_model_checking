module p_bridge_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import f_atms.* from "../f_atms/f_atms"    
    import g_clock.* from "../g_clock/g_clock"    
    import f_atms_types.* from "../f_atms/f_atms_types"    
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_ledger_types.* from "../g_ledger/g_ledger_types" 
    import f_pki_types.* from "../f_pki/f_pki_types"
    
    type BRD_BVK = ATMS_VK 
    type BRD_ROT = ATMS_AVK 
    type BRD_AUX = {bvks: List[BRD_BVK], com: Committee}  

    type BRD_Message =
        | BKGenRequest({cfg: MsgConfig}) 
        | BKGenATMGenWait({cfg: MsgConfig})
        | BKGenPKIWait({cfg: MsgConfig, bvk: BRD_BVK})
        | BKGenOKWait({cfg: MsgConfig, bvk: BRD_BVK})   
        | BKGenResponse({cfg: MsgConfig, bvk: BRD_BVK})

        | RoTRequest({cfg: MsgConfig})
        | RoTPKIGetAllWait({cfg: MsgConfig})
        | RoTComSelWait({cfg: MsgConfig, bvks: List[BRD_BVK]})
        | RoTAKeyWait({cfg: MsgConfig, bvks: List[BRD_BVK], com: Committee})
        | RoTOKWait({cfg: MsgConfig, bvks: List[BRD_BVK], com: Committee, avk: BRD_ROT})
        | RoTResponse({cfg: MsgConfig, bvks: List[BRD_BVK], com: Committee, avk: BRD_ROT})

        | CertifyRequest({cfg: MsgConfig, rot: BRD_ROT, x: LED_Tx})
        | CertifyReadWait({cfg: MsgConfig})
        | CertifyAPLWait({cfg: MsgConfig})
        | CertifyHashWait({cfg: MsgConfig})
        | CertifyProveWait({cfg: MsgConfig})
        | CertifyResponse({cfg: MsgConfig, res: Option[({pi: int, h: int})]})

    pure def brd_get_all_response_messages(messages: Set[BRD_Message]): Set[BRD_Message] = {
        messages.filterMap(m => {
            match m {
                | BKGenResponse(_) => Some(m)
                | RoTResponse(_) => Some(m)
                | CertifyResponse(_) => Some(m)
                | _ => None
            }
        })
    }

    type BRD_SystemParams = {
        P: Set[Party],
        C: Set[Party],
        SIDs: Set[SID],
        ssid : SID,
        P1: Set[Party],
        P2: Set[Party],
        r0: Time,
        delta: int, // for diffuse
        t: int, // for F-ATMS
        R: int, // length of rounds? 
    }

    type BRD_StateVars = {
        bvk: Party -> BRD_BVK,
        rot: Party -> BRD_ROT,
        aux: Party -> BRD_AUX,

        bkgen_flag: Set[Party],
        rot_flag: Set[Party],
        bridge_flag: Set[(Party, SID)],
    }


    type BRD_System = {
        params : BRD_SystemParams,
        state : BRD_StateVars,
        input : Set[BRD_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: BRD_Message, }], // state: PKI_StateVars
        handles : Set[BRD_Message],
    }  

    type BRD_Effect = 
      | BRD_LogMessage(BRD_Message)
      | BRD_UpdateInput(Set[BRD_Message])
      | BRD_AppendHandle(BRD_Message)
      | BRD_RemoveHandle(BRD_Message)      

    type BRD_Transition = {
        post_state: BRD_StateVars,
        effects: Set[BRD_Effect]
    }       


    type BRD_CSystem = {
        brd: BRD_System,
        clock: GC_System,
        atms: ATMS_System,
        pki: PKI_System,
        ledger1: LED_System,
    }    

    type BRD_CTransition = {
        brd: BRD_Transition,
        clock: GC_Transition,
        atms: ATMS_Transition,
        pki: PKI_Transition,
        ledger1: LED_Transition,
    }      

    pure def brd_id_ctrans(sys: BRD_CSystem) : BRD_CTransition = {
        {
            brd: {
                post_state: sys.brd.state,
                effects : Set(
                    BRD_UpdateInput(Set())
                )
            },
            clock: sys.clock.gc_id_trans(),
            atms: sys.atms.atms_id_trans(), 
            pki: sys.pki.pki_id_trans(),
            ledger1: sys.ledger1.led_id_trans(), 
        }
    }      
    pure def brd_get_bkgen_atm_gen_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BKGenATMGenWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bkgen_pki_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvk: int}] = {
        messages.filterMap(m => {
            match m {
                | BKGenPKIWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bkgen_ok_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvk: int}] = {
        messages.filterMap(m => {
            match m {
                | BKGenOKWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_rot_pki_get_all_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | RoTPKIGetAllWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_rot_com_sel_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvks: List[BRD_BVK]}] = {
        messages.filterMap(m => {
            match m {
                | RoTComSelWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_rot_akey_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvks: List[BRD_BVK], com: Committee}] = {
        messages.filterMap(m => {
            match m {
                | RoTAKeyWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_rot_ok_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvks: List[BRD_BVK], com: Committee, avk: BRD_ROT}] = {
        messages.filterMap(m => {
            match m {
                | RoTOKWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_certify_read_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | CertifyReadWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_certify_apl_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | CertifyAPLWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_certify_hash_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | CertifyHashWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_certify_prove_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | CertifyProveWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_access_check(cfg: MsgConfig, params: BRD_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    }         

    pure def brd_get_bkgen_request_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BKGenRequest(p) => Some(p)
                | _ => None
            }
        })
    }
    pure def brd_get_rot_request_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | RoTRequest(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_certify_request_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, rot: BRD_ROT, x: LED_Tx}] = {
        messages.filterMap(m => {
            match m {
                | CertifyRequest(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bkgen_response_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvk: int}] = {
        messages.filterMap(m => {
            match m {
                | BKGenResponse(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_rot_response_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvks: List[BRD_BVK], com: Committee, avk: BRD_ROT}] = {
        messages.filterMap(m => {
            match m {
                | RoTResponse(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_certify_response_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, res: Option[({pi: int, h: int})]}] = {
        messages.filterMap(m => {
            match m {
                | CertifyResponse(p) => Some(p)
                | _ => None
            }
        })
    }
    
}