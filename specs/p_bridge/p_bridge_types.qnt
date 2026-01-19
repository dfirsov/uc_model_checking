module p_bridge_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import f_atms.* from "../f_atms/f_atms"    
    import g_clock.* from "../g_clock/g_clock"    
    import f_atms_types.* from "../f_atms/f_atms_types"    
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_ledger_types.* from "../g_ledger/g_ledger_types" 
    import f_diff_types.* from "../f_diff/f_diff_types" 
    import f_sig_types.* from "../f_sig/f_sig_types" 
    import f_pki_types.* from "../f_pki/f_pki_types"
    
    type BRD_BVK = ATMS_VK 
    type BRD_ROT = ATMS_AVK 
    type BRD_AUX = {bvks: List[BRD_BVK], com: Committee}  
    type BRD_BR_Args = {r: Time, ms: Set[{party: Party, avk: ATMS_AVK, sig: SIG_Signature}], L1: List[LED_Tx], L2: List[LED_Tx]}
    type BRD_BST_Entry = {
                bvk: Option[BRD_BVK],
                rot: Option[BRD_ROT],
                cID: Option[int],
                m: Set[{party: Party, avk: ATMS_AVK, sig: SIG_Signature}],
                aux_prim: Option[{bvks: Party -> BRD_BVK, com: Committee}],
                aux: Option[{bvks: Party -> BRD_BVK, com: Committee}],
                avk_prim: Option[ATMS_AVK],
                h: Option[int],
    }

    type BRD_BR_Resp = {
            btx1: Option[LED_Tx], 
            btx2: Option[LED_Tx], 
            m: Option[{party: Party, avk: ATMS_AVK, sig: SIG_Signature}], 
            bst: Option[BRD_BST_Entry] }

    type BRD_Message =
        | BKGenRequest({cfg: MsgConfig}) 
        | BKGenATMGenWait({cfg: MsgConfig})
        | BKGenPKIWait({cfg: MsgConfig, bvk: BRD_BVK})
        | BKGenOKWait({cfg: MsgConfig, bvk: BRD_BVK})   
        | BKGenResponse({cfg: MsgConfig, bvk: BRD_BVK})

        | RoTRequest({cfg: MsgConfig})
        | RoTPKIGetAllWait({cfg: MsgConfig})
        | RoTComSelWait({cfg: MsgConfig, bvks: Party -> BRD_BVK})
        | RoTAKeyWait({cfg: MsgConfig, bvks: Party -> BRD_BVK, com: Committee})
        | RoTOKWait({cfg: MsgConfig, bvks: Party -> BRD_BVK, com: Committee, avk: BRD_ROT})
        | RoTResponse({cfg: MsgConfig, bvks: Party -> BRD_BVK, com: Committee, avk: BRD_ROT})

        | BridgeRequest({cfg: MsgConfig})
        | BridgeFetchWait({cfg: MsgConfig, r: Time})
        | BridgeL1ReadWait({cfg: MsgConfig, r: Time, ms: Set[{party: Party, avk: ATMS_AVK, sig: SIG_Signature}]})
        | BridgeL2ReadWait({cfg: MsgConfig, r: Time, ms: Set[{party: Party, avk: ATMS_AVK, sig: SIG_Signature}], L1: List[LED_Tx]})
        | BridgeBRAlgorithmRequest({cfg: MsgConfig, args: BRD_BR_Args})
        | BridgeBRAlgorithmResponse({cfg: MsgConfig, args: BRD_BR_Args, resp: BRD_BR_Resp})

        | BridgeComSelWait({cfg: MsgConfig})
        | BridgeAKeyWait({cfg: MsgConfig})
        | BridgeAPLWait({cfg: MsgConfig})
        | BridgeHashWait({cfg: MsgConfig})
        | BridgeSigWait({cfg: MsgConfig})
        | BridgeASigWait({cfg: MsgConfig, args: BRD_BR_Args, bst: BRD_BST_Entry, msg: {h: int, avk: ATMS_AVK}, cID: int})
        | BridgeDiffuseWait({cfg: MsgConfig})
        | BridgeL1SubmitWait({cfg: MsgConfig})
        | BridgeL2SubmitWait({cfg: MsgConfig})
        | BridgeOKWait({cfg: MsgConfig})
        | BridgeResponse({cfg: MsgConfig})

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
                | BridgeResponse(_) => Some(m)
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
        //bvk: Party -> BRD_BVK,
        //rot: Party -> BRD_ROT,
        //aux: Party -> BRD_AUX,
        bst: Party -> BRD_BST_Entry,

        bkgen_flag: Set[Party],
        rot_flag: Set[Party],
        bridge_flag: Set[(Party, Time)],
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
        ledger2: LED_System,
        dif: DIF_System,
    }    

    type BRD_CTransition = {
        brd: BRD_Transition,
        clock: GC_Transition,
        atms: ATMS_Transition,
        pki: PKI_Transition,
        ledger1: LED_Transition,
        ledger2: LED_Transition,
        dif: DIF_Transition,
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
            ledger2: sys.ledger2.led_id_trans(), 
            dif: sys.dif.dif_id_trans(), 
        }
    }    

    pure def brd_get_bridge_request_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeRequest(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_fetch_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, r: Time}] = {
        messages.filterMap(m => {
            match m {
                | BridgeFetchWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_l1_read_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, r: Time, ms: Set[{party: Party, avk: ATMS_AVK, sig: SIG_Signature}]}] = {
        messages.filterMap(m => {
            match m {
                | BridgeL1ReadWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_br_algorithm_request_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, args: BRD_BR_Args}] = {
        messages.filterMap(m => {
            match m {
                | BridgeBRAlgorithmRequest(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_l2_read_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, r: Time, ms: Set[{party: Party, avk: ATMS_AVK, sig: SIG_Signature}], L1: List[LED_Tx]}] = {
        messages.filterMap(m => {
            match m {
                | BridgeL2ReadWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_com_sel_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeComSelWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_akey_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeAKeyWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_apl_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeAPLWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_hash_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeHashWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_sig_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeSigWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_asig_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, args: BRD_BR_Args, bst: BRD_BST_Entry, msg: {h: int, avk: ATMS_AVK}, cID: int}] = {
        messages.filterMap(m => {
            match m {
                | BridgeASigWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_diffuse_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeDiffuseWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_l1_submit_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeL1SubmitWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_l2_submit_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeL2SubmitWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_ok_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeOKWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_bridge_response_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
            match m {
                | BridgeResponse(p) => Some(p)
                | _ => None
            }
        })
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

    pure def brd_get_rot_com_sel_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvks: Party -> BRD_BVK}] = {
        messages.filterMap(m => {
            match m {
                | RoTComSelWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_rot_akey_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvks: Party -> BRD_BVK, com: Committee}] = {
        messages.filterMap(m => {
            match m {
                | RoTAKeyWait(p) => Some(p)
                | _ => None
            }
        })
    }

    pure def brd_get_rot_ok_wait_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvks: Party -> BRD_BVK, com: Committee, avk: BRD_ROT}] = {
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

    pure def brd_get_rot_response_messages(messages: Set[BRD_Message]): Set[{cfg: MsgConfig, bvks: Party -> BRD_BVK, com: Committee, avk: BRD_ROT}] = {
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