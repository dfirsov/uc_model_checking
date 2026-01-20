module p_bridge_bkgen{

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import f_pki_types.* from "../f_pki/f_pki_types"    
    import f_pki.* from "../f_pki/f_pki"    
    import f_atms_types.* from "../f_atms/f_atms_types"    
    import f_atms.* from "../f_atms/f_atms"    
    
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_clock.* from "../g_clock/g_clock"

    import g_ledger_types.* from "../g_ledger/g_ledger_types"
    import g_ledger.* from "../g_ledger/g_ledger"    

    import p_bridge_types.* from "./p_bridge_types"    
    import p_bridge.* from "./p_bridge"    
    
    

    pure def brd_bkgen_1(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        sys.brd.input.brd_get_bkgen_request_messages().map(p => {
            if(brd_access_check(p.cfg, sys.brd.params)){
                val r = sys.clock.state.TIME
                if(r != sys.brd.params.r0 or sys.brd.state.bkgen_flag.contains(p.cfg.party)){

                    Set({...sys.brd_id_ctrans(),
                         brd: sys.brd.brd_skip_trans()})

                }else{

                    val brd_trans = {
                        post_state: {...sys.brd.state,
                            bkgen_flag: sys.brd.state.bkgen_flag.union(Set(p.cfg.party))
                        },

                        effects: Set(
                            BRD_LogMessage(BKGenRequest(p)),
                            BRD_UpdateInput(Set()),
                            BRD_AppendHandle(BKGenATMGenWait(p))  
                        )
                    }

                    val atms_trans = {
                        post_state: sys.atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set(GenRequest({cfg: p.cfg})))
                        )   
                    }
                    
                    Set({...sys.brd_id_ctrans(),
                        brd: brd_trans,
                        atms: atms_trans})
                }
            }else{
                Set({
                    ...sys.brd_id_ctrans(),
                    brd: sys.brd.brd_skip_trans()
                })
            }            
        }).flatten()
    }

    pure def brd_bkgen_2(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.atms.input.atms_get_gen_response_messages().map(p => {
            sys.brd.handles.brd_get_bkgen_atm_gen_wait_messages().filter(q => p.cfg == q.cfg).map(q => {
                val brd_trans = {
                    post_state: {...sys.brd.state,
                        //bvk: sys.brd.state.bvk.put(p.cfg.party, p.vk)
                        bst: sys.brd.state.bst.put(p.cfg.party, {...sys.brd.state.bst.get(p.cfg.party),
                            bvk: Some(p.vk)
                        })
                    },
                    effects: Set(
                        BRD_UpdateInput(Set()),
                        BRD_RemoveHandle(BKGenATMGenWait(q)),
                        BRD_AppendHandle(BKGenPKIWait({cfg: q.cfg, bvk: p.vk}))
                    )
                }

                val pki_trans = { 
                    post_state: sys.pki.state,
                    effects: Set(
                        PKI_UpdateInput(Set(PKI_RegisterRequest({cfg: q.cfg, vk: p.vk})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    pki: pki_trans,
                    atms: sys.atms.atms_skip_trans()
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_bkgen_3(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.pki.input.pki_get_register_response_messages().map(p => {
            sys.brd.handles.brd_get_bkgen_pki_wait_messages().filter(q => p.cfg == q.cfg).map(q => {
                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BKGenPKIWait(q)),
                        BRD_AppendHandle(BKGenOKWait(q))
                    )
                }

                val clock_trans = {
                    post_state: sys.clock.state,
                    effects: Set(
                        GC_UpdateInput(Set(OkayRequest({cfg: q.cfg})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    clock: clock_trans,
                    pki: sys.pki.pki_skip_trans()
                })

            }).flatten()
        }).flatten()
    }

    pure def brd_bkgen_4(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.clock.input.get_okay_response_messages().map(p => {
            sys.brd.handles.brd_get_bkgen_ok_wait_messages().filter(q => p.cfg == q.cfg).map(q => {
                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BKGenOKWait(q)),
                        BRD_UpdateInput(Set(BKGenResponse({cfg: q.cfg, bvk: q.bvk})))
                    )
                }

                val clock_trans = {
                    post_state: sys.clock.state,
                    effects: Set(
                        GC_UpdateInput(Set())
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    clock: clock_trans
                })
            }).flatten()
        }).flatten()
    }


    pure def brd_bkgen_main_listener(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        Set(
            brd_bkgen_1(sys),
            brd_bkgen_2(sys),
            brd_bkgen_3(sys),
            brd_bkgen_4(sys),
        ).flatten()
    }    
    
}