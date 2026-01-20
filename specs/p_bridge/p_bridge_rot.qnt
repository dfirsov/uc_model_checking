module p_bridge_rot{

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

    pure def brd_rot_1(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        sys.brd.input.brd_get_rot_request_messages().map(p => {
            if(brd_access_check(p.cfg, sys.brd.params)){
                val r = sys.clock.state.TIME
                if(r != sys.brd.params.r0 + 1 or sys.brd.state.rot_flag.contains(p.cfg.party)){

                    Set({...sys.brd_id_ctrans(),
                         brd: sys.brd.brd_skip_trans()})

                }else{

                    val brd_trans = {
                        post_state: {...sys.brd.state,
                            rot_flag: sys.brd.state.rot_flag.union(Set(p.cfg.party))
                        },

                        effects: Set(
                            BRD_LogMessage(RoTRequest(p)),
                            BRD_UpdateInput(Set()),
                            BRD_AppendHandle(RoTPKIGetAllWait(p))  
                        )
                    }

                    val pki_trans = {
                        post_state: sys.pki.state,
                        effects: Set(
                            PKI_UpdateInput(Set(PKI_GetAllRequest({cfg: p.cfg})))
                        )   
                    }
                    
                    Set({...sys.brd_id_ctrans(),
                        brd: brd_trans,
                        pki: pki_trans})
                }
            }else{
                Set({
                    ...sys.brd_id_ctrans(),
                    brd: sys.brd.brd_skip_trans()
                })
            }            
        }).flatten()
    }        

    pure def brd_rot_2(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.pki.input.pki_get_get_all_response_messages().map(p => {
            sys.brd.handles.brd_get_rot_pki_get_all_wait_messages().filter(q => p.cfg == q.cfg).map(q => {
                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set()),
                        BRD_RemoveHandle(RoTPKIGetAllWait(q)),
                        BRD_AppendHandle(RoTComSelWait({cfg: q.cfg, bvks: p.vks}))
                    )
                }

                val ledger1_trans = {
                    post_state: sys.ledger1.state,
                    effects: Set(
                        LED_UpdateInput(Set(ComSelRequest({cfg: p.cfg})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    pki: sys.pki.pki_skip_trans(),
                    ledger1: ledger1_trans
                })
            }).flatten()
        }).flatten()
    }        
    

    pure def brd_rot_3(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.ledger1.input.led_get_comsel_response_messages().map(p => {
            sys.brd.handles.brd_get_rot_com_sel_wait_messages().filter(q => p.cfg == q.cfg).map(q => {

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set()),
                        BRD_RemoveHandle(RoTComSelWait(q)),
                        BRD_AppendHandle(RoTAKeyWait({cfg: q.cfg, bvks: q.bvks, com: p.com}))
                    )
                }

                val vks = p.com.listFilterMap(party => if (q.bvks.has(party) and p.com.listHas(party)) Some(q.bvks.get(party)) else None)

                val atms_trans = {
                    post_state: sys.atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(AKeyRequest({cfg: q.cfg, vks: vks }))), 
                    )
                }                

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    ledger1: sys.ledger1.led_skip_trans(),
                    atms: atms_trans
                })
            }).flatten()
        }).flatten()
    }        

    pure def brd_rot_4(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.atms.input.atms_get_akey_response_messages().map(p => {
            sys.brd.handles.brd_get_rot_akey_wait_messages().filter(q => p.cfg == q.cfg).map(q => {
                if(p.avk == None){
                    Set({...sys.brd_id_ctrans(),
                        brd: {
                            post_state: sys.brd.state,
                            effects: Set(
                                BRD_UpdateInput(Set()),
                                BRD_RemoveHandle(RoTAKeyWait(q))
                            )
                        },
                        atms: sys.atms.atms_skip_trans()
                    })
                }else{
                    val brd_trans = {
                        post_state: {...sys.brd.state, 
                            bst: sys.brd.state.bst.put(p.cfg.party, {...sys.brd.state.bst.get(p.cfg.party),
                                rot: p.avk,
                                aux: Some({bvks: q.bvks, com: q.com})
                            })
                            //rot: sys.brd.state.rot.put(p.cfg.party, p.avk.unwrap()),
                            //aux: sys.brd.state.aux.put(p.cfg.party, {bvks: q.bvks, com: q.com})
                        },
                        effects: Set(
                            BRD_UpdateInput(Set()),
                            BRD_RemoveHandle(RoTAKeyWait(q)),
                            BRD_AppendHandle(RoTOKWait({cfg: q.cfg, bvks: q.bvks, com: q.com, avk: p.avk.unwrap()})) 
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
                        atms: sys.atms.atms_skip_trans()
                    })
                }
            }).flatten()
        }).flatten()
    }    

    pure def brd_rot_5(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.clock.input.get_okay_response_messages().map(p => {
            sys.brd.handles.brd_get_rot_ok_wait_messages().filter(q => p.cfg == q.cfg).map(q => {

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set(RoTResponse({
                            cfg: q.cfg,
                            bvks: q.bvks,
                            com: q.com,
                            avk: q.avk 
                        }))),
                        BRD_RemoveHandle(RoTOKWait(q)),
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    clock: sys.clock.gc_skip_trans(),
                    atms: sys.atms.atms_skip_trans()
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_rot_main_listener(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        Set(
            brd_rot_1(sys),
            brd_rot_2(sys),
            brd_rot_3(sys),
            brd_rot_4(sys),
            brd_rot_5(sys),
        ).flatten()
    }             
        
}