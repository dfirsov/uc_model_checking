module p_bridge_br{


    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import f_pki_types.* from "../f_pki/f_pki_types"    
    import f_pki.* from "../f_pki/f_pki"    

    import f_atms_types.* from "../f_atms/f_atms_types"    
    import f_atms.* from "../f_atms/f_atms"    

    import f_diff_types.* from "../f_diff/f_diff_types"    
    import f_diff.* from "../f_diff/f_diff"        
    
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_clock.* from "../g_clock/g_clock"

    import g_ledger_types.* from "../g_ledger/g_ledger_types"
    import g_ledger.* from "../g_ledger/g_ledger"    

    import p_bridge_types.* from "./p_bridge_types"    
    import p_bridge.* from "./p_bridge"     
    

    pure def brd_br_1(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        sys.brd.input.brd_get_rot_request_messages().map(p => {
            if(brd_access_check(p.cfg, sys.brd.params)){
                val r = sys.clock.state.TIME
                if(r < sys.brd.params.r0 + 2 or sys.brd.state.bridge_flag.contains((p.cfg.party, r))){

                    Set({...sys.brd_id_ctrans(),
                         brd: sys.brd.brd_skip_trans()})

                }else{

                    val brd_trans = {
                        post_state: {...sys.brd.state,
                            bridge_flag: sys.brd.state.bridge_flag.union(Set((p.cfg.party, r)))
                        },

                        effects: Set(
                            BRD_LogMessage(BridgeRequest(p)),
                            BRD_UpdateInput(Set()),
                            BRD_AppendHandle(BridgeFetchWait({cfg: p.cfg, r: r}))
                        )
                    }

                    val dif_trans = {
                        post_state: sys.dif.state,
                        effects: Set(
                            DIF_UpdateInput(Set(DIF_FetchRequest({cfg: p.cfg})))
                        )   
                    }
                    
                    Set({...sys.brd_id_ctrans(),
                        brd: brd_trans,
                        dif: dif_trans})
                }
            }else{
                Set({
                    ...sys.brd_id_ctrans(),
                    brd: sys.brd.brd_skip_trans()
                })
            }
        }).flatten()
    }

    pure def brd_br_2(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.dif.input.dif_get_fetch_response_messages().map(p => {
            sys.brd.handles.brd_get_bridge_fetch_wait_messages().filter(q => p.cfg == q.cfg).map(q => {

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BridgeFetchWait(q)),
                        BRD_AppendHandle(BridgeL1ReadWait({cfg: p.cfg, r: q.r, ms: p.data}))
                    )
                }

                val ledger1_trans = {
                    post_state: sys.ledger1.state,
                    effects: Set(
                        LED_UpdateInput(Set(ReadRequest({cfg: p.cfg})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    dif: sys.dif.dif_skip_trans(),
                    ledger1: ledger1_trans
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_br_3(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.ledger1.input.led_get_read_response_messages().map(p => {
            sys.brd.handles.brd_get_bridge_l1_read_wait_messages().filter(q => p.cfg == q.cfg).map(q => {

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BridgeL1ReadWait(q)),
                        BRD_AppendHandle(BridgeL2ReadWait({cfg: p.cfg, r: q.r, ms: q.ms, L1: p.result}))
                    )
                }

                val ledger2_trans = {
                    post_state: sys.ledger2.state,
                    effects: Set(
                        LED_UpdateInput(Set(ReadRequest({cfg: p.cfg})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    ledger1: sys.ledger1.led_skip_trans(),
                    ledger2: ledger2_trans
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_br_4(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.ledger2.input.led_get_read_response_messages().map(p => {
            sys.brd.handles.brd_get_bridge_l2_read_wait_messages().filter(q => p.cfg == q.cfg).map(q => {

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BridgeL2ReadWait(q)),
                        BRD_UpdateInput(Set(BridgeBRAlgorithmRequest({cfg: p.cfg, args: {r: q.r, ms: q.ms, L1: q.L1, L2: p.result}}))),
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    ledger2: sys.ledger2.led_skip_trans(),
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_br_5(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.brd.input.brd_get_bridge_br_algorithm_request_messages().map(p => {
            val r = p.args.r
            val r0 = sys.brd.params.r0
            val R = sys.brd.params.R
            val delta = sys.brd.params.delta
            val bstP = sys.brd.state.bst.get(p.cfg.party)
            if(r == r0 + 2){
                brd_br_5_case1(sys, p, bstP)
            }else if(r0 + 3 <= r and r <= r0 + 3 + (R - ((r0 + 3) % R)) - 1){
                brd_br_5_case2(sys, p, bstP)
            }else if(r % R == 0){
                Set()
            }else if(1 <= r % R and r % R <= delta + 1){
                brd_br_5_case4(sys, p, bstP)
            }else if(r % R == delta + 2){
                brd_br_5_case5(sys, p, bstP)
            }else if(delta + 3 <= r % R){
                brd_br_5_case6(sys, p, bstP)
            }else{
                Map().get(1) // should not happen
            }
        }).flatten()
    }    

    pure def brd_br_5_case1(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {
        val rot = bst.rot.unwrap() // can it be None here?
        val code = rot
        val cID = rot
        val btx2 = Install({cID: cID, code: code})

        val brd_trans = {
            post_state: sys.brd.state,
            effects: Set(
                BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                    cfg: p.cfg, 
                    args: p.args, 
                    resp: ({
                        btx1: None,
                        btx2: Some(btx2),
                        m: None,
                        bst: Some({...bst, cID: Some(cID)})
                    })})))
            )
        }

        Set({...sys.brd_id_ctrans(),
            brd: brd_trans,
        })
    }

    pure def brd_br_5_case2(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {

        val brd_trans = {
            post_state: sys.brd.state,
            effects: Set(
                BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                    cfg: p.cfg, 
                    args: p.args, 
                    resp: ({
                        btx1: None,
                        btx2: None,
                        m: None,
                        bst: Some(bst)
                    })})))
            )
        }

        Set({...sys.brd_id_ctrans(),
            brd: brd_trans,
        })
    }

    pure def brd_br_5_case4(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {

        val brd_trans = {
            post_state: sys.brd.state,
            effects: Set(
                BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                    cfg: p.cfg, 
                    args: p.args, 
                    resp: ({
                        btx1: None,
                        btx2: None,
                        m: None,
                        bst: Some({...bst, m: bst.m.union(p.args.ms)})
                    })})))
            )
        }

        Set({...sys.brd_id_ctrans(),
            brd: brd_trans,
        })
    }        

    pure def brd_br_5_case5(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {
        val bvks = bst.aux.unwrap().bvks
        val com = bst.aux.unwrap().com
        val cID = bst.cID.unwrap() 
        val msg = {h: bst.h.unwrap(), avk: bst.avk_prim.unwrap()}

// | ASignRequest({cfg: MsgConfig, m: ATMS_Document, vks: List[ATMS_VK], S: List[{vk:ATMS_VK, sig: ATMS_Signature}]})
        val S = com.foldl(List(), (acc, party) => {
            if (bvks.has(party)){
                val bvk = bvks.get(party)
                val e = bst.m.find(x => x.party == party and x.avk == bvk)
                if (e == None){
                    acc.append({vk: bvk, sig: e.unwrap().sig})
                }else{
                    acc
                }
            }else{
                acc
            }
        })

        val vks = com.listFilterMap(party => if (bvks.has(party)) Some(party) else None)

        val br_trans = {
            post_state: sys.brd.state,
            effects: Set(
                BRD_AppendHandle((BridgeASigWait({
                    cfg: p.cfg, 
                    args: p.args, 
                    bst: bst,
                    msg: msg,
                    cID: cID
                    })))
            )
        }

        val atms_trans = {
            post_state: sys.atms.state,
            effects: Set(
                ATMS_UpdateInput(Set(ASignRequest({
                    cfg: p.cfg,
                    m: msg,
                    vks: vks,
                    S: S
                })))
            )
        }
        

        Set({...sys.brd_id_ctrans(),
            brd: br_trans,
            atms: atms_trans,
        })
    }  

    pure def brd_br_5_case5_2(sys : BRD_CSystem) : Set[BRD_CTransition] = {          
        sys.atms.input.atms_get_asign_response_messages().map(p => {
            sys.brd.handles.brd_get_bridge_asig_wait_messages().filter(q => p.cfg == q.cfg).map(q => {
                if(p.tau == None){
                    Set()
                }else{
                    val btx2 = Update({msg: q.msg, sigma: p.tau.unwrap()})
                    val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_RemoveHandle(BridgeASigWait(q)),
                            BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                                cfg: p.cfg, 
                                args: q.args, 
                                resp: ({
                                    btx1: None,
                                    btx2: Some(btx2),
                                    m: None,
                                    bst: Some({...q.bst, m : Set(), aux: q.bst.aux_prim})
                                })})))
                        )
                    }

                    Set({...sys.brd_id_ctrans(),
                        atms: sys.atms.atms_skip_trans(),
                        brd: brd_trans
                    })
                }
            }).flatten()
        }).flatten()
    }


    pure def brd_br_5_case6(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {

        val brd_trans = {
            post_state: sys.brd.state,
            effects: Set(
                BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                    cfg: p.cfg, 
                    args: p.args, 
                    resp: ({
                        btx1: None,
                        btx2: None,
                        m: None,
                        bst: Some(bst)
                    })})))
            )
        }

        Set({...sys.brd_id_ctrans(),
            brd: brd_trans,
        })
    }


}