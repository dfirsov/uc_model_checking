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
                Set()
            }else if(r % R == 0){
                Set()
            }else if(1 <= r % R and r % R <= delta + 1){
                Set()
            }else if(r % R == delta + 2){
                Set()
            }else if(delta + 3 <= r % R){
                Set()
            }else{
                Set() // TODO: make this an error
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



}