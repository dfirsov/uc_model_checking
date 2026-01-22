module p_bridge_env{ 
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"

    import common.* from "../common"

    import f_pki_types.* from "../f_pki/f_pki_types"    
    import f_pki.* from "../f_pki/f_pki"

    import f_hash_types.* from "../f_hash/f_hash_types"    
    import f_hash.* from "../f_hash/f_hash"    

    import f_atms_types.* from "../f_atms/f_atms_types"    
    import f_atms.* from "../f_atms/f_atms"    

    import g_ledger_types.* from "../g_ledger/g_ledger_types" 
    import g_ledger.* from "../g_ledger/g_ledger" 
    import g_ledger_comsel.* from "../g_ledger/g_ledger_comsel"
    import g_ledger_apl.* from "../g_ledger/g_ledger_apl"
    import g_ledger_ival.* from "../g_ledger/g_ledger_ival"

    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_clock.* from "../g_clock/g_clock"

    import f_diff_types.* from "../f_diff/f_diff_types"    
    import f_diff.* from "../f_diff/f_diff"        

    import p_bridge_types.* from "./p_bridge_types" 
    import p_bridge.* from "./p_bridge" 
    import p_bridge_bkgen.* from "./p_bridge_bkgen"     
    import p_bridge_rot.* from "./p_bridge_rot"
    import p_bridge_br.* from "./p_bridge_br"
    import p_bridge_led2.* from "./p_bridge_led2"
    
   
    val Parties = Set(1,2)
    val C = Set()
    val SIDs = Set(1)
    val P1 = Set(1,2)
    val P2 = Set(1,2)

    pure def brd_main_listener(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        brd_bkgen_main_listener(sys)
            .union(brd_rot_main_listener(sys))
            .union(brd_br_main_listener(sys))
    }

    action brd_step_pure : bool = {
        all{
            val sys = {
                brd: p_bridge_s,
                clock: g_clock_s,
                atms: f_atms_s,
                pki: pki_s,
                ledger1: g_ledger_s,
                ledger2: g_ledger_s2,
                hash: f_hash_s,
                dif: f_dif_s,
            }
            val ts = brd_main_listener(sys)
            all {
                nondet transition = ts.oneOf()
                val new_sys = brd_process_ctransitions(sys, transition)
                all {
                    p_bridge_s' = new_sys.brd,
                    g_clock_s' = new_sys.clock,
                    f_atms_s' = new_sys.atms,
                    pki_s' = new_sys.pki,
                    g_ledger_s' = new_sys.ledger1,
                    g_ledger_s2' = new_sys.ledger2,
                    f_hash_s' = new_sys.hash,
                    f_dif_s' = new_sys.dif,
                }
            }
        }
    }     

    action brd_step : bool =  all {
        brd_step_pure
    }    


    action step : bool = any {
        brd_step,
        all { // clock steps
            gc_step,
            p_bridge_s' = p_bridge_s,
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
        },
        all { // pki steps
            pki_step,
            p_bridge_s' = p_bridge_s,
            g_clock_s' = g_clock_s,
            f_atms_s' = f_atms_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
        },
        all { // atms steps
            atms_step,
            p_bridge_s' = p_bridge_s,
            g_clock_s' = g_clock_s,
            pki_s' = pki_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
        },
        all{ // ledger1 steps
            any {
                led_step,
                led_step_apl,
                led_step_comsel,
            },
            p_bridge_s' = p_bridge_s,
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            f_hash_s' = f_hash_s,
            g_ledger_s2' = g_ledger_s2,
            f_dif_s' = f_dif_s,
        },
        all { // ledger1 ival steps
            led_step_ival,
            p_bridge_s' = p_bridge_s,
            pki_s' = pki_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
        },
        all{ // ledger2 steps
            any {
                led2_step,
            },
            p_bridge_s' = p_bridge_s,
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            f_hash_s' = f_hash_s,
            g_ledger_s' = g_ledger_s,
            f_dif_s' = f_dif_s,
        },  
        all { // ledger2 ival steps
            led2_step_ival,
            p_bridge_s' = p_bridge_s,
            pki_s' = pki_s,
            g_ledger_s' = g_ledger_s,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
        },              
        all { // hash steps
            hash_step,
            p_bridge_s' = p_bridge_s,
            g_clock_s' = g_clock_s,
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_dif_s' = f_dif_s,
        },
        all { // diffuse steps
            dif_step,
            p_bridge_s' = p_bridge_s,
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
        },
        p_bridge_consume_responses,
        p_bridge_request_messages,
        g_ledger1_request_messages,
        g_ledger1_response_messages,
    }

   pure def g_ledger1_response_messages_trans(sys: LED_System) : Set[LED_Transition] = {
         sys.input.led_get_submit_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                LED_LogMessage(SubmitResponse(msg_log)),
                LED_UpdateInput(Set())
            )
         })
    }

    action g_ledger1_response_messages : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = g_ledger1_response_messages_trans(g_ledger_s)
        
        all {
        // Non-deterministically choose one response message to consume
            nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
            g_ledger_s' = led_process_transitions(g_ledger_s, g_clock_s, transition),
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            g_clock_s' = g_clock_s, 
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,          
            f_dif_s' = f_dif_s,   
            p_bridge_s' = p_bridge_s,
        }
   }            

    action p_bridge_request_messages : bool = all {
        p_bridge_s.input.size() == 0,
        f_atms_s.input.size() == 0,
        pki_s.input.size() == 0,
        g_clock_s.input.size() == 0,
        g_ledger_s.input.size() == 0,
        g_ledger_s2.input.size() == 0,
        f_hash_s.input.size() == 0,
        f_dif_s.input.size() == 0,

        all {        
            nondet from = Parties.oneOf()
            nondet sid = SIDs.oneOf()

            val cfg = {party: from, sid: sid, nonce: p_bridge_s.nonce}
            any {
                p_bridge_bkgen_request(cfg),
                p_bridge_rot_request(cfg),
                p_bridge_br_request(cfg),
            },
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            g_clock_s' = g_clock_s, 
            g_ledger_s' = g_ledger_s, 
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,          
            f_dif_s' = f_dif_s,     
        }
    }    

    action g_ledger1_request_messages : bool = all {
        p_bridge_s.input.size() == 0,
        f_atms_s.input.size() == 0,
        pki_s.input.size() == 0,
        g_clock_s.input.size() == 0,
        g_ledger_s.input.size() == 0,
        g_ledger_s2.input.size() == 0,
        f_hash_s.input.size() == 0,
        f_dif_s.input.size() == 0,
        and { g_ledger_s.state.buffer.keys().size() < 1
           , g_ledger_s.state.log.length() < 3 },        

        all {        
            nondet from = P1.oneOf()
            nondet sid = SIDs.oneOf()

            val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}
            any {
                g_ledger1_submit_request(cfg),
            },
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            g_clock_s' = g_clock_s, 
            p_bridge_s' = p_bridge_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,          
            f_dif_s' = f_dif_s,     
        }
    }    

    action g_ledger1_submit_request(cfg: MsgConfig) : bool = all {
        nondet tx = 1.to(100).oneOf() // TODO: determine appropriate range
        val ntx = SubmitRequest({cfg: cfg, tx: Normal({tx: tx, b: true})})

        g_ledger_s' = {...g_ledger_s, 
            input: Set(ntx),
            nonce: g_ledger_s.nonce + 1
        }
    }


    action p_bridge_bkgen_request(cfg: MsgConfig) : bool = all {
        p_bridge_s' = {...p_bridge_s, 
            input: Set(BKGenRequest({cfg: cfg})),
            nonce: p_bridge_s.nonce + 1
        }
    }

    action p_bridge_rot_request(cfg: MsgConfig) : bool = all {
        p_bridge_s' = {...p_bridge_s, 
            input: Set(RoTRequest({cfg: cfg})),
            nonce: p_bridge_s.nonce + 1
        }
    }

    action p_bridge_br_request(cfg: MsgConfig) : bool = all {
        p_bridge_s' = {...p_bridge_s, 
            input: Set(BridgeRequest({cfg: cfg})),
            nonce: p_bridge_s.nonce + 1
        }
    }    
 
    pure def p_bridge_consume_response(brd: BRD_System) : Set[BRD_Transition] = {
         brd.input.brd_get_all_response_messages().map(msg_log => {
            post_state : brd.state,
            effects : Set(
                BRD_LogMessage(msg_log),
                BRD_UpdateInput(Set())
            )
         })
    }

    action p_bridge_consume_responses : bool =  all {
        val response_msgs = p_bridge_s.p_bridge_consume_response()
        nondet transition = response_msgs.oneOf()

        p_bridge_s' = brd_process_transitions(p_bridge_s, transition),
        g_clock_s' = g_clock_s,
        f_atms_s' = f_atms_s,
        pki_s' = pki_s,
        g_ledger_s' = g_ledger_s,
        g_ledger_s2' = g_ledger_s2,
        f_hash_s' = f_hash_s,
        f_dif_s' = f_dif_s,
    }

    action init : bool = all {
        val r0 = 0
        val delta = 1
        val t = 1
        val R = 4

        all{
            hash_init(Parties, SIDs),
            pki_init(Parties, SIDs),
            gc_init(C, r0, all_parties_at_all_sids(Parties, SIDs)),
            dif_init(Parties, C, SIDs, delta),

            brd_init(Parties, C, SIDs, r0, delta, R),
            atms_init({P : Parties, C : C, SIDs: SIDs, t: (t-1)}),

            g_ledger_s' = led_initial({P: Parties, C: C, SIDs: SIDs, delta_latency: 1, delta_slack: 1, delta_apl: 1, Val: true, comsel_a: 1, comsel_b: 1, comsel_c: 1}),
            g_ledger_s2' = led_initial({P: Parties, C: C, SIDs: SIDs, delta_latency: 1, delta_slack: 1, delta_apl: 1, Val: true, comsel_a: 1, comsel_b: 1, comsel_c: 1})
            
        }
    }

    val Q1 = p_bridge_s.log.size() <= 1
    val Q2 = p_bridge_s.state.bst.get(1).rot == None
    val Q3 = g_clock_s.state.TIME <= 4
    val Q4 = f_hash_s.state.get(1).hash_map.keys().size() <= 1
    val Q5 = g_clock_s.state.TIME <= 8

    val RoT_Agreement = p_bridge_s.log.map(x => x.msg).brd_get_rot_response_messages().forall(rsp1 =>
        p_bridge_s.log.map(x => x.msg).brd_get_rot_response_messages().forall(rsp2 =>
            rsp1.avk == rsp2.avk
        )
    )

    val Bridge_State_Consistency = p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(p =>
        p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(q =>
            (p.args.r == q.args.r and p.resp.bst != None) implies {
                val p_bst = p.resp.bst.unwrap()
                val q_bst = q.resp.bst.unwrap()
                and {
                    p_bst.rot == q_bst.rot,
                    p_bst.cID == q_bst.cID,
                    p_bst.h == q_bst.h,

                    
                    p_bst.avk_prim == q_bst.avk_prim,
                    p_bst.aux == q_bst.aux,
                    p_bst.aux_prim == q_bst.aux_prim,

                   //p_bst.bvk == q_bst.bvk, // falsified
                   // p_bst.m == q_bst.m, // falsified
                }
            }
        )
    )

    //Bridge aux update: For all rounds r and all parties P, we have that bst[P].aux @ (r + R) = bst[P].aux’ @ r.
    val Bridge_Aux_Update = p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(p =>
        p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(q =>
            (p.cfg.party == q.cfg.party and p.args.r + p_bridge_s.params.R + p_bridge_s.params.delta + 2 == q.args.r and p.resp.bst != None and q.resp.bst != None) implies {
                val p_bst = p.resp.bst.unwrap()
                val q_bst = q.resp.bst.unwrap()
                p_bst.aux == q_bst.aux_prim
            }
        )
    )

    // (VIOLATED) Bridge democracy: For all rounds r and all parties P, we have that bst[P].aux.Com @ r ≠ bst[P].aux.Com @ (r + R) (doesn’t hold – authoritarian regime possible)
    val Bridge_Democracy = p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(p =>
        p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(q =>
            (p.cfg.party == q.cfg.party and p.args.r + p_bridge_s.params.R == q.args.r and p.resp.bst != None and q.resp.bst != None) implies {
                val p_bst = p.resp.bst.unwrap()
                val q_bst = q.resp.bst.unwrap()
                (p_bst.aux != None and q_bst.aux != None) implies p_bst.aux.unwrap().com != q_bst.aux.unwrap().com
            }
        )
    )

    // Bridge state persistence: For all rounds r and r’ and all parties P,  we have 
    // bst[P].(rot, cID, h, avk’, aux, aux’) @ r = bst[P].(rot, cID, h, avk’, aux, aux’) @ r’ as long as  1 ≤ (r % R),  (r’ % R) ≤ ∆ + 1; 
    val Bridge_State_Persistence = p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(p =>
        p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(q =>
            and {
                p.cfg.party == q.cfg.party,
                p.args.r % p_bridge_s.params.R >= 1, 
                q.args.r % p_bridge_s.params.R >= 1, 

                p.args.r % p_bridge_s.params.R <= p_bridge_s.params.delta + 1,
                q.args.r % p_bridge_s.params.R <= p_bridge_s.params.delta + 1,
                
                p.resp.bst != None,
                q.resp.bst != None,
                p.args.r + p_bridge_s.params.R < q.args.r,
            } implies {
                val p_bst = p.resp.bst.unwrap()
                val q_bst = q.resp.bst.unwrap()
                and {
                    p_bst.rot == q_bst.rot,
                    p_bst.cID == q_bst.cID,
                    p_bst.h == q_bst.h,

                    p_bst.avk_prim == q_bst.avk_prim,
                    p_bst.aux == q_bst.aux,
                    p_bst.aux_prim == q_bst.aux_prim,
                }
            }
        )
    )


}