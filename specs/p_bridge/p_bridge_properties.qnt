module p_bridge_properties{
    
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
    import p_bridge_env.* from "./p_bridge_env" 
   

    val Parties = Set(1,2)
    val C = Set()
    val SIDs = Set(1)
    val P1 = Set(1,2)
    val P2 = Set(1,2)   

    action init = all {
        p_bridge_env_init({
            P: Parties, 
            SIDs: SIDs, 
            C: C, 
            r0: 0, 
            delta: 1, 
            t: 1, 
            R: 4, 
            delta_latency: 1, 
            delta_slack: 1, 
            delta_apl: 1, 
            comsel_a: 1, 
            comsel_b: 1, 
            comsel_c: 1})
    }

    action step = {
        val x = {Parties: Parties, SIDs: SIDs}

        any { 
            p_bridge_env_step(x),
            p_bridge_consume_responses,
            g_ledger1_response_messages,
            p_bridge_request_messages(x),
            g_ledger1_request_messages(x),        
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

                   // p_bst.bvk == q_bst.bvk, // falsified
                   // p_bst.m == q_bst.m, // falsified
                }
            }
        )
    )

    //Bridge aux update: For all rounds r and all parties P, we have that bst[P].aux @ (r + R) = bst[P].aux’ @ r.
    val Bridge_Aux_Update = p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(p =>
        p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(q =>
            and {
                p.cfg.party == q.cfg.party,
                p.args.r + p_bridge_s.params.R + p_bridge_s.params.delta + 2 == q.args.r,
                p.resp.bst != None,
                q.resp.bst != None
            } implies {
                val p_bst = p.resp.bst.unwrap()
                val q_bst = q.resp.bst.unwrap()
                p_bst.aux == q_bst.aux_prim
            }
        )
    )

    // (VIOLATED) Bridge democracy: 
    // For all rounds r and all parties P, we have 
    // that bst[P].aux.Com @ r ≠ bst[P].aux.Com @ (r + R) (doesn’t hold – authoritarian regime possible)
    val Bridge_Democracy = p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(p =>
        p_bridge_s.log.map(x => x.msg).brd_get_bridge_br_algorithm_response_messages().forall(q =>
            and {
                p.cfg.party == q.cfg.party,
                p.args.r + p_bridge_s.params.R == q.args.r,
                p.resp.bst != None,
                q.resp.bst != None
            } implies {
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