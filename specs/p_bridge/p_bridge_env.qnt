module p_bridge_env{ 
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"

    import f_pki_types.* from "../f_pki/f_pki_types"    
    import f_pki.* from "../f_pki/f_pki"
    import f_atms_types.* from "../f_atms/f_atms_types"    
    import f_atms.* from "../f_atms/f_atms"    

    import g_ledger_types.* from "../g_ledger/g_ledger_types" 
    import g_ledger.* from "../g_ledger/g_ledger" 
    import g_ledger_comsel.* from "../g_ledger/g_ledger_comsel"
    import g_ledger_apl.* from "../g_ledger/g_ledger_apl"
    import g_ledger_ival.* from "../g_ledger/g_ledger_ival"

    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_clock.* from "../g_clock/g_clock"
    import p_bridge_types.* from "./p_bridge_types" 
    import p_bridge.* from "./p_bridge" 

    import f_hash_types.* from "../f_hash/f_hash_types"    
    import f_hash.* from "../f_hash/f_hash"    

    import f_diff_types.* from "../f_diff/f_diff_types"    
    import f_diff.* from "../f_diff/f_diff"        

    import p_bridge_bkgen.* from "./p_bridge_bkgen"     
    import p_bridge_rot.* from "./p_bridge_rot"
    import p_bridge_br.* from "./p_bridge_br"
    
    var g_ledger_s2 : LED_System    


    action led2_gc_atms_step_with(q: (LED_System, GC_System, ATMS_System) => Set[(LED_Transition, GC_Transition, ATMS_Transition)]): bool = {
        all{
            val ts = q(g_ledger_s2, g_clock_s, f_atms_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s2' = led_process_transitions(g_ledger_s2, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2),
                    f_atms_s' = atms_process_transitions(f_atms_s, transition._3)
                }
            }
        }
    }     

    action led2_iread_2_act = {
        all {
            nondet simI_set = 1.to(10).powerset().oneOf() // TODO: determine appropriate size
            val simI = simI_set.toList((a,b) => EQ)
            nondet simPtr_prim = 1.to(10).oneOf() // TODO: what is the right range here?

            val ts = led_iread_2(g_ledger_s2, g_clock_s, simI, simPtr_prim)
            all {
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s2' = led_process_transitions(g_ledger_s2, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }

    action led2_step_ival : bool =  any {
        led2_gc_atms_step_with(led_main_listener_ival),
        all{
            led2_iread_2_act,
            f_atms_s' = f_atms_s,
        }
    }    

    action led2_gc_step_with(q: (LED_System, GC_System) => Set[(LED_Transition, GC_Transition)]): bool = {
        all{
            val ts = q(g_ledger_s2, g_clock_s)
            all {
                ts.size() > 0,
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s2' = led_process_transitions(g_ledger_s2, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }    

    action led2_step : bool =  any {
        led2_gc_step_with(led_main_listener),
        led2_read_2_act,
    }   

    action led2_read_2_act = {
        all {
            nondet simI_set = 1.to(10).powerset().oneOf() // TODO: determine appropriate size
            val simI = simI_set.toList((a,b) => EQ)
            nondet simPtr_prim = 1.to(10).oneOf() // TODO: what is the right range here?

            val ts = led_read_2(g_ledger_s2, g_clock_s, simI, simPtr_prim)
            all {
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s2' = led_process_transitions(g_ledger_s2, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }    

    val Parties = Set(1)
    val C = Set()
    val SIDs = Set(1)
    val P1 = Set(1)
    val P2 = Set(1)

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
        brd_init(Parties, C, SIDs, P1, P2),
        gc_init(C,0, Parties.map(x => SIDs.map(y => {party:x,sid:y})).flatten()),
        pki_init(Parties, 1.to(5), SIDs),

        dif_init(Parties, C, SIDs, 1),
        hash_init(Parties, SIDs, 1.to(5)),

        atms_init({

            P : Parties,
            C : C, 
            SIDs: SIDs, 
            t: 1,

            ALL_VKS: 1.to(5),
            ALL_SIGS: 1.to(5),
            ALL_AVKS: 1.to(5),
            ALL_ASIGS: 1.to(5),
        }),
        led_init(Parties, C, SIDs, 1, 1, 1, true, 1, 1, 1),
        all {
            g_ledger_s2' = led_initial(Parties, C, SIDs, 1, 1, 1, true, 1, 1, 1)
        }
    }

    val Q1 = p_bridge_s.log.size() <= 1
    val Q2 = p_bridge_s.state.bst.get(1).rot == None
    val Q3 = g_clock_s.state.TIME <= 2



}