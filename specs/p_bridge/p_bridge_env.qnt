module p_bridge_env{ 
    
    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import f_pki_types.* from "../f_pki/f_pki_types"    
    import f_pki.* from "../f_pki/f_pki"    
    import f_atms_types.* from "../f_atms/f_atms_types"    
    import f_atms.* from "../f_atms/f_atms"    
    import g_ledger_types.* from "../g_ledger/g_ledger_types" 
    import g_ledger.* from "../g_ledger/g_ledger" 
    import g_ledger_comsel.* from "../g_ledger/g_ledger_comsel"
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_clock.* from "../g_clock/g_clock"
    import p_bridge_types.* from "./p_bridge_types" 
    import p_bridge.* from "./p_bridge" 
    import p_bridge_bkgen.* from "./p_bridge_bkgen"     
    import p_bridge_rot.* from "./p_bridge_rot"
    


    val Parties = Set(1)
    val C = Set()
    val SIDs = Set(1)
    val P1 = Set(1)
    val P2 = Set(1)

    action brd_step_pure : bool = {
        all{
            val sys = {
                brd: p_bridge_s,
                clock: g_clock_s,
                atms: f_atms_s,
                pki: pki_s,
                ledger1: g_ledger_s,
            }
            val ts = brd_bkgen_main_listener(sys).union(brd_rot_main_listener(sys))
            all {
                nondet transition = ts.oneOf()
                val new_sys = brd_process_ctransitions(sys, transition)
                all {
                    p_bridge_s' = new_sys.brd,
                    g_clock_s' = new_sys.clock,
                    f_atms_s' = new_sys.atms,
                    pki_s' = new_sys.pki,
                    g_ledger_s' = new_sys.ledger1,              
                }
            }
        }
    }     

    action brd_step : bool =  all {
        brd_step_pure
    }    


    action step : bool = any {
        brd_step,
        all {
            gc_step,
            p_bridge_s' = p_bridge_s,
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            g_ledger_s' = g_ledger_s,
        },
        all {
            pki_step,
            p_bridge_s' = p_bridge_s,
            g_clock_s' = g_clock_s,
            f_atms_s' = f_atms_s,
            g_ledger_s' = g_ledger_s,
        },
        all {
            atms_step,
            p_bridge_s' = p_bridge_s,
            g_clock_s' = g_clock_s,
            pki_s' = pki_s,
            g_ledger_s' = g_ledger_s,
        },
        all{
            led_step_comsel,
            p_bridge_s' = p_bridge_s,
            //g_clock_s' = g_clock_s, // updated in led_step
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
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

        all {        
            nondet from = Parties.oneOf()
            nondet sid = SIDs.oneOf()

            val cfg = {party: from, sid: sid, nonce: p_bridge_s.nonce}
            any {
                p_bridge_bkgen_request(cfg),
                p_bridge_rot_request(cfg),
            },
            f_atms_s' = f_atms_s,
            pki_s' = pki_s,
            g_clock_s' = g_clock_s, 
            g_ledger_s' = g_ledger_s,                
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
    }

    action init : bool = all {
        brd_init(Parties, C, SIDs, P1, P2),
        gc_init(C,0, Parties.map(x => SIDs.map(y => {party:x,sid:y})).flatten()),
        pki_init(Parties, 1.to(5), SIDs),
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
    }

    val Q1 = p_bridge_s.log.size() <= 1
    val Q2 = p_bridge_s.state.aux.keys().size() == 0



}