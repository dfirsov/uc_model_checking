module p_bridge_led2{

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
    import p_bridge_types.* from "./p_bridge_types" 
    import p_bridge.* from "./p_bridge" 



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
}