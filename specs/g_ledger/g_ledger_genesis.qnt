module g_ledger_genesis {
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"


    pure def led_genesis_1(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.filterMap(m => match m {
            | GenesisRequest(p) => Some(p)
            | _ => None
        }).map(p => {
            if(led_access_check(p.cfg, led.params)){
                {
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set(LED_SIMControl)),
                        LED_AppendHandle(GenesisSIMHold({cfg: p.cfg})),
                        LED_LogMessage(GenesisRequest(p))
                    )
                }
            }else{
                {
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set()),
                    )
                }
            }
        }).map(t => (t, gc_id_trans(clock)))        
    }    


    pure def led_genesis_2(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.filterMap(m => match m {
            | LED_SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            led.handles.filterMap(h => match h {
                | GenesisSIMHold(p) => Some(p)
                | _ => None
            }).map(p => {
                {
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set(GenesisSIMRelease({cfg: p.cfg}))),
                        LED_RemoveHandle(GenesisSIMHold({cfg: p.cfg})),
                    )
                }
            })
        }).flatten().map(t => (t, gc_id_trans(clock)))
    }
 

    pure def led_genesis_3(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.filterMap(m => match m {
            | GenesisSIMRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set(GenesisResponse({cfg: p.cfg, genesis: led.state.genesis}))),
                )
            }
        }).map(t => (t, gc_id_trans(clock)))
    }



   def led_main_listener_genesis(led: LED_System, clock: GC_System) : (Set[(LED_Transition, GC_Transition)]) = {
        Set(
            led_genesis_1(led, clock),
            led_genesis_2(led, clock),
            led_genesis_3(led, clock),
        ).flatten()
    }

    
    action led_step_genesis : bool =  any {
        led_gc_step_with(led_main_listener_genesis),
    }     

}