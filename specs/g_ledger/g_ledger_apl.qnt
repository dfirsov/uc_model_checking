module g_ledger_apl {
    

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"


    pure def led_apl_1(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_apl_request_messages().map(p => {
            if(led_access_check(p.cfg, led.params)){
                {
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set(SIMControl)),
                        LED_AppendHandle(APLSIMHold({cfg: p.cfg, r: clock.state.TIME})),
                        LED_LogMessage(APLRequest(p))
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

    action led_apl_2_act(led: LED_System, clock: GC_System) : bool = {
        all {
            nondet sim_p = 1.to(100).oneOf()
            val ts = led_apl_2(led, clock, sim_p)
            all {
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s' = led_process_transitions(g_ledger_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }    

    pure def led_apl_2(led: LED_System, clock: GC_System, sim_p: Pointer) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_sim_control_messages().map(_ => {
            led.handles.led_get_apl_sim_hold_messages().map(p => {
                {
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set(APLSIMRelease({cfg: p.cfg, r: p.r, sim_p: sim_p}))),
                        LED_RemoveHandle(APLSIMHold({cfg: p.cfg, r: p.r})),
                    )
                }
            })
        }).flatten().map(t => (t, gc_id_trans(clock)))
    }

    def led_apl_san(p: Pointer) : Option[Pointer] = {
        val S = g_ledger_s.state.ptr.keys().filter(x => x.in(g_ledger_s.params.P)).map(x => g_ledger_s.state.ptr.get(x))

        if(S == Set()){
            None
        }else{
            if(led_apl_Clean(S, g_ledger_s.params.delta_apl, p)){
                Some(p)
            } else {
                0.to(100).find(new_p => led_apl_Clean(S, g_ledger_s.params.delta_apl, new_p))
            }
        }
    }   

    // TODO: check with Pooya
    pure def led_apl_Clean(S: Set[Pointer], delta: int, p: Pointer) : bool = {
        val min_S = S.setMin(p) 
        and {
            max(min_S - delta, 0) <= p,
            p <= min_S
        }
    }


    def led_apl_3(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_apl_sim_release_messages().map(p => {
            if(led.state.APL.has(p.r)){
                Set({
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set(APLResponse({cfg: p.cfg, r:p.r, p: led.state.APL.get(p.r)}))),
                    )
                })
            }else{
                val maybe_p = led_apl_san(p.sim_p)
                if(maybe_p != None){
                    val new_p = maybe_p.unwrap()

                    Set({
                        post_state: {...led.state, 
                            APL: led.state.APL.put(p.r, new_p)
                        },
                        effects: Set(
                            LED_UpdateInput(Set(APLResponse({cfg: p.cfg, r:p.r, p: new_p})))
                        )
                    })
                }else{
                    Set({
                        post_state: led.state,
                        effects: Set(
                            LED_UpdateInput(Set())
                        )
                    })
                }
            }
        }).flatten().map(t => (t, gc_id_trans(clock)))
    }

   def led_main_listener_apl(led: LED_System, clock: GC_System) : (Set[(LED_Transition, GC_Transition)]) = {
        Set(
            led_apl_1(led, clock),
            led_apl_3(led, clock),
        ).flatten()
    }

    
    action led_step_apl : bool =  any {
        led_gc_step_with(led_main_listener_apl),
        led_apl_2_act(g_ledger_s, g_clock_s)
    }     

}