module g_ledger{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock.* from "../g_clock/g_clock" 
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_ledger_types.* from "./g_ledger_types"
    
 
    var g_ledger_s : LED_System
    //const instance : int


    pure def led_skip_trans(led: LED_System) : LED_Transition = {
        {
            post_state: led.state,
            effects : Set(
                LED_UpdateInput(Set())
            )
        }
    }    

    // SIM gives control back to the environment
    pure def led_sim_loses_ctrl(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_sim_control_messages().map(c =>  // sim must have control
            {   
                post_state : led.state,
                effects : Set( 
                    LED_UpdateInput(Set()),
                    )  
            }).map(t => (t, gc_id_trans(clock)))
    }
 

    pure def led_submit_1(ledger : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition)] = {
        ledger.input.led_get_submit_request_messages().filter(p => led_access_check(p.cfg,ledger.params)).map(p => {

            val ledger_transition = {
                post_state: ledger.state,
                effects: Set(
                    LED_AppendHandle(SubmitTimeRequestHold(p)),
                    LED_LogMessage(SubmitRequest(p)),
                    LED_UpdateInput(Set())
                )
            }

            val gc_transition = {
                post_state: clock.state, 
                effects: Set(GC_UpdateInput(Set(TimeRequest({cfg: p.cfg}))))
            }

            (ledger_transition, gc_transition)

        })
    }

    pure def led_submit_2(ledger : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition)] = {
        clock.input.get_time_response_messages().map(q => {
            ledger.handles.led_get_submit_time_request_hold_messages().filter(p => p.cfg.nonce == q.cfg.nonce).map(p => {
                val t = q.time
                val eID = ledger.state.eID + 1
                val buffer = ledger.state.buffer.put(eID, ({tx: p.tx, time:t}))

                val ledger_transition = {
                    post_state: {...ledger.state, buffer: buffer, eID: eID},
                    effects: Set(
                        LED_RemoveHandle(SubmitTimeRequestHold(p)),
                        LED_AppendHandle(SubmitSIMHold({cfg: p.cfg, tx: p.tx, t: t, eID: eID})),
                        LED_UpdateInput(Set(LED_SIMControl))
                    )
                }

                val gc_transition = {
                    post_state: clock.state,
                    effects: Set(
                        GC_UpdateInput(Set())
                    )
                }
                (ledger_transition, gc_transition)
          })
        }).flatten()        
    }

    // simulator
    pure def led_submit_3(ledger : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition)] = {
        ledger.input.led_get_sim_control_messages().map(q => {
            ledger.handles.led_get_submit_sim_hold_messages().map(p => {
                {
                    post_state: ledger.state,
                    effects: Set(
                        LED_RemoveHandle(SubmitSIMHold(p)),
                        LED_UpdateInput(Set(SubmitSIMRelease(p)))
                    )
                }
            })
        }).flatten().map(t => (t, gc_id_trans(clock)))        
    }

    pure def led_submit_4(led : LED_System, clock : GC_System) : Set[(LED_Transition, GC_Transition) ] = {
        led.input.led_get_submit_sim_release_messages().map(p => {
            {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set(SubmitResponse(p))),
                )
            }
        }).map(t => (t,gc_id_trans(clock)))
    }

    pure def led_ok_1(led : LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_ok_request_messages().filter(p => led_access_check(p.cfg, led.params)).map(msg => {

            val led_transition = {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set()),
                    LED_AppendHandle(OKRequestWait(msg)),
                    LED_LogMessage(OKRequest(msg))
                )
            }

            val gc_transition = {
                post_state: clock.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({cfg: msg.cfg})))
                )
            }

            (led_transition, gc_transition)
        })
    }     

    pure def led_ok_2(led : LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.handles.led_get_ok_request_wait_messages().map(msg => {
            clock.input.get_okay_response_messages().filter(msg2 => msg2.cfg.nonce == msg.cfg.nonce).map(msg2 => {

                    val led_transition = {
                        post_state: led.state,
                        effects: Set(
                            LED_RemoveHandle(OKRequestWait(msg)),                            
                            LED_UpdateInput(Set(OKResponse(msg))),
                        )
                    }

                    val gc_transition = {
                        post_state: clock.state,
                        effects: Set(
                            GC_UpdateInput(Set())
                        )
                    }     

                    (led_transition, gc_transition)
            })
        }).flatten()
    }           

    pure def led_read_1(led : LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_read_request_messages().filter(p => led_access_check(p.cfg, led.params)).map(p => {
            {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set(LED_SIMControl)),
                    LED_AppendHandle(ReadSIMHold(p)),
                    LED_LogMessage(ReadRequest(p))
                )
            }
        }).map(t => (t, gc_id_trans(clock)))
    }
 
    pure def led_read_2(led : LED_System, clock: GC_System, simI : List[Pointer], simPtr_prim:Pointer) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_sim_control_messages().map(q => {  
            led.handles.led_get_read_sim_hold_messages().map(p => {
                {
                    post_state: led.state,
                    effects: Set(
                        LED_RemoveHandle(ReadSIMHold(p)),
                        LED_UpdateInput(Set(ReadSIMRelease({cfg:p.cfg, I: simI, ptr_prim: simPtr_prim}))) 
                    )
                }
            })
        }).flatten().map(t => (t, gc_id_trans(clock)))        
    }

    action led_read_2_act = {
        all {
            //nondet simI_set = apalache::generate(5) // TODO: determine appropriate size
            //val simI = simI_set.toList((a,b) => EQ)
            nondet simI_set = 1.to(10).powerset().oneOf() // TODO: determine appropriate size
            val simI = simI_set.toList((a,b) => EQ)
            nondet simPtr_prim = 1.to(10).oneOf() // TODO: what is the right range here?
            // val simI = List()
            // val simPtr_prim = -1

            val ts = led_read_2(g_ledger_s, g_clock_s, simI, simPtr_prim)
            all {
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s' = led_process_transitions(g_ledger_s, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }

    pure def led_read_3(led: LED_System, clock: GC_System) : Set[(LED_Transition,GC_Transition)] = {
        led.input.led_get_read_sim_release_messages().map(p => {

            val led_transition = {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set()),
                    LED_AppendHandle(ReadTimeRequestHold({cfg: p.cfg, I: p.I, ptr_prim: p.ptr_prim}))
                )
            }

            val gc_transition = {
                post_state: clock.state,
                effects: Set(
                    GC_UpdateInput(Set(TimeRequest({cfg: p.cfg})))
                )
            }

            (led_transition, gc_transition)
        })
    }

    pure def led_read_4(led: LED_System, clock: GC_System) : Set[(LED_Transition,GC_Transition)] = {
        clock.input.get_time_response_messages().map(q => {
            led.handles.led_get_read_time_request_hold_messages().filter(p => p.cfg.nonce == q.cfg.nonce).map(p => {

                val t = q.time
                val P = p.cfg.party
                val I = p.I    // simulators chosen I
                val ptr_prim = p.ptr_prim // simulator chosen ptr_prim
                
                val X = led_san_I(led, I, t)
                val I = X            

                val loop = I.foldl({v: led.state.Val, b: led.state.buffer, l:led.state.log}, (x, i) => {
                    val y = led_valid(x.v, x.l, x.b.get(i).tx)
                    if(y.valid){
                        {v:y.val_state, b: x.b.mapRemove(i), l:x.l.append({tx: x.b.get(i).tx, time: t})}
                    }else{
                        {v:y.val_state, b: x.b.mapRemove(i), l: x.l}
                    }
                })

                val log = loop.l
                val buffer = loop.b
                val validator_state = loop.v

                val ptrP = led_san_ptr(ptr_prim, P, led.params.delta_slack, log, led.state.ptr, t)
 
                val led_transition = {
                    if(log == List() or ptrP == None){
                        {
                            post_state: {...led.state, // ptr unchanged
                                buffer: buffer,
                                log: log,
                                Val: validator_state
                            },
                            effects: Set(
                                LED_RemoveHandle(ReadTimeRequestHold(p)),
                                LED_UpdateInput(Set(ReadResponse({cfg: p.cfg, result: List()})))
                            )
                        }
                    }else{
                        val ptr = led.state.ptr.put(P, ptrP.unwrap())
                        val result = log.mySlice(0,ptr.get(P)+1).listMap(b => b.tx) // TODO: check off-by-one

                        {
                            post_state: {...led.state, 
                                buffer: buffer,
                                log: log,
                                Val: validator_state,
                                ptr: ptr
                            },
                            effects: Set(
                                LED_RemoveHandle(ReadTimeRequestHold(p)),
                                LED_UpdateInput(Set(ReadResponse({cfg: p.cfg, result: result})))
                            )
                        }
                    }

                }

                val gc_transition = {
                    post_state: clock.state,
                    effects: Set(
                        GC_UpdateInput(Set())
                    )
                }

                (led_transition, gc_transition)
          })
        }).flatten()
    }

    pure def led_san_I(ledger: LED_System, I : List[Pointer], t : Time) : List[Pointer] = {
        val filteredI = I.listFilter(i => ledger.state.buffer.has(i))

        val readyI = ledger.state.buffer.keys().filter(i => {
             and {
                not (I.listHas(i)),
                t - ledger.state.buffer.get(i).time >= ledger.params.delta_latency
             }
        })

        val readyIList = readyI.toList(intCompare)

        filteredI.concat(readyIList)
    }

    pure def led_san_ptr(ptr_prim: Pointer, P : Party, delta_slack: int, log: LED_Log, ptr: Party -> Pointer, t : Time) : Option[Pointer] = {
        val maxPtr = log.indices().filterMap(idx => {
            val b = log[idx]
            if(t - b.time >= delta_slack) { 
                Some(idx)
            } else {
                None
            }
        }).union(Set(ptr_prim)).setMax(ptr_prim)   

        val new_ptr_prim_val = {
            val ptrP = if (ptr.has(P)) ptr.get(P) else maxPtr // todo check
            if(ptrP < maxPtr) maxPtr else ptrP
        }

        val new_ptr_prim_val_final = if (new_ptr_prim_val >= log.length()) (log.length() - 1) else new_ptr_prim_val 

        if (new_ptr_prim_val_final < 0){
            None
        }else{
            Some(new_ptr_prim_val_final)
        }
    }
    
    // None: instantiated to a particular function
    pure def led_valid(vallidator: LED_ValidatorState, log: LED_Log, tx: LED_Tx): {val_state: LED_ValidatorState, valid: bool} = {
        {val_state: vallidator, valid: log.indices().forall(i => log[i].tx != tx)} 
    }


   def led_main_listener(led: LED_System, clock: GC_System) : (Set[(LED_Transition, GC_Transition)]) = {
        Set(
            led_ok_1(led, clock),
            led_ok_2(led, clock),
            led_submit_1(led, clock),
            led_submit_2(led, clock),
            led_submit_3(led, clock),
            led_submit_4(led, clock),
            //led_read_1(led, clock),
            //led_read_2(led, clock), // a direct action instead
            //led_read_3(led, clock),
            //led_read_4(led, clock),

            //led_sim_loses_ctrl(led, clock) // wrong place
        ).flatten()
    }    
 

    pure def led_apply_effect(sys: LED_System, clock: GC_System, eff: LED_Effect): LED_System = {
        match eff {
        | LED_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg,  time: clock.state.TIME, state: sys.state}) 
            }
        | LED_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | LED_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | LED_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }

    pure def led_process_transitions(sys: LED_System, clock: GC_System, transition: LED_Transition) : LED_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => led_apply_effect(a1, clock, a2)))
    }     

    action led_gc_step_with(q: (LED_System, GC_System) => Set[(LED_Transition, GC_Transition)]): bool = {
        all{
            val ts = q(g_ledger_s, g_clock_s)
            all {
                ts.size() > 0,
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s' = led_process_transitions(g_ledger_s, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }    

    action led_step : bool =  any {
        led_gc_step_with(led_main_listener),
        //led_read_2_act,
    }    

    pure def led_initial(x : {P: Set[Party], C: Set[Party], SIDs: Set[SID], delta_latency: Time, delta_slack: Time, delta_apl: Time, Val: LED_ValidatorState, comsel_a: int, comsel_b: int, comsel_c: int}) : LED_System = {
        {
            params : {
                P : x.P,
                C : x.C,
                SIDs: x.SIDs,
                delta_latency: x.delta_latency,
                delta_slack: x.delta_slack,
                delta_apl: x.delta_apl,

                comsel_a: x.comsel_a,
                comsel_b: x.comsel_b,
                comsel_c: x.comsel_c,  
            },
            state : 
                {
                    buffer : Map(),
                    eID : 0,
                    log: List(),
                    ptr: Map(),
                    Val: x.Val,
                    Com: Map(),
                    APL: Map(),
                    genesis: 0,
                    mem : Map(),
                },
            input : Set(),
            nonce : 0,      
            log : Set(),
            handles : Set()
        }        
    }

    action led_init(P: Set[Party], C: Set[Party], SIDs: Set[SID], delta_latency: Time, delta_slack: Time, delta_apl: Time, Val: LED_ValidatorState, comsel_a: int, comsel_b: int, comsel_c: int) : bool =  all {
        g_ledger_s' = led_initial({P: P, C: C, SIDs: SIDs, delta_latency: delta_latency, delta_slack: delta_slack, delta_apl: delta_apl, Val: Val, comsel_a: comsel_a, comsel_b: comsel_b, comsel_c: comsel_c})
    }

}